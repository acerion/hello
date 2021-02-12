/*
 * File: gif.c
 *
 * Copyright (C) 1997 Raph Levien <raph@acm.org>
 * Copyright (C) 2000-2007 Jorge Arellano Cid <jcid@dillo.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 */

/*
 * The GIF decoder for dillo. It is responsible for decoding GIF data
 * and transferring it to the dicache.
 * https://www.w3.org/Graphics/GIF/spec-gif89a.txt
 */


/* Notes 13 Oct 1997 --RLL
 *
 * Today, just for the hell of it, I implemented a new decoder from
 * scratch. It's oriented around pushing bytes, while the old decoder
 * was based around reads which may suspend. There were basically
 * three motivations.
 *
 * 1. To increase the speed.
 *
 * 2. To fix some bugs I had seen, most likely due to suspension.
 *
 * 3. To make sure that the code had no buffer overruns or the like.
 *
 * 4. So that the code could be released under a freer license.
 *
 * Let's see how we did on speed. I used a large image for testing
 * (fvwm95-2.gif).
 *
 * The old decoder spent a total of about 1.04 seconds decoding the
 * image. Another .58 seconds went into Image_line, almost
 * entirely conversion from colormap to RGB.
 *
 * The new decoder spent a total of 0.46 seconds decoding the image.
 * However, the time for Image_line went up to 1.01 seconds.
 * Thus, even though the decoder seems to be about twice as fast,
 * the net gain is pretty minimal. Could this be because of cache
 * effects?
 *
 * Lessons learned: The first, which I keep learning over and over, is
 * not to try to optimize too much. It doesn't work. Just keep things
 * simple.
 *
 * Second, it seems that the colormap to RGB conversion is really a
 * significant part of the overall time. It's possible that going
 * directly to 16 bits would help, but that's optimization again :)
 */


/* TODO:
 * + Make sure to handle error cases gracefully (including aborting the
 * connection, if necessary).
 */

#include <config.h>
#include <stdbool.h>
#include <stdio.h>              /* for sprintf */
#include <string.h>             /* for memcpy and memmove */

#include "msg.h"
#include "image.hh"
#include "cache.h"
#include "dicache.h"




#define LM_to_uint(a,b)   ((((uchar_t)b)<<8)|((uchar_t)a))

#define MAX_COLORMAP_SIZE     256
#define MAX_LWZ_BITS           12
#define INTERLACE            0x40

#define ImageSeparator        (0x2c)
#define Trailer               (0x3B)
#define ExtensionIntroducer   (0x21) /* First byte of Extension. "23. Graphic Control Extension." */




typedef enum {
   InitialState    = 0,
   State1          = 1,
   State2          = 2,
   StatePixelsData = 3,
   InvalidState    = 999,
} ParserState;


typedef struct img_chunk {
   uchar_t * buf;   /* New data received, but not processed yet. */
   int size;        /* Size of the new data. */
} img_chunk;


typedef struct {
   /* Where the next unprocessed part of data starts. */
   int start_offset;
   ParserState parser_state;

   DilloImage *Image;
   DilloUrl *url;
   int version;

   uint_t Flags;
   bool interlace;

   uchar_t input_code_size;
   uchar_t *row_data;
   int pass;

   uint_t row_number; /* Row number of decoded image. */

   /* state for lwz_read_byte */
   int code_size;

   /* The original GifScreen from giftopnm */
   uint_t Width;
   uint_t Height;

   bool global_color_map_present;
   uchar_t global_color_map[3 * MAX_COLORMAP_SIZE]; /* "sequence of bytes representing red-green-blue color triplets. */
   uint_t global_color_map_triplets_count; /* Number of color triplets in color map. */

   bool local_color_map_present;
   uchar_t local_color_map[3 * MAX_COLORMAP_SIZE]; /* "sequence of bytes representing red-green-blue color triplets. */
   uint_t local_color_map_triplets_count; /* Number of color triplets in color map. */


   uint_t color_resolution; /* "18. Logical Screen Descriptor." -> "<Packed Fields>" -> "Color Resolution" */

   int    Background;
   uint_t spill_line_index;
#if 0
   uint_t AspectRatio;    /* AspectRatio (not used) */
#endif


   /* [1] "23. Graphic Control Extension.", Required version: Gif89. */
   int transparent_color_index;
#if 1
   int delay_time;
   int user_input_flag;
   int disposal_method;
#endif


   /* state for the new push-oriented decoder */
   int packet_size;       /* The amount of the data block left to process */
   uint_t window;
   int bits_in_window;
   uint_t last_code;        /* Last "compressed" code in the look up table */
   uint_t line_index;
   uchar_t **spill_lines;
   int num_spill_lines_max;
   int length[(1 << MAX_LWZ_BITS) + 1];
   int code_and_byte[(1 << MAX_LWZ_BITS) + 1];
} DilloGif;

/* Some invariants:
 *
 * last_code <= code_mask
 *
 * code_and_byte is stored packed: (code << 8) | byte
 */


typedef bool (* extension_sub_block_handler_t)(DilloGif *gif, img_chunk chunk);
/*
 * Forward declarations
 */
static int gif_write(DilloGif *gif, img_chunk chunk);
static void gif_close(DilloGif *gif, CacheClient_t *Client);
static void gif_free(DilloGif *gif);

static int do_image_descriptor2(DilloGif * gif, img_chunk chunk);
static int gif_do_extension(DilloGif * gif, img_chunk chunk);

static int gif_try_consume_extension_generic(DilloGif *gif, img_chunk chunk, extension_sub_block_handler_t sub_block_data_handler);
static bool handle_extension_sub_block_graphic_control(DilloGif * gif, img_chunk chunk);
static bool handle_extension_sub_block_comment(DilloGif * gif, img_chunk chunk);

static int peek_lzw(DilloGif * gif, img_chunk img_descriptor);


static bool color_table_is_present(uchar_t flags)
{
   const uchar_t mask = 0x80; /* Mask for 'Color Table Flag' bit (both Global and Local color table). */
   return flags & mask;
}


static int get_color_table_triplets_count(uchar_t flags)
{
   /* "To determine that actual size of the color table, raise 2 to [the
      value of the field + 1]". */
   return 1 << ((flags & 0x07) + 1);
}


static void img_chunk_forward(img_chunk * chunk, int n)
{
   chunk->buf += n;
   chunk->size -= n;
}


static void img_chunk_dump_at(img_chunk chunk, const char * where)
{
   for (int i = -6; i < 6; i++) {
      fprintf(stderr, "        dump at '%s': '%02x'%s\n", where, chunk.buf[i], i == 0 ? " @0" : "");
   }
}


/*
 * Create a new gif structure for decoding a gif into a RGB buffer
 */
void *a_Gif_new(DilloImage *Image, DilloUrl *url, int version)
{
   DilloGif * gif = malloc(sizeof(DilloGif));
   memset(gif, 0, sizeof (DilloGif));

   gif->parser_state = InitialState;
   gif->Image = Image;
   gif->url = url;
   gif->version = version;
   gif->Background = Image->bg_color;
   gif->transparent_color_index = -1;

   return gif;
}

/*
 * Free the gif-decoding data structure.
 */
static void gif_free(DilloGif *gif)
{
   int i;

   _MSG("gif_free: gif=%p\n", gif);

   dFree(gif->row_data);
   if (gif->spill_lines != NULL) {
      for (i = 0; i < gif->num_spill_lines_max; i++)
         dFree(gif->spill_lines[i]);
      dFree(gif->spill_lines);
   }
   dFree(gif);
}

/*
 * This function is a cache client, it receives data from the cache
 * and dispatches it to the appropriate gif-processing functions
 */
void a_Gif_callback(int Op, void *data)
{
   if (Op == CacheOperationAddData) {
      CacheClient_t *Client = data;

      /* Sanity checks */
      if (!Client->Buf || Client->BufSize == 0)
         return;

      DilloGif * gif = Client->CbData;
      /* Beginning of all image data received so far. Part of it has already
         been parsed and sent to cache. */
      const uchar_t * all_data = (uchar_t *) Client->Buf;
      /* Total size of all of the image data received so far. */
      const int all_size = Client->BufSize;
      img_chunk chunk = {
         .buf  = all_data + gif->start_offset,
         .size = all_size - gif->start_offset
      };

      MSG("GIF: %d bytes in new chunk\n", chunk.size);
      /* Process the bytes in the input buffer. */
      const int consumed_size = gif_write(gif, chunk);
      if (consumed_size > 0) {
         gif->start_offset += consumed_size;
         MSG("GIF: consumed %d bytes\n", consumed_size);
      }

   } else if (Op == CacheOperationClose) {
      CacheClient_t *Client = data;
      gif_close(Client->CbData, Client);
   } else if (Op == CacheOperationAbort) {
      gif_free(data);
   }
}

/*
 * Finish the decoding process (and free the memory)
 */
static void gif_close(DilloGif *gif, CacheClient_t *Client)
{
   _MSG("gif_close: destroy gif %p\n", gif);
   a_Dicache_close(gif->url, gif->version, Client);
   gif_free(gif);
}


/* --- GIF Extensions ----------------------------------------------------- */



static inline int gif_data_sub_block_get_available_bytes(img_chunk chunk)
{
   if (0 == chunk.size) {
      /* There is not enough data for any kind of parsing. */
      fprintf(stderr, "Available data 0 = %u\n", 0);
      return -1;
   }

   const uchar_t sub_block_size = chunk.buf[0];
   if (0x00 == sub_block_size) {
      /* [1] "16. Block Terminator." */
      fprintf(stderr, "Available data 1 = %u\n", 0);
      return 0;
   }

   if (sub_block_size <= chunk.size) {
      fprintf(stderr, "Available data 2 = %u\n", sub_block_size);
      return sub_block_size;
   } else {
      fprintf(stderr, "Available data 3 = %u, chunk size = %u\n", sub_block_size, chunk.size);
      return -1;
   }
}



static bool handle_extension_sub_block_graphic_control(DilloGif * gif, img_chunk sub_block)
{
   const int sub_block_size = sub_block.buf[0];
   /* TODO: we can/should peek behind the end of this block and verify that
      there is a Block Terminator right after this sub-block. The GIF spec
      tells us clearly that there is only one non-empty sub-block, followed
      by Block Terminator. */

   const uchar_t flags = sub_block.buf[1];

#if 1
   gif->disposal_method = (flags >> 2) & 0x7;
   gif->user_input_flag = (flags >> 1) & 0x1;
   gif->delay_time = LM_to_uint(sub_block.buf[2], sub_block.buf[3]);
#endif

   /* Transparent color index, may not be valid (unless flag is set) */
   const bool transparent_color_flag = flags & 0x01;
   if (transparent_color_flag) {
      gif->transparent_color_index = sub_block.buf[4];
   }

   fprintf(stderr,
           "disposal_method         = %02x\n"
           "user_input_flag         = %02x\n"
           "delay_time              = %02x\n"
           "transparent_color_index = %02x\n",
           gif->disposal_method,
           gif->user_input_flag,
           gif->delay_time,
           gif->transparent_color_index);


   return true;
}


static bool handle_extension_sub_block_comment(DilloGif * gif, img_chunk sub_block)
{
   // return true; /* Enable this statement if you don't want to process and print comment. */

   const int sub_block_size = sub_block.buf[0];

   /* Since this is a comment extension, we can try to print the comment as text. */
   const uchar_t * string_start = sub_block.buf + 1; /* Skip size byte. */
   char buf[256] = { 0 }; /* Sub-block has no more than 256 bytes (not including sub-block size byte). */
   memcpy(buf, string_start, sub_block_size);
   fprintf(stderr, "Comment Extension:  sub-block size = %d, comment = '%s'\n", sub_block_size, buf);

   return true;
}



/*
  Try to parse all bytes from any Extension, from Extension Introducer to
  Block Terminator, inclusive.
*/
static int gif_try_consume_extension_generic(DilloGif * gif, img_chunk extension_chunk, extension_sub_block_handler_t sub_block_data_handler)
{
   int consumed_size = 0;
   if (extension_chunk.size < 3) {
      /* Not enough data for Extension Introducer, Extension Label and at
         least Block Terminator. */
      return consumed_size;
   }

   img_chunk sub_block = extension_chunk;
   img_chunk_forward(&sub_block, 2); /* Skip Extension Introducer and Extension Label. */
   consumed_size += 2;

#if 0
   for (int i = 0; i < 20; i++) {
      fprintf(stderr, "sub-block pointer: 0x%02x, %c\n", sub_block.buf[i], sub_block.buf[i]);
   }
#endif

   int sub_block_size = 0;
   const int block_terminator_size = 0x00; /* Size of sub-block indicated by Block Terminator is zero. */
   int sub_blocks_counter = 0;
   while ((sub_block_size = gif_data_sub_block_get_available_bytes(sub_block)) != -1) {

      if (sub_block_size > 1 && sub_block_data_handler) {
         sub_block_data_handler(gif, sub_block);
      }

      /* Skip byte size in this sub-block. */
      img_chunk_forward(&sub_block, 1);
      consumed_size += 1;

      /* Skip sub-block data (perhaps zero bytes of value in Block
         Terminator). */
      img_chunk_forward(&sub_block, sub_block_size);
      consumed_size += sub_block_size;

      if (block_terminator_size == sub_block_size) {
         fprintf(stderr, "sub-block #%d is Block Terminator, stopping processing of this extension\n", sub_blocks_counter);
         break;
      } else {
         fprintf(stderr, "sub-block #%d is not Block Terminator, continuing with parsing of this extension\n", sub_blocks_counter);
      }
      sub_blocks_counter++;
   }

   if (block_terminator_size == sub_block_size) {
      /* Encountered Block Terminator, so this extension was parsed
         correctly, from beginning to end. */
      fprintf(stderr, "returning consumed size = %d\n", consumed_size);
      return consumed_size;
   } else {
      /* There was not enough data to parse this extension. */
      fprintf(stderr, "returing consumed size = %d\n", 0);
      return 0;
   }
}



#define ExtensionTypeGraphicControl   (0xf9) /* "23. Graphic Control Extension." */
#define ExtensionTypeComment          (0xfe) /* "24. Comment Extension." */
#define ExtensionTypePlainText        (0x01) /* "25. Plain Text Extension." */
#define ExtensionTypeApplication      (0xff) /* "26. Application Extension." */


/*
 */
static int gif_do_extension_sub(DilloGif * gif, img_chunk extension_chunk)
{
   /* Get extension label. */
   const uchar_t extension_introducer = extension_chunk.buf[0];
   const uchar_t extension_label      = extension_chunk.buf[1];

   int extension_size = 0;

   switch (extension_label) {
   case ExtensionTypeGraphicControl:
      fprintf(stderr, "Extension Type Graphic Control\n");
      /*
        Try to parse all bytes shown in [1] "23. Graphic Control Extension.",
        from Extension Introducer to Block Terminator, inclusive.
      */
      extension_size = gif_try_consume_extension_generic(gif, extension_chunk, handle_extension_sub_block_graphic_control);
      break;

   case ExtensionTypeComment:
      fprintf(stderr, "Extension Type Comment\n");
      /*
        Try to parse all bytes shown in [1] "24. Comment Extension.", from
        Extension Introducer to Block Terminator, inclusive.
      */
      extension_size = gif_try_consume_extension_generic(gif, extension_chunk, handle_extension_sub_block_comment);
      break;

   case ExtensionTypePlainText:
      fprintf(stderr, "Extension Type Plain Text\n");
      extension_size = gif_try_consume_extension_generic(gif, extension_chunk, NULL);    /*Ignore Extension */
      break;

   case ExtensionTypeApplication:
      fprintf(stderr, "Extension Type Application\n");
      extension_size = gif_try_consume_extension_generic(gif, extension_chunk, NULL);    /*Ignore Extension */
      break;

   default:
      fprintf(stderr, "Extension Type unhandled\n");
      extension_size = gif_try_consume_extension_generic(gif, extension_chunk, NULL);    /*Ignore Extension */
      break;
   }

   return extension_size;
}

/* --- General Image Decoder ----------------------------------------------- */
/* Here begins the new push-oriented decoder. */

/*
 * ?
 */
static void Gif_lwz_init(DilloGif *gif)
{
   gif->num_spill_lines_max = 1;
   gif->spill_lines = malloc(sizeof(uchar_t *) * gif->num_spill_lines_max);

   gif->spill_lines[0] = malloc(gif->Width);
   gif->bits_in_window = 0;

   /* First code in table = clear_code +1
    * Last code in table = first code in table
    * clear_code = (1<< input code size)
    */
   gif->last_code = (1 << gif->input_code_size) + 1;
   memset(gif->code_and_byte, 0,
          (1 + gif->last_code) * sizeof(gif->code_and_byte[0]));
   gif->code_size = gif->input_code_size + 1;
   gif->line_index = 0;
}

/*
 * Send the image line to the dicache, also handling the interlacing.
 */
static void Gif_emit_line(DilloGif *gif, const uchar_t *row_data)
{
   a_image_cache_add_row(gif->url, gif->version, row_data, gif->row_number);
   if (gif->interlace) {
      switch (gif->pass) {
      case 0:
      case 1:
         gif->row_number += 8;
         break;
      case 2:
         gif->row_number += 4;
         break;
      case 3:
         gif->row_number += 2;
         break;
      }
      if (gif->row_number >= gif->Height) {
         gif->pass++;
         switch (gif->pass) {
         case 1:
            gif->row_number = 4;
            break;
         case 2:
            gif->row_number = 2;
            break;
         case 3:
            gif->row_number = 1;
            break;
         default:
            /* arriving here is an error in the input image. */
            gif->row_number = 0;
            break;
         }
      }
   } else {
      if (gif->row_number < gif->Height)
         gif->row_number++;
   }
}

/*
 * Decode the packetized lwz bytes
 */
static void Gif_literal(DilloGif *gif, uint_t code)
{
   gif->row_data[gif->line_index++] = code;
   if (gif->line_index >= gif->Width) {
      Gif_emit_line(gif, gif->row_data);
      gif->line_index = 0;
   }
   gif->length[gif->last_code + 1] = 2;
   gif->code_and_byte[gif->last_code + 1] = (code << 8);
   gif->code_and_byte[gif->last_code] |= code;
}

/*
 * ?
 */
/* Profiling reveals over half the GIF time is spent here: */
static void Gif_sequence(DilloGif *gif, uint_t code)
{
   uint_t o_index, o_size, orig_code;
   uint_t sequence_length = gif->length[code];
   uint_t line_index = gif->line_index;
   int num_spill_lines;
   int spill_line_index = gif->spill_line_index;
   uchar_t *last_byte_ptr, *obuf;

   gif->length[gif->last_code + 1] = sequence_length + 1;
   gif->code_and_byte[gif->last_code + 1] = (code << 8);

   /* We're going to traverse the sequence backwards. Thus,
    * we need to allocate spill lines if the sequence won't
    * fit entirely within the present scan line. */
   if (line_index + sequence_length <= gif->Width) {
      num_spill_lines = 0;
      obuf = gif->row_data;
      o_index = line_index + sequence_length;
      o_size = sequence_length - 1;
   } else {
      num_spill_lines = (line_index + sequence_length - 1) /
          gif->Width;
      o_index = (line_index + sequence_length - 1) % gif->Width + 1;
      if (num_spill_lines > gif->num_spill_lines_max) {
         /* Allocate more spill lines. */
         spill_line_index = gif->num_spill_lines_max;
         gif->num_spill_lines_max = num_spill_lines << 1;
         gif->spill_lines = dRealloc(gif->spill_lines,
                                      gif->num_spill_lines_max *
                                      sizeof(uchar_t *));

         for (; spill_line_index < gif->num_spill_lines_max;
              spill_line_index++)
            gif->spill_lines[spill_line_index] =
                malloc(gif->Width);
      }
      spill_line_index = num_spill_lines - 1;
      obuf = gif->spill_lines[spill_line_index];
      o_size = o_index;
   }
   gif->line_index = o_index;   /* for afterwards */

   /* for fixing up later if last_code == code */
   orig_code = code;
   last_byte_ptr = obuf + o_index - 1;

   /* spill lines are allocated, and we are clear to
    * write. This loop does not write the first byte of
    * the sequence, however (last byte traversed). */
   while (sequence_length > 1) {
      sequence_length -= o_size;
      /* Write o_size bytes to
       * obuf[o_index - o_size..o_index). */
      for (; o_size > 0 && o_index > 0; o_size--) {
         uint_t code_and_byte = gif->code_and_byte[code];

         _MSG("%d ", gif->code_and_byte[code] & 255);

         obuf[--o_index] = code_and_byte & 255;
         code = code_and_byte >> 8;
      }
      /* Prepare for writing to next line. */
      if (o_index == 0) {
         if (spill_line_index > 0) {
            spill_line_index--;
            obuf = gif->spill_lines[spill_line_index];
            o_size = gif->Width;
         } else {
            obuf = gif->row_data;
            o_size = sequence_length - 1;
         }
         o_index = gif->Width;
      }
   }
   /* Ok, now we write the first byte of the sequence. */
   /* We are sure that the code is literal. */
   _MSG("%d", code);
   obuf[--o_index] = code;
   gif->code_and_byte[gif->last_code] |= code;

   /* Fix up the output if the original code was last_code. */
   if (orig_code == gif->last_code) {
      *last_byte_ptr = code;
      _MSG(" fixed (%d)!", code);
   }
   _MSG("\n");

   /* Output any full lines. */
   if (gif->line_index >= gif->Width) {
      Gif_emit_line(gif, gif->row_data);
      gif->line_index = 0;
   }
   if (num_spill_lines) {
      if (gif->line_index)
         Gif_emit_line(gif, gif->row_data);
      for (spill_line_index = 0;
           spill_line_index < num_spill_lines - (gif->line_index ? 1 : 0);
           spill_line_index++)
         Gif_emit_line(gif, gif->spill_lines[spill_line_index]);
   }
   if (num_spill_lines) {
      /* Swap the last spill line with the gif line, using
       * row_data as the swap temporary. */
      uchar_t *row_data = gif->spill_lines[num_spill_lines - 1];

      gif->spill_lines[num_spill_lines - 1] = gif->row_data;
      gif->row_data = row_data;
   }
   gif->spill_line_index = spill_line_index;
}

/*
 * ?
 *
 * Return Value:
 *   2 -- quit
 *   1 -- new last code needs to be done
 *   0 -- okay, but reset the code table
 *   < 0 on error
 *   -1 if the decompression code was not in the lookup table
 */
static int Gif_process_code(DilloGif *gif, uint_t code, uint_t clear_code)
{

   /* A short table describing what to do with the code:
    * code < clear_code  : This is uncompressed, raw data
    * code== clear_code  : Reset the decompression table
    * code== clear_code+1: End of data stream
    * code > clear_code+1: Compressed code; look up in table
    */
   if (code < clear_code) {
      /* a literal code. */
      _MSG("literal\n");
      Gif_literal(gif, code);
      return 1;
   } else if (code >= clear_code + 2) {
      /* a sequence code. */
      if (code > gif->last_code)
         return -1;
      Gif_sequence(gif, code);
      return 1;
   } else if (code == clear_code) {
      /* clear code. Resets the whole table */
      _MSG("clear\n");
      return 0;
   } else {
      /* end code. */
      _MSG("end\n");
      return 2;
   }
}

/*
 * ?
 */
static int Gif_decode(DilloGif *gif, img_chunk chunk)
{
   img_chunk_forward(&chunk, 1); /* First byte was already inspected by peek_lzw(). */

   const uchar_t *buf = chunk.buf;
   int bsize = chunk.size;
   /*
    * Data block processing.  The image stuff is a series of data blocks.
    * Each data block is 1 to 256 bytes long.  The first byte is the length
    * of the data block.  0 == the last data block.
    */

   int bufsize = bsize;

   /* Want to get all inner loop state into local variables. */
   int packet_size = gif->packet_size;
   uint_t window = gif->window;
   int bits_in_window = gif->bits_in_window;
   int code_size = gif->code_size;
   uint_t code_mask = (1 << code_size) - 1;
   uint_t clear_code = 1 << gif->input_code_size;

   /* If packet size == 0, we are at the start of a data block.
    * The first byte of the data block indicates how big it is (0 == last
    * datablock)
    * packet size is set to this size; it indicates how much of the data block
    * we have left to process.
    */
   while (bufsize > 0) {
      /* lwz_bytes is the number of remaining lwz bytes in the packet. */
      int lwz_bytes = MIN(packet_size, bufsize);

      bufsize -= lwz_bytes;
      packet_size -= lwz_bytes;
      for (; lwz_bytes > 0; lwz_bytes--) {
         /* printf ("%d ", *buf) would print the depacketized lwz stream. */

         /* Push the byte onto the "end" of the window (MSB).  The low order
          * bits always come first in the LZW stream. */
         window = (window >> 8) | (*buf++ << 24);
         bits_in_window += 8;

         while (bits_in_window >= code_size) {
            /* Extract the code.  The code is code_size (3 to 12) bits long,
             * at the start of the window */
            uint_t code = (window >> (32 - bits_in_window)) & code_mask;

            _MSG("code = %d, ", code);

            bits_in_window -= code_size;
            switch (Gif_process_code(gif, code, clear_code)) {
            case 1:             /* Increment last code */
               gif->last_code++;
               /*gif->code_and_byte[gif->last_code+1]=0; */

               if ((gif->last_code & code_mask) == 0) {
                  if (gif->last_code == (1 << MAX_LWZ_BITS))
                     gif->last_code--;
                  else {
                     code_size++;
                     code_mask = (1 << code_size) - 1;
                  }
               }
               break;

            case 0:         /* Reset codes size and mask */
               gif->last_code = clear_code + 1;
               code_size = gif->input_code_size + 1;
               code_mask = (1 << code_size) - 1;
               break;

            case 2:         /* End code... consume remaining data chunks..? */
               goto error;  /* Could clean up better? */
            default:
               MSG("Gif_decode: error!\n");
               goto error;
            }
         }
      }

      /* We reach here if
       * a) We have reached the end of the data block;
       * b) we ran out of data before reaching the end of the data block
       */
      if (bufsize <= 0)
         break;                 /* We are out of data; */

      /* Start of new data block */
      bufsize--;
      if (!(packet_size = *buf++)) {
         /* This is the "block terminator" -- the last data block */
         gif->parser_state = InvalidState;     /* BUG: should Go back to getting GIF blocks. */
         break;
      }
   }

   gif->packet_size = packet_size;
   gif->window = window;
   gif->bits_in_window = bits_in_window;
   gif->code_size = code_size;
   return bsize - bufsize;

 error:
   gif->parser_state = InvalidState;
   return bsize - bufsize;
}

/*
 * ?
 */
static int gif_parse_header(img_chunk chunk)
{
   const int header_size = 6;
   if (chunk.size < header_size) {
      return 0;
   }

   if (memcmp(chunk.buf, "GIF87a", header_size) != 0 &&
       memcmp(chunk.buf, "GIF89a", header_size) != 0) {
      return -1;
   } else {
      return header_size;
   }
}

static inline int gif_get_color_table(DilloGif * gif, bool global, img_chunk chunk, int color_table_triplets_count)
{
   if (global) {
      gif->global_color_map_present = false;
   } else {
      gif->local_color_map_present = false;
   }

   const int color_table_bytes_count = 3 * color_table_triplets_count;
   if (color_table_bytes_count > chunk.size) {
      return 0;
   }

   if (global) {
      gif->global_color_map_triplets_count = color_table_triplets_count;
      memcpy(gif->global_color_map, chunk.buf, color_table_bytes_count);
      gif->global_color_map_present = true;
   } else {
      gif->local_color_map_triplets_count = color_table_triplets_count;
      memcpy(gif->local_color_map, chunk.buf, color_table_bytes_count);
      gif->local_color_map_present = true;
   }
   return color_table_bytes_count;
}

/**
   This implements, from the spec:
   <Logical Screen> ::= Logical Screen Descriptor [Global Color Table]

   This block is required by GIF spec.

   @return count of bytes consumed from input data part
*/
static int gif_parse_logical_screen_descriptor(DilloGif *gif, img_chunk descriptor)
{
   int consumed_size = 0;

   /* screen descriptor */
   int block_size = 7;
   if (descriptor.size < block_size)
      return 0;

   const uchar_t flags = descriptor.buf[4]; /* "<Packed Fields>" from "18. Logical Screen Descriptor." */
   const bool global_color_table_is_present = color_table_is_present(flags);
   if (global_color_table_is_present) {
#if 0
      gif->Background = descriptor.buf[5];
#endif
   }

#if 0
   gif->color_resolution = (((descriptor.buf[4] & 0x70) >> 3) + 1);
   gif->Width    = LM_to_uint(descriptor.buf[0], descriptor.buf[1]);
   gif->Height   = LM_to_uint(descriptor.buf[2], descriptor.buf[3]);
   gif->AspectRatio         = descriptor.buf[6];
#endif
   img_chunk_forward(&descriptor, block_size);
   consumed_size += block_size;


   if (global_color_table_is_present) {
      fprintf(stderr, "Detected Global Color Table\n");
      const int color_table_triplets_count = get_color_table_triplets_count(flags);
      const int color_table_size = gif_get_color_table(gif, true, descriptor, color_table_triplets_count);
      if (0 == color_table_size) {
         return 0;
      }

      img_chunk_forward(&descriptor, color_table_size);
      consumed_size += color_table_size;
   }

   gif->parser_state = State2;
   return consumed_size;
}



static int gif_do_image_descriptor_header(DilloGif * gif, const img_chunk image_descriptor)
{
   const uchar_t image_separator = image_descriptor.buf[0];
   if (image_separator != ImageSeparator) {
      fprintf(stderr, "%s:%d\n", __func__, __LINE__);
      return 0;
   }

#if 0
   if (bsize < 10)
      return 0;
#endif

   gif->Width  = LM_to_uint(image_descriptor.buf[5], image_descriptor.buf[6]);
   gif->Height = LM_to_uint(image_descriptor.buf[7], image_descriptor.buf[8]);

   /* check max image size */
   if (gif->Width <= 0 || gif->Height <= 0 ||
       gif->Width > IMAGE_MAX_AREA / gif->Height) {
      MSG("gif_do_image_descriptor: suspicious image size request %u x %u\n",
          gif->Width, gif->Height);
      gif->parser_state = InvalidState;
      return 0;
   }

   gif->Flags = image_descriptor.buf[9];
   gif->interlace = gif->Flags & INTERLACE;

   return 10;
}

/*
 * This implements, from the spec:
 * <Table-Based Image> ::= Image Descriptor [Local Color Table] Image Data
 *
 * ('Buf' points to just after the Image separator)
 * we should probably just check that the local stuff is consistent
 * with the stuff at the header. For now, we punt...
 */
static int gif_do_image_descriptor(DilloGif *gif, img_chunk img_descriptor)
{
   int total_size = 0;

   const int image_descriptor_size = gif_do_image_descriptor_header(gif, img_descriptor);
   if (0 == image_descriptor_size) {
      return 0;
   }
   img_chunk_forward(&img_descriptor, image_descriptor_size);
   total_size += image_descriptor_size;

   /** \todo Gamma for GIF? */
   a_Dicache_set_parms(gif->url, gif->version, gif->Image,
                       gif->Width, gif->Height, DILLO_IMG_TYPE_INDEXED,
                       1 / 2.2);
   gif->Image = NULL; /* safeguard: hereafter it may be freed by its owner */
   gif->pass = 0;

   if (color_table_is_present(gif->Flags)) {
      fprintf(stderr, "Detected Local Color Table\n");
      const int color_table_triplets_count = get_color_table_triplets_count(gif->Flags);
      const int consumed_size = gif_get_color_table(gif, false, img_descriptor, color_table_triplets_count);
      if (0 == consumed_size) {
         return 0;
      }
      img_chunk_forward(&img_descriptor, consumed_size);
      total_size += consumed_size;
   }

   if (gif->local_color_map_present) {
      /* TODO: use local color map. */
   } else {
      a_Dicache_set_color_map(gif->url, gif->version, gif->Background,
                              gif->global_color_map,
                              gif->global_color_map_triplets_count, MAX_COLORMAP_SIZE, gif->transparent_color_index);
   }

   return total_size;
}

/* --- Top level data block processors ------------------------------------ */
/*
 * This identifies which kind of GIF blocks are next, and processes them.
 * It returns if there isn't enough data to process the next blocks, or if
 * the next block is the lzw data (which is streamed differently)
 *
 * This implements, from the spec, <Data>* Trailer
 * <Data> ::= <Graphic Block> | <Special-Purpose Block>
 * <Special-Purpose Block> ::= Application Extension | Comment Extension
 * <Graphic Block> ::= [Graphic Control Extension] <Graphic-Rendering Block>
 * <Graphic-Rendering Block> ::= <Table-Based Image> | Plain Text Extension
 *
 * <Data>* --> GIF_Block
 * <Data>  --> while (...)
 * <Special-Purpose Block>   --> gif_do_extension
 * Graphic Control Extension --> gif_do_extension
 * Plain Text Extension      --> gif_do_extension
 * <Table-Based Image>       --> gif_do_image_descriptor
 *
 * Return Value
 * 0 if not enough data is present, otherwise the number of bytes
 * "consumed"
 */
static int GIF_Block(DilloGif * gif, img_chunk chunk)
{
   int total_consumed_size = 0;

   if (chunk.size < 1) {
      return 0;
   }

   while (gif->parser_state == State2) {
      if (chunk.size < 1) {
         return 0;
      }

      const uchar_t block_id = chunk.buf[0];;
      switch (block_id) {
      case ExtensionIntroducer:
         {
            fprintf(stderr, "Extension Introducer\n");
            int consumed_size = gif_do_extension(gif, chunk);
            if (0 != consumed_size) {
               img_chunk_forward(&chunk, consumed_size);
               total_consumed_size += consumed_size;
            }
         }

         /* Do more GIF Blocks */
         continue;

      case ImageSeparator:
         {
            /* Image descriptor */
            fprintf(stderr, "Image Descriptor\n");
            int consumed_size = do_image_descriptor2(gif, chunk);
            if (0 != consumed_size) {
               img_chunk_forward(&chunk, consumed_size);
               total_consumed_size += consumed_size;

               if (0 == peek_lzw(gif, chunk)) {
                  return 0;
               }
            }
         }
         goto L_fin;

      case Trailer: /* GIF terminator */
         gif->parser_state = InvalidState;      /* BUG: should close the rest of the file */
         goto L_fin;

      default:                  /* Unknown */
         /* gripe and complain */
         MSG ("gif.c::GIF_Block: Error, 0x%x found\n", *(chunk.buf-1));
         gif->parser_state = InvalidState;
         goto L_fin;
      }
   }
   goto L_fin;

 L_fin:
   return total_consumed_size;
}


int do_image_descriptor2(DilloGif * gif, img_chunk img_descriptor)
{
   const int consumed_size = gif_do_image_descriptor(gif, img_descriptor);
   if (0 == consumed_size) {
      return 0;
   }
   img_chunk_forward(&img_descriptor, consumed_size);

   return consumed_size;
}


int peek_lzw(DilloGif * gif, img_chunk img_descriptor)
{
   /* Finally, get the first byte of the LZW image data */
   if (img_descriptor.size < 1) {
      fprintf(stderr, "%s:%d\n", __func__, __LINE__);
      return 0;
   }

   gif->input_code_size = img_descriptor.buf[0]; /* Just peek. */
   if (gif->input_code_size > 8) {
      gif->parser_state = InvalidState;
      fprintf(stderr, "%s:%d\n", __func__, __LINE__);
      return 0;
   }
   gif->row_number = 0;
   Gif_lwz_init(gif);
   gif->spill_line_index = 0;
   gif->row_data = malloc(gif->Width);
   gif->parser_state = StatePixelsData;              /*Process the lzw data next */

   return 1;
}

int gif_do_extension(DilloGif * gif, img_chunk extension_chunk)
{
   if (extension_chunk.size < 2) {
      return 0;
   }

   fprintf(stderr, "======== DO EXTENSION: introducer = %02x, label = %02x\n", extension_chunk.buf[0], extension_chunk.buf[1]);

   const int consumed_size = gif_do_extension_sub(gif, extension_chunk);
   if (0 == consumed_size) {
      /* Not all of the extension is there.. quit until more data
       * arrives */
      return 0;
   }

   /* Increment the amount consumed by the extension introducer
    * and id, and extension block size */
   img_chunk_forward(&extension_chunk, consumed_size);

   img_chunk_dump_at(extension_chunk, "end of do_extension");

   return consumed_size;
}

/*
 * Process some bytes from the input gif stream. It's a state machine.
 *
 * From the GIF spec:
 * <GIF Data Stream> ::= Header <Logical Screen> <Data>* Trailer
 *
 * <GIF Data Stream> --> gif_write
 * Header            --> ParserState::InitialState
 * <Logical Screen>  --> ParserState::State1
 * <Data>*           --> ParserState::State2
 * Trailer           --> ParserState::InvalidState ?
 *
 * ParserState::StatePixelsData is special... this is inside of <Data> but
 * all of the stuff in there has been gotten and set up.  So we stream it
 * outside.
 */
static int gif_write(DilloGif *gif, img_chunk main_chunk)
{
   int initial_chunk_size = main_chunk.size;
   int total_consumed_size = 0;

   switch (gif->parser_state) {
   case InitialState: {

      int consumed_size = gif_parse_header(main_chunk);
      if (InitialState == consumed_size) {
         /* Not enough information was read to go past a header into next
            section of image. */
         break;
      } else if (-1 == consumed_size) {
         gif->parser_state = InvalidState;
         /* Attempt to recognize GIF has failed: this is not a GIF file. */
         MSG_WARN("\"%s\" is not a GIF file.\n", URL_STR(gif->url));
         break;
      } else {
         /* Header recognized. Fall through to parsing next part of image. */
         gif->parser_state = State1;
         img_chunk_forward(&main_chunk, consumed_size);
         total_consumed_size += consumed_size;
      }
   }

   case State1: {
      int consumed_size = gif_parse_logical_screen_descriptor(gif, main_chunk);
      if (0 == consumed_size || State1 == gif->parser_state) {
         /* Logical Screen Descriptor was not parsed. Break now, try next time. */
         break;
      }
      img_chunk_forward(&main_chunk, consumed_size);
      total_consumed_size += consumed_size;
   }

   case State2: {
      /* Ok, this loop construction looks weird.  It implements the <Data>* of
       * the GIF grammar.  All sorts of stuff is allocated to set up for the
       * decode part (state ==2) and then there is the actual decode part (3)
       */
      int consumed_size = GIF_Block(gif, main_chunk);
      if (0 == consumed_size)
         break;
      img_chunk_forward(&main_chunk, consumed_size);
      total_consumed_size += consumed_size;
      if (gif->parser_state != StatePixelsData)
         break;
   }

   case StatePixelsData:
      {
         /* get an image byte */
         /* The users sees all of this stuff */
         int consumed_size = Gif_decode(gif, main_chunk);
         if (0 == consumed_size)
            break;
         img_chunk_forward(&main_chunk, consumed_size);
         total_consumed_size += consumed_size;
      }
   default:
      /* error - just consume all input */
      total_consumed_size = initial_chunk_size;
      break;
   }

   _MSG("gif_write: final state %d, %d bytes consumed\n", gif->parser_state, total_consumed_size);
   return total_consumed_size;
}

