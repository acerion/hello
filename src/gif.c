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
#ifdef ENABLE_GIF

#include <stdio.h>              /* for sprintf */
#include <string.h>             /* for memcpy and memmove */

#include "msg.h"
#include "image.hh"
#include "cache.h"
#include "dicache.h"

#define INTERLACE      0x40
#define LOCALCOLORMAP  0x80

#define LM_to_uint(a,b)   ((((uchar_t)b)<<8)|((uchar_t)a))

#define        MAXCOLORMAPSIZE         256
#define        MAX_LWZ_BITS            12


typedef enum {
   InitialState    = 0,
   State1          = 1,
   State2          = 2,
   StatePixelsData = 3,
   InvalidState    = 999,
} ParserState;

typedef struct {
   ParserState state;

   /* Beginning of all image data received so far. Part of it has already
      been parsed and sent to cache. */
   uchar_t * all_data;
   /* Total size of all of the image data received so far. */
   int all_size;

   /* Where the next unprocessed part of data starts. */
   size_t start_offset;

   /* New data received, but not processed yet. */
   uchar_t * chunk_start;
   /* Size of the new data. */
   int chunk_size;
} gif_parser;


typedef struct {
   gif_parser parser;

   DilloImage *Image;
   DilloUrl *url;
   int version;

   uint_t Flags;

   uchar_t input_code_size;
   uchar_t *row_data;
   int pass;

   uint_t row_number; /* Row number of decoded image. */

   /* state for lwz_read_byte */
   int code_size;

   /* The original GifScreen from giftopnm */
   uint_t Width;
   uint_t Height;
   size_t color_map_offset;
   uint_t ColorResolution;
   uint_t color_map_size; /* Number of colors in color map */
   int    Background;
   uint_t spill_line_index;
#if 0
   uint_t AspectRatio;    /* AspectRatio (not used) */
#endif

   /* Gif89 extensions */
   int transparent;
#if 0
   /* None are used: */
   int delayTime;
   int inputFlag;
   int disposal;
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


/*
 * Forward declarations
 */
static void Gif_write(DilloGif *gif, gif_parser * parser);
static void Gif_close(DilloGif *gif, CacheClient_t *Client);
static size_t Gif_process_bytes(DilloGif *gif, gif_parser * parser);


/*
 * Create a new gif structure for decoding a gif into a RGB buffer
 */
void *a_Gif_new(DilloImage *Image, DilloUrl *url, int version)
{
   DilloGif *gif = dMalloc(sizeof(DilloGif));
   _MSG("a_Gif_new: gif=%p\n", gif);

   memset(&gif->parser, 0, sizeof (gif->parser));
   gif->parser.state = InitialState;

   gif->Image = Image;
   gif->url = url;
   gif->version = version;
   gif->Flags = 0;
   gif->row_data = NULL;
   gif->Background = Image->bg_color;
   gif->transparent = -1;
   gif->num_spill_lines_max = 0;
   gif->spill_lines = NULL;
   gif->window = 0;
   gif->packet_size = 0;
   gif->color_map_offset = 0;

   return gif;
}

/*
 * Free the gif-decoding data structure.
 */
static void Gif_free(DilloGif *gif)
{
   int i;

   _MSG("Gif_free: gif=%p\n", gif);

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
      gif->parser.all_data = (uchar_t *) Client->Buf;
      gif->parser.all_size = Client->BufSize;
      gif->parser.chunk_start = gif->parser.all_data + gif->parser.start_offset;
      gif->parser.chunk_size = gif->parser.all_size - gif->parser.start_offset;

      Gif_write(gif, &gif->parser);
   } else if (Op == CacheOperationClose) {
      CacheClient_t *Client = data;
      Gif_close(Client->CbData, Client);
   } else if (Op == CacheOperationAbort) {
      Gif_free(data);
   }
}

/*
 * Receive and process new chunks of GIF image data
 */
static void Gif_write(DilloGif *gif, gif_parser * parser)
{
   _MSG("Gif_write: %d bytes\n", parser->chunk_size);

   /* Process the bytes in the input buffer. */
   const int bytes_consumed = Gif_process_bytes(gif, parser);

   if (bytes_consumed < 1)
      return;
   gif->parser.start_offset += bytes_consumed;

   _MSG("exit Gif_write, parser.chunk_size = %d\n", parser->chunk_size);
}

/*
 * Finish the decoding process (and free the memory)
 */
static void Gif_close(DilloGif *gif, CacheClient_t *Client)
{
   _MSG("Gif_close: destroy gif %p\n", gif);
   a_Dicache_close(gif->url, gif->version, Client);
   Gif_free(gif);
}


/* --- GIF Extensions ----------------------------------------------------- */

/*
 * This reads a sequence of GIF data blocks.. and ignores them!
 * Buf points to the first data block.
 *
 * Return Value
 * 0 = There wasn't enough bytes read yet to read the whole datablock
 * otherwise the size of the data blocks
 */
static inline size_t Gif_data_blocks(const uchar_t *Buf, size_t BSize)
{
   size_t Size = 0;

   if (BSize < 1)
      return 0;
   while (Buf[0]) {
      if (BSize <= (size_t)(Buf[0] + 1))
         return 0;
      Size += Buf[0] + 1;
      BSize -= Buf[0] + 1;
      Buf += Buf[0] + 1;
   }
   return Size + 1;
}

/*
 * This is a GIF extension.  We ignore it with this routine.
 * Buffer points to just after the extension label.
 *
 * Return Value
 * 0 -- block not processed
 * otherwise the size of the extension label.
 */
static inline size_t Gif_do_generic_ext(const uchar_t *Buf, size_t BSize)
{

   size_t Size = Buf[0] + 1,  /* (uchar_t + 1) can't overflow size_t */
          DSize;

   /* The Block size (the first byte) is supposed to be a specific size
    * for each extension... we don't check.
    */

   if (Size > BSize)
      return 0;
   DSize = Gif_data_blocks(Buf + Size, BSize - Size);
   if (!DSize)
      return 0;
   Size += DSize;
   return Size <= BSize ? Size : 0;
}

/*
 * ?
 */
static inline size_t
 Gif_do_gc_ext(DilloGif *gif, const uchar_t *Buf, size_t BSize)
{
   /* Graphic Control Extension */
   size_t Size = Buf[0] + 2;
   uint_t Flags;

   if (BSize < 6 || Size > BSize)
      return 0;
   Buf++;
   Flags = Buf[0];

   /* The packed fields */
#if 0
   gif->disposal = (Buf[0] >> 2) & 0x7;
   gif->inputFlag = (Buf[0] >> 1) & 0x1;

   /* Delay time */
   gif->delayTime = LM_to_uint(Buf[1], Buf[2]);
#endif

   /* Transparent color index, may not be valid  (unless flag is set) */
   if ((Flags & 0x1)) {
      gif->transparent = Buf[3];
   }
   return Size;
}

#define App_Ext  (0xff)
#define Cmt_Ext  (0xfe)
#define GC_Ext   (0xf9)
#define Txt_Ext  (0x01)

/*
 * ?
 * Return value:
 *    TRUE when the extension is over
 */
static size_t Gif_do_extension(DilloGif *gif, uint_t Label,
                               const uchar_t *buf,
                               size_t BSize)
{
   switch (Label) {
   case GC_Ext:         /* Graphics extension */
      return Gif_do_gc_ext(gif, buf, BSize);

   case Cmt_Ext:                /* Comment extension */
      return Gif_data_blocks(buf, BSize);

   case Txt_Ext:                /* Plain text Extension */
   case App_Ext:                /* Application Extension */
   default:
      return Gif_do_generic_ext(buf, BSize);    /*Ignore Extension */
   }
}

/* --- General Image Decoder ----------------------------------------------- */
/* Here begins the new push-oriented decoder. */

/*
 * ?
 */
static void Gif_lwz_init(DilloGif *gif)
{
   gif->num_spill_lines_max = 1;
   gif->spill_lines = dMalloc(sizeof(uchar_t *) * gif->num_spill_lines_max);

   gif->spill_lines[0] = dMalloc(gif->Width);
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
   if (gif->Flags & INTERLACE) {
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
                dMalloc(gif->Width);
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
static int Gif_decode(DilloGif *gif, const uchar_t *buf, size_t bsize)
{
   /*
    * Data block processing.  The image stuff is a series of data blocks.
    * Each data block is 1 to 256 bytes long.  The first byte is the length
    * of the data block.  0 == the last data block.
    */
   size_t bufsize, packet_size;
   uint_t clear_code;
   uint_t window;
   int bits_in_window;
   uint_t code;
   int code_size;
   uint_t code_mask;

   bufsize = bsize;

   /* Want to get all inner loop state into local variables. */
   packet_size = gif->packet_size;
   window = gif->window;
   bits_in_window = gif->bits_in_window;
   code_size = gif->code_size;
   code_mask = (1 << code_size) - 1;
   clear_code = 1 << gif->input_code_size;

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
            code = (window >> (32 - bits_in_window)) & code_mask;

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
         gif->parser.state = InvalidState;     /* BUG: should Go back to getting GIF blocks. */
         break;
      }
   }

   gif->packet_size = packet_size;
   gif->window = window;
   gif->bits_in_window = bits_in_window;
   gif->code_size = code_size;
   return bsize - bufsize;

 error:
   gif->parser.state = InvalidState;
   return bsize - bufsize;
}

/*
 * ?
 */
static int Gif_check_sig(DilloGif *gif, const uchar_t *ibuf, int ibsize)
{
   /* at beginning of file - read magic number */
   if (ibsize < 6)
      return 0;
   if (memcmp(ibuf, "GIF87a", 6) != 0 &&
       memcmp(ibuf, "GIF89a", 6) != 0) {
      MSG_WARN("\"%s\" is not a GIF file.\n", URL_STR(gif->url));
      gif->parser.state = InvalidState;
      return 6;
   }
   gif->parser.state = State1;
   return 6;
}

/* Read the color map
 *
 * Implements, from the spec:
 * Global Color Table
 * Local Color Table
 */
static inline size_t
Gif_do_color_table(DilloGif *gif, gif_parser * parser,
                   const uchar_t *buf, size_t bsize, size_t CT_Size)
{
   size_t Size = 3 * (1 << (1 + CT_Size));

   if (Size > bsize)
      return 0;

   gif->color_map_offset = (ulong_t) buf - (ulong_t) parser->all_data;
   gif->color_map_size = (1 << (1 + CT_Size));
   return Size;
}

/**
   This implements, from the spec:
   <Logical Screen> ::= Logical Screen Descriptor [Global Color Table]

   @return count of bytes consumed from input data part
*/
static size_t Gif_get_descriptor(DilloGif *gif, gif_parser * parser, int bsize)
{
   /* screen descriptor */
   size_t Size = 7;

   if (bsize < 7)
      return 0;
   const uchar_t Flags = parser->chunk_start[4];

   if (Flags & LOCALCOLORMAP) {
      /* consumed_size is size of color table. */
      const size_t consumed_size = Gif_do_color_table(gif, parser, parser->chunk_start + 7, (size_t)bsize - 7, Flags & (size_t)0x7);
      if (0 == consumed_size)
         return 0;
      Size += consumed_size;           /* Size of the color table that follows */
   /*   gif->Background = parser->chunk_start[5]; */
   }
   /*   gif->Width = LM_to_uint(parser->chunk_start[0], parser->chunk_start[1]);
        gif->Height = LM_to_uint(parser->chunk_start[2], parser->chunk_start[3]); */
   gif->ColorResolution = (((parser->chunk_start[4] & 0x70) >> 3) + 1);
   /*   gif->AspectRatio     = parser->chunk_start[6]; */

   return Size;
}

/*
 * This implements, from the spec:
 * <Table-Based Image> ::= Image Descriptor [Local Color Table] Image Data
 *
 * ('Buf' points to just after the Image separator)
 * we should probably just check that the local stuff is consistent
 * with the stuff at the header. For now, we punt...
 */
static size_t Gif_do_img_desc(DilloGif *gif, gif_parser * parser,
                              const uchar_t *buf, size_t bsize)
{
   size_t Size = 9 + 1; /* image descriptor size + first byte of image data */

   if (bsize < 10)
      return 0;

   gif->Width   = LM_to_uint(buf[4], buf[5]);
   gif->Height  = LM_to_uint(buf[6], buf[7]);

   /* check max image size */
   if (gif->Width <= 0 || gif->Height <= 0 ||
       gif->Width > IMAGE_MAX_AREA / gif->Height) {
      MSG("Gif_do_img_desc: suspicious image size request %u x %u\n",
          gif->Width, gif->Height);
      gif->parser.state = InvalidState;
      return 0;
   }

   /** \todo Gamma for GIF? */
   a_Dicache_set_parms(gif->url, gif->version, gif->Image,
                       gif->Width, gif->Height, DILLO_IMG_TYPE_INDEXED,
                       1 / 2.2);
   gif->Image = NULL; /* safeguard: hereafter it may be freed by its owner */

   const uchar_t Flags = buf[8];

   gif->Flags |= Flags & INTERLACE;
   gif->pass = 0;
   bsize -= 9;
   buf += 9;
   if (Flags & LOCALCOLORMAP) {
      size_t LSize = Gif_do_color_table(gif, parser, buf, bsize, Flags & (size_t)0x7);

      if (!LSize)
         return 0;
      Size += LSize;
      buf += LSize;
      bsize -= LSize;
   }
   /* Finally, get the first byte of the LZW image data */
   if (bsize < 1)
      return 0;
   gif->input_code_size = *buf++;
   if (gif->input_code_size > 8) {
      gif->parser.state = InvalidState;
      return Size;
   }
   gif->row_number = 0;
   Gif_lwz_init(gif);
   gif->spill_line_index = 0;
   gif->row_data = dMalloc(gif->Width);
   gif->parser.state = StatePixelsData;              /*Process the lzw data next */
   if (gif->color_map_offset) {
      const uchar_t * color_map = (uchar_t *) parser->all_data + gif->color_map_offset;
      a_Dicache_set_color_map(gif->url, gif->version, gif->Background,
                              color_map,
                              gif->color_map_size, 256, gif->transparent);
   }
   return Size;
}

/* --- Top level data block processors ------------------------------------ */
#define Img_Desc (0x2c)
#define Trailer  (0x3B)
#define Ext_Id   (0x21)

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
 * <Special-Purpose Block> --> Gif_do_extension
 * Graphic Control Extension --> Gif_do_extension
 * Plain Text Extension --> Gif_do_extension
 * <Table-Based Image> --> Gif_do_img_desc
 *
 * Return Value
 * 0 if not enough data is present, otherwise the number of bytes
 * "consumed"
 */
static size_t GIF_Block(DilloGif * gif, gif_parser * parser,
                        const uchar_t *buf, size_t bsize)
{
   size_t Size = 0, mysize;
   uchar_t C;

   if (bsize < 1)
      return 0;
   while (gif->parser.state == State2) {
      if (bsize < 1)
         return Size;
      bsize--;
      switch (*buf++) {
      case Ext_Id:
         /* get the extension type */
         if (bsize < 2)
            return Size;

         /* Have the extension block intepreted. */
         C = *buf++;
         bsize--;
         mysize = Gif_do_extension(gif, C, buf, bsize);

         if (!mysize)
            /* Not all of the extension is there.. quit until more data
             * arrives */
            return Size;

         bsize -= mysize;
         buf += mysize;

         /* Increment the amount consumed by the extension introducer
          * and id, and extension block size */
         Size += mysize + 2;
         /* Do more GIF Blocks */
         continue;

      case Img_Desc:            /* Image descriptor */
         mysize = Gif_do_img_desc(gif, parser, buf, bsize);
         if (!mysize)
            return Size;

         /* Increment the amount consumed by the Image Separator and the
          * Resultant blocks */
         Size += 1 + mysize;
         return Size;

      case Trailer:
         gif->parser.state = InvalidState;      /* BUG: should close the rest of the file */
         return Size + 1;
         break;                 /* GIF terminator */

      default:                  /* Unknown */
         /* gripe and complain */
         MSG ("gif.c::GIF_Block: Error, 0x%x found\n", *(buf-1));
         gif->parser.state = InvalidState;
         return Size + 1;
      }
   }
   return Size;
}


/*
 * Process some bytes from the input gif stream. It's a state machine.
 *
 * From the GIF spec:
 * <GIF Data Stream> ::= Header <Logical Screen> <Data>* Trailer
 *
 * <GIF Data Stream> --> Gif_process_bytes
 * Header            --> State 0
 * <Logical Screen>  --> State 1
 * <Data>*           --> State 2
 * Trailer           --> State > 3
 *
 * State == 3 is special... this is inside of <Data> but all of the stuff in
 * there has been gotten and set up.  So we stream it outside.
 */
static size_t Gif_process_bytes(DilloGif *gif, gif_parser * parser)
{
   int initial_chunk_size = parser->chunk_size;
   size_t consumed_size = 0;

   switch (gif->parser.state) {
   case InitialState:
      consumed_size = Gif_check_sig(gif, parser->chunk_start, initial_chunk_size);
      if (0 == consumed_size)
         break;
      initial_chunk_size -= consumed_size;
      parser->chunk_start += consumed_size;
      if (gif->parser.state != State1)
         break;

   case State1:
      consumed_size = Gif_get_descriptor(gif, parser, initial_chunk_size);
      if (0 == consumed_size)
         break;
      initial_chunk_size -= consumed_size;
      parser->chunk_start += consumed_size;
      gif->parser.state = State2;

   case State2:
      /* Ok, this loop construction looks weird.  It implements the <Data>* of
       * the GIF grammar.  All sorts of stuff is allocated to set up for the
       * decode part (state ==2) and then there is the actual decode part (3)
       */
      consumed_size = GIF_Block(gif, parser, parser->chunk_start, (size_t) initial_chunk_size);
      if (0 == consumed_size)
         break;
      initial_chunk_size -= consumed_size;
      parser->chunk_start += consumed_size;
      if (gif->parser.state != StatePixelsData)
         break;

   case StatePixelsData:
      /* get an image byte */
      /* The users sees all of this stuff */
      consumed_size = Gif_decode(gif, parser->chunk_start, (size_t) initial_chunk_size);
      if (0 == consumed_size)
         break;
      parser->chunk_start += consumed_size;
      initial_chunk_size -= consumed_size;

   default:
      /* error - just consume all input */
      initial_chunk_size = 0;
      break;
   }

   const size_t bytes_consumed = parser->chunk_size - initial_chunk_size
   _MSG("Gif_process_bytes: final state %d, %zd bytes consumed\n",
        gif->state, bytes_consumed);
   return bytes_consumed;
}

#else /* ENABLE_GIF */

void *a_Gif_new() { return 0; }
void a_Gif_callback() { return; }

#endif /* ENABLE_GIF */
