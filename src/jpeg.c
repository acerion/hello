/*
 * File: jpeg.c
 *
 * Copyright (C) 2000-2007 Jorge Arellano Cid <jcid@dillo.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 */

/*
 * The jpeg decoder. It is responsible for decoding JPEG data
 * and transferring it to the dicache. It uses libjpeg to do the actual
 * decoding.
 */

#include <config.h>
#ifdef ENABLE_JPEG

#include <stdio.h>
#include <setjmp.h>

/* avoid a redefinition of HAVE_STDLIB_H with old jpeglib.h */
#ifdef HAVE_STDLIB_H
#  undef HAVE_STDLIB_H
#endif
#include <jpeglib.h>
#ifdef HAVE_STDLIB_H
#  undef HAVE_STDLIB_H
#endif

#include "image.hh"
#include "cache.h"
#include "dicache.h"
#include "capi.h"       /* get cache entry status */
#include "msg.h"

typedef enum {
   DILLO_JPEG_INIT,
   DILLO_JPEG_STARTING,
   DILLO_JPEG_READ_BEGIN_SCAN,
   DILLO_JPEG_READ_IN_SCAN,
   DILLO_JPEG_READ_END_SCAN,
   DILLO_JPEG_DONE,
   DILLO_JPEG_ERROR
} DilloJpegState;

typedef struct DilloJpeg DilloJpeg;

/* An implementation of a suspending source manager */

typedef struct {
   struct jpeg_source_mgr pub;  /* public fields */
   DilloJpeg *jpeg;             /* a pointer back to the jpeg object */
} my_source_mgr;

struct my_error_mgr {
   struct jpeg_error_mgr pub;    /* "public" fields */
   jmp_buf setjmp_buffer;        /* for return to caller */
};
typedef struct my_error_mgr *my_error_ptr;

typedef struct {
   /* Beginning of all image data received so far. Part of it has already
      been parsed and sent to cache. */
   uchar_t * all_data;
   /* Total size of all of the image data received so far. */
   int all_size;

   /* Where the next unprocessed part of data starts. */
   size_t start_offset;

   DilloJpegState state;
} jpeg_parser;

struct DilloJpeg {
   jpeg_parser parser;

   DilloImage *Image;
   DilloUrl *url;
   int version;

   my_source_mgr Src;

   size_t Skip;

   uint_t row_number; /* Row number of decoded image. */

   struct jpeg_decompress_struct cinfo;
   struct my_error_mgr jerr;
};

/*
 * Forward declarations
 */
static void Jpeg_write(DilloJpeg *jpeg, jpeg_parser * parser);


/* this is the routine called by libjpeg when it detects an error. */
METHODDEF(void) Jpeg_errorexit (j_common_ptr cinfo)
{
   /* display message and return to setjmp buffer */
   my_error_ptr myerr = (my_error_ptr) cinfo->err;
   if (prefs.show_msg) {
      DilloJpeg *jpeg =
                     ((my_source_mgr *) ((j_decompress_ptr) cinfo)->src)->jpeg;
      MSG_WARN("\"%s\": ", URL_STR(jpeg->url));
      (*cinfo->err->output_message) (cinfo);
   }
   longjmp(myerr->setjmp_buffer, 1);
}

/*
 * Free the jpeg-decoding data structure.
 */
static void Jpeg_free(DilloJpeg *jpeg)
{
   _MSG("Jpeg_free: jpeg=%p\n", jpeg);
   jpeg_destroy_decompress(&(jpeg->cinfo));
   dFree(jpeg);
}

/*
 * Finish the decoding process
 */
static void Jpeg_close(DilloJpeg *jpeg, CacheClient_t *Client)
{
   _MSG("Jpeg_close\n");
   a_Dicache_close(jpeg->url, jpeg->version, Client);
   Jpeg_free(jpeg);
}

static void init_source(j_decompress_ptr cinfo)
{
}

static boolean fill_input_buffer(j_decompress_ptr cinfo)
{
   DilloJpeg *jpeg = ((my_source_mgr *) cinfo->src)->jpeg;

   _MSG("fill_input_buffer\n");
#if 0
   if (!cinfo->src->bytes_in_buffer) {
      _MSG("fill_input_buffer: %ld bytes in buffer\n",
           (long)cinfo->src->bytes_in_buffer);

      jpeg->parser.start_offset = (ulong_t) jpeg->cinfo.src->next_input_byte -
         (ulong_t) jpeg->parser.all_data;
#endif
      if (jpeg->Skip) {
         jpeg->parser.start_offset = jpeg->parser.all_size + jpeg->Skip - 1;
         jpeg->Skip = 0;
      } else {
         jpeg->parser.start_offset = (ulong_t) jpeg->cinfo.src->next_input_byte -
            (ulong_t) jpeg->parser.all_data;
      }
      return FALSE;
#if 0
   }
   return TRUE;
#endif
}

static void skip_input_data(j_decompress_ptr cinfo, long num_bytes)
{
   if (num_bytes < 1)
      return;

   DilloJpeg * jpeg = ((my_source_mgr *) cinfo->src)->jpeg;

   _MSG("skip_input_data: start_offset = %zd, num_bytes = %ld,"
        " %ld bytes in buffer\n",
        jpeg->parser.start_offset, num_bytes,(long)cinfo->src->bytes_in_buffer);

   cinfo->src->next_input_byte += num_bytes;
   if (num_bytes < (long)cinfo->src->bytes_in_buffer) {
      cinfo->src->bytes_in_buffer -= num_bytes;
   } else {
      jpeg->Skip += num_bytes - cinfo->src->bytes_in_buffer + 1;
      cinfo->src->bytes_in_buffer = 0;
   }
}

static void term_source(j_decompress_ptr cinfo)
{
}

void *a_Jpeg_new(DilloImage *Image, DilloUrl *url, int version)
{
   my_source_mgr *src;
   DilloJpeg *jpeg = dMalloc(sizeof(*jpeg));
   _MSG("a_Jpeg_new: jpeg=%p\n", jpeg);

   memset(&jpeg->parser, 0, sizeof (jpeg->parser));
   jpeg->parser.state = DILLO_JPEG_INIT;

   jpeg->Image = Image;
   jpeg->url = url;
   jpeg->version = version;

   jpeg->Skip = 0;

   /* decompression step 1 (see libjpeg.doc) */
   jpeg->cinfo.err = jpeg_std_error(&jpeg->jerr.pub);
   jpeg->jerr.pub.error_exit = Jpeg_errorexit;

   jpeg_create_decompress(&jpeg->cinfo);

   /* decompression step 2 (see libjpeg.doc) */
   jpeg->cinfo.src = &jpeg->Src.pub;
   src = &jpeg->Src;
   src->pub.init_source = init_source; /* Empty function. */
   src->pub.fill_input_buffer = fill_input_buffer;
   src->pub.skip_input_data = skip_input_data;
   src->pub.resync_to_restart = jpeg_resync_to_restart;/* use default method */
   src->pub.term_source = term_source;  /* Empty function */
   src->pub.bytes_in_buffer = 0;   /* forces fill_input_buffer on first read */
   src->pub.next_input_byte = NULL;/* until buffer loaded */

   src->jpeg = jpeg;

   /* decompression steps continue in write method */
   return jpeg;
}

void a_Jpeg_callback(int Op, void *data)
{
   if (Op == CacheOperationAddData) {
      CacheClient_t *Client = data;

      DilloJpeg * jpeg = Client->CbData;
      jpeg->parser.all_data = Client->Buf;
      jpeg->parser.all_size = Client->BufSize;

      Jpeg_write(jpeg, &jpeg->parser);
   } else if (Op == CacheOperationClose) {
      CacheClient_t *Client = data;
      Jpeg_close(Client->CbData, Client);
   } else if (Op == CacheOperationAbort) {
      Jpeg_free(data);
   }
}

/*
 * Receive and process new chunks of JPEG image data
 */
static void Jpeg_write(DilloJpeg *jpeg, jpeg_parser * parser)
{
   DilloImgType type;

   _MSG("Jpeg_write: (%p) Bytes in buff: %d Ofs: %zd\n", jpeg,
        parser->all_size, jpeg->parser.start_offset);

   /* See if we are supposed to skip ahead. */
   if (parser->all_size <= parser->start_offset)
      return;

   /* Concatenate with the partial input, if any. */
   jpeg->cinfo.src->next_input_byte = parser->all_data + jpeg->parser.start_offset;
   jpeg->cinfo.src->bytes_in_buffer = parser->all_size - jpeg->parser.start_offset;

   if (setjmp(jpeg->jerr.setjmp_buffer)) {
      /* If we get here, the JPEG code has signaled an error. */
      parser->state = DILLO_JPEG_ERROR;
   }

   /* Process the bytes in the input buffer. */
   if (parser->state == DILLO_JPEG_INIT) {

      /* decompression step 3 (see libjpeg.doc) */
      if (jpeg_read_header(&(jpeg->cinfo), TRUE) != JPEG_SUSPENDED) {
         type = DILLO_IMG_TYPE_GRAY;
         if (jpeg->cinfo.num_components == 1) {
            type = DILLO_IMG_TYPE_GRAY;
         } else if (jpeg->cinfo.num_components == 3) {
            type = DILLO_IMG_TYPE_RGB;
         } else {
            if (jpeg->cinfo.jpeg_color_space == JCS_YCCK)
               MSG("YCCK JPEG. Are the colors wrong?\n");
            if (!jpeg->cinfo.saw_Adobe_marker)
               MSG("No adobe marker! Is the image shown in reverse video?\n");
            type = DILLO_IMG_TYPE_CMYK_INV;
         }
         /*
          * If a multiple-scan image is not completely in cache,
          * use progressive display, updating as it arrives.
          */
         if (jpeg_has_multiple_scans(&jpeg->cinfo) &&
             !(a_Capi_get_flags(jpeg->url) & CAPI_Completed))
            jpeg->cinfo.buffered_image = TRUE;

         /* check max image size */
         if (jpeg->cinfo.image_width <= 0 || jpeg->cinfo.image_height <= 0 ||
             jpeg->cinfo.image_width >
             IMAGE_MAX_AREA / jpeg->cinfo.image_height) {
            MSG("Jpeg_write: suspicious image size request %u x %u\n",
                (uint_t)jpeg->cinfo.image_width,
                (uint_t)jpeg->cinfo.image_height);
            parser->state = DILLO_JPEG_ERROR;
            return;
         }

         /** \todo Gamma for JPEG? */
         a_Dicache_set_parms(jpeg->url, jpeg->version, jpeg->Image,
                             (uint_t)jpeg->cinfo.image_width,
                             (uint_t)jpeg->cinfo.image_height,
                             type, 1 / 2.2);
         jpeg->Image = NULL; /* safeguard: may be freed by its owner later */

         /* decompression step 4 (see libjpeg.doc) */
         parser->state = DILLO_JPEG_STARTING;
      }
   }
   if (parser->state == DILLO_JPEG_STARTING) {
      /* decompression step 5 (see libjpeg.doc) */
      if (jpeg_start_decompress(&(jpeg->cinfo))) {
         jpeg->row_number = 0;
         parser->state = jpeg->cinfo.buffered_image ?
                          DILLO_JPEG_READ_BEGIN_SCAN : DILLO_JPEG_READ_IN_SCAN;
      }
   }

   /*
    * A progressive jpeg contains multiple scans that can be used to display
    * an increasingly sharp image as it is being received. The reading of each
    * scan must be surrounded by jpeg_start_output()/jpeg_finish_output().
    */

   if (parser->state == DILLO_JPEG_READ_END_SCAN) {
      if (jpeg_finish_output(&jpeg->cinfo)) {
         if (jpeg_input_complete(&jpeg->cinfo)) {
            parser->state = DILLO_JPEG_DONE;
         } else {
            parser->state = DILLO_JPEG_READ_BEGIN_SCAN;
         }
      }
   }

   if (parser->state == DILLO_JPEG_READ_BEGIN_SCAN) {
      if (jpeg_start_output(&jpeg->cinfo, jpeg->cinfo.input_scan_number)) {
         a_Dicache_new_scan(jpeg->url, jpeg->version);
         parser->state = DILLO_JPEG_READ_IN_SCAN;
      }
   }

   if (parser->state == DILLO_JPEG_READ_IN_SCAN) {
      uchar_t * row_data = dMalloc(jpeg->cinfo.image_width *
                                    jpeg->cinfo.num_components);
      JSAMPLE *array[1] = { row_data };

      while (1) {
         const int num_read = jpeg_read_scanlines(&jpeg->cinfo, array, 1); /* Read at most one row of image. */
         if (num_read == 0) {
            /* out of input */
            break;
         }
         a_image_cache_add_row(jpeg->url, jpeg->version, row_data, jpeg->row_number); /* Add that row to cache. */

         jpeg->row_number++;

         if (jpeg->row_number == jpeg->cinfo.image_height) {
            /* end of scan */
            if (!jpeg->cinfo.buffered_image) {
               /* single scan */
               parser->state = DILLO_JPEG_DONE;
               break;
            } else {
               jpeg->row_number = 0;
               if (jpeg_input_complete(&jpeg->cinfo)) {
                  if (jpeg->cinfo.input_scan_number ==
                      jpeg->cinfo.output_scan_number) {
                     parser->state = DILLO_JPEG_DONE;
                     break;
                  } else {
                       /* one final loop through the scanlines */
                       jpeg_finish_output(&jpeg->cinfo);
                       jpeg_start_output(&jpeg->cinfo,
                                         jpeg->cinfo.input_scan_number);
                       continue;
                  }
               }
               parser->state = DILLO_JPEG_READ_END_SCAN;
               if (!jpeg_finish_output(&jpeg->cinfo)) {
                  /* out of input */
                  break;
               } else {
                  if (jpeg_input_complete(&jpeg->cinfo)) {
                     parser->state = DILLO_JPEG_DONE;
                     break;
                  } else {
                     parser->state = DILLO_JPEG_READ_BEGIN_SCAN;
                  }
               }
               if (!jpeg_start_output(&jpeg->cinfo,
                                      jpeg->cinfo.input_scan_number)) {
                  /* out of input */
                  break;
               }
               a_Dicache_new_scan(jpeg->url, jpeg->version);
               parser->state = DILLO_JPEG_READ_IN_SCAN;
            }
         }
      }
      dFree(row_data);
   }
}

#else /* ENABLE_JPEG */

void *a_Jpeg_new() { return 0; }
void a_Jpeg_callback() { return; }

#endif /* ENABLE_JPEG */
