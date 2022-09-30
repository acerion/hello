/*
 * The png decoder. It is responsible for decoding PNG data
 * and transferring it to the dicache.
 *
 * Geoff Lane nov 1999 zzassgl@twirl.mcc.ac.uk
 * Luca Rota, Jorge Arellano Cid, Eric Gaudet 2000
 * Jorge Arellano Cid 2009
 *
 * "PNG: The Definitive Guide" by Greg Roelofs, O'Reilly
 * ISBN 1-56592-542-4
 */

#include <config.h>
#ifdef ENABLE_PNG

#include <stdlib.h> /* For abort() */

#ifdef HAVE_LIBPNG_PNG_H
#include <libpng/png.h>
#else
#include <png.h>
#endif

#include "msg.h"
#include "image.hh"
#include "cache.h"
#include "dicache.h"

typedef enum {
   IS_finished,
   IS_init,
   IS_nextdata
} ParserState;

#if 0
static char *prog_state_name[] =
{
   "IS_finished", "IS_init", "IS_nextdata"
};
#endif


typedef struct {
/*
 * 0                                              last byte
 * +-----------+-+-----------------------------------+-+
 * |           | |     -- data to be processed --    | |
 * +-----------+-+-----------------------------------+-+
 * ^            ^                                     ^
 * all_data     start_offset                          all_size
 */


   ParserState state;

   /* Beginning of all image data received so far. Part of it has already
      been parsed and sent to cache. */
   uchar_t * all_data;
   /* Total size of all of the image data received so far. */
   int all_size;

   /* Where the next unprocessed part of data starts. */
   size_t start_offset;

} png_parser;

/*
 * This holds the data that must be saved between calls to this module.
 * Each time it is called it is supplied with a vector of data bytes
 * obtained from the web server. The module can process any amount of the
 * supplied data.  The next time the module is called, the vector may be
 * extended with additional data bytes to be processed.  The module must
 * keep track of the current start and cursor position of the input data
 * vector.  As complete output rasters are determined they are sent out of
 * the module for additional processing.
 *
 * NOTE:  There is no external control of the splitting of the input data
 * vector (only this module understands PNG format data.) This means that
 * the complete state of a PNG image being processed must be held in the
 * structure below so that processing can be suspended or resumed at any
 * point within an input image.
 *
 * In the case of the libpng library, it maintains its own state in
 * png_ptr and into_ptr so the FSM is very simple - much simpler than the
 * ones for XBM and PNM are.
 */

typedef struct {
   png_parser parser;

   DilloImage *Image;           /* Image meta data */
   DilloUrl *url;               /* Primary Key for the dicache */
   int version;                 /* Secondary Key for the dicache */
   int bgcolor;                 /* Parent widget background color */

   png_uint_32 width;           /* png image width */
   png_uint_32 height;          /* png image height */
   png_structp png_ptr;         /* libpng private data */
   png_infop info_ptr;          /* libpng private info */
   uchar_t *image_data;         /* decoded image data    */
   uchar_t **row_pointers;      /* pntr to row starts    */
   jmp_buf jmpbuf;              /* png error processing */
   int error;                   /* error flag */
   png_uint_32 previous_row;
   int bytes_per_row;           /* No. bytes in image row */
   short channels;              /* No. image channels */

   uchar_t * row_data;          /* o/p raster data */

} DilloPng;


static int initialize_png_lib(DilloPng * png);

static
void Png_error_handling(png_structp png_ptr, png_const_charp msg)
{
   DilloPng *png;

   MSG("Png_error_handling: %s\n", msg);
   png = png_get_error_ptr(png_ptr);

   png->error = 1;
   png->parser.state = IS_finished;

   longjmp(png->jmpbuf, 1);
}

/* performs local init functions */
static void
Png_datainfo_callback(png_structp png_ptr, png_infop info_ptr)
{
   _MSG("Png_datainfo_callback:\n");

   DilloPng * png = png_get_progressive_ptr(png_ptr);
   if (NULL == png) {
      return;
   }

   int color_type;
   int bit_depth;
   int interlace_type;
   png_get_IHDR(png_ptr, info_ptr, &png->width, &png->height,
                &bit_depth, &color_type, &interlace_type, NULL, NULL);

   /* check max image size */
   if (png->width == 0 || png->height == 0 ||
       png->width > IMAGE_MAX_AREA / png->height) {
      MSG("Png_datainfo_callback: suspicious image size request %lu x %lu\n",
          (ulong_t) png->width, (ulong_t) png->height);
      Png_error_handling(png_ptr, "Aborting...");
      return; /* not reached */
   }

   _MSG("Png_datainfo_callback: png->width  = %lu\n"
        "Png_datainfo_callback: png->height = %lu\n",
        (ulong_t) png->width, (ulong_t) png->height);

   /* we need RGB/RGBA in the end */
   if (color_type == PNG_COLOR_TYPE_PALETTE && bit_depth <= 8) {
      /* Convert indexed images to RGB */
      png_set_expand (png_ptr);
   } else if (color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8) {
      /* Convert grayscale to RGB */
      png_set_expand (png_ptr);
   } else if (png_get_valid (png_ptr, info_ptr, PNG_INFO_tRNS)) {
      /* We have transparency header, convert it to alpha channel */
      png_set_expand(png_ptr);
   } else if (bit_depth < 8) {
      png_set_expand(png_ptr);
   }

   if (bit_depth == 16) {
      png_set_strip_16(png_ptr);
   }

   /* Get and set gamma information. Beware: gamma correction 2.2 will
      only work on PC's. TODO: select screen gamma correction for other
      platforms. */
   double file_gamma = 1 / 2.2;
   if (png_get_gAMA(png_ptr, info_ptr, &file_gamma))
      png_set_gamma(png_ptr, 2.2, file_gamma);

   /* Convert gray scale to RGB */
   if (color_type == PNG_COLOR_TYPE_GRAY ||
       color_type == PNG_COLOR_TYPE_GRAY_ALPHA) {
      png_set_gray_to_rgb(png_ptr);
   }

   /* Interlaced */
   if (interlace_type != PNG_INTERLACE_NONE) {
      png_set_interlace_handling(png_ptr);
   }

   /* get libpng to update its state */
   png_read_update_info(png_ptr, info_ptr);

   png_get_IHDR(png_ptr, info_ptr, &png->width, &png->height,
                &bit_depth, &color_type, &interlace_type, NULL, NULL);

   png->bytes_per_row = png_get_rowbytes(png_ptr, info_ptr);
   png->channels = png_get_channels(png_ptr, info_ptr);

   /* init browser specifics */
   _MSG("Png_datainfo_callback: bytes per row = %d\n"
        "Png_datainfo_callback: width         = %lu\n"
        "Png_datainfo_callback: height        = %lu\n",
        png->bytes_per_row, (ulong_t) png->width, (ulong_t) png->height);

   png->image_data = (uchar_t *) dMalloc(png->bytes_per_row * png->height);
   png->row_pointers = (uchar_t **) dMalloc(png->height * sizeof(uchar_t *));

   for (uint_t row = 0; row < png->height; row++) {
      png->row_pointers[row] = png->image_data + (row * png->bytes_per_row);
   }

   png->row_data = dMalloc(3 * png->width);

   /* Initialize the dicache-entry here */
   a_Dicache_set_parms(png->url, png->version, png->Image,
                       (uint_t)png->width, (uint_t)png->height,
                       DILLO_IMG_TYPE_RGB, file_gamma);
   png->Image = NULL; /* safeguard: hereafter it may be freed by its owner */
}

/* performs per row action */
static void
Png_datarow_callback(png_structp png_ptr, png_bytep new_row,
                     png_uint_32 row_num, int pass)
{
   if (!new_row)                /* work to do? */
      return;

   _MSG("Png_datarow_callback: row_num = %ld\n", row_num);

   DilloPng * png = png_get_progressive_ptr(png_ptr);

   png_progressive_combine_row(png_ptr, png->row_pointers[row_num], new_row);

   _MSG("png: row_num=%u previous_row=%u\n", row_num, png->previous_row);
   if (row_num < png->previous_row) {
      a_Dicache_new_scan(png->url, png->version);
   }
   png->previous_row = row_num;

   if (3 != png->channels && 4 != png->channels) {
      MSG("Png_datarow_callback: unexpected number of channels=%d pass=%d\n",
          png->channels, pass);
      return;
   }

   uint8_t * row_data = NULL;
   if (3 == png->channels) {
      /* Without alpha channel. */
      row_data = png->image_data + (row_num * png->bytes_per_row);
   } else {
      /* With alpha channel */
      /* TODO: get the backgound color from the parent
       * of the image widget -- Livio.                 */
      uchar_t * out_ptr = png->row_data;
      const uchar_t *in_ptr = png->image_data + (row_num * png->bytes_per_row);

      /* TODO: maybe change prefs.bg_color to `a_Dw_widget_get_bg_color`,
       * when background colors are correctly implementated */
      const int bg_blue  = (png->bgcolor) & 0xFF;
      const int bg_green = (png->bgcolor>>8) & 0xFF;
      const int bg_red   = (png->bgcolor>>16) & 0xFF;

      for (uint_t i = 0; i < png->width; i++) {
         const int alpha = *(in_ptr+3);

         if (alpha == 255) {
            *(out_ptr++) = *(in_ptr++);
            *(out_ptr++) = *(in_ptr++);
            *(out_ptr++) = *(in_ptr++);
            in_ptr++; /* Skip remaining alpha byte. */
         } else if (alpha == 0) {
            *(out_ptr++) = bg_red;
            *(out_ptr++) = bg_green;
            *(out_ptr++) = bg_blue;
            in_ptr += (3 + 1); /* Skip 3 bytes of colors and one byte of alpha. */
         } else {
            png_composite(*(out_ptr++), *(in_ptr++), alpha, bg_red);
            png_composite(*(out_ptr++), *(in_ptr++), alpha, bg_green);
            png_composite(*(out_ptr++), *(in_ptr++), alpha, bg_blue);
            in_ptr++; /* Skip remaining alpha byte. */
         }
      }
      row_data = png->row_data;
   }
   a_image_cache_add_row(png->url, png->version, row_data, (uint_t)row_num);
}

/* performs cleanup actions */
static void Png_dataend_callback(png_structp png_ptr, png_infop info_ptr)
{
   _MSG("Png_dataend_callback:\n");
   if (!info_ptr)
      MSG("Png_dataend_callback: info_ptr = NULL\n");

   DilloPng * png = png_get_progressive_ptr(png_ptr);
   png->parser.state = IS_finished;
}

/*
 * Free up the resources for this image.
 */
static void Png_free(DilloPng *png)
{
   _MSG("Png_free: png=%p\n", png);

   dFree(png->image_data);
   dFree(png->row_pointers);
   dFree(png->row_data);
   if (setjmp(png->jmpbuf))
      MSG_WARN("PNG: can't destroy read structure\n");
   else if (png->png_ptr)
      png_destroy_read_struct(&png->png_ptr, &png->info_ptr, NULL);
   dFree(png);
}

/*
 * Finish the decoding process (and free the memory)
 */
static void Png_close(DilloPng *png, CacheClient_t *Client)
{
   _MSG("Png_close\n");
   /* Let dicache know decoding is over */
   a_Dicache_close(png->url, png->version, Client);
   Png_free(png);
}

/*
 * Receive and process new chunks of PNG image data
 */
static void Png_write(DilloPng *png, png_parser * parser)
{
   /* Keep local copies so we don't have to pass multiple args to
    * a number of functions. */

   /* start/resume the FSM here */
   while (png->parser.state != IS_finished && (png->parser.all_size - png->parser.start_offset)) {
      _MSG("State = %s\n", prog_state_name[png->state]);

      switch (png->parser.state) {
      case IS_init:
         if (-1 == initialize_png_lib(png)) {
            return;
         }
         break;

      case IS_nextdata:
         if (setjmp(png->jmpbuf)) {
            png->parser.state = IS_finished;
         } else if (!png->error) {
            const uint_t data_size = png->parser.all_size - png->parser.start_offset;
            png_process_data( png->png_ptr,
                              png->info_ptr,
                              png->parser.all_data + png->parser.start_offset,
                              (png_size_t) data_size);

            png->parser.start_offset += data_size;
         }
         break;

      default:
         MSG_WARN("PNG decoder: bad state = %d\n", png->parser.state);
         abort();
      }
   }
}

/*
  Initialize png library's structure
*/
static int initialize_png_lib(DilloPng * png)
{
   const uint_t data_size = png->parser.all_size - png->parser.start_offset;
   if (data_size < 8) {
      return -1;            /* need MORE data */
   }
   /* check the image signature - DON'T update parser->start_offset! */
   if (png_sig_cmp(png->parser.all_data, 0, data_size)) {
      MSG_WARN("\"%s\" is not a PNG file.\n", URL_STR(png->url));
      png->parser.state = IS_finished;
      return 0;
   }
   /* OK, it looks like a PNG image, lets do some set up stuff */
   png->png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,
                                         png,
                                         (png_error_ptr)Png_error_handling,
                                         (png_error_ptr)Png_error_handling);
   if (NULL == png->png_ptr) {
      return -1;
   }
   png->info_ptr = png_create_info_struct(png->png_ptr);
   if (NULL == png->info_ptr) {
      return -1;
   }

   setjmp(png->jmpbuf);
   if (!png->error) {
      png_set_progressive_read_fn(png->png_ptr,
                                  png,
                                  Png_datainfo_callback,   /* performs local init functions */
                                  Png_datarow_callback,    /* performs per row action */
                                  Png_dataend_callback);   /* performs cleanup actions */
      png->parser.state = IS_nextdata;
   }

   return 0;
}

/*
 * Op:  Operation to perform.
 *   If (Op == 0)
 *      start or continue processing an image if image data exists.
 *   else
 *       terminate processing, cleanup any allocated memory,
 *       close down the decoding process.
 *
 * Client->CbData  : pointer to previously allocated DilloPng work area.
 *  This holds the current state of the image processing and is kept
 *  across calls to this routine.
 * Client->Buf     : Pointer to data start.
 * Client->BufSize : the size of the data buffer.
 *
 * You have to keep track of where you are in the image data and
 * how much has been processed.
 *
 * It's entirely possible that you will not see the end of the data.  The
 * user may terminate transfer via a Stop button or there may be a network
 * failure.  This means that you can't just wait for all the data to be
 * presented before starting conversion and display.
 */
void a_Png_callback(int Op, void *data)
{
   if (Op == CacheOperationAddData) {
      CacheClient_t *Client = data;

      if (NULL == Client->Buf || 0 == Client->BufSize) {
         return;
      }

      DilloPng * png = Client->CbData;
      png->parser.all_data = Client->Buf;
      png->parser.all_size = Client->BufSize;

      Png_write(png, &png->parser);
   } else if (Op == CacheOperationClose) {
      CacheClient_t *Client = data;
      Png_close(Client->CbData, Client);
   } else if (Op == CacheOperationAbort) {
      Png_free(data);
   }
}

/*
 * Create the image state data that must be kept between calls
 */
void *a_Png_new(DilloImage *Image, DilloUrl *url, int version)
{
   DilloPng *png = dNew0(DilloPng, 1);
   _MSG("a_Png_new: png=%p\n", png);

   memset(&png->parser, 0, sizeof (png->parser));
   png->parser.state = IS_init;

   png->Image = Image;
   png->url = url;
   png->version = version;
   png->bgcolor = Image->bg_color;
   png->error = 0;
   png->row_data = NULL;
   png->image_data = NULL;
   png->row_pointers = NULL;
   png->previous_row = 0;

   return png;
}

#else /* ENABLE_PNG */

void *a_Png_new() { return 0; }
void a_Png_callback() { return; }

#endif /* ENABLE_PNG */
