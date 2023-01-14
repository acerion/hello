#ifndef __IMGBUF_HH__
#define __IMGBUF_HH__

// Imgbuf wrappers


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#include "image.hh"

/*
 * Function prototypes
 */
void a_Imgbuf_ref(void *v_imgbuf);
void a_Imgbuf_unref(void *v_imgbuf);
void *a_Imgbuf_new(void *v_ir, int img_type, unsigned int width, unsigned int height,
                   double gamma);
int a_Imgbuf_last_reference(void *v_imgbuf);
void a_Imgbuf_update(void *v_imgbuf, const unsigned char *buf, DilloImgType type,
                     unsigned char *color_map, unsigned int width, unsigned int height, unsigned int row_number);
void a_Imgbuf_new_scan(void *v_imgbuf);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __IMGBUF_HH__ */

