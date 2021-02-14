#ifndef _HELLO_H_
#define _HELLO_H_

#include <stdbool.h>
#include "../gif.h"

/* URL */
bool hll_hostIsIP(const char * hostname);

/* cookies */
int hll_lookupActionForDomain(const char * domain);

/* GIF */
int hll_parseExtension(hll_Gif * hll_gif, const uchar_t * buf, int size);


#endif
