#ifndef _HELLO_H_
#define _HELLO_H_

#include <stdbool.h>

/* URL */
bool hll_hostIsIP(const char * hostname);

/* cookies */
int hll_lookupActionForDomain(const char * domain);


#endif
