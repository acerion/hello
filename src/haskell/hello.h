#ifndef _HELLO_H_
#define _HELLO_H_

#include <stdbool.h>
#include <stdint.h>
#include "../gif.h"



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */




typedef struct hll_CssParser {
   bool spaceSeparatedC;
   int consumedLenC; /* How much of input buffer has been consumed? By how much to move offset? */
   int tokenTypeC;
} hll_CssParser;




/* URL */
bool hll_hostIsIP(const char * hostname);

/* cookies */
int hll_lookupActionForDomain(const char * domain);

/* Gif */
int hll_parseExtension(hll_Gif * hll_gif, const unsigned char * buf, int size);

/* Colors */
int hll_colorsStringToColor(const char * str, int default_color);
int hll_colorsVisitedColor(int candidate, int txt, int lnk, int bg);

/* HtmlEntity */
int64_t hll_htmlEntityToIsoCode(const char * token, int tokenLen);

/* HtmlTag */
char * hll_getAttrValue(const char * tag, int tagSize, const char * attrName);

/* CssParser */
/* Token value is returned through return statement. */
char * hll_nextToken(hll_CssParser * hll_parser, const char * remainder, int inBlock);




#ifdef __cplusplus
}
#endif /* __cplusplus */




#endif
