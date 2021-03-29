#ifndef _HELLO_H_
#define _HELLO_H_

#include <stdbool.h>
#include <stdint.h>
#include "../gif.h"



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */




typedef struct hll_CssParser {
   int spaceSeparatedC;
   int bufOffsetC;
   int tokenTypeC;
   int withinBlockC;
   int isEndC;
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
char * hll_nextToken(hll_CssParser * hll_parser, const char * remainder);
/* Function returns color through return statement. */
int hll_parseRgbFunction(hll_CssParser * hll_parser, const char * remainder);




#ifdef __cplusplus
}
#endif /* __cplusplus */




#endif
