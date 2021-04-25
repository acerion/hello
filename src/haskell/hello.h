#ifndef _HELLO_H_
#define _HELLO_H_

#include <stdbool.h>
#include <stdint.h>
#include "../css.h"



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */




typedef struct c_gif_t {

   /* [1] "23. Graphic Control Extension.", Required version: Gif89. */
   int c_transparent_color_index;
#if 1
   int c_delay_time;
   int c_user_input_flag;
   int c_disposal_method;
#endif
} c_gif_t;




typedef struct c_css_parser_t {
   int c_space_separated;
   int c_buf_offset;
   int c_token_type;
   int c_within_block;
   char * c_token_value;
} c_css_parser_t;




typedef struct c_css_simple_selector_t {
   /* It's possible that more than one of these is set in a single
      CssSimpleSelector struct. */
   char * c_selector_class[10];
   int c_selector_class_size;

   /* In CSS there can be more pseudo-classes and Haskell can read them, but
      for now C/C++ code will only use first one. */
   char * c_selector_pseudo_class[10];
   int c_selector_pseudo_class_size;

   char * c_selector_id;
   int c_selector_element; /* Index corresponding to html.cc::Tags[]. */
} c_css_simple_selector_t;




/* URL */
bool hll_hostIsIP(const char * hostname);

/* cookies */
int hll_lookupActionForDomain(const char * domain);

/* Gif */
int hll_parseExtension(c_gif_t * hll_gif, const unsigned char * buf, int size);

/* Colors */
int hll_colorsStringToColor(const char * str, int default_color);
int hll_colorsVisitedColor(int candidate, int txt, int lnk, int bg);

/* HtmlEntity */
int64_t hll_htmlEntityToIsoCode(const char * token, int tokenLen);




/* HtmlTag */
char * hll_getAttrValue(const char * tag, int tagSize, const char * attrName);
/* Return index of tag named \p tagName. The index is an index to 'TagInfo
   Tags[]' array. Return -1 if tag name was not found. */
int hll_htmlTagIndex(const char * tagName);




/* CssParser */
/* Token value is returned through return statement. */
char * hll_nextToken(c_css_parser_t * hll_parser, const char * remainder);
/* Function returns color through return statement. */
//int hll_parseRgbFunction(c_css_parser_t * hll_parser, const char * remainder);

int hll_declarationValueAsInt(c_css_parser_t * hll_parser, int tokType, const char * tokValue, const char * remainder, int valueType, int property);
char * hll_declarationValueAsString(c_css_parser_t * hll_parser, int tokType, const char * tokValue, const char * remainder, int valueType, int property);
int hll_declarationValueAsMultiEnum(c_css_parser_t * hll_parser, int tokType, const char * tokValue, const char * remainder, int property);
int hll_tokenMatchesProperty(int tokType, const char * tokValue, int property);
int hll_ignoreBlock(c_css_parser_t * hll_parser, const char * remainder);
int hll_ignoreStatement(c_css_parser_t * hll_parser, const char * remainder);

int hll_cssPropertyInfoIdxByName(const char * propertyName);
const char * hll_cssPropertyNameString(int property);


int hll_cssParseWeight(c_css_parser_t * hll_parser, int tokType, const char * tokValue, const char * remainder);

CssLengthType hll_cssLengthType(int cssLength);
float hll_cssLengthValue(int cssLength);
int hll_cssCreateLength(float val, CssLengthType t);

/* Return true if valid simple selector was found.
   Return false otherwise. */
bool hll_cssParseSimpleSelector(c_css_parser_t * hll_parser, c_css_simple_selector_t * simpleSelector, int tokType, const char * tokValue, const char * remainder);


#ifdef __cplusplus
}
#endif /* __cplusplus */




#endif
