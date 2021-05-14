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
   int c_in_block;
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

   int c_combinator;
} c_css_simple_selector_t;



typedef struct c_css_selector_t {
   int c_match_case_offset;
   struct c_css_simple_selector_t * c_simple_selector_list[10];
   int c_simple_selector_list_size;
} c_css_selector_t;



typedef struct c_css_declaration_ffi_t {
   int c_type_tag;
   int c_int_val;
   char * c_text_val;
   int c_important;
   int c_property;
} c_css_declaration_ffi_t;


typedef struct c_css_token_t {
   int c_type;
   char * c_value;
} c_css_token_t;



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
char * hll_nextToken(c_css_parser_t * hll_parser, c_css_token_t * token, const char * remainder);
/* Function returns color through return statement. */
//int hll_parseRgbFunction(c_css_parser_t * hll_parser, const char * remainder);

char * hll_declarationValueAsString(c_css_parser_t * hll_parser, c_css_token_t * token, const char * remainder, int valueType, int property);
int hll_tokenMatchesProperty(c_css_token_t * token, int property);
int hll_ignoreBlock(c_css_parser_t * hll_parser, c_css_token_t * token, const char * remainder);
int hll_ignoreStatement(c_css_parser_t * hll_parser, c_css_token_t * token, const char * remainder);

int hll_cssShorthandInfoIdxByName(const char * shorthandName);
int hll_cssPropertyInfoIdxByName(const char * propertyName);
const char * hll_cssPropertyNameString(int property);


CssLengthType hll_cssLengthType(int cssLength);
float hll_cssLengthValue(int cssLength);
int hll_cssCreateLength(float val, CssLengthType t);

/* Return allocated selector parsing succeeded.
   Return NULL otherwise. */
c_css_selector_t * hll_cssParseSelector(c_css_parser_t * hll_parser, c_css_token_t * token, const char * remainder);

int hll_parseDeclarationNormal(c_css_parser_t * hll_parser, c_css_token_t * token, const char * remainder, c_css_declaration_ffi_t * declarations);

// Return value is boolean
int hll_parseDeclarationValue(c_css_parser_t * hll_parser, c_css_token_t * token, const char * remainder, int declValueType, int declProperty, c_css_declaration_ffi_t * declaration);

int hll_parseDeclarationShorthand(c_css_parser_t * hll_parser, c_css_token_t * token, const char * remainder, int * properties, c_css_declaration_ffi_t * declarations, int shorthand_type);


#ifdef __cplusplus
}
#endif /* __cplusplus */




#endif
