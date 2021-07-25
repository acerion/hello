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




struct c_doctree_node_t;
/* From doctree.h */
typedef struct c_doctree_node_t {
      int c_unique_num; // unique ascending id
      int c_html_element_idx; /* Index to html.cc::Tags */

      /* Css Selectors. */
      char * c_element_selector_pseudo_class;
      char * c_element_selector_id;
      char * c_element_selector_class[10];
      int c_element_selector_class_size;

      struct c_doctree_node_t * c_parent;
      struct c_doctree_node_t * c_sibling;
      struct c_doctree_node_t * c_last_child;
} c_doctree_node_t;




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
   int c_match_cache_offset;
   c_css_simple_selector_t * c_simple_selectors[10];
   int c_simple_selectors_size;
} c_css_selector_t;


typedef struct c_css_value_t {
   int c_type_tag;
   int c_int_val;
   int32_t c_bg_pos_x;
   int32_t c_bg_pos_y;
   char * c_text_val;
} c_css_value_t;

/**
 * \brief This class holds a CSS declaration: a pair of property and value.
 */
typedef struct c_css_declaration_t {
   int c_important;
   int c_property;
   c_css_value_t * c_value;
} c_css_declaration_t;

/**
 * \brief A list of c_css_declaration_t objects.
 */
#define DECLARATIONS_COUNT_IN_SET 100
typedef struct c_css_declaration_set_t {
   int c_is_safe; // TODO: this should be true by default
   c_css_declaration_t * c_declarations[DECLARATIONS_COUNT_IN_SET];
   int c_declarations_size;
} c_css_declaration_set_t;

typedef struct c_css_token_t {
   int c_type;
   char * c_value;
} c_css_token_t;


/**
 * \brief A pair of CSS selector and CSS declarations set.
 *
 *  The c_css_declaration_set_t is applied if the c_css_selector_t matches.
 */
typedef struct c_css_rule_t {
      c_css_selector_t * c_selector;
      c_css_declaration_set_t * c_decl_set;
      int c_specificity;
      int c_position;
} c_css_rule_t;


#define RULES_LIST_SIZE 1024
typedef struct c_css_rules_list_t {
   c_css_rule_t * c_rules[RULES_LIST_SIZE];
   int c_rules_size;
} c_css_rules_list_t;


/* Hash map: key: string, value: rules list */
   #define RULES_MAP_SIZE 1024
typedef struct c_css_rules_map_t {
   char * c_strings[RULES_MAP_SIZE];
   c_css_rules_list_t * c_rules_lists[RULES_MAP_SIZE];
   int c_rules_map_size;
} c_css_rules_map_t;


/*
  TODO: don't hardcode the value.

  90 is the full number of html4 elements, including those which we have
  implemented. From html5, let's add: article, header, footer, mark, nav,
  section, aside, figure, figcaption, wbr, audio, video, source, embed.
*/
static const int css_style_sheet_n_tags = 90 + 14;


/**
 * \brief A list of c_css_rule_t rules.
 *
 * In apply_style_sheet() all matching rules are applied.
 */
typedef struct c_css_style_sheet_t {
   c_css_rules_map_t * c_rules_by_id;
   c_css_rules_map_t * c_rules_by_class;
   c_css_rules_list_t * c_rules_by_element[90 + 14 /* css_style_sheet_n_tags */];
   c_css_rules_list_t * c_rules_by_any_element;

   int c_required_match_cache;
} c_css_style_sheet_t;

/* Origin and weight. Used only internally.*/
typedef enum {
   CSS_PRIMARY_USER_AGENT,
   CSS_PRIMARY_USER,
   CSS_PRIMARY_AUTHOR,
   CSS_PRIMARY_AUTHOR_IMPORTANT,
   CSS_PRIMARY_USER_IMPORTANT,

   CSS_PRIMARY_ORDER_SIZE,
} CssPrimaryOrder;

typedef enum {
   CSS_ORIGIN_USER_AGENT,
   CSS_ORIGIN_USER,
   CSS_ORIGIN_AUTHOR,
} CssOrigin;

typedef struct c_css_match_cache_t {
   int c_cache_items[10];
   int c_cache_items_size;
} c_css_match_cache_t;

/**
 * \brief A set of c_css_style_sheet_t sheets
 */
typedef struct c_css_context_t {
   c_css_style_sheet_t * c_sheets[CSS_PRIMARY_ORDER_SIZE];
   c_css_match_cache_t * c_match_cache;
   int c_rule_position;
} c_css_context_t;


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
int hll_ignoreBlock(c_css_parser_t * hll_parser, c_css_token_t * token, const char * remainder);
int hll_ignoreStatement(c_css_parser_t * hll_parser, c_css_token_t * token, const char * remainder);

int hll_cssShorthandInfoIdxByName(const char * shorthandName);
int hll_cssPropertyInfoIdxByName(const char * propertyName);
const char * hll_cssPropertyNameString(int property);


CssLengthType hll_cssLengthType(int cssLength);
float hll_cssLengthValue(int cssLength);
int hll_cssCreateLength(float val, CssLengthType t);

// Return count of selectors in @p selectors
int hll_cssParseSelectors(c_css_parser_t * hll_parser, c_css_token_t * token, const char * remainder, c_css_selector_t ** selectors);

int hll_declarationListAddOrUpdateDeclaration(c_css_declaration_set_t * declList, c_css_declaration_t * declaration);

void hll_parseDeclarationWrapper(c_css_parser_t * hll_parser, c_css_token_t * token, const char * remainder,
                                 c_css_declaration_set_t * declList, c_css_declaration_set_t * declListImportant);

void hll_declarationListAppend(c_css_declaration_set_t * target, const c_css_declaration_set_t * source);

void hll_cssParseElementStyleAttribute(const void /* DilloUrl */ *baseUrl, const char * cssStyleAttribute, int buflen, c_css_declaration_set_t * declSet, c_css_declaration_set_t * declSetImp);




/* Css.hsc */

/* Function returns boolean. */
int hll_simpleSelectorMatches(const c_css_simple_selector_t * simSel, const c_doctree_node_t * dtn);
int hll_selectorSpecificity(const c_css_selector_t * selector);


c_css_rules_list_t * hll_rulesMapGetList(const c_css_rules_map_t * rules_map, const char * key);



void hll_matchCacheSetSize(c_css_match_cache_t * match_cache, int size);

void hll_cssContextAddRule(c_css_context_t * context, c_css_rule_t * rule, CssPrimaryOrder order);


#ifdef __cplusplus
}
#endif /* __cplusplus */




#endif
