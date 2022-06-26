#ifndef _HELLO_H_
#define _HELLO_H_

#include <stdbool.h>
#include <stdint.h>
#include "../css.h"



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */



/**
 * \brief Type for representing all lengths within dw::core::style.
 *
 * Lengths are int's. Absolute lengths are represented in the following way:
 *
 * \image html dw-style-length-absolute.png
 *
 * Percentages:
 *
 * \image html dw-style-length-percentage.png
 *
 * Relative lengths (only used in HTML):
 *
 * \image html dw-style-length-relative.png
 *
 * This is an implementation detail, use one of the following functions:
 *
 * Creating lengths:
 *
 * <ul>
 * <li> dw::core::style::createAbsoluteDwLength
 * <li> dw::core::style::createPercentageDwLength
 * <li> dw::core::style::createRelativeDwLength
 * </ul>
 *
 * Examine lengths:
 *
 * <ul>
 * <li> dw::core::style::isAbsoluteDwLength
 * <li> dw::core::style::isPercentageDwLength
 * <li> dw::core::style::isRelativeDwLength
 * <li> dw::core::style::getAbsoluteDwLengthValue
 * <li> dw::core::style::getPercentageDwLengthValue
 * <li> dw::core::style::getRelativeDwLengthValue
 * </ul>
 *
 * "auto" lengths are represented as dw::core::style::LENGTH_AUTO.
 */
typedef struct {
   double dw_length_value;
   int dw_length_type;
   int dw_length_hash;
} DwLength;



typedef struct {
        int top; // TODO: use BorderStyle type instead of int
        int right;
        int bottom;
        int left;
} c_border_style_t;


// TODO: in dillo the borderWidth variables were of type Box. The comment for
// Box type was:
//
// Represents a dimension box according to the CSS box model.
typedef struct {
        int top;
        int right;
        int bottom;
        int left;
} c_border_width_t;

inline void borderWidthSetVal(c_border_width_t * w, int val)
{
        w->top = w->right = w->bottom = w->left = val;
}
inline int borderWidthHashValue(c_border_width_t * w)
{
        return w->top + w->right + w->bottom + w->left;
}
inline bool borderWidthEquals(c_border_width_t * w, c_border_width_t * other)
{
        return w->top == other->top &&
                w->right == other->right &&
                w->bottom == other->bottom &&
                w->left == other->left;
}




typedef struct {
        int top;
        int right;
        int bottom;
        int left;
} c_border_color_t;




// TODO: in dillo the margin variables were of type Box. The comment for
// Box type was:
//
// Represents a dimension box according to the CSS box model.
typedef struct c_style_margin_t {
        int top;
        int right;
        int bottom;
        int left;
} c_style_margin_t;

inline void styleMarginSetVal(c_style_margin_t * m, int val)
{
        m->top = m->right = m->bottom = m->left = val;
}
inline int styleMarginHashValue(c_style_margin_t * m)
{
        return m->top + m->right + m->bottom + m->left;
}
inline bool styleMarginEquals(c_style_margin_t * m, c_style_margin_t * other)
{
        return m->top == other->top &&
                m->right == other->right &&
                m->bottom == other->bottom &&
                m->left == other->left;
}




// TODO: in dillo the padding variables were of type Box. The comment for
// Box type was:
//
// Represents a dimension box according to the CSS box model.
typedef struct c_style_padding_t {
        int top;
        int right;
        int bottom;
        int left;
} c_style_padding_t;

inline void stylePaddingSetVal(c_style_padding_t * p, int val)
{
        p->top = p->right = p->bottom = p->left = val;
}
inline int stylePaddingHashValue(c_style_padding_t * p)
{
        return p->top + p->right + p->bottom + p->left;
}
inline bool stylePaddingEquals(c_style_padding_t * p, c_style_padding_t * other)
{
        return p->top == other->top &&
                p->right == other->right &&
                p->bottom == other->bottom &&
                p->left == other->left;
}




typedef struct c_style_attrs_t {
        c_border_style_t * c_border_style;
        c_border_width_t * c_border_width;
        c_border_color_t * c_border_color;
        c_style_margin_t * c_margin;
        c_style_padding_t * c_padding;

        int c_text_align;
        int c_text_decoration;
        DwLength * c_text_indent;
        int c_text_transform;

        int c_vertical_align;
        int c_white_space;

        DwLength * c_width;
        DwLength * c_height;
        DwLength * c_line_height;

        int c_list_style_position;
        int c_list_style_type;

        int c_display;
        int c_color;
        int c_cursor;

        int c_h_border_spacing;
        int c_v_border_spacing;
        int c_word_spacing;
} c_style_attrs_t;




typedef struct c_font_attrs_t {
        int size;
        int weight;

        const char * name;
        int fontVariant; // TODO: change to enum FontVariant
        int style;       // TODO: change to enum FontStyle

        int xHeight;       // TODO: in dillo this field was in font attrs' parent class "Font".
        int letterSpacing; // TODO: in dillo this field was in font attrs' parent class "Font".
} c_font_attrs_t;



typedef struct c_prefs_t {

        // Font preferences.
        char * font_serif;
        char * font_sans_serif;
        char * font_cursive;
        char * font_fantasy;
        char * font_monospace;
        float font_factor;
        int32_t font_min_size;
        int32_t font_max_size;
} c_prefs_t;



typedef struct c_html_doctype_t {
   int   c_doc_type; /* DilloHtmlDocumentType, as given by DOCTYPE tag */
   float c_doc_type_version;          /* HTML or XHTML version number */
} c_html_doctype_t;




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

   int c_parent_num;
   int c_sibling_num;
   int c_last_child_num;
} c_doctree_node_t;


/**
 * \brief HTML document tree interface.
 *
 * The Doctree class defines the interface to the parsed HTML document tree
 * as it is used for CSS selector matching.
 */
typedef struct c_doctree_t {
        int c_top_node_num;
        c_doctree_node_t * c_root_node;
        int c_num_nodes;
        c_doctree_node_t * c_nodes_array[2048];
} c_doctree_t;

c_doctree_node_t * hll_doctreeNodeNew(void);
void hll_doctreePrint(c_doctree_t * doctree);
int hll_doctreeCtor(void);
void hll_doctreeUpdate(int doctree_ref, int some_value);
int hll_doctreePushNode(int doctree_ref, int element_idx);
void hll_doctreePopNode(int doctree_ref);
const char * hll_doctreeGetTopNodeElementSelectorId(int doctree_ref);



typedef struct c_css_parser_t {
   int c_space_separated;
   int c_buf_offset;
   int c_in_block;

   const char * c_parser_buf;
   int c_parser_buflen;

   int c_origin; // CssOrigin
} c_css_parser_t;




typedef struct c_css_complex_selector_link_t {
   /* It's possible that more than one of these is set in a single
      CssComplexSelectorLink struct. */
   char * c_selector_class[10];
   int c_selector_class_size;

   /* In CSS there can be more pseudo-classes and Haskell can read them, but
      for now C/C++ code will only use first one. */
   char * c_selector_pseudo_class[10];
   int c_selector_pseudo_class_size;

   char * c_selector_id;
   int c_selector_type; /* Index corresponding to html.cc::Tags[]. */

   int c_combinator;
} c_css_complex_selector_link_t;


typedef struct c_css_compound_selector_t {
   /* It's possible that more than one of these is set in a single
      CssComplexSelectorLink struct. */
   char * c_selector_class[10];
   int c_selector_class_size;

   /* In CSS there can be more pseudo-classes and Haskell can read them, but
      for now C/C++ code will only use first one. */
   char * c_selector_pseudo_class[10];
   int c_selector_pseudo_class_size;

   char * c_selector_id;
   int c_selector_type; /* Index corresponding to html.cc::Tags[]. */
} c_css_compound_selector_t;



typedef struct c_css_cached_complex_selector_t {
   int c_match_cache_offset;
   c_css_complex_selector_link_t * c_links[10];
   int c_links_size;
} c_css_cached_complex_selector_t;


typedef struct c_css_value_t {
   int c_type_tag;
   int c_int_val;
   int32_t c_bg_pos_x;
   int32_t c_bg_pos_y;
   char * c_text_val;

   float c_length_val;
   int c_length_type;
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
 *  The c_css_declaration_set_t is applied if the c_css_cached_complex_selector_t matches.
 */
typedef struct c_css_rule_t {
      c_css_cached_complex_selector_t * c_cached_complex_selector;
      c_css_declaration_set_t * c_decl_set;
      int c_specificity;
      int c_position;
} c_css_rule_t;


#define RULES_LIST_SIZE 128
typedef struct c_css_rules_list_t {
   c_css_rule_t * c_rules[RULES_LIST_SIZE];
   int c_rules_size;
} c_css_rules_list_t;


/* Hash map: key: string, value: rules list */
   #define RULES_MAP_SIZE 256
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
   c_css_rules_list_t * c_rules_by_type[90 + 14 /* css_style_sheet_n_tags */];
   c_css_rules_list_t * c_rules_by_any_element;
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
   int c_cache_items[2000]; // For soylentnews.net it's over 1000.
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




bool hll_htmlValidateNameOrIdValue(c_html_doctype_t * doctype, const char * attrName, const char * attrValue);
void hll_getDoctype4(c_html_doctype_t * doctype, const char * buf);
void hll_getDoctypeFromBuffer(c_html_doctype_t * doctype, const char * buf, int buflen);




/* HtmlTag */
char * hll_htmlAttributeGetValue(const char * documentRem, int tagSize, const char * attrName);
/* Return index of tag named \p tagName. The index is an index to 'TagInfo
   Tags[]' array. Return -1 if tag name was not found. */
int hll_htmlTagIndex(const char * tagName);




/* CssParser */
/* Token value is returned through return statement. */
char * hll_nextToken(c_css_parser_t * hll_parser, c_css_token_t * token);
/* Function returns color through return statement. */
//int hll_parseRgbFunction(c_css_parser_t * hll_parser, const char * remainder);

char * hll_declarationValueAsString(c_css_parser_t * hll_parser, c_css_token_t * token, int valueType, int property);
int hll_ignoreBlock(c_css_parser_t * hll_parser, c_css_token_t * token);
int hll_ignoreStatement(c_css_parser_t * hll_parser, c_css_token_t * token);

int hll_cssShorthandInfoIdxByName(const char * shorthandName);
int hll_cssPropertyInfoIdxByName(const char * propertyName);
const char * hll_cssPropertyNameString(int property);



// Return count of selectors in @p selectors
int hll_cssParseSelectors(c_css_parser_t * hll_parser, c_css_token_t * token, c_css_cached_complex_selector_t ** selectors);

//c_css_declaration_set_t * hll_declarationListAddOrUpdateDeclaration(c_css_declaration_set_t * declList, c_css_declaration_t * declaration);

void hll_declarationListAppend(c_css_declaration_set_t * target, const c_css_declaration_set_t * source);

void hll_cssParseElementStyleAttribute(const void /* DilloUrl */ *baseUrl, const char * cssStyleAttribute, int buflen, c_css_declaration_set_t * declSet, c_css_declaration_set_t * declSetImp);




typedef struct {
        float c_length_value;
        int c_length_type;
} c_css_length_t;
void hll_htmlParseAttributeWidthOrHeight(const char * attribute_value, c_css_length_t * length);



/* Css.hsc */

   
/**
 * \brief CSS selector class.
 *
 * \todo Implement missing selector options.
 */
typedef enum {
              CssSelectorCombinatorNone,
              CssSelectorCombinatorDescendant,      // ' '
              CssSelectorCombinatorChild,           // '>'
              CssSelectorCombinatorAdjacentSibling, // '+'
} Combinator;




c_css_rules_list_t * hll_rulesMapGetList(const c_css_rules_map_t * rules_map, const char * key);

void hll_fn(const c_css_rules_list_t ** rules_lists, int numLists, int * index, int * minSpecIndex);

void hll_applyCssRule(int doc_tree_ref, const c_doctree_node_t * dtn, c_css_match_cache_t * match_cache,
                      c_css_declaration_set_t * target,
                      const c_css_rules_list_t ** rules_lists, int numLists, int * index, int minSpecIndex);



void hll_cssStyleSheetApplyStyleSheet(c_css_style_sheet_t * style_sheet, c_css_declaration_set_t * decl_set, int doc_tree_ref, const c_doctree_node_t *dtn, c_css_match_cache_t * match_cache);

void hll_printCssDeclarationSet(c_css_declaration_set_t * declSet);
void hll_printCssIndex(int * index);


void hll_cssContextApplyCssContext(int context_ref,
                                   c_css_declaration_set_t * targetDeclSet,
                                   int doc_tree_ref, int dtn_num,
                                   c_css_declaration_set_t * mainDeclSet, c_css_declaration_set_t * importnatDeclSet, c_css_declaration_set_t * nonCssDeclSet);

void hll_cssContextPrint(const char * path, int css_context_ref);


void hll_parseCss(c_css_parser_t * parser, c_css_token_t * token, int context_ref);


int hll_cssContextCtor(void);


int hll_isTokenComma(c_css_token_t * token);
int hll_isTokenSemicolon(c_css_token_t * token);


/* StyleEngine */
c_css_declaration_t * hll_makeCssDeclaration(int property, c_css_value_t * value);
c_css_declaration_set_t * hll_styleEngineSetNonCssHintOfNodeInt(c_css_declaration_set_t * declSet, int property, int valueType, int intVal, float lengthValue, int lengthType);
c_css_declaration_set_t * hll_styleEngineSetNonCssHintOfNodeString(c_css_declaration_set_t * declSet, int property, int valueType, const char * stringVal);
int hll_styleEngineComputeAbsoluteLengthValue(float lengthValue, int lengthType, c_font_attrs_t * fontAttrs, int percentageBase, float dpiX, float dpiY, int * ptrOut);


void hll_styleEngineSetElementId(int doc_tree_ref, const char * element_id);
void hll_styleEngineSetElementClass(int doc_tree_ref, const char * element_class_tokens);
void hll_styleEngineSetElementPseudoClass(int doc_tree_ref, const char * element_pseudo_class);




void hll_setFontFamily(c_css_value_t * value, c_prefs_t * prefs, c_font_attrs_t * fontAttrs);
void hll_setFontWeight(c_font_attrs_t * fontAttrs, c_css_value_t * cssValue);
void hll_setFontSize(c_css_value_t * cssValue, c_prefs_t * prefs, float dpiX, float dpiY, c_font_attrs_t * parentFontAttrs, c_font_attrs_t * fontAttrs);
void hll_setFontStyle(c_font_attrs_t * fontAttrs, c_css_value_t * cssValue);
void hll_setFontLetterSpacing(c_css_value_t * value, float dpiX, float dpiY, c_font_attrs_t * parentFontAttrs, c_font_attrs_t * fontAttrs);
void hll_setFontVariant(c_font_attrs_t * fontAttrs, c_css_value_t * cssValue);

void hll_styleEngineApplyStyleToFont(c_css_declaration_set_t * declSet, c_prefs_t * prefs, float dpiX, float dpiY, c_font_attrs_t * parentFontAttrs, c_font_attrs_t * fontAttrs);

float hll_styleEngineComputeBorderWidth(c_css_value_t * value, c_font_attrs_t * fontAttrs, float dpiX, float dpiY);
void hll_styleEngineSetStyle(int property, c_css_value_t * value, float lenVal, int lenType, c_font_attrs_t * fontAttrs, float dpiX, float dpiY, c_style_attrs_t * style_attrs);



void hll_createPercentageDwLength(DwLength * length, double v);
void hll_createAbsoluteDwLength(DwLength * length, int v);

int hll_computeDwLength(DwLength * length, double val, int type, c_font_attrs_t * fontAttrs, float dpiX, float dpiY);


#ifdef __cplusplus
}
#endif /* __cplusplus */




#endif
