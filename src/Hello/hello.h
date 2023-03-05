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
 * <li> dw::core::style::getAbsoluteDwLengthValue
 * <li> dw::core::style::getPercentageDwLengthValue
 * </ul>
 *
 * "auto" lengths are represented as dw::core::style::LENGTH_AUTO.
 */
typedef struct {
   double dw_length_value;
   int dw_length_type;
   //int dw_length_hash;
} DwLength;


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




typedef struct c_font_attrs_t {
        int size;
        int weight;

        char * name;
        int fontVariant; // TODO: change to enum FontVariant
        int style;       // TODO: change to enum FontStyle

        int xHeight;       // TODO: in dillo this field was in font attrs' parent class "Font".
        int letterSpacing; // TODO: in dillo this field was in font attrs' parent class "Font".
} c_font_attrs_t;




typedef struct c_style_attrs_t {
        int c_style_attrs_ref; // Handle of Haskell object, to be used in C++ code.

        c_font_attrs_t * c_font_attrs;

        c_border_width_t * c_border_width;
        c_border_color_t * c_border_color;
        c_style_margin_t * c_margin;
        c_style_padding_t * c_padding;

        int c_color;
        int c_background_color;

        int c_word_spacing;

        char c_x_lang[3]; /* Either x_lang[0] == x_lang[1] == 0 (no language
                             set), or x_lang contains the RFC 1766 country
                             code in lower case letters. (Only two letters
                             allowed, currently.) */
        char * c_x_tooltip; /* TODO: what is the limit of size of this buffer? */
} c_style_attrs_t;

int ffiStyleAttrsCtor(void);
void ffiStyleAttrsInitValues(int ref);
bool ffiStyleAttrsEqual(int ref1, int ref2);
int ffiStyleAttrsHashValue(int ref);
void ffiStyleAttrsCopy(int refTo, int refFrom);
void ffiStyleAttrsReset(int ref);

int ffiStyleAttrsTextAlign(int ref);

int ffiStyleAttrsVerticalAlign(int ref);

int ffiStyleAttrsTextDecoration(int ref);
void ffiStyleAttrsSetTextDecoration(int ref, int val);

int ffiStyleAttrsTextTransform(int ref);

int ffiStyleAttrsCursor(int ref);
void ffiStyleAttrsSetCursor(int ref, int val);

int ffiStyleAttrsWhiteSpace(int ref);

int ffiStyleAttrsListStylePosition(int ref);
int ffiStyleAttrsListStyleType(int ref);

int ffiStyleAttrsXLink(int ref);
void ffiStyleAttrsSetXLink(int ref, int val);
int ffiStyleAttrsXImg(int ref);

int ffiStyleAttrsBorderCollapse(int ref);

void ffiStyleAttrsSetCollapseTableAttrs(int refTable, int refCell);
void ffiStyleAttrsSetCollapseCellAttrs(int ref);

int ffiStyleAttrsBorderStyleTop(int ref);
int ffiStyleAttrsBorderStyleRight(int ref);
int ffiStyleAttrsBorderStyleBottom(int ref);
int ffiStyleAttrsBorderStyleLeft(int ref);
void ffiStyleAttrsSetBorderStyle(int ref, int val);

void ffiStyleAttrsGetWidth(int ref, DwLength * len);
void ffiStyleAttrsGetHeight(int ref, DwLength * len);

int ffiStyleAttrsSetWidth(int ref, DwLength * len);
int ffiStyleAttrsSetHeight(int ref, DwLength * len);

void ffiStyleAttrsGetTextIndent(int ref, DwLength * len);

void ffiStyleAttrsGetLineHeight(int ref, DwLength * len);

void ffiStyleAttrsBgPositionX(int ref, DwLength * len);
void ffiStyleAttrsBgPositionY(int ref, DwLength * len);
void ffiStyleAttrsSetBgPositionX(int ref, DwLength * len);
void ffiStyleAttrsSetBgPositionY(int ref, DwLength * len);

int ffiStyleAttrsHorizBorderSpacing(int ref);
int ffiStyleAttrsVertBorderSpacing(int ref);
void ffiStyleAttrsSetHorizBorderSpacing(int ref, int val);
void ffiStyleAttrsSetVertBorderSpacing(int ref, int val);

int ffiStyleAttrsDisplay(int ref);




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



// FIXME: en.wikipedia.org has a class selector for <body> that has 23 elements. So 10 is too little.
#define SELECTOR_CLASS_MAX 10
struct c_doctree_node_t;
/* From doctree.h */
typedef struct c_doctree_node_t {
   int c_unique_num; // unique ascending id
   int c_html_element_idx; /* Index to html.cc::Tags */

   /* Css Selectors. */
   char * c_element_selector_pseudo_class;
   char * c_element_selector_id;
   char * c_element_selector_class[SELECTOR_CLASS_MAX];
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

c_doctree_node_t * ffiDoctreeNodeNew(void);
void ffiDoctreePrint(c_doctree_t * doctree);
int ffiDoctreeCtor(void);
int ffiDoctreePushNode(int doctree_ref, int element_idx);
void ffiDoctreePopNode(int doctree_ref);
const char * ffiDoctreeGetTopNodeElementSelectorId(int doctree_ref);



typedef struct c_css_parser_t {
   int c_space_separated;
   int c_buf_offset;
   int c_in_block;

   const char * c_parser_buf;
   int c_parser_buflen;

   int c_origin; // CssOrigin
} c_css_parser_t;




typedef struct c_css_token_t {
   int c_type;
   char * c_value;
} c_css_token_t;


/*
  TODO: don't hardcode the value.

  90 is the full number of html4 elements, including those which we have
  implemented. From html5, let's add: article, header, footer, mark, nav,
  section, aside, figure, figcaption, wbr, audio, video, source, embed.
*/
static const int css_style_sheet_n_tags = 90 + 14;

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

/*
typedef struct c_css_match_cache_t {
   int c_cache_items[2000]; // For soylentnews.net it's over 1000.
   int c_cache_items_size;
} c_css_match_cache_t;
*/



/* URL */
bool ffiHostIsIP(const char * hostname);

/* cookies */
int ffiLookupActionForDomain(const char * domain);

/* Gif */
int ffiParseExtension(c_gif_t * ffi_gif, const unsigned char * buf, int size);

/* Colors */
int ffiColorsStringToColor(const char * str, int default_color);
int ffiColorsVisitedColor(int candidate, int txt, int lnk, int bg);

/* HtmlEntity */
int64_t ffiHtmlEntityToIsoCode(const char * token, int tokenLen);




bool ffiHtmlValidateNameOrIdValue(c_html_doctype_t * doctype, const char * attrName, const char * attrValue);
void ffiGetDoctype4(c_html_doctype_t * doctype, const char * buf);
void ffiGetDoctypeFromBuffer(c_html_doctype_t * doctype, const char * buf, int buflen);




/* HtmlTag */
char * ffiHtmlAttributeGetValue(const char * documentRem, int tagSize, const char * attrName);
/* Return index of tag named \p tagName. The index is an index to 'TagInfo
   Tags[]' array. Return -1 if tag name was not found. */
int ffiHtmlTagIndex(const char * tagName);




/* CssParser */
/* Token value is returned through return statement. */
char * ffiNextToken(c_css_parser_t * parser, c_css_token_t * token);
/* Function returns color through return statement. */
//int ffiParseRgbFunction(c_css_parser_t * parser, const char * remainder);

//char * ffiDeclarationValueAsString(c_css_parser_t * parser, c_css_token_t * token, int valueType);
int ffiIgnoreBlock(c_css_parser_t * parser, c_css_token_t * token);
int ffiIgnoreStatement(c_css_parser_t * parser, c_css_token_t * token);




void ffiCssParseElementStyleAttribute(const void /* DilloUrl */ *baseUrl, const char * cssStyleAttribute, int buflen, int main_decl_set_ref, int important_decl_set_ref);




typedef struct {
        float c_length_value;
        int c_length_type;
} c_css_length_t;
void ffiHtmlParseAttributeWidthOrHeight(const char * attribute_value, c_css_length_t * length);



int ffiCssContextApplyCssContext(int context_ref,
                                 int doc_tree_ref, int dtn_num,
                                 int main_decl_set_ref,
                                 int important_decl_set_ref,
                                 int non_css_decl_set_ref);

void ffiCssContextPrint(const char * path, int css_context_ref);


void ffiParseCss(c_css_parser_t * parser, c_css_token_t * token, int context_ref);


int ffiCssContextCtor(void);


int ffiIsTokenComma(c_css_token_t * token);
int ffiIsTokenSemicolon(c_css_token_t * token);



int ffiStyleEngineSetNonCssHintOfNodeLength(int non_css_decl_set_ref, int property, float lengthValue, int lengthType);
int ffiStyleEngineSetNonCssHintOfNodeEnum(int non_css_decl_set_ref, int property, int enumVal);
int ffiStyleEngineSetNonCssHintOfNodeColor(int non_css_decl_set_ref, int property, int color);
int ffiStyleEngineSetNonCssHintOfNodeString(int non_css_decl_set_ref, int property, const char * stringVal);

/* For setting pseudo-css-properties. */
int ffiStyleEngineSetXImgOfNode(int non_css_decl_set_ref, int intVal);
int ffiStyleEngineSetXLangOfNode(int non_css_decl_set_ref, const char * stringVal);
int ffiStyleEngineSetXLinkOfNode(int non_css_decl_set_ref, int intVal);
int ffiStyleEngineSetXTooltipOfNode(int non_css_decl_set_ref, const char * stringVal);


void ffiStyleEngineSetElementId(int doc_tree_ref, const char * element_id);
void ffiStyleEngineSetElementClass(int doc_tree_ref, const char * element_class_tokens);
void ffiStyleEngineSetElementPseudoClass(int doc_tree_ref, const char * element_pseudo_class);




void ffiStyleEngineApplyStyleToGivenNode(int merged_decl_set_ref, c_prefs_t * prefs, float dpiX, float dpiY, c_style_attrs_t * parent_style_attrs, c_style_attrs_t * style_attrs);


void ffiStyleEngineMakeWordStyle(int refAttrs, int refBwAttrs);
void ffiStyleEnginePreprocessAttrsInheritBackground(int refTo, int refFrom);
void ffiStyleEngineMakeWordStyleInheritBackground(int refTo, int refFrom);
void ffiStyleEnginePostprocessAttrs(int ref);



int ffiDeclarationSetCtor(void);

int ffiInheritNonCssHints(int parent_non_css_decl_set_ref, int non_css_decl_set_ref);

void ffiCreatePercentageDwLength(DwLength * length, double v);
void ffiCreateAbsoluteDwLength(DwLength * length, int v);
void ffiCreateAutoDwLength(DwLength * length);

bool ffiIsAutoDwLength(DwLength * length);
bool ffiIsAbsoluteDwLength(DwLength * length);
bool ffiIsPercentageDwLength(DwLength * length);

int ffiGetAbsoluteDwLengthValue(DwLength * length);
double ffiGetPercentageDwLengthValue(DwLength * length);

int ffiGetDwLengthHash(DwLength * length);


#ifdef __cplusplus
}
#endif /* __cplusplus */




#endif
