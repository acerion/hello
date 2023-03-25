#ifndef _HELLO_H_
#define _HELLO_H_

#include <stdbool.h>
#include <stdint.h>
#include "../css.h"



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */




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
} c_style_attrs_t;




int ffiStyleAttrsCtor(void);
int ffiStyleAttrsCopyCtor(int ref);
void ffiStyleAttrsInitValues(int ref);
bool ffiStyleAttrsEqual(int ref1, int ref2);
int ffiStyleAttrsHashValue(int ref);
void ffiStyleAttrsCopy(int refTo, int refFrom);
void ffiStyleAttrsResetNonInheritedValues(int ref);

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

void ffiStyleAttrsSetCollapseTableAttrs(int refTable, int refCell, int borderWidthTop);
void ffiStyleAttrsSetCollapseCellAttrs(int ref, int borderWidthTop);

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

int ffiStyleAttrsWordSpacing(int ref);

char * ffiStyleAttrsXTooltip(int ref);

void ffiStyleAttrsXLang(int ref, char * buf, int buf_size);
// This function allows only setting of lang, but doesn't allow clearing it.
void ffiStyleAttrsSetXLang(int ref, char first, char second);

int ffiStyleAttrsBgRepeat(int ref);
void ffiStyleAttrsSetBgRepeat(int ref, int val);

int ffiStyleAttrsBgAttachment(int ref);

void ffiStyleAttrsMargin(int ref, c_style_margin_t * margin);
void ffiStyleAttrsSetMargin(int ref, int val);
void ffiStyleAttrsSetMargin2(int ref, c_style_margin_t * margin);

void ffiStyleAttrsPadding(int ref, c_style_padding_t * padding);
void ffiStyleAttrsSetPadding(int ref, int val);

void ffiStyleAttrsBorderWidth(int ref, c_border_width_t * border_width);
void ffiStyleAttrsSetBorderWidth(int ref, int val);

int ffiStyleAttrsBoxOffsetX(int ref);
int ffiStyleAttrsBoxOffsetY(int ref);

int ffiStyleAttrsBoxRestWidth(int ref);
int ffiStyleAttrsBoxRestHeight(int ref);

int ffiStyleAttrsBoxDiffWidth(int ref);
int ffiStyleAttrsBoxDiffHeight(int ref);

int ffiStyleAttrsColor(int ref);
void ffiStyleAttrsSetColor(int ref, int val);

int ffiStyleAttrsBackgroundColor(int ref);
void ffiStyleAttrsSetBackgroundColor(int ref, int val);

void ffiStyleAttrsBorderColor(int ref, c_border_color_t * color);
void ffiStyleAttrsSetBorderColor(int ref, int val);
void ffiStyleAttrsSetBorderColor2(int ref, c_border_color_t * color);

void ffiStyleAttrsFontAttrs(int ref, c_font_attrs_t * font_attrs);
void ffiStyleAttrsSetFontAttrs(int ref, c_font_attrs_t * font_attrs);

char * ffiStyleAttrsBgImage(int ref);

void ffiFontAttrsMakeFontAttrsFromPrefs(c_font_attrs_t * font_attrs,  c_prefs_t * prefs);




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




void ffiStyleEngineDoctreePopNode(int style_engine_ref);
const char * ffiStyleEngineDoctreeGetTopNodeElementSelectorId(int style_engine_ref);
int ffiStyleEngineDoctreeGetTopNode(int style_engine_ref);
int ffiStyleEngineDoctreeGetTopNodeHtmlElementIdx(int style_engine_ref);




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




void ffiCssParseElementStyleAttribute(int style_engine_ref, const void /* DilloUrl */ *baseUrl, const char * cssStyleAttribute, int buflen);




typedef struct {
        float c_length_value;
        int c_length_type;
} c_css_length_t;
void ffiHtmlParseAttributeWidthOrHeight(const char * attribute_value, c_css_length_t * length);



void ffiCssContextPrint(const char * path, int css_context_ref);


void ffiParseCss(c_css_parser_t * parser, c_css_token_t * token, int context_ref);


int ffiCssContextCtor(void);


int ffiIsTokenComma(c_css_token_t * token);
int ffiIsTokenSemicolon(c_css_token_t * token);


int ffiStyleEngineCtor(void);
int ffiStyleEngineStyleNodesStackSize(int ref);
void ffiStyleEngineStyleNodesStackPushEmptyNode(int ref);
void ffiStyleEngineStyleNodesStackPop(int ref);
void ffiStyleEngineStyleNodesClearNonCssHints(int ref);

void ffiStyleEngineSetNonCssHintOfNodeLength(int non_css_decl_set_ref, int property, float lengthValue, int lengthType);
void ffiStyleEngineSetNonCssHintOfNodeEnum(int non_css_decl_set_ref, int property, int enumVal);
void ffiStyleEngineSetNonCssHintOfNodeColor(int non_css_decl_set_ref, int property, int color);
void ffiStyleEngineSetNonCssHintOfNodeString(int non_css_decl_set_ref, int property, const char * stringVal);

/* For setting pseudo-css-properties. */
void ffiStyleEngineSetXImgOfNode(int non_css_decl_set_ref, int intVal);
void ffiStyleEngineSetXLangOfNode(int non_css_decl_set_ref, const char * stringVal);
void ffiStyleEngineSetXLinkOfNode(int non_css_decl_set_ref, int intVal);
void ffiStyleEngineSetXTooltipOfNode(int non_css_decl_set_ref, const char * stringVal);


void ffiStyleEngineDoctreeSetElementId(int style_engine_ref, const char * element_id);
void ffiStyleEngineDoctreeSetElementClass(int style_engine_ref, const char * element_class_tokens);
void ffiStyleEngineDoctreeSetElementPseudoClass(int style_engine_ref, const char * element_pseudo_class);




void ffiStyleEngineMakeStyleAttrs(int style_engine_ref, int context_ref, int style_node_idx, c_prefs_t * prefs, float dpiX, float dpiY, int parent_style_attrs_ref, int this_style_attrs_ref);


void ffiStyleEngineMakeWordStyle(int refAttrs, int refBwAttrs);
void ffiStyleEnginePreprocessAttrsInheritBackground(int refTo, int refFrom);
void ffiStyleEnginePreprocessAttrs(int refTo);
void ffiStyleEngineMakeWordStyleInheritBackground(int refTo, int refFrom);
void ffiStyleEnginePostprocessAttrs(int ref);

void ffiStyleEngineStartElement(int style_engin_ref, int html_element_idx);


int ffiDeclarationSetCtor(void);

void ffiInheritNonCssHints(int style_engine_ref);

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
