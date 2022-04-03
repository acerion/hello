#ifndef __CSS_HH__
#define __CSS_HH__

#include "dw/core.hh"
#include "doctree.hh"
#include "css.h"
#include "Hello/hello.h"

typedef struct c_css_declaration_lists_t {
   c_css_declaration_set_t * main;
   c_css_declaration_set_t * important;
   c_css_declaration_set_t * nonCss;
} c_css_declaration_lists_t;

/* This is needed only for debug prints. */
typedef enum {
   CSS_TYPE_INTEGER,
   CSS_TYPE_ENUM,
   CSS_TYPE_MULTI_ENUM,
   CSS_TYPE_LENGTH_PERCENTAGE,
   CSS_TYPE_LENGTH,
   CSS_TYPE_SIGNED_LENGTH,
   CSS_TYPE_LENGTH_PERCENTAGE_NUMBER,
   CSS_TYPE_AUTO,
   CSS_TYPE_COLOR,
   CSS_TYPE_FONT_WEIGHT,
   CSS_TYPE_STRING,
   CSS_TYPE_SYMBOL,
   CSS_TYPE_URI,
   CSS_TYPE_BACKGROUND_POSITION,
   CSS_TYPE_UNUSED
} CssValueType;

enum CssDeclarationValueType {
   CssDeclarationValueTypeINTEGER,            /* This type is only used internally, for x-* properties. */
   CssDeclarationValueTypeENUM,               /* Value is i, if represented by enum_symbols[i]. */
   CssDeclarationValueTypeMULTI_ENUM,         /* For all enum_symbols[i], 1 << i are combined. */
   CssDeclarationValueTypeLENGTH_PERCENTAGE,  /* <length> or <percentage>. Represented by CssLength. */
   CssDeclarationValueTypeLENGTH,             /* <length>, represented as CssLength. Note: In some
                                                 cases, CSS_TYPE_LENGTH is used instead of
                                                 CSS_TYPE_LENGTH_PERCENTAGE, only because Dw cannot
                                                 handle percentages in this particular case (e.g.
                                                 'margin-*-width'). */
   CssDeclarationValueTypeSIGNED_LENGTH,      /* As CSS_TYPE_LENGTH but may be negative. */
   CssDeclarationValueTypeLENGTH_PERCENTAGE_NUMBER,  /* <length> or <percentage>, or <number> */
   CssDeclarationValueTypeAUTO,               /* Represented as CssLength of type CSS_LENGTH_TYPE_AUTO */
   CssDeclarationValueTypeCOLOR,              /* Represented as integer. */
   CssDeclarationValueTypeFONT_WEIGHT,        /* this very special and only used by 'font-weight' */
   CssDeclarationValueTypeSTRING,             /* <string> */
   CssDeclarationValueTypeSYMBOL,             /* Symbols, which are directly copied (as opposed to
                                                 CSS_PROPERTY_DATA_TYPE_ENUM and
                                                 CSS_PROPERTY_DATA_TYPE_MULTI_ENUM). Used for
                                                 'font-family'. */
   CssDeclarationValueTypeURI,                /* <uri> */
   CssDeclarationValueTypeBACKGROUND_POSITION,
   CssDeclarationValueTypeUNUSED              /* Not yet used. Will itself get unused some day. */
} ;




typedef enum {
   CSS_PROPERTY_END = -1, // used as terminator in CssShorthandInfo
   CSS_PROPERTY_BACKGROUND_ATTACHMENT,
   CSS_PROPERTY_BACKGROUND_COLOR,
   CSS_PROPERTY_BACKGROUND_IMAGE,
   CSS_PROPERTY_BACKGROUND_POSITION,
   CSS_PROPERTY_BACKGROUND_REPEAT,
   CSS_PROPERTY_BORDER_BOTTOM_COLOR,
   CSS_PROPERTY_BORDER_BOTTOM_STYLE,
   CSS_PROPERTY_BORDER_BOTTOM_WIDTH,
   CSS_PROPERTY_BORDER_COLLAPSE,
   CSS_PROPERTY_BORDER_LEFT_COLOR,
   CSS_PROPERTY_BORDER_LEFT_STYLE,
   CSS_PROPERTY_BORDER_LEFT_WIDTH,
   CSS_PROPERTY_BORDER_RIGHT_COLOR,
   CSS_PROPERTY_BORDER_RIGHT_STYLE,
   CSS_PROPERTY_BORDER_RIGHT_WIDTH,
   CSS_PROPERTY_BORDER_SPACING,
   CSS_PROPERTY_BORDER_TOP_COLOR,
   CSS_PROPERTY_BORDER_TOP_STYLE,
   CSS_PROPERTY_BORDER_TOP_WIDTH,
   CSS_PROPERTY_BOTTOM,
   CSS_PROPERTY_CAPTION_SIDE,
   CSS_PROPERTY_CLEAR,
   CSS_PROPERTY_CLIP,
   CSS_PROPERTY_COLOR,
   CSS_PROPERTY_CONTENT,
   CSS_PROPERTY_COUNTER_INCREMENT,
   CSS_PROPERTY_COUNTER_RESET,
   CSS_PROPERTY_CURSOR,
   CSS_PROPERTY_DIRECTION,
   CSS_PROPERTY_DISPLAY,
   CSS_PROPERTY_EMPTY_CELLS,
   CSS_PROPERTY_FLOAT,
   CSS_PROPERTY_FONT_FAMILY,
   CSS_PROPERTY_FONT_SIZE,
   CSS_PROPERTY_FONT_SIZE_ADJUST,
   CSS_PROPERTY_FONT_STRETCH,
   CSS_PROPERTY_FONT_STYLE,
   CSS_PROPERTY_FONT_VARIANT,
   CSS_PROPERTY_FONT_WEIGHT,
   CSS_PROPERTY_HEIGHT,
   CSS_PROPERTY_LEFT,
   CSS_PROPERTY_LETTER_SPACING,
   CSS_PROPERTY_LINE_HEIGHT,
   CSS_PROPERTY_LIST_STYLE_IMAGE,
   CSS_PROPERTY_LIST_STYLE_POSITION,
   CSS_PROPERTY_LIST_STYLE_TYPE,
   CSS_PROPERTY_MARGIN_BOTTOM,
   CSS_PROPERTY_MARGIN_LEFT,
   CSS_PROPERTY_MARGIN_RIGHT,
   CSS_PROPERTY_MARGIN_TOP,
   CSS_PROPERTY_MARKER_OFFSET,
   CSS_PROPERTY_MARKS,
   CSS_PROPERTY_MAX_HEIGHT,
   CSS_PROPERTY_MAX_WIDTH,
   CSS_PROPERTY_MIN_HEIGHT,
   CSS_PROPERTY_MIN_WIDTH,
   CSS_PROPERTY_OUTLINE_COLOR,
   CSS_PROPERTY_OUTLINE_STYLE,
   CSS_PROPERTY_OUTLINE_WIDTH,
   CSS_PROPERTY_OVERFLOW,
   CSS_PROPERTY_PADDING_BOTTOM,
   CSS_PROPERTY_PADDING_LEFT,
   CSS_PROPERTY_PADDING_RIGHT,
   CSS_PROPERTY_PADDING_TOP,
   CSS_PROPERTY_POSITION,
   CSS_PROPERTY_QUOTES,
   CSS_PROPERTY_RIGHT,
   CSS_PROPERTY_TEXT_ALIGN,
   CSS_PROPERTY_TEXT_DECORATION,
   CSS_PROPERTY_TEXT_INDENT,
   CSS_PROPERTY_TEXT_SHADOW,
   CSS_PROPERTY_TEXT_TRANSFORM,
   CSS_PROPERTY_TOP,
   CSS_PROPERTY_UNICODE_BIDI,
   CSS_PROPERTY_VERTICAL_ALIGN,
   CSS_PROPERTY_VISIBILITY,
   CSS_PROPERTY_WHITE_SPACE,
   CSS_PROPERTY_WIDTH,
   CSS_PROPERTY_WORD_SPACING,
   CSS_PROPERTY_Z_INDEX,
   CSS_PROPERTY_X_LINK,
   CSS_PROPERTY_X_COLSPAN,
   CSS_PROPERTY_X_ROWSPAN,
   PROPERTY_X_LINK,
   PROPERTY_X_LANG,
   PROPERTY_X_IMG,
   PROPERTY_X_TOOLTIP,
   CSS_PROPERTY_LAST
} CssDeclarationProperty;

typedef enum {
   CSS_BORDER_WIDTH_THIN,
   CSS_BORDER_WIDTH_MEDIUM,
   CSS_BORDER_WIDTH_THICK,
} CssBorderWidthExtensions;

typedef enum {
   CSS_FONT_WEIGHT_BOLD,
   CSS_FONT_WEIGHT_BOLDER,
   CSS_FONT_WEIGHT_LIGHT,
   CSS_FONT_WEIGHT_LIGHTER,
   CSS_FONT_WEIGHT_NORMAL,
} CssFontWeightExtensions;

typedef enum {
   CSS_FONT_SIZE_LARGE,
   CSS_FONT_SIZE_LARGER,
   CSS_FONT_SIZE_MEDIUM,
   CSS_FONT_SIZE_SMALL,
   CSS_FONT_SIZE_SMALLER,
   CSS_FONT_SIZE_XX_LARGE,
   CSS_FONT_SIZE_XX_SMALL,
   CSS_FONT_SIZE_X_LARGE,
   CSS_FONT_SIZE_X_SMALL,
} CssFontSizeExtensions;

typedef enum {
   CSS_LETTER_SPACING_NORMAL
} CssLetterSpacingExtensions;

typedef enum {
   CSS_WORD_SPACING_NORMAL
} CssWordSpacingExtensions;


void css_declaration_print(FILE * file, c_css_declaration_t * declaration);


c_css_declaration_set_t * declarationListNew(void);
c_css_declaration_set_t * declarationListNew(const c_css_declaration_set_t * declList);

void css_declaration_set_add_or_update_declaration(c_css_declaration_set_t * decl_set, CssDeclarationProperty property, c_css_value_t value);

enum class CssSelectorType {
   NONE,
   CLASS,
   PSEUDO_CLASS,
   ID,
};





enum {
      CssSimpleSelectorElementNone = -1,
      CssSimpleSelectorElementAny = -2,
};



/* c_css_complex_selector_link_t methods. */
void css_complex_selector_link_print(FILE * file, c_css_complex_selector_link_t * link);
/* Print compound simple selector in one line. Fields are printed in the same order as
   in Haskell record. */
void css_complex_selector_link_print_flat(FILE * file, const c_css_complex_selector_link_t * link);
int css_complex_selector_link_specificity(c_css_complex_selector_link_t * link);






/* c_css_style_sheet_t methods. */
void css_style_sheet_apply_style_sheet(c_css_style_sheet_t * style_sheet, c_css_declaration_set_t * decl_set, Doctree * docTree,
                                       const c_doctree_node_t * dtn, c_css_match_cache_t * match_cache);




c_css_context_t * c_css_context_new(void);
void css_context_apply_css_context(c_css_context_t * context,
                                   c_css_declaration_set_t * mergedDeclList,
                                   Doctree *docTree, c_doctree_node_t * dtn,
                                   c_css_declaration_lists_t * declLists);




#endif
