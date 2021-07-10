#ifndef __CSS_HH__
#define __CSS_HH__

#include "dw/core.hh"
#include "doctree.hh"
#include "css.h"
#include "haskell/hello.h"

/* Origin and weight. Used only internally.*/
typedef enum {
   CSS_PRIMARY_USER_AGENT,
   CSS_PRIMARY_USER,
   CSS_PRIMARY_AUTHOR,
   CSS_PRIMARY_AUTHOR_IMPORTANT,
   CSS_PRIMARY_USER_IMPORTANT,
   CSS_PRIMARY_LAST,
} CssPrimaryOrder;

typedef enum {
   CSS_ORIGIN_USER_AGENT,
   CSS_ORIGIN_USER,
   CSS_ORIGIN_AUTHOR,
} CssOrigin;

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

void css_declaration_set_print(FILE * file, c_css_declaration_set_t * decl_set);
void css_declaration_set_add_or_update_declaration(c_css_declaration_set_t * decl_set, CssDeclarationProperty property, c_css_value_t value);

enum class CssSelectorType {
   NONE,
   CLASS,
   PSEUDO_CLASS,
   ID,
};




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

enum {
      CssSimpleSelectorElementNone = -1,
      CssSimpleSelectorElementAny = -2,
};



typedef struct MatchCache {
   int arr[10];
   int size;
} MatchCache;
void match_cache_set_size(MatchCache * match_cache, int new_size);




/* c_css_simple_selector_t methods. */
void css_simple_selector_print(FILE * file, c_css_simple_selector_t * selector);
/* Print simple selector in one line. Fields are printed in the same order as
   in Haskell record. */
void css_simple_selector_print_flat(FILE * file, const c_css_simple_selector_t * sim_sel);
int css_simple_selector_specificity(c_css_simple_selector_t * selector);




/* c_css_selector_t methods. */
bool css_selector_matches(c_css_selector_t * selector, Doctree * dt, const c_doctree_node_t * dtn, int sim_sel_idx, Combinator comb, MatchCache * match_cache);
c_css_simple_selector_t * css_selector_get_top_simple_selector(c_css_selector_t * selector);
void css_selector_set_match_cache_offset(c_css_selector_t * selector, int offset);
int css_selector_get_required_match_cache(c_css_selector_t * selector);
bool css_selector_has_pseudo_class(c_css_selector_t * selector);
int css_selector_specificity(c_css_selector_t * selector);
void css_selector_print(FILE * file, c_css_selector_t * selector);




/* c_css_rule_t methods. */
bool css_rule_is_safe(const c_css_rule_t * rule);
c_css_rule_t * css_rule_new(c_css_selector_t * selector, c_css_declaration_set_t * decl_set, int rule_position);




/* c_css_rules_list_t methods. */
void css_rules_list_insert_rule(c_css_rules_list_t * list, c_css_rule_t * rule);




/* c_css_style_sheet_t methods. */
void css_style_sheet_add_rule(c_css_style_sheet_t * style_sheet, c_css_rule_t * rule);
void css_style_sheet_apply_style_sheet(c_css_style_sheet_t * style_sheet, FILE * file, c_css_declaration_set_t * decl_set, Doctree * docTree,
                                       const c_doctree_node_t * dtn, MatchCache * match_cache);




/**
 * \brief A set of c_css_style_sheet_t sheets
 */
class CssContext {
   public:
      static c_css_style_sheet_t userAgentSheet;
      c_css_style_sheet_t sheet[CSS_PRIMARY_USER_IMPORTANT + 1];
      MatchCache match_cache;
      int rulePosition;

      CssContext();

      void apply_css_context(c_css_declaration_set_t * mergedDeclList,
                             Doctree *docTree, c_doctree_node_t * dtn,
                             c_css_declaration_set_t * declList,
                             c_css_declaration_set_t * declListImportant,
                             c_css_declaration_set_t * declListNonCss);
};


CssLengthType cssLengthType(CssLength len);
float cssLengthValue(CssLength len);
CssLength cssCreateLength(float val, CssLengthType t);
void css_context_add_rule(CssContext * context, c_css_rule_t * rule, CssPrimaryOrder order);

#endif
