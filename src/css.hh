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

typedef struct {
   int32_t posX;
   int32_t posY;
} CssBackgroundPosition;

typedef struct {
   CssDeclarationValueType type;

   int32_t intVal;
   char *strVal;
   CssBackgroundPosition posVal;
} CssDeclarationValue;

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


/**
 * \brief This class holds a CSS declaration: a pair of property and value.
 */
typedef struct c_css_declaration_t {
      CssDeclarationProperty c_property;
      CssDeclarationValue c_value;
} c_css_declaration_t;
void printCssDeclaration(c_css_declaration_t * declaration, FILE * file);




/**
 * \brief A list of c_css_declaration_t objects.
 */
class CssDeclartionList {
public:
   bool isSafe;

   c_css_declaration_t * declarations[100] = { 0 };
   int declarations_count = 0;

   inline CssDeclartionList() {
      isSafe = true;
   };
   CssDeclartionList(const CssDeclartionList & declList);
};
void declarationListPrint(CssDeclartionList * declList, FILE * file);
void declarationListAddOrUpdateDeclaration(CssDeclartionList * declList, CssDeclarationProperty property, CssDeclarationValue value);
void declarationListAppend(const CssDeclartionList * declList, CssDeclartionList * targetDeclList);

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

void printCssSimpleSelector(c_css_simple_selector_t * selector, FILE * file);
bool simple_selector_matches(c_css_simple_selector_t * selector, const DoctreeNode *node);
int cssSimpleSelectorSpecificity(c_css_simple_selector_t * selector);




class MatchCache : public lout::misc::SimpleVector <int> {
   public:
      MatchCache() : lout::misc::SimpleVector <int> (0) {};
};

bool selector_full_selector_matches(c_css_selector_t * selector, Doctree *dt, const DoctreeNode *node, int i, Combinator comb,  MatchCache *matchCache);
bool selector_full_selector_submatches(c_css_selector_t * selector, Doctree *dt, const DoctreeNode *node, MatchCache *matchCache);

c_css_simple_selector_t * selectorGetTopSimpleSelector(c_css_selector_t * selector);
void selectorSetMatchCacheOffset(c_css_selector_t * selector, int mo);
int selectorGetRequiredMatchCache(c_css_selector_t * selector);
bool selectorChecksPseudoClass(c_css_selector_t * selector);
int selectorSpecificity(c_css_selector_t * selector);
void printCssSelector(c_css_selector_t * selector, FILE * file);



/**
 * \brief A c_css_selector_t CssDeclartionList pair.
 *
 *  The CssDeclartionList is applied if the c_css_selector_t matches.
 */
class CssRule {
   public:
      c_css_selector_t *selector;
      int specificity;
      int position;
      CssDeclartionList * declList = nullptr;

      CssRule(c_css_selector_t *selector, CssDeclartionList * declList, int rulePosition);

      void apply_css_rule(FILE * file, CssDeclartionList * outDeclList, Doctree *docTree,
                          const DoctreeNode *node, MatchCache *matchCache) const;
      inline bool isSafe() {
         return !selectorChecksPseudoClass(selector) || declList->isSafe;
      };
      void printCssRule(FILE * file) const;
};

/**
 * \brief A list of CssRules.
 *
 * In apply_style_sheet() all matching rules are applied.
 */
class CssStyleSheet {
   private:
      class RuleList : public lout::misc::SimpleVector <CssRule*>,
                       public lout::object::Object {
         public:
            RuleList () : lout::misc::SimpleVector <CssRule*> (1) {};

            void insert (CssRule *rule);
            inline bool equals (lout::object::Object *other) {
               return this == other;
            };
            inline int hashValue () { return (intptr_t) this; };
      };

      class RuleMap : public lout::container::typed::HashTable
                             <lout::object::ConstString, RuleList > {
         public:
            RuleMap () : lout::container::typed::HashTable
               <lout::object::ConstString, RuleList > (true, true, 256) {};
      };

      static const int ntags = 90 + 14; // \todo don't hardcode
      /* 90 is the full number of html4 elements, including those which we have
       * implemented. From html5, let's add: article, header, footer, mark,
       * nav, section, aside, figure, figcaption, wbr, audio, video, source,
       * embed.
       */

      RuleList elementTable[ntags];
      RuleList anyTable;
      RuleMap idTable, classTable;
      int requiredMatchCache;

   public:
      CssStyleSheet () { requiredMatchCache = 0; }
      void addRule (CssRule *rule);
      void apply_style_sheet(FILE * file, CssDeclartionList * declList, Doctree *docTree,
                  const DoctreeNode *node, MatchCache *matchCache) const;
      int getRequiredMatchCache () { return requiredMatchCache; }
};

/**
 * \brief A set of CssStyleSheets.
 */
class CssContext {
   public:
      static CssStyleSheet userAgentSheet;
      CssStyleSheet sheet[CSS_PRIMARY_USER_IMPORTANT + 1];
      MatchCache matchCache;
      int rulePosition;


      CssContext();

      void apply_css_context(CssDeclartionList * mergedDeclList,
                             Doctree *docTree, DoctreeNode *node,
                             CssDeclartionList * declList,
                             CssDeclartionList * declListImportant,
                             CssDeclartionList * declListNonCss);
};


CssLengthType cssLengthType(CssLength len);
float cssLengthValue(CssLength len);
CssLength cssCreateLength(float val, CssLengthType t);
void addRuleToContext(CssContext * context, CssRule * rule, CssPrimaryOrder order);

#endif
