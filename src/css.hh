#ifndef __CSS_HH__
#define __CSS_HH__

#include "dw/core.hh"
#include "doctree.hh"
#include "css.h"
#include "Hello/hello.h"

enum CssDeclarationValueType {
   // CssDeclarationValueTypeINTEGER                  =  0,  /* This type is only used internally, for x-* properties. */
   // CssDeclarationValueTypeENUM                     =  1,  /* Value is i, if represented by enum_symbols[i]. */
   // CssDeclarationValueTypeMULTI_ENUM               =  2,  /* For all enum_symbols[i], 1 << i are combined. */
   CssDeclarationValueTypeLENGTH_PERCENTAGE        =  3,  /* <length> or <percentage>. Represented by CssLength. */
   CssDeclarationValueTypeLENGTH                   =  4,  /* <length>, represented as CssLength. Note: In some
                                                             cases, CSS_TYPE_LENGTH is used instead of
                                                             CSS_TYPE_LENGTH_PERCENTAGE, only because Dw cannot
                                                             handle percentages in this particular case (e.g.
                                                             'margin-*-width'). */
   CssDeclarationValueTypeSIGNED_LENGTH            =  5,  /* As CSS_TYPE_LENGTH but may be negative. */
   CssDeclarationValueTypeLENGTH_PERCENTAGE_NUMBER =  6,  /* <length> or <percentage>, or <number> */
   // CssDeclarationValueTypeAUTO                     =  7,  /* Represented as CssLength of type CSS_LENGTH_TYPE_AUTO */
   // CssDeclarationValueTypeCOLOR                    =  8,  /* Represented as integer. */
   // CssDeclarationValueTypeFONT_WEIGHT              =  9,  /* this very special and only used by 'font-weight' */
   CssDeclarationValueTypeSTRING                   = 10,  /* <string> */
   //CssDeclarationValueTypeSYMBOL                   = 11,  /* Symbols, which are directly copied (as opposed to
   //                                                          CSS_PROPERTY_DATA_TYPE_ENUM and
   //                                                          CSS_PROPERTY_DATA_TYPE_MULTI_ENUM). Used for
   //                                                         'font-family'. */
   CssDeclarationValueTypeURI                      = 12,  /* <uri> */
   // CssDeclarationValueTypeBACKGROUND_POSITION      = 13,
   // CssDeclarationValueTypeUNUSED                   = 14   /* Not yet used. Will itself get unused some day. */
};




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
   CSS_PROPERTY_X_LANG,
   CSS_PROPERTY_X_IMG,
   PROPERTY_X_TOOLTIP,
   CSS_PROPERTY_LAST
} CssDeclarationProperty;




#endif

