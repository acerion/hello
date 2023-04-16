#ifndef __CSS_HH__
#define __CSS_HH__

typedef enum {
   CSS_PROPERTY_BACKGROUND_COLOR,
   CSS_PROPERTY_BORDER_BOTTOM_STYLE,
   CSS_PROPERTY_BORDER_BOTTOM_WIDTH,
   CSS_PROPERTY_BORDER_LEFT_STYLE,
   CSS_PROPERTY_BORDER_LEFT_WIDTH,
   CSS_PROPERTY_BORDER_RIGHT_STYLE,
   CSS_PROPERTY_BORDER_RIGHT_WIDTH,
   CSS_PROPERTY_BORDER_SPACING,
   CSS_PROPERTY_BORDER_TOP_STYLE,
   CSS_PROPERTY_BORDER_TOP_WIDTH,
   CSS_PROPERTY_COLOR,
   CSS_PROPERTY_FONT_FAMILY,
   CSS_PROPERTY_HEIGHT,
   CSS_PROPERTY_LIST_STYLE_POSITION, // TODO: double-check if we didn't remove from C++ code the usage of this enum
   CSS_PROPERTY_LIST_STYLE_TYPE,
   CSS_PROPERTY_MARGIN_BOTTOM,
   CSS_PROPERTY_MARGIN_LEFT,
   CSS_PROPERTY_MARGIN_RIGHT,
   CSS_PROPERTY_MARGIN_TOP,
   CSS_PROPERTY_PADDING_BOTTOM,
   CSS_PROPERTY_PADDING_LEFT,
   CSS_PROPERTY_PADDING_RIGHT,
   CSS_PROPERTY_PADDING_TOP,
   CSS_PROPERTY_TEXT_ALIGN,
   CSS_PROPERTY_VERTICAL_ALIGN,
   CSS_PROPERTY_WHITE_SPACE,
   CSS_PROPERTY_WIDTH
} CssDeclarationProperty;




typedef struct {
   double length_value;
   int length_type;
} CssLength;

typedef enum {
   CSS_LENGTH_TYPE_NONE       = 0,
   CSS_LENGTH_TYPE_PX         = 1,
   CSS_LENGTH_TYPE_MM         = 2, /* "cm", "in", "pt" and "pc" are converted into millimeters. */
   CSS_LENGTH_TYPE_EM         = 3,
   CSS_LENGTH_TYPE_EX         = 4,
   CSS_LENGTH_TYPE_PERCENTAGE = 5,
   CSS_LENGTH_TYPE_RELATIVE   = 6,   /* This does not exist in CSS but is used in HTML */
   CSS_LENGTH_TYPE_AUTO       = 7 /* This can be used as a simple value. */
} CssLengthType;


CssLengthType cpp_cssLengthType(CssLength len);
float         cpp_cssLengthValue(CssLength len);
CssLength     cpp_cssCreateLength(float v, CssLengthType t);


#endif

