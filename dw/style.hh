#ifndef __DW_STYLE_HH__
#define __DW_STYLE_HH__

#include <stdint.h>

#ifndef __INCLUDED_FROM_DW_CORE_HH__
#   error Do not include this file directly, use "core.hh" instead.
#endif

#include "../lout/signal.hh"
#include "../lout/debug.hh"
#include "../src/css.h"
#include "../src/Hello/hello.h"

namespace dw {
namespace core {

/**
 * \brief Anything related to Dillo %Widget styles is defined here.
 *
 * <h3>Overview</h3>
 *
 * dw::core::style::Style provides some resources and attributes for
 * drawing widgets, as well as for parts of a widget (e.g., dw::Textblock
 * uses styles for its words). Creating a style is done by filling a
 * dw::core::style::StyleAttrs with the attributes and calling
 * dw::core::style::Style::create:
 *
 * \code
 * dw::core::style::Style styleAttrs;
 * dw::core::style::Style *style;
 * dw::core::Layout *layout;
 *
 * // ...
 *
 * styleAttrs.foo = bar;
 * // etc.
 * style = dw::core::style::Style::create (&styleAttrs, layout);
 * // do something with style
 * \endcode
 *
 * After this, the attributes of a dw::core::style::Style should not be
 * changed anymore, since styles are often shared between different
 * widgets etc. (see below). Most times, you simply copy the attributes
 * of another style (possible, since dw::core::style::Style is a sub
 * class of dw::core::style::StyleAttrs), modify them and create a new
 * style:
 *
 * \code
 * styleAttrs = *anotherStyle;
 * styleAttrs.foo = baz;
 * style = dw::core::style::Style::create (&styleAttrs, layout);
 * \endcode
 *
 * The dw::core::style::Font structure can be created by
 * dw::core::style::Font::create, in a similar, with
 * dw::core::style::FontAttrs, and colors by
 * dw::core::style::Color::create, passing 0xrrggbb as an
 * argument. Furthermore, there is dw::core::style::Tooltip, created by
 * dw::core::style::Tooltip::create.
 *
 * Notice that fonts, colors and tooltips are only intended to be used in
 * conjunction with dw::core::style::Style.
 *
 *
 * <h3>Naming</h3>
 *
 * dw::core::style::Style will become important for CSS, each CSS
 * attribute, which is supported by dillo, will refer to an attribute in
 * dw::core::style::Style. For this reason, the attributes in
 * dw::core::style::Style get the names from the CSS attributes, with
 * "camelCase" instead of hyphens (e.g. "background-color" becomes
 * "backgroundColor").
 *
 * However, dw::core::style::Style will be extended by some more
 * attributes, which are not defined by CSS. To distinguish them, they
 * get the prefix "x_", e.g. dw::core::style::Style::x_link.
 *
 *
 * <h3>Lengths and Percentages</h3>
 *
 * dw::core::style::Length is a simple data type for lengths and
 * percentages:
 *
 * <ul>
 * <li> A length refers to an absolute measurement. It is used to
 *      represent the HTML type %Pixels; and the CSS type \<length\>.
 *
 *      For CSS lengths, there are two units: (i) pixels and absolute
 *      units, which have to be converted to pixels (a pixel is, unlike
 *      in the CSS specification, treated as absolute unit), and (ii) the
 *      relative units "em" and "ex" (see below).
 *
 * <li> A percentage refers to a value relative to another value. It is
 *      used for the HTML type %Length; (except %Pixels;), and the CSS
 *      type \<percentage\>.
 *
 * <li> A relative length can be used in lists of HTML MultiLengths.
 * </ul>
 *
 * Since many values in CSS may be either lengths or percentages, a
 * single type is very useful.
 *
 * <h4>Useful Functions</h4>
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
 *
 * <h3>Boxes</h3>
 *
 * <h4>The CSS %Box Model</h4>
 *
 * For borders, margins etc., the box model defined by CSS2 is
 * used. dw::core::style::Style contains some members defining these
 * attributes. A dw::core::Widget must use these values for any
 * calculation of sizes. There are some helper functions (see
 * dw/style.hh). A dw::core::style::Style box looks quite similar to a
 * CSS box:
 *
 * \image html dw-style-box-model.png
 *
 * <h4>Background colors</h4>
 *
 * The background color is stored in
 * dw::core::style::Style::backgroundColor, which may be NULL (the
 * background color of the parent widget is shining through).
 *
 * For toplevel widgets, this color is set as the background color of the
 * views (dw::core::View::setBgColor), for other widgets, a filled
 * rectangle is drawn, covering the content and padding. (This is
 * compliant with CSS2, the background color of the toplevel element
 * covers the whole canvas.)
 *
 * <h4>Drawing</h4>
 *
 * The following methods may be useful:
 *
 * <ul>
 * <li> dw::core::Widget::drawWidgetBox for drawing the box of a widget
 *      (typically at the beginning of the implementation of
 *      dw::core::Widget::draw), and
 *
 * <li> dw::core::Widget::drawBox, for drawing parts of a widget (e.g.
 *      dw::Textblock::Word, which has its own dw::Textblock::Word::style).
 * </ul>
 *
 *
 * <h3>Notes on Memory Management</h3>
 *
 * Memory management is done by reference counting,
 * dw::core::style::Style::create returns a pointer to
 * dw::core::style::Style with an increased reference counter, so you
 * should care about calling dw::core::style::Style::unref if it is not
 * used anymore. You do \em not need to care about the reference counters
 * of fonts and styles.
 *
 * In detail:
 *
 * <ul>
 * <li> dw::core::style::Style::ref is called in
 *
 *      <ul>
 *      <li> dw::core::Widget::setStyle to assign a style to a widget,
 *      <li> dw::Textblock::addText, dw::Textblock::addWidget,
 *           dw::Textblock::addAnchor, dw::Textblock::addSpace,
 *           dw::Textblock::addParbreak and dw::Textblock::addLinebreak,
 *           to assign a style to a dw::Textblock::Word, and
 *      <li> by the HTML parser, when pushing an element on the stack.
 *      </ul>
 *
 * <li> dw::core::style::Style::unref is called in
 *
 *      <ul>
 *      <li> dw::core::Widget::~Widget, dw::Textblock::~Textblock, by the
 *           HTML parser, when popping an element fom the stack, and
 *      <li> dw::core::Widget::setStyle, dw::Textblock::addText etc.,
 *           these methods overwrite an existing style.
 *      </ul>
 * </ul>
 */
namespace style {

enum Cursor {
   CURSOR_CROSSHAIR,
   CURSOR_DEFAULT,
   CURSOR_POINTER,
   CURSOR_MOVE,
   CURSOR_E_RESIZE,
   CURSOR_NE_RESIZE,
   CURSOR_NW_RESIZE,
   CURSOR_N_RESIZE,
   CURSOR_SE_RESIZE,
   CURSOR_SW_RESIZE,
   CURSOR_S_RESIZE,
   CURSOR_W_RESIZE,
   CURSOR_TEXT,
   CURSOR_WAIT,
   CURSOR_HELP
};

enum BorderCollapse {
   BORDER_MODEL_SEPARATE,
   BORDER_MODEL_COLLAPSE
};

enum BorderStyle {
   BORDER_NONE,
   BORDER_HIDDEN,
   BORDER_DOTTED,
   BORDER_DASHED,
   BORDER_SOLID,
   BORDER_DOUBLE,
   BORDER_GROOVE,
   BORDER_RIDGE,
   BORDER_INSET,
   BORDER_OUTSET
};

enum BackgroundRepeat {
   BACKGROUND_REPEAT,
   BACKGROUND_REPEAT_X,
   BACKGROUND_REPEAT_Y,
   BACKGROUND_NO_REPEAT
};

enum BackgroundAttachment {
   BACKGROUND_ATTACHMENT_SCROLL,
   BACKGROUND_ATTACHMENT_FIXED
};

enum TextAlignType {
   TEXT_ALIGN_LEFT,
   TEXT_ALIGN_RIGHT,
   TEXT_ALIGN_CENTER,
   TEXT_ALIGN_JUSTIFY,
   TEXT_ALIGN_STRING
};

enum VAlignType {
   VALIGN_TOP,
   VALIGN_BOTTOM,
   VALIGN_MIDDLE,
   VALIGN_BASELINE,
   VALIGN_SUB,
   VALIGN_SUPER,
   VALIGN_TEXT_TOP,
   VALIGN_TEXT_BOTTOM,
};

enum TextTransform {
   TEXT_TRANSFORM_NONE,
   TEXT_TRANSFORM_CAPITALIZE,
   TEXT_TRANSFORM_UPPERCASE,
   TEXT_TRANSFORM_LOWERCASE,
};

/**
 * \todo Incomplete. Has to be completed for a CSS implementation.
 */
enum DisplayType {
   DISPLAY_BLOCK,
   DISPLAY_INLINE,
   DISPLAY_INLINE_BLOCK,
   DISPLAY_LIST_ITEM,
   DISPLAY_NONE,
   DISPLAY_TABLE,
   DISPLAY_TABLE_ROW_GROUP,
   DISPLAY_TABLE_HEADER_GROUP,
   DISPLAY_TABLE_FOOTER_GROUP,
   DISPLAY_TABLE_ROW,
   DISPLAY_TABLE_CELL
};

enum LineType {
   LINE_NORMAL,
   LINE_DOTTED,
   LINE_DASHED
};

enum ListStylePosition {
   LIST_STYLE_POSITION_INSIDE,
   LIST_STYLE_POSITION_OUTSIDE
};

enum ListStyleType {
   LIST_STYLE_TYPE_DISC,
   LIST_STYLE_TYPE_CIRCLE,
   LIST_STYLE_TYPE_SQUARE,
   LIST_STYLE_TYPE_DECIMAL,
   LIST_STYLE_TYPE_DECIMAL_LEADING_ZERO,
   LIST_STYLE_TYPE_LOWER_ROMAN,
   LIST_STYLE_TYPE_UPPER_ROMAN,
   LIST_STYLE_TYPE_LOWER_GREEK,
   LIST_STYLE_TYPE_LOWER_ALPHA,
   LIST_STYLE_TYPE_LOWER_LATIN,
   LIST_STYLE_TYPE_UPPER_ALPHA,
   LIST_STYLE_TYPE_UPPER_LATIN,
   LIST_STYLE_TYPE_HEBREW,
   LIST_STYLE_TYPE_ARMENIAN,
   LIST_STYLE_TYPE_GEORGIAN,
   LIST_STYLE_TYPE_CJK_IDEOGRAPHIC,
   LIST_STYLE_TYPE_HIRAGANA,
   LIST_STYLE_TYPE_KATAKANA,
   LIST_STYLE_TYPE_HIRAGANA_IROHA,
   LIST_STYLE_TYPE_KATAKANA_IROHA,
   LIST_STYLE_TYPE_NONE
};

enum FontStyle {
  FONT_STYLE_NORMAL,
  FONT_STYLE_ITALIC,
  FONT_STYLE_OBLIQUE
};

enum FontVariant {
   FONT_VARIANT_NORMAL,
   FONT_VARIANT_SMALL_CAPS
};

enum TextDecoration {
   TEXT_DECORATION_NONE         = 0,
   TEXT_DECORATION_UNDERLINE    = 1 << 0,
   TEXT_DECORATION_OVERLINE     = 1 << 1,
   TEXT_DECORATION_LINE_THROUGH = 1 << 2,
   TEXT_DECORATION_BLINK        = 1 << 3
};

enum WhiteSpace {
   WHITE_SPACE_NORMAL,
   WHITE_SPACE_PRE,
   WHITE_SPACE_NOWRAP,
   WHITE_SPACE_PRE_WRAP,
   WHITE_SPACE_PRE_LINE,
};


/** \brief Returns a length of \em n pixels. */
// Notice that this function is already re-implemented in Haskell with hll_createAutoDwLength()
inline DwLength createAutoLength(void)
{
   DwLength l = {};
   l.dw_length_value = 0.0;
   l.dw_length_type = 0;
   l.dw_length_hash = 0;
   return l;
}

/** \brief Returns a length of \em n pixels. */
// Notice that this function is already re-implemented in Haskell with hll_createAbsoluteDwLength.
inline DwLength createAbsoluteDwLength(int n)
{
   DwLength l = {};
   l.dw_length_value = (double) n;
   l.dw_length_type = 1;
   l.dw_length_hash = (n << 2) | 1;
   return l;
}

/** \brief Returns a percentage, \em v is relative to 1, not to 100. */
// Notice that this function is already re-implemented in Haskell with hll_createPercentageDwLength.
inline DwLength createPercentageDwLength(double v)
{
   DwLength l = {};
   l.dw_length_value = v;
   l.dw_length_type = 2;
   l.dw_length_hash = ((int)(v * (1 << 18)) & ~3) | 2;
   return l;
}

inline bool isAutoLength(DwLength l) { return l.dw_length_type == 0; }

/** \brief Returns true if \em l is an absolute length. */
inline bool isAbsoluteDwLength(DwLength l) { return l.dw_length_type == 1; }

/** \brief Returns true if \em l is a percentage. */
inline bool isPercentageDwLength(DwLength l) { return l.dw_length_type == 2; }

/** \brief Returns true if \em l is a relative length. */
inline bool isRelativeDwLength(DwLength l) { return l.dw_length_type == 3; }

/** \brief Returns the value of a length in pixels, as an integer. */
inline int getAbsoluteDwLengthValue(DwLength l)
{
   return (int) l.dw_length_value;
}

/** \brief Returns the value of a percentage, relative to 1, as a double.
 *
 * When possible, do not use this function directly; it may be removed
 * soon. Instead, use multiplyWithPercentageDwLength or multiplyWithPercentageDwLengthRounded.
 */
inline double getPercentageDwLengthValue(DwLength l)
{
   return l.dw_length_value;
}

/** \brief Returns the value of a relative length, as a float.
 *
 * When possible, do not use this function directly; it may be removed
 * soon.
 */
inline double getRelativeDwLengthValue(DwLength l)
{
   return l.dw_length_value;
}

/**
 * \brief Multiply an int with a percentage length, returning int.
 *
 * Use this instead of getPercentageDwLengthValue, when possible.
 */
inline int multiplyWithPercentageDwLength(int x, DwLength l) {
   return x * getPercentageDwLengthValue(l);
}

/**
 * \brief Like multiplyWithPercentageDwLength, but rounds to nearest integer
 *    instead of down.
 *
 * (This function exists for backward compatibility.)
 */
inline int multiplyWithPercentageDwLengthRounded (int x, DwLength l) {
   return lout::misc::roundInt (x * getPercentageDwLengthValue(l));
}

inline int multiplyWithRelativeDwLength(int x, DwLength l)
{
   return x * getRelativeDwLengthValue(l);
}


enum {
   /** \brief Represents "auto" lengths. */
   LENGTH_AUTO = 0
};

/**
 * \brief Represents a dimension box according to the CSS box model.
 *
 * Used for dw::core::style::Style::margin,
 * dw::core::style::Style::borderWidth, and dw::core::style::Style::padding.
 */
class Box
{
public:
   /* in future also percentages */
   int top, right, bottom, left;

   inline void setVal(int val) { top = right = bottom = left = val; }
   inline bool equals (Box *other) {
      return top == other->top &&
         right == other->right &&
         bottom == other->bottom &&
         left == other->left;
   }
   inline int hashValue () {
      return top + right + bottom + left;
   }
};

class Tooltip;
class Font;
class Color;
class StyleImage;

/**
 * \sa dw::core::style
 */
class StyleAttrs : public lout::object::Object
{
public:
   Font *font;

   Color *backgroundColor;
   StyleImage *backgroundImage;
   BackgroundRepeat backgroundRepeat;
   BackgroundAttachment backgroundAttachment;
   DwLength backgroundPositionX; // "left" defined by "0%" etc. (see CSS spec)
   DwLength backgroundPositionY; // "top" defined by "0%" etc. (see CSS spec)

   char textAlignChar; /* In future, strings will be supported. */

   int wordSpacing;


   BorderCollapse borderCollapse;
   struct { Color *top, *right, *bottom, *left; } borderColor;

   c_border_width_t borderWidth;
   c_border_style_t borderStyle;
   c_style_margin_t margin;
   c_style_padding_t padding;
   int textAlign; // TODO: use TextAlignType type
   int textDecoration; /* No TextDecoration because of problems converting TextDecoration <-> int */
   DwLength textIndent;
   int textTransform; // TODO: use TextTransform type
   int verticalAlign;  // TODO: use VAlignType
   int whiteSpace; // TODO: use WhiteSpace
   DwLength width;
   DwLength height;
   DwLength lineHeight;
   int listStylePosition; // TODO: use ListStylePosition
   int listStyleType; // TODO: use ListStyleType
   int display; // TODO: use DisplayType type
   Color * color = nullptr;
   int cursor; // TODO: use Cursor type
   int hBorderSpacing;
   int vBorderSpacing;

   int x_link;
   int x_img;
   Tooltip *x_tooltip;
   char x_lang[2]; /* Either x_lang[0] == x_lang[1] == 0 (no language
                      set), or x_lang contains the RFC 1766 country
                      code in lower case letters. (Only two letters
                      allowed, currently.) */

   void initValues ();
   void resetValues ();

   bool sizeDiffs (StyleAttrs *otherStyleAttrs);

   inline void setBorderColor(Color *val) {
      borderColor.top = borderColor.right = borderColor.bottom
         = borderColor.left = val; }
   inline void setBorderStyle(BorderStyle val) {
      borderStyle.top = borderStyle.right = borderStyle.bottom
         = borderStyle.left = val; }

   inline int boxOffsetX ()
   {
      return margin.left + borderWidth.left + padding.left;
   }
   inline int boxRestWidth ()
   {
      return margin.right + borderWidth.right + padding.right;
   }
   inline int boxDiffWidth () { return boxOffsetX () + boxRestWidth (); }
   inline int boxOffsetY ()
   {
      return margin.top + borderWidth.top + padding.top;
   }
   inline int boxRestHeight ()
   {
      return margin.bottom + borderWidth.bottom + padding.bottom;
   }
   inline int boxDiffHeight () { return boxOffsetY () + boxRestHeight (); }

   inline bool hasBackground ()
   { return backgroundColor != NULL || backgroundImage != NULL; }

   bool equals (lout::object::Object *other);
   int hashValue ();
};


/**
 * \sa dw::core::style
 */
class Style: public StyleAttrs
{
private:
   static int totalRef;
   int refCount;
   static lout::container::typed::HashTable <StyleAttrs, Style> *styleTable;

   Style (StyleAttrs *attrs);

protected:
   ~Style();

   void copyAttrs (StyleAttrs *attrs);

public:
   inline static Style *create (StyleAttrs *attrs)
   {
      Style *style = styleTable->get (attrs);
      if (style) {
         style->ref ();
      } else {
         style = new Style (attrs);
         styleTable->put(style, style);
      }
      return style;
   }

   inline void ref () { refCount++; }
   inline void unref () { if (--refCount == 0) delete this; }
};


/**
 * \sa dw::core::style
 */
class TooltipAttrs: public lout::object::String
{
public:
   TooltipAttrs(const char *text): lout::object::String(text) { }
};

/**
 * \sa dw::core::style
 */
class Tooltip: public TooltipAttrs
{
private:
   int refCount;

protected:
   Tooltip (const char *text): TooltipAttrs(text) { refCount = 0; }

public:
   static Tooltip *create (dw::core::Layout *layout, const char *text);
   inline void ref () { refCount++; }
   inline void unref ()
   { if (--refCount == 0) delete this; }

   inline virtual void onEnter () { }
   inline virtual void onLeave () { }
   inline virtual void onMotion () { }
};


/**
 * \sa dw::core::style
 */
class FontAttrs: public lout::object::Object
{
public:
   c_font_attrs_t font_attrs;

   bool equals(lout::object::Object *other);
   int hashValue();
};


/**
 * \sa dw::core::style
 */
class Font: public FontAttrs
{
private:
   int refCount;

   static Font *create0 (Layout *layout, FontAttrs *attrs, bool tryEverything);

protected:
   inline Font () {
      DBG_OBJ_CREATE ("dw::core::style::Font");
      refCount = 0;
   }
   virtual ~Font ();

   void copyAttrs (FontAttrs *attrs);

public:
   int ascent, descent;
   int spaceWidth;

   static Font *create (Layout *layout, FontAttrs *attrs);
   static bool exists (Layout *layout, const char *name);

   inline void ref () { refCount++; }
   inline void unref () { if (--refCount == 0) delete this; }
};


/**
 * \sa dw::core::style
 */
class ColorAttrs: public lout::object::Object
{
protected:
   int color;

public:
   inline ColorAttrs(int color)
   {
      this->color = color;
   }

   inline int getColor () { return color; }

   bool equals(lout::object::Object *other);
   int hashValue();
};


/**
 * \sa dw::core::style
 */
class Color: public ColorAttrs
{
private:
   int refCount;

   void remove(dw::core::Layout *layout);
   int shadeColor (int color, int d);

protected:
   inline Color (int color): ColorAttrs (color) {
      DBG_OBJ_CREATE ("dw::core::style::Color");
      refCount = 0;
   }
   virtual ~Color ();

public:
   enum Shading { SHADING_NORMAL, SHADING_INVERSE, SHADING_DARK, SHADING_LIGHT,
                  SHADING_NUM };

protected:
   int shadeColor (int color, Shading shading);

public:
   static Color *create (Layout *layout, int color);

   inline void ref () { refCount++; }
   inline void unref ()
   { if (--refCount == 0) delete this; }
};


class StyleImage: public lout::signal::ObservedObject
{
private:
   class StyleImgRenderer: public ImgRenderer
   {
   private:
      StyleImage *image;

   public:
      inline StyleImgRenderer (StyleImage *image) { this->image = image; }

      void setBuffer (core::Imgbuf *buffer, bool resize);
      void drawRow (int row);
      void finish ();
      void fatal ();
   };

   int refCount, tilesX, tilesY;
   Imgbuf *imgbufSrc, *imgbufTiled;
   ImgRendererDist *imgRendererDist;
   StyleImgRenderer *styleImgRenderer;

   StyleImage ();
   ~StyleImage ();

public:
   /**
    * \brief Useful (but not mandatory) base class for updates of
    *    areas with background images.
    */
   class ExternalImgRenderer: public ImgRenderer
   {
   public:
      void setBuffer (core::Imgbuf *buffer, bool resize);
      void drawRow (int row);
      void finish ();
      void fatal ();

      /**
       * \brief If this method returns false, nothing is done at all.
       */
      virtual bool readyToDraw () = 0;

      /**
       * \brief Return the area covered by the background image.
       */
      virtual void getBgArea (int *x, int *y, int *width, int *height) = 0;

      /**
       * \brief Return the "reference area".
       *
       * See comment of "drawBackground".
       */
      virtual void getRefArea (int *xRef, int *yRef, int *widthRef,
                               int *heightRef) = 0;

      virtual StyleImage *getBackgroundImage () = 0;
      virtual BackgroundRepeat getBackgroundRepeat () = 0;
      virtual BackgroundAttachment getBackgroundAttachment () = 0;
      virtual DwLength getBackgroundPositionX () = 0;
      virtual DwLength getBackgroundPositionY () = 0;

      /**
       * \brief Draw (or queue for drawing) an area, which is given in
       *    canvas coordinates.
       */
      virtual void draw (int x, int y, int width, int height) = 0;
   };

   /**
    * \brief Suitable for widgets and parts of widgets.
    */
   class ExternalWidgetImgRenderer: public ExternalImgRenderer
   {
   public:
      void getPaddingArea (int *x, int *y, int *width, int *height);

      StyleImage *getBackgroundImage ();
      BackgroundRepeat getBackgroundRepeat ();
      BackgroundAttachment getBackgroundAttachment ();
      DwLength getBackgroundPositionX ();
      DwLength getBackgroundPositionY ();

      /**
       * \brief Return the style this background image is part of.
       */
      virtual Style *getStyle () = 0;
   };

   static StyleImage *create () { return new StyleImage (); }

   inline void ref () { refCount++; }
   inline void unref ()
   { if (--refCount == 0) delete this; }

   inline Imgbuf *getImgbufSrc () { return imgbufSrc; }
   inline Imgbuf *getImgbufTiled (bool repeatX, bool repeatY)
   { return (imgbufTiled && repeatX && repeatY) ? imgbufTiled : imgbufSrc; }
   inline int getTilesX (bool repeatX, bool repeatY)
   { return (imgbufTiled && repeatX && repeatY) ? tilesX : 1; }
   inline int getTilesY (bool repeatX, bool repeatY)
   { return (imgbufTiled && repeatX && repeatY) ? tilesY : 1; }
   inline ImgRenderer *getMainImgRenderer () { return imgRendererDist; }

   /**
    * \brief Add an additional ImgRenderer, especially used for
    *    drawing.
    */
   inline void putExternalImgRenderer (ImgRenderer *ir)
   { imgRendererDist->put (ir); }

   /**
    * \brief Remove a previously added additional ImgRenderer.
    */
   inline void removeExternalImgRenderer (ImgRenderer *ir)
   { imgRendererDist->remove (ir); }
};

void drawBorder (View *view, Layout *layout, Rectangle *area,
                 int x, int y, int width, int height,
                 Style *style, bool inverse);
void drawBackground (View *view, Layout *layout, Rectangle *area,
                     int x, int y, int width, int height,
                     int xRef, int yRef, int widthRef, int heightRef,
                     Style *style, bool inverse, bool atTop);
void drawBackgroundImage (View *view, StyleImage *backgroundImage,
                          BackgroundRepeat backgroundRepeat,
                          BackgroundAttachment backgroundAttachment,
                          DwLength backgroundPositionX,
                          DwLength backgroundPositionY,
                          int x, int y, int width, int height,
                          int xRef, int yRef, int widthRef, int heightRef);
void numtostr (int num, char *buf, int buflen, ListStyleType listStyleType);

} // namespace style
} // namespace core
} // namespace dw

#endif // __DW_STYLE_HH__

