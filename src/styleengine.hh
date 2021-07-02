#ifndef __STYLEENGINE_HH__
#define __STYLEENGINE_HH__

class StyleEngine;

#include "dw/core.hh"
#include "doctree.hh"
#include "css.hh"
#include "cssparser.hh"

/**
 * \brief This class provides the glue between HTML parser and CSS subsystem.
 *
 * It maintains a document tree and creates and caches style objects for use
 * by the HTML parser.
 * The HTML parser in turn informs StyleEngine about opened or closed
 * HTML elements and their attributes via the startElement() / endElement()
 * methods.
 */
class StyleEngine {
   private:
      struct Node {
         c_css_declaration_set_t * declList;
         c_css_declaration_set_t * declListImportant;
         c_css_declaration_set_t * declListNonCss;
         dw::core::style::Style *style;
         dw::core::style::Style *wordStyle;
         dw::core::style::Style *backgroundStyle;
         bool inheritBackgroundColor;
         bool displayNone;
         c_doctree_node_t *doctreeNode;
      };

      dw::core::Layout *layout;
      lout::misc::SimpleVector <Node> *styleNodesStack;
      CssContext *cssContext;
      Doctree * doctree;
      int importDepth;
      DilloUrl *pageUrl, *baseUrl;

      void stackPush ();
      void stackPop ();
      void buildUserStyle ();
      dw::core::style::Style *getStyle0 (int some_idx, BrowserWindow *bw);
      dw::core::style::Style *getWordStyle0 (BrowserWindow *bw);
      inline void setNonCssHintOfProperty(CssDeclarationProperty property, c_css_value_t value, CssDeclarationValueType type) {
         Node *n = styleNodesStack->getRef(styleNodesStack->size () - 1);

         if (!n->declListNonCss)
            n->declListNonCss = declarationListNew();

         value.c_type_tag = type;
         declarationListAddOrUpdateDeclaration(n->declListNonCss, property, value);
      }
      void preprocessAttrs (dw::core::style::StyleAttrs *attrs);
      void postprocessAttrs (dw::core::style::StyleAttrs *attrs);
      void apply(int some_idx, dw::core::style::StyleAttrs *attrs, c_css_declaration_set_t * declList, BrowserWindow *bw);
      bool computeAbsoluteLengthValue (int *dest, CssLength value, dw::core::style::Font *font);
      bool computeAbsoluteLengthValue (int *dest, CssLength value, dw::core::style::Font *font, int percentageBase);
      bool computeLength (dw::core::style::Length *dest, CssLength value, dw::core::style::Font *font);
      void computeBorderWidth (int *dest, c_css_declaration_t * decl, dw::core::style::Font *font);

   public:
      static void init ();

      StyleEngine (dw::core::Layout *layout,
                   const DilloUrl *pageUrl, const DilloUrl *baseUrl);
      ~StyleEngine ();

      void parse(DilloHtml *html, DilloUrl *url, const char *buf, int buflen, CssOrigin origin);

      void startElement (int tag, BrowserWindow *bw);
      void startElement (const char *tagname, BrowserWindow *bw);
      void endElement (int tag);

      void setElementId(const char *id);
      const char * getElementId() { return doctree->top()->c_element_selector_id; };

      void setElementClass(const char * element_class);

      void setCssStyleForCurrentNode(const char * cssStyle);

      void setPseudoLink ();
      void setPseudoVisited ();
      inline void setNonCssHintOfCurrentNode(CssDeclarationProperty property, CssDeclarationValueType type, int value) {
         c_css_value_t v;
         v.c_int_val = value;
         setNonCssHintOfProperty(property, v, type);
      }
      inline void setNonCssHintOfCurrentNode(CssDeclarationProperty property, CssDeclarationValueType type, const char *value) {
         c_css_value_t v;
         v.c_text_val = dStrdup(value);
         setNonCssHintOfProperty(property, v, type);
      }
      inline void setNonCssHintOfCurrentNode(CssDeclarationProperty property, CssDeclarationValueType type, CssLength cssLength) {
         c_css_value_t v;
         v.c_int_val = cssLength.bits;
         setNonCssHintOfProperty(property, v, type);
      }
      void inheritNonCssHints ();
      void clearNonCssHints ();
      void restyle (BrowserWindow *bw);
      void inheritBackgroundColor (); /* \todo get rid of this somehow */
      dw::core::style::Style *getBackgroundStyle (BrowserWindow *bw);
      dw::core::style::Color *getBackgroundColor ();
      dw::core::style::StyleImage *getBackgroundImage
         (dw::core::style::BackgroundRepeat *bgRepeat,
          dw::core::style::BackgroundAttachment *bgAttachment,
          dw::core::style::Length *bgPositionX,
          dw::core::style::Length *bgPositionY);

      inline dw::core::style::Style *getStyle (BrowserWindow *bw) {
         dw::core::style::Style *s = styleNodesStack->getRef(styleNodesStack->size () - 1)->style;
         if (s)
            return s;
         else
            return getStyle0(styleNodesStack->size () - 1, bw);
      };

      inline dw::core::style::Style *getWordStyle (BrowserWindow *bw) {
         dw::core::style::Style *s = styleNodesStack->getRef(styleNodesStack->size()-1)->wordStyle;
         if (s)
            return s;
         else
            return getWordStyle0 (bw);
      };
};

#endif
