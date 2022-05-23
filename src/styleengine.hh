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



struct StyleNode {
   c_css_declaration_lists_t declLists;

   dw::core::style::Style *style;
   dw::core::style::Style *wordStyle;
   dw::core::style::Style *backgroundStyle;
   bool inheritBackgroundColor;
   bool displayNone;
   int doctreeNodeIdx;
};

class StyleEngine;
StyleNode * getCurrentNode(StyleEngine * styleEngine);

void print_css_declaration_set(FILE * file, c_css_declaration_set_t * props);

c_css_declaration_set_t * hll_styleEngineSetNonCssHintOfCurrentNodeLength(c_css_declaration_set_t * set, CssDeclarationProperty property, CssDeclarationValueType type, CssLength length);

class StyleEngine {
public:
   StyleNode styleNodesStack[64] = {};
   int styleNodesStackSize = 0;

   private:

      dw::core::Layout *layout;

      c_css_context_t * css_context_ptr = NULL;

      c_doctree_t * doc_tree_ptr = NULL;
      int doc_tree_ref = 0;

      int importDepth;
      DilloUrl *pageUrl, *baseUrl;

      void stackPush ();
      void stackPop ();

      void buildUserStyle(c_css_context_t * context);

      dw::core::style::Style *getStyle0 (int some_idx, BrowserWindow *bw);
      dw::core::style::Style *getWordStyle0 (BrowserWindow *bw);

      void preprocessAttrs (dw::core::style::StyleAttrs *attrs);
      void postprocessAttrs (dw::core::style::StyleAttrs *attrs);
      void apply(int some_idx, dw::core::style::StyleAttrs *attrs, c_css_declaration_set_t * declList, BrowserWindow *bw);
      bool computeAbsoluteLengthValue (int *dest, CssLength value, dw::core::style::Font *font);
      bool computeAbsoluteLengthValue (int *dest, CssLength value, dw::core::style::Font *font, int percentageBase);
      bool computeDwLength (dw::core::style::DwLength *dest, CssLength value, dw::core::style::Font *font);
      void computeBorderWidth (int *dest, c_css_declaration_t * decl, dw::core::style::Font *font);

   public:


      StyleEngine (dw::core::Layout *layout,
                   const DilloUrl *pageUrl, const DilloUrl *baseUrl);
      ~StyleEngine ();

      void parseCssWithOrigin(DilloHtml *html, DilloUrl *url, const char *buf, int buflen, CssOrigin origin);

      void startElement (int tag, BrowserWindow *bw);
      void startElement (const char *tagname, BrowserWindow *bw);
      void endElement (int tag);

      void setElementId(const char *id);
      const char * getElementId() { return doctreeGetTopNode(this->doc_tree_ptr)->c_element_selector_id; };

      void setElementClass(const char * element_class);

      void setCssStyleForCurrentNode(const char * cssStyle);

      void setPseudoLink ();
      void setPseudoVisited ();

      void inheritNonCssHints ();
      void clearNonCssHints ();
      void restyle (BrowserWindow *bw);
      void inheritBackgroundColor (); /* \todo get rid of this somehow */
      dw::core::style::Style *getBackgroundStyle (BrowserWindow *bw);
      dw::core::style::Color *getBackgroundColor ();
      dw::core::style::StyleImage *getBackgroundImage
         (dw::core::style::BackgroundRepeat *bgRepeat,
          dw::core::style::BackgroundAttachment *bgAttachment,
          dw::core::style::DwLength *bgPositionX,
          dw::core::style::DwLength *bgPositionY);

      inline dw::core::style::Style *getStyle (BrowserWindow *bw) {
         StyleNode * currentNode = getCurrentNode(this);
         dw::core::style::Style *s = currentNode->style;
         if (s)
            return s;
         else
            return getStyle0(styleNodesStackSize - 1, bw);
      };

      inline dw::core::style::Style *getWordStyle (BrowserWindow *bw) {
         StyleNode * currentNode = getCurrentNode(this);
         dw::core::style::Style *s = currentNode->wordStyle;
         if (s)
            return s;
         else
            return getWordStyle0 (bw);
      };
};

inline StyleNode * getCurrentNode(StyleEngine * styleEngine)
{
   int idx = styleEngine->styleNodesStackSize - 1;
   return &styleEngine->styleNodesStack[idx];
}

inline StyleNode * getParentNode(StyleEngine * styleEngine)
{
   int idx = styleEngine->styleNodesStackSize - 2;
   return &styleEngine->styleNodesStack[idx];
}


#endif
