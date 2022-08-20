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


void cpp_styleEngineSetNonCssHintOfNodeLength(StyleNode * styleNode, CssDeclarationProperty property, CssLength length);
void cpp_styleEngineSetNonCssHintOfNodeEnum(StyleNode * styleNode, int property, int enumVal);
void cpp_styleEngineSetNonCssHintOfNodeColor(StyleNode * styleNode, int property, int color);
void cpp_styleEngineSetNonCssHintOfNodeString(StyleNode * styleNode, int property, const char * stringVal);

void cpp_styleEngineSetXImgOfNode(StyleNode * styleNode, int intVal);
void cpp_styleEngineSetXLangOfNode(StyleNode * styleNode, const char * stringVal);
void cpp_styleEngineSetXLinkOfNode(StyleNode * styleNode, int intVal);
void cpp_styleEngineSetXTooltipOfNode(StyleNode * styleNode, const char * stringVal);

class StyleEngine {
public:
   StyleNode styleNodesStack[64] = {};
   int styleNodesStackSize = 0;

   private:

      dw::core::Layout *layout;

      /* Reference to CSS context variable stored in Haskell. The variable(s)
         is (are) stored in src/Hello/Css/ContextGlobal.hs. */
      int css_context_ref = 0;

      c_doctree_t * doc_tree_ptr = NULL;
      int doc_tree_ref = 0;

      int importDepth;
      DilloUrl *pageUrl, *baseUrl;

      void stackPush ();
      void stackPop ();

      void buildUserStyle(int context_ref);

      dw::core::style::Style *getStyle0 (int some_idx, BrowserWindow *bw);
      dw::core::style::Style *getWordStyle0 (BrowserWindow *bw);

      void preprocessAttrs (dw::core::style::StyleAttrs *attrs);
      void postprocessAttrs (dw::core::style::StyleAttrs *attrs);
      void applyStyleToGivenNode(int some_idx, dw::core::style::StyleAttrs * parentAttrs, dw::core::style::StyleAttrs * attrs, int merged_decl_set_ref, BrowserWindow *bw);

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
          DwLength *bgPositionX,
          DwLength *bgPositionY);

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
