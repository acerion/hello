#ifndef __STYLEENGINE_HH__
#define __STYLEENGINE_HH__

class StyleEngine;

#include "dw/core.hh"
#include "css.hh"

class DilloHtml;

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
   dw::core::style::Style *style;
   dw::core::style::Style *wordStyle;
   dw::core::style::Style *backgroundStyle;
   bool inheritBackgroundColor;
   bool displayNone;
};

class StyleEngine;
StyleNode * getCurrentNode(StyleEngine * styleEngine);


void cpp_styleEngineSetNonCssHintOfNodeLength(StyleEngine * styleEngine, CssDeclarationProperty property, CssLength length);
void cpp_styleEngineSetNonCssHintOfNodeEnum(StyleEngine * styleEngine, int property, int enumVal);
void cpp_styleEngineSetNonCssHintOfNodeColor(StyleEngine * styleEngine, int property, int color);
void cpp_styleEngineSetNonCssHintOfNodeString(StyleEngine * styleEngine, int property, const char * stringVal);

void cpp_styleEngineSetXImgOfNode(StyleEngine * styleEngine, int intVal);
void cpp_styleEngineSetXLangOfNode(StyleEngine * styleEngine, const char * stringVal);
void cpp_styleEngineSetXLinkOfNode(StyleEngine * styleEngine, int intVal);
void cpp_styleEngineSetXTooltipOfNode(StyleEngine * styleEngine, const char * stringVal);

class StyleEngine {
public:
   StyleNode styleNodesStack[64] = {};

   /* Reference to CSS Style Engine variable stored in Haskell. The variable(s)
      is (are) stored in src/Hello/Css/StyleEngineGlobal.hs. */
   int style_engine_ref = 0;

   private:

      dw::core::Layout *layout;

      /* Reference to CSS context variable stored in Haskell. The variable(s)
         is (are) stored in src/Hello/Css/ContextGlobal.hs. */
      int css_context_ref = 0;

      int importDepth;
      DilloUrl *pageUrl, *baseUrl;

      void styleNodesStackPushEmptyNode ();
      void styleNodesStackPop ();

      void buildUserStyle(int context_ref);

      /* Make new style, put it in stack at given index. */
      dw::core::style::Style * makeStyle(int styleNodeIndex, BrowserWindow *bw);
      dw::core::style::Style * makeWordStyle(BrowserWindow *bw);

      void preprocessAttrs (dw::core::style::StyleAttrs *attrs);

      void downloadBgImage(BrowserWindow * bw, dw::core::style::StyleAttrs * attrs, int styleNodeIndex);

   public:


      StyleEngine (dw::core::Layout *layout,
                   const DilloUrl *pageUrl, const DilloUrl *baseUrl);
      ~StyleEngine ();

      void parseCssWithOrigin(DilloHtml *html, DilloUrl *url, const char *buf, int buflen, CssOrigin origin);

      void startElement (int tag, BrowserWindow *bw);
      void startElement (const char *tagname, BrowserWindow *bw);
      void endElement (int tag);

      void setElementId(const char *id);
      const char * getElementId() { return ffiStyleEngineDoctreeGetTopNodeElementSelectorId(this->style_engine_ref); }; // FIXME: pointer returned by this function is not freed anywhere.

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
            return makeStyle(ffiStyleEngineStyleNodesStackSize(this->style_engine_ref) - 1, bw);
      };

      inline dw::core::style::Style *getWordStyle (BrowserWindow *bw) {
         StyleNode * currentNode = getCurrentNode(this);
         dw::core::style::Style *s = currentNode->wordStyle;
         if (s)
            return s;
         else
            return makeWordStyle(bw);
      };
};

inline StyleNode * getCurrentNode(StyleEngine * styleEngine)
{
   int idx = ffiStyleEngineStyleNodesStackSize(styleEngine->style_engine_ref) - 1;
   return &styleEngine->styleNodesStack[idx];
}

inline StyleNode * getParentNode(StyleEngine * styleEngine)
{
   int idx = ffiStyleEngineStyleNodesStackSize(styleEngine->style_engine_ref) - 2;
   return &styleEngine->styleNodesStack[idx];
}


#endif
