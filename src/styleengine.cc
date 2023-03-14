/*
 * File: styleengine.cc
 *
 * Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 */

#include "../dlib/dlib.h"
#include "msg.h"
#include "prefs.h"
#include "misc.h"
#include "html_common.hh"
#include "styleengine.hh"
#include "web.hh"
#include "capi.h"
#include "Hello/hello.h"

#include <sys/time.h>
extern struct timeval g_parse_acc;
extern struct timeval g_parse_start;
extern struct timeval g_parse_stop;
extern struct timeval g_apply_get_acc;
extern struct timeval g_apply_get_start;
extern struct timeval g_apply_get_stop;
extern struct timeval g_apply_do_acc;
extern struct timeval g_apply_do_start;
extern struct timeval g_apply_do_stop;

using namespace lout::misc;
using namespace dw::core::style;

/**
 * Signal handler for "delete": This handles the case when an instance
 * of StyleImage is deleted, possibly when the cache client is still
 * active.
 *
 * \todo Not neccessary for dw::Image? (dw::Image also implements
 * lout::signal::ObservedObject.)
 */
class StyleImageDeletionReceiver:
   public lout::signal::ObservedObject::DeletionReceiver
{
   int clientKey;

public:
   StyleImageDeletionReceiver (int clientKey);
   ~StyleImageDeletionReceiver ();

   void deleted (lout::signal::ObservedObject *object);
};

StyleImageDeletionReceiver::StyleImageDeletionReceiver (int clientKey)
{
   this->clientKey = clientKey;
}

StyleImageDeletionReceiver::~StyleImageDeletionReceiver ()
{
}

void StyleImageDeletionReceiver::deleted (lout::signal::ObservedObject *object)
{
   a_Capi_stop_client (clientKey, 0);
   delete this;
}

// ----------------------------------------------------------------------

StyleEngine::StyleEngine (dw::core::Layout *layout, const DilloUrl *pageUrl, const DilloUrl *baseUrl)
{
   this->doc_tree_ptr = doctreeCtor();
   this->doc_tree_ref = ffiDoctreeCtor();
   this->style_engine_ref = ffiStyleEngineCtor();

   //styleNodesStack = new lout::misc::SimpleVector <StyleNode> (1);

   {
      this->css_context_ref = ffiCssContextCtor();
      buildUserStyle(this->css_context_ref);
   }

   this->layout = layout;
   this->pageUrl = pageUrl ? a_Url_dup(pageUrl) : NULL;
   this->baseUrl = baseUrl ? a_Url_dup(baseUrl) : NULL;
   importDepth = 0;

   StyleAttrs style_attrs = {};
   style_attrs.initValues();

   /* Create a dummy font, color and bg color for the bottom of the stack. */
   FontAttrs font_attrs = {};
   ffiFontAttrsMakeFontAttrsFromPrefs(&font_attrs.font_attrs, &prefs.preferences);
   ffiStyleAttrsSetFontAttrs(style_attrs.c_style_attrs_ref, &font_attrs.font_attrs);

   style_attrs.font = Font::create (layout, &font_attrs);
   style_attrs.color = Color::create (layout, 0);
   style_attrs.backgroundColor = Color::create (layout, prefs.bg_color);

   stackPushEmptyNode();
   styleNodesStack[ffiStyleEngineStyleNodesStackSize(this->style_engine_ref) - 1].style = Style::create (&style_attrs);
}

StyleEngine::~StyleEngine () {
   while (doctreeGetTopNode(this->doc_tree_ptr))
      endElement (doctreeGetTopNode(this->doc_tree_ptr)->c_html_element_idx);

   stackPop (); // dummy node on the bottom of the stack
   assert (ffiStyleEngineStyleNodesStackSize(this->style_engine_ref) == 0);

   a_Url_free(pageUrl);
   a_Url_free(baseUrl);

   delete doc_tree_ptr;
}

void StyleEngine::stackPushEmptyNode () {
   static const StyleNode emptyNode = {
      .style = NULL,
      .wordStyle = NULL,
      .backgroundStyle = NULL,
      .inheritBackgroundColor = false,
      .displayNone = false,
      .doctreeNodeIdx = 0
   };

   memcpy(&styleNodesStack[ffiStyleEngineStyleNodesStackSize(this->style_engine_ref)], &emptyNode, sizeof (emptyNode));
   ffiStyleEngineStyleNodesStackPushEmptyNode(this->style_engine_ref);
}

void StyleEngine::stackPop () {
   StyleNode * currentNode = getCurrentNode(this);

   //delete currentNode->declLists.main;
   //delete currentNode->declLists.important;
   //delete currentNode->declLists.nonCss;
   if (currentNode->style)
      currentNode->style->unref ();
   if (currentNode->wordStyle)
      currentNode->wordStyle->unref ();
   if (currentNode->backgroundStyle)
      currentNode->backgroundStyle->unref ();
   ffiStyleEngineStyleNodesStackPop(this->style_engine_ref);
}

/**
 * \brief tell the styleEngine that a new html element has started.
 */
void StyleEngine::startElement (int html_element_idx, BrowserWindow *bw) {
   getStyle (bw); // ensure that style of current node is computed

   stackPushEmptyNode();
   StyleNode *n = getCurrentNode(this);

   n->doctreeNodeIdx = doctreePushNode(this->doc_tree_ptr, html_element_idx);
   //n->doctreeNodeIdx = ffiDoctreePushNode(this->doc_tree_ref, html_element_idx);
   ffiDoctreePushNode(this->doc_tree_ref, html_element_idx);

   if (ffiStyleEngineStyleNodesStackSize(this->style_engine_ref) > 1) {
      StyleNode * parentNode = getParentNode(this);
      n->displayNone = parentNode->displayNone;
   }
}

void StyleEngine::startElement (const char *tagname, BrowserWindow *bw) {
   startElement (ffiHtmlTagIndex(tagname), bw);
}

void StyleEngine::setElementId (const char *id) {
   c_doctree_node_t * dtn = doctreeGetTopNode(this->doc_tree_ptr);
   assert (dtn->c_element_selector_id == NULL);
   dtn->c_element_selector_id = strdup (id);

   ffiStyleEngineSetElementId(this->doc_tree_ref, id);
}

void StyleEngine::setElementClass(const char * element_class_tokens) {
   c_doctree_node_t * dtn = doctreeGetTopNode(this->doc_tree_ptr);
   assert (dtn->c_element_selector_class_size == 0);

   char * saveptr = NULL;
   const char * sep = " ";
   char * in = strdup(element_class_tokens);
   int i = 0;
   for (char * tok = strtok_r(in, sep, &saveptr); tok; tok = strtok_r(NULL, sep, &saveptr)) {
      dtn->c_element_selector_class[i] = strdup(tok);
      i++;

      if (i == SELECTOR_CLASS_MAX) {
         fprintf(stderr, "[WW] 'class' selector string has more tokens than limit==%d\n", SELECTOR_CLASS_MAX);
         fprintf(stderr, "[WW] the 'class' selector is '%s'\n", element_class_tokens);
         break;
      }
   }
   dtn->c_element_selector_class_size = i;
   free(in);

   ffiStyleEngineSetElementClass(this->doc_tree_ref, element_class_tokens);
}

// 'cssStyleAttribute' is contents of element's "style" attribute, describing CSS
// properties-values for a Node.
void StyleEngine::setCssStyleForCurrentNode(const char * cssStyleAttribute)
{
   StyleNode * currentNode = getCurrentNode(this);
   //assert (currentNode->declLists.main == NULL);
   // parse style information from style="" attribute, if it exists
   if (cssStyleAttribute && prefs.parse_embedded_css) {

      timer_start(&g_parse_start);

      ffiCssParseElementStyleAttribute(this->style_engine_ref, baseUrl, cssStyleAttribute, strlen (cssStyleAttribute));

      timer_stop(&g_parse_start, &g_parse_stop, &g_parse_acc);
      fprintf(stderr, "[II] Total parse time increased to %ld:%06ld\n", g_parse_acc.tv_sec, g_parse_acc.tv_usec);
   }
}

/**
 * \brief Instruct StyleEngine to use the nonCssHints from parent element
 * This is only used for tables where nonCssHints on the TABLE-element
 * (e.g. border=1) also affect child elements like TD.
 */
void StyleEngine::inheritNonCssHints()
{
   ffiInheritNonCssHints(this->style_engine_ref);
}

void StyleEngine::clearNonCssHints()
{
   ffiStyleEngineStyleNodesClearNonCssHints(this->style_engine_ref);
}

/**
 * \brief Use of the background color of the parent style as default.
 *   This is only used in table code to allow for colors specified for
 *   table rows as table rows are currently no widgets and therefore
 *   don't draw any background.
 */
void StyleEngine::inheritBackgroundColor () {
   StyleNode * currentNode = getCurrentNode(this);
   currentNode->inheritBackgroundColor = true;
}

dw::core::style::Color *StyleEngine::getBackgroundColor () {
   for (int i = 1; i < ffiStyleEngineStyleNodesStackSize(this->style_engine_ref); i++) {
      StyleNode *n = &styleNodesStack[i];

      if (n->style && n->style->backgroundColor)
         return n->style->backgroundColor;
   }

   return NULL;
}

dw::core::style::StyleImage *StyleEngine::getBackgroundImage
   (dw::core::style::BackgroundRepeat *bgRepeat,
    dw::core::style::BackgroundAttachment *bgAttachment,
    DwLength *bgPositionX,
    DwLength *bgPositionY) {
   for (int i = 1; i < ffiStyleEngineStyleNodesStackSize(this->style_engine_ref); i++) {
      StyleNode *n = &styleNodesStack[i];

      if (n->style && n->style->backgroundImage) {
         *bgRepeat     = (BackgroundRepeat) ffiStyleAttrsBgRepeat(n->style->c_style_attrs_ref);
         *bgAttachment = (BackgroundAttachment) ffiStyleAttrsBgAttachment(n->style->c_style_attrs_ref);
         ffiStyleAttrsBgPositionX(n->style->c_style_attrs_ref, bgPositionX);
         ffiStyleAttrsBgPositionY(n->style->c_style_attrs_ref, bgPositionY);
         return n->style->backgroundImage;
      }
   }

   return NULL;
}

/**
 * \brief set the CSS pseudo class :link.
 */
void StyleEngine::setPseudoLink () {
   c_doctree_node_t * dtn = doctreeGetTopNode(this->doc_tree_ptr);
   dtn->c_element_selector_pseudo_class = "link";

   ffiStyleEngineSetElementPseudoClass(this->doc_tree_ref, "link");
}

/**
 * \brief set the CSS pseudo class :visited.
 */
void StyleEngine::setPseudoVisited () {
   c_doctree_node_t * dtn = doctreeGetTopNode(this->doc_tree_ptr);
   dtn->c_element_selector_pseudo_class = "visited";

   ffiStyleEngineSetElementPseudoClass(this->doc_tree_ref, "visited");
}

/**
 * \brief tell the styleEngine that a html element has ended.
 */
void StyleEngine::endElement (int element) {
   assert (element == doctreeGetTopNode(this->doc_tree_ptr)->c_html_element_idx);

   stackPop ();
   doctreePopNode(this->doc_tree_ptr);
   ffiDoctreePopNode(this->doc_tree_ref);
}

void StyleEngine::preprocessAttrs (dw::core::style::StyleAttrs *attrs) {
   /* workaround for styling of inline elements */
   StyleNode * parentNode = getParentNode(this);

   if (parentNode->inheritBackgroundColor) {
      attrs->backgroundColor      = parentNode->style->backgroundColor;
      attrs->backgroundImage      = parentNode->style->backgroundImage;

      // NOTE: if parentNode->inheritBackgroundColor is false, parentNode->style is NULL.
      ffiStyleEnginePreprocessAttrsInheritBackground(attrs->c_style_attrs_ref, parentNode->style->c_style_attrs_ref);
   }

   ffiStyleEnginePreprocessAttrs(attrs->c_style_attrs_ref);
}

// Construct ui-related objects in @p attrs (objects that can't be accessed
// from Haskell's object and must be done in C++).
static void style_attrs_make_ui_objects(StyleAttrs * attrs, dw::core::Layout * layout)
{
   // Special initial value meaning "value of given attribute is not set".
   const int not_set = -1;

   c_border_color_t border_color = {};
   ffiStyleAttrsBorderColor(attrs->c_style_attrs_ref, &border_color);
   attrs->borderColor.top    = border_color.top == not_set    ? nullptr : Color::create(layout, border_color.top);
   attrs->borderColor.right  = border_color.right == not_set  ? nullptr : Color::create(layout, border_color.right);
   attrs->borderColor.bottom = border_color.bottom == not_set ? nullptr : Color::create(layout, border_color.bottom);
   attrs->borderColor.left   = border_color.left == not_set   ? nullptr : Color::create(layout, border_color.left);

   // Notice that we don't assign NULL if attrs->color is "not set". Some
   // code relies on attrs->color always being non-NULL.
   int color = ffiStyleAttrsColor(attrs->c_style_attrs_ref);
   attrs->color = color == not_set ? attrs->color : Color::create(layout, color);

   int bg_color = ffiStyleAttrsBackgroundColor(attrs->c_style_attrs_ref);
   if (not_set != bg_color) {
      // TODO: check the logic in if(). Wouldn't it be more natural to write it this way?
      // if (color attribute == white && don't allow white bg
      //    then use white bg replacement
      // else
      //    use given color attribute
      if (prefs.allow_white_bg || bg_color != 0xffffff) {
         attrs->backgroundColor = Color::create(layout, bg_color);
      } else {
         attrs->backgroundColor = Color::create(layout, prefs.white_bg_replacement);
      }
   }

   {
      char * tooltip = ffiStyleAttrsXTooltip(attrs->c_style_attrs_ref);
      if (tooltip) {
         attrs->x_tooltip = Tooltip::create(layout, tooltip);
         // FIXME: Here we should free() style_attrs->c_x_tooltip, but it has
         // been allocated in Haskell so I don't want to dig into freeing of
         // such pointers. As in other cases where FFI code is leaking
         // memory: this is only temporary, until all code is moved to
         // Haskell.
      }
   }

   {
      FontAttrs fontAttrs = {};
      ffiStyleAttrsFontAttrs(attrs->c_style_attrs_ref, &fontAttrs.font_attrs);
      attrs->font = Font::create(layout, &fontAttrs);
   }
}


/**
 * \brief Make changes to StyleAttrs attrs according to element's declarations set (referenced by merged_decl_set_ref)
 */
void StyleEngine::applyStyleToGivenNode(int styleNodeIndex, StyleAttrs * parentAttrs, StyleAttrs * attrs, int merged_decl_set_ref, BrowserWindow *bw)
{
   // TODO: this should be set from style_attrs calculated by
   // ffiStyleEngineApplyStyleToGivenNode for CSS_PROPERTY_BACKGROUND_IMAGE
   // property.
   DilloUrl *imgUrl = nullptr;

   timer_start(&g_apply_do_start);
   ffiStyleEngineApplyStyleToGivenNode(merged_decl_set_ref, &prefs.preferences, layout->dpiX(), layout->dpiY(), parentAttrs->c_style_attrs_ref, attrs->c_style_attrs_ref);
   timer_stop(&g_apply_do_start, &g_apply_do_stop, &g_apply_do_acc);
   fprintf(stderr, "[II] Total apply-do time increased to %ld:%06ld\n", g_apply_do_acc.tv_sec, g_apply_do_acc.tv_usec);

   /* Handle additional things that were not handled in Haskell. */
   if (ffiStyleAttrsDisplay(attrs->c_style_attrs_ref) == DISPLAY_NONE) {
      styleNodesStack[styleNodeIndex].displayNone = true;
   }

   if (imgUrl && prefs.load_background_images &&
       !styleNodesStack[styleNodeIndex].displayNone &&
       !(URL_FLAGS(pageUrl) & URL_SpamSafe))
   {
      attrs->backgroundImage = StyleImage::create();
      DilloImage *image = a_Image_new(layout, (void*)attrs->backgroundImage->getMainImgRenderer(), 0xffffff);

      // we use the pageUrl as requester to prevent cross
      // domain requests as specified in domainrc
      DilloWeb *web = a_Web_new(bw, imgUrl, pageUrl);
      web->Image = image;
      a_Image_ref(image);
      web->flags |= WEB_Image;

      int clientKey;
      if ((clientKey = a_Capi_open_url(web, NULL, NULL)) != 0) {
                  a_Bw_add_client(bw, clientKey, 0);
                  a_Bw_add_url(bw, imgUrl);
                  attrs->backgroundImage->connectDeletion(new StyleImageDeletionReceiver (clientKey));
      }
   }
   a_Url_free (imgUrl);
}

/**
 * \brief Similar to StyleEngine::style(), but with backgroundColor set.
 * A normal style might have backgroundColor == NULL to indicate a transparent
 * background. This method ensures that backgroundColor is set.
 */
Style * StyleEngine::getBackgroundStyle (BrowserWindow *bw) {
   StyleNode * currentNode = getCurrentNode(this);
   if (!currentNode->backgroundStyle) {
      StyleAttrs attrs = *getStyle (bw);

      for (int i = ffiStyleEngineStyleNodesStackSize(this->style_engine_ref) - 1; i >= 0 && ! attrs.backgroundColor; i--) {
         attrs.backgroundColor = styleNodesStack[i].style->backgroundColor;
      }

      assert (attrs.backgroundColor);
      currentNode->backgroundStyle = Style::create (&attrs);
   }
   return currentNode->backgroundStyle;
}

/**
 * \brief Create a new style object based on the previously opened / closed
 * HTML elements and the declListNonCss that have been set.
 * This method is private. Call style() to get a current style object.
 */
Style * StyleEngine::makeStyle(int styleNodeIndex, BrowserWindow *bw)
{
   StyleAttrs parentStyleAttrs = *styleNodesStack[styleNodeIndex - 1].style;

   // Initially styleAttrs will be the same as style attributes of parent
   // node, but after applyStyleToGivenNode is executed the styleAttrs will
   // have their own set of style attributes (different than parent's).
   //
   // This assignment also copies parent's ui members (color, background
   // color, etc.) which aren't copied/handled in Haskell code. They may be
   // however overwritten in style_attrs_make_ui_objects().
   StyleAttrs styleAttrs = parentStyleAttrs;
   styleAttrs.c_style_attrs_ref = ffiStyleAttrsCtor();
   /* TODO: check if we could use ffiStyleAttrsCopyCtor() above and remove
      the call to ffiStyleAttrsCopy(). */
   ffiStyleAttrsCopy(styleAttrs.c_style_attrs_ref, parentStyleAttrs.c_style_attrs_ref);

   // Ensure that StyleEngine::style0() has not been called before for
   // this element.
   // Style computation is expensive so limit it as much as possible.
   // If this assertion is hit, you need to rearrange the code that is
   // doing styleEngine calls to call setNonCssHintOfCurrentNode() before calling
   // style() or wordStyle() for each new element.
   assert (styleNodesStack[styleNodeIndex].style == NULL);

   styleAttrs.resetNonInheritedValues();

   preprocessAttrs(&styleAttrs);

   const int dtnNum = styleNodesStack[styleNodeIndex].doctreeNodeIdx;

   timer_start(&g_apply_get_start);

   // Merge style information from main (non-important) declarations,
   // important declarations, and non-CSS hints.
   int merged_decl_set_ref = ffiCssContextApplyCssContext(this->style_engine_ref,
                                                          this->css_context_ref,
                                                          this->doc_tree_ref,
                                                          dtnNum);

   timer_stop(&g_apply_get_start, &g_apply_get_stop, &g_apply_get_acc);
   fprintf(stderr, "[II] Total apply-get time increased to %ld:%06ld\n", g_apply_get_acc.tv_sec, g_apply_get_acc.tv_usec);

   // apply style
   applyStyleToGivenNode(styleNodeIndex, &parentStyleAttrs, &styleAttrs, merged_decl_set_ref, bw);

   ffiStyleEnginePostprocessAttrs(styleAttrs.c_style_attrs_ref);

   style_attrs_make_ui_objects(&styleAttrs, this->layout);

   styleNodesStack[styleNodeIndex].style = Style::create(&styleAttrs);

   return styleNodesStack[styleNodeIndex].style;
}

Style * StyleEngine::makeWordStyle(BrowserWindow *bw) {
   StyleAttrs attrs = *getStyle (bw);
   attrs.c_style_attrs_ref = ffiStyleAttrsCopyCtor(attrs.c_style_attrs_ref);
   attrs.resetNonInheritedValues();

   StyleNode * node = getCurrentNode(this);
   if (node->inheritBackgroundColor) {
      attrs.backgroundColor      = getStyle (bw)->backgroundColor;
      attrs.backgroundImage      = getStyle (bw)->backgroundImage;
      ffiStyleEngineMakeWordStyleInheritBackground(attrs.c_style_attrs_ref, getStyle(bw)->c_style_attrs_ref);
   }

   ffiStyleEngineMakeWordStyle(attrs.c_style_attrs_ref, getStyle(bw)->c_style_attrs_ref);

   node->wordStyle = Style::create(&attrs);
   return node->wordStyle;
}

/**
 * \brief Recompute all style information from scratch
 * This is used to take into account CSS styles for the HTML-element.
 * The CSS data is only completely available after parsing the HEAD-section
 * and thereby after the HTML-element has been opened.
 * Note that restyle() does not change any styles in the widget tree.
 */
void StyleEngine::restyle (BrowserWindow *bw) {
   for (int styleNodeIndex = 1; styleNodeIndex < ffiStyleEngineStyleNodesStackSize(this->style_engine_ref); styleNodeIndex++) {
      StyleNode *n = &styleNodesStack[styleNodeIndex];
      if (n->style) {
         n->style->unref ();
         n->style = NULL;
      }
      if (n->wordStyle) {
         n->wordStyle->unref ();
         n->wordStyle = NULL;
      }
      if (n->backgroundStyle) {
         n->backgroundStyle->unref ();
         n->backgroundStyle = NULL;
      }

      makeStyle(styleNodeIndex, bw);
   }
}

void StyleEngine::parseCssWithOrigin(DilloHtml *html, DilloUrl *url, const char *buf, int buflen, CssOrigin origin)
{
   if (importDepth > 10) { // avoid looping with recursive @import directives
      MSG_WARN("Maximum depth of CSS @import reached--ignoring stylesheet.\n");
      return;
   }

   static int i = 0;
   char path[50] = { 0 };
   snprintf(path, sizeof (path), "/tmp/hello_browser_context_%03d", i);
   FILE * file = fopen(path, "w");
   i++;

   importDepth++;
   {
      timer_start(&g_parse_start);

      CssParser parser_(origin, url, buf, buflen);
      ffiParseCss(&parser_.m_parser, &parser_.m_token, this->css_context_ref);

      struct timeval diff = timer_stop(&g_parse_start, &g_parse_stop, &g_parse_acc);
      fprintf(stderr, "[II] Total parse time increased by %ld:%06ld to %ld:%06ld (url = %s)\n",
              diff.tv_sec, diff.tv_usec,
              g_parse_acc.tv_sec, g_parse_acc.tv_usec,
              dStr_printable(url->url_string, 130));
   }
   importDepth--;

   ffiCssContextPrint(path, this->css_context_ref);
   fclose(file);
}

void StyleEngine::buildUserStyle(int context_ref)
{
   Dstr *style;
   char *filename = dStrconcat(dGethomedir(), "/" PROGRAM_LOCAL_DIR "/style.css", NULL);

   if ((style = a_Misc_file2dstr(filename))) {
      {
         timer_start(&g_parse_start);

         CssParser parser_(CSS_ORIGIN_USER, NULL, style->str, style->len);
         ffiParseCss(&parser_.m_parser, &parser_.m_token, context_ref);

         struct timeval diff = timer_stop(&g_parse_start, &g_parse_stop, &g_parse_acc);
         fprintf(stderr, "[II] Total parse time increased by %ld:%06ld to %ld:%06ld\n",
                 diff.tv_sec, diff.tv_usec,
                 g_parse_acc.tv_sec, g_parse_acc.tv_usec);
      }
      dStr_free (style, 1);
   }
   dFree (filename);
}

void cpp_styleEngineSetNonCssHintOfNodeLength(StyleEngine * styleEngine, CssDeclarationProperty property, CssLength cssLength)
{
   float lengthValue = cpp_cssLengthValue(cssLength);
   int lengthType  = (int) cpp_cssLengthType(cssLength);
   ffiStyleEngineSetNonCssHintOfNodeLength(styleEngine->style_engine_ref, property, lengthValue, lengthType);
   return;
}

void cpp_styleEngineSetNonCssHintOfNodeColor(StyleEngine * styleEngine, int property, int color)
{
   ffiStyleEngineSetNonCssHintOfNodeColor(styleEngine->style_engine_ref, property, color);
   return;
}

void cpp_styleEngineSetNonCssHintOfNodeString(StyleEngine * styleEngine, int property, const char * stringVal)
{
   ffiStyleEngineSetNonCssHintOfNodeString(styleEngine->style_engine_ref, property, stringVal);
   return;
}

void cpp_styleEngineSetNonCssHintOfNodeEnum(StyleEngine * styleEngine, int property, int enumVal)
{
   ffiStyleEngineSetNonCssHintOfNodeEnum(styleEngine->style_engine_ref, property, enumVal);
   return;
}




void cpp_styleEngineSetXImgOfNode(StyleEngine * styleEngine, int intVal)
{
   ffiStyleEngineSetXImgOfNode(styleEngine->style_engine_ref, intVal);
   return;
}

void cpp_styleEngineSetXLangOfNode(StyleEngine * styleEngine, const char * stringVal)
{
   ffiStyleEngineSetXLangOfNode(styleEngine->style_engine_ref, stringVal);
   return;
}

void cpp_styleEngineSetXLinkOfNode(StyleEngine * styleEngine, int intVal)
{
   ffiStyleEngineSetXLinkOfNode(styleEngine->style_engine_ref, intVal);
   return;
}

void cpp_styleEngineSetXTooltipOfNode(StyleEngine * styleEngine, const char * stringVal)
{
   ffiStyleEngineSetXTooltipOfNode(styleEngine->style_engine_ref, stringVal);
   return;
}

