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

StyleEngine::StyleEngine (dw::core::Layout *layout,
                          const DilloUrl *pageUrl, const DilloUrl *baseUrl) {
   StyleAttrs style_attrs;
   FontAttrs font_attrs;

   this->doc_tree_ptr = doctreeCtor();
   this->doc_tree_ref = ffiDoctreeCtor();

   //styleNodesStack = new lout::misc::SimpleVector <StyleNode> (1);

   {
      this->css_context_ref = ffiCssContextCtor();
      buildUserStyle(this->css_context_ref);
   }

   this->layout = layout;
   this->pageUrl = pageUrl ? a_Url_dup(pageUrl) : NULL;
   this->baseUrl = baseUrl ? a_Url_dup(baseUrl) : NULL;
   importDepth = 0;

   /* Create a dummy font, attribute, and tag for the bottom of the stack. */
   font_attrs.font_attrs.name = prefs.preferences.font_sans_serif;
   font_attrs.font_attrs.size = roundInt(14 * prefs.preferences.font_factor);
   if (font_attrs.font_attrs.size < prefs.preferences.font_min_size)
      font_attrs.font_attrs.size = prefs.preferences.font_min_size;
   if (font_attrs.font_attrs.size > prefs.preferences.font_max_size)
      font_attrs.font_attrs.size = prefs.preferences.font_max_size;
   font_attrs.font_attrs.weight = 400;
   font_attrs.font_attrs.style = FONT_STYLE_NORMAL;
   font_attrs.font_attrs.letterSpacing = 0;
   font_attrs.font_attrs.fontVariant = FONT_VARIANT_NORMAL;

   style_attrs.initValues ();
   style_attrs.font = Font::create (layout, &font_attrs);
   style_attrs.color = Color::create (layout, 0);
   style_attrs.backgroundColor = Color::create (layout, prefs.bg_color);

   stackPushEmptyNode();
   styleNodesStack[styleNodesStackSize - 1].style = Style::create (&style_attrs);
}

StyleEngine::~StyleEngine () {
   while (doctreeGetTopNode(this->doc_tree_ptr))
      endElement (doctreeGetTopNode(this->doc_tree_ptr)->c_html_element_idx);

   stackPop (); // dummy node on the bottom of the stack
   assert (styleNodesStackSize == 0);

   a_Url_free(pageUrl);
   a_Url_free(baseUrl);

   delete doc_tree_ptr;
}

void StyleEngine::stackPushEmptyNode () {
   static const StyleNode emptyNode = {
      .declLists = { -1, -1, -1 },
      .style = NULL,
      .wordStyle = NULL,
      .backgroundStyle = NULL,
      .inheritBackgroundColor = false,
      .displayNone = false,
      .doctreeNodeIdx = 0
   };

   memcpy(&styleNodesStack[styleNodesStackSize], &emptyNode, sizeof (emptyNode));
   styleNodesStackSize++;
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
   styleNodesStackSize--;
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

   if (styleNodesStackSize > 1) {
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
      currentNode->declLists.main_decl_set_ref      = ffiDeclarationSetCtor();
      currentNode->declLists.important_decl_set_ref = ffiDeclarationSetCtor();

      gettimeofday(&g_parse_start, NULL);

      ffiCssParseElementStyleAttribute(baseUrl, cssStyleAttribute, strlen (cssStyleAttribute),
                                       currentNode->declLists.main_decl_set_ref,
                                       currentNode->declLists.important_decl_set_ref);

      gettimeofday(&g_parse_stop, NULL);
      struct timeval diff = {};
      timersub(&g_parse_stop, &g_parse_start, &diff);
      timeradd(&diff, &g_parse_acc, &g_parse_acc);
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
   StyleNode * currentNode = getCurrentNode(this);
   StyleNode * parentNode  = getParentNode(this);

   int ref = currentNode->declLists.non_css_decl_set_ref;
   currentNode->declLists.non_css_decl_set_ref = ffiInheritNonCssHints(parentNode->declLists.non_css_decl_set_ref, ref);
}

void StyleEngine::clearNonCssHints()
{
   StyleNode * currentNode = getCurrentNode(this);

   // TODO: how to delete this in Haskell?
   //delete currentNode->declLists.nonCss;
   currentNode->declLists.non_css_decl_set_ref = -1;
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
   for (int i = 1; i < styleNodesStackSize; i++) {
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
   for (int i = 1; i < styleNodesStackSize; i++) {
      StyleNode *n = &styleNodesStack[i];

      if (n->style && n->style->backgroundImage) {
         *bgRepeat     = (BackgroundRepeat) ffiStyleAttrsBgRepeat(n->style->c_attrs.c_style_attrs_ref);
         *bgAttachment = (BackgroundAttachment) ffiStyleAttrsBgAttachment(n->style->c_attrs.c_style_attrs_ref);
         ffiStyleAttrsBgPositionX(n->style->c_attrs.c_style_attrs_ref, bgPositionX);
         ffiStyleAttrsBgPositionY(n->style->c_attrs.c_style_attrs_ref, bgPositionY);
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
      ffiStyleEnginePreprocessAttrsInheritBackground(attrs->c_attrs.c_style_attrs_ref, parentNode->style->c_attrs.c_style_attrs_ref);
   }
   attrs->borderColor.top = (Color *) -1;
   attrs->borderColor.bottom = (Color *) -1;
   attrs->borderColor.left = (Color *) -1;
   attrs->borderColor.right = (Color *) -1;

   ffiStyleEnginePreprocessAttrs(attrs->c_attrs.c_style_attrs_ref);
}

void StyleEngine::postprocessAttrs (dw::core::style::StyleAttrs *attrs) {
   /* if border-color is not specified, use color as computed value */
   if (attrs->borderColor.top == (Color *) -1)
      attrs->borderColor.top = attrs->color;
   if (attrs->borderColor.bottom == (Color *) -1)
      attrs->borderColor.bottom = attrs->color;
   if (attrs->borderColor.left == (Color *) -1)
      attrs->borderColor.left = attrs->color;
   if (attrs->borderColor.right == (Color *) -1)
      attrs->borderColor.right = attrs->color;

   ffiStyleEnginePostprocessAttrs(attrs->c_attrs.c_style_attrs_ref);
}

c_style_attrs_t * c_style_attrs_calloc(void)
{
   c_style_attrs_t * style_attrs = (c_style_attrs_t *) calloc(1, sizeof (c_style_attrs_t));
   style_attrs->c_font_attrs   = (c_font_attrs_t *) calloc(1, sizeof (c_font_attrs_t));

   return style_attrs;
}

void c_style_attrs_dealloc(c_style_attrs_t ** style_attrs)
{
   if (nullptr == style_attrs) {
      return;
   }
   if (nullptr == *style_attrs) {
      return;
   }

   if ((*style_attrs)->c_font_attrs) {
      if ((*style_attrs)->c_font_attrs->name) {
         free((*style_attrs)->c_font_attrs->name);
      }
   }

   free((*style_attrs)->c_font_attrs);
   free((*style_attrs));
}

void c_style_attrs_copy_from(c_style_attrs_t * style_attrs, StyleAttrs *attrs)
{
   ffiStyleAttrsCopy(style_attrs->c_style_attrs_ref, attrs->c_attrs.c_style_attrs_ref);

   *(style_attrs->c_font_attrs) = attrs->font->font_attrs;
   if (attrs->font->font_attrs.name) {
      style_attrs->c_font_attrs->name = strdup(attrs->font->font_attrs.name);
   }
}

void c_style_attrs_copy_to(StyleAttrs * attrs, c_style_attrs_t * style_attrs, dw::core::Layout * layout)
{
   ffiStyleAttrsCopy(attrs->c_attrs.c_style_attrs_ref, style_attrs->c_style_attrs_ref);

   // Special initial value meaning "value of given attribute is not set".
   const int not_set = -1;

   c_border_color_t border_color = {};
   ffiStyleAttrsBorderColor(style_attrs->c_style_attrs_ref, &border_color);
   attrs->borderColor.top    = border_color.top == not_set    ? nullptr : Color::create(layout, border_color.top);
   attrs->borderColor.right  = border_color.right == not_set  ? nullptr : Color::create(layout, border_color.right);
   attrs->borderColor.bottom = border_color.bottom == not_set ? nullptr : Color::create(layout, border_color.bottom);
   attrs->borderColor.left   = border_color.left == not_set   ? nullptr : Color::create(layout, border_color.left);

   // Notice that we don't assign NULL if attrs->color is "not set". Some
   // code relies on attrs->color always being non-NULL.
   int color = ffiStyleAttrsColor(style_attrs->c_style_attrs_ref);
   attrs->color = color == not_set ? attrs->color : Color::create(layout, color);

   int bg_color = ffiStyleAttrsBackgroundColor(style_attrs->c_style_attrs_ref);
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
      FontAttrs fontAttrs = *attrs->font;
      fontAttrs.font_attrs = *(style_attrs->c_font_attrs);
      if (style_attrs->c_font_attrs->name) {
         if (fontAttrs.font_attrs.name) {
            // TODO: for some reason this crashes. Maybe because some default
            // font name is made from string literal?
            // free(fontAttrs.font_attrs.name);
         }
         fontAttrs.font_attrs.name = strdup(style_attrs->c_font_attrs->name);
      }
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

   c_style_attrs_t * style_attrs = c_style_attrs_calloc();
   c_style_attrs_copy_from(style_attrs, attrs);
   style_attrs->c_style_attrs_ref = attrs->c_attrs.c_style_attrs_ref;

   c_style_attrs_t * parent_style_attrs = c_style_attrs_calloc();
   c_style_attrs_copy_from(parent_style_attrs, parentAttrs);
   parent_style_attrs->c_style_attrs_ref = parentAttrs->c_attrs.c_style_attrs_ref;

   gettimeofday(&g_apply_do_start, NULL);

#if 0
   fprintf(stderr, "before calling ffiStyleEngineApplyStyleToGivenNode: parentAttrs: %d, attrs: %d\n", parentAttrs->c_attrs.c_style_attrs_ref, attrs->c_attrs.c_style_attrs_ref);
   fprintf(stderr, "before calling ffiStyleEngineApplyStyleToGivenNode: parent_style_attrs: %d, style_attrs: %d\n", parent_style_attrs->c_style_attrs_ref, style_attrs->c_style_attrs_ref);
   fprintf(stderr, "before calling ffiStyleEngineApplyStyleToGivenNode: VALUE in parentAttrs: %d, VALUE in attrs: %d\n", ffiStyleAttrsListStyleType(parentAttrs->c_attrs.c_style_attrs_ref), ffiStyleAttrsListStyleType(attrs->c_attrs.c_style_attrs_ref));
   fprintf(stderr, "before calling ffiStyleEngineApplyStyleToGivenNode: VALUE in parent_style_attrs: %d, VALUE in style_attrs: %d\n", ffiStyleAttrsListStyleType(parent_style_attrs->c_style_attrs_ref), ffiStyleAttrsListStyleType(style_attrs->c_style_attrs_ref));
#endif


   ffiStyleEngineApplyStyleToGivenNode(merged_decl_set_ref, &prefs.preferences, layout->dpiX(), layout->dpiY(), parent_style_attrs, style_attrs);


#if 0
   fprintf(stderr, "after calling ffiStyleEngineApplyStyleToGivenNode: parentAttrs: %d, attrs: %d\n", parentAttrs->c_attrs.c_style_attrs_ref, attrs->c_attrs.c_style_attrs_ref);
   fprintf(stderr, "after calling ffiStyleEngineApplyStyleToGivenNode: parent_style_attrs: %d, style_attrs: %d\n", parent_style_attrs->c_style_attrs_ref, style_attrs->c_style_attrs_ref);

   fprintf(stderr, "after calling ffiStyleEngineApplyStyleToGivenNode: VALUE in parentAttrs: %d, VALUE in attrs: %d\n", ffiStyleAttrsListStyleType(parentAttrs->c_attrs.c_style_attrs_ref), ffiStyleAttrsListStyleType(attrs->c_attrs.c_style_attrs_ref));
   fprintf(stderr, "after calling ffiStyleEngineApplyStyleToGivenNode: VALUE in parent_style_attrs: %d, VALUE in style_attrs: %d\n", ffiStyleAttrsListStyleType(parent_style_attrs->c_style_attrs_ref), ffiStyleAttrsListStyleType(style_attrs->c_style_attrs_ref));
#endif


   gettimeofday(&g_apply_do_stop, NULL);
   struct timeval diff = {};
   timersub(&g_apply_do_stop, &g_apply_do_start, &diff);
   timeradd(&diff, &g_apply_do_acc, &g_apply_do_acc);
   fprintf(stderr, "[II] Total apply-do time increased to %ld:%06ld\n", g_apply_do_acc.tv_sec, g_apply_do_acc.tv_usec);

   c_style_attrs_copy_to(attrs, style_attrs, this->layout);
   attrs->c_attrs.c_style_attrs_ref = style_attrs->c_style_attrs_ref;

   /* Handle additional things that were not handled in Haskell. */
   if (ffiStyleAttrsDisplay(style_attrs->c_style_attrs_ref) == DISPLAY_NONE) {
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
   c_style_attrs_dealloc(&style_attrs);
   c_style_attrs_dealloc(&parent_style_attrs);
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

      for (int i = styleNodesStackSize - 1; i >= 0 && ! attrs.backgroundColor; i--) {
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

   // Here "attrs" are the style attributes of previous/parent node, but
   // after they are passed to applyStyleToGivenNode and processed in the
   // function, they are then used to create new style of current node when
   // the "attrs" are passed to "Style::create(&attrs)".
   StyleAttrs styleAttrs = parentStyleAttrs;
   styleAttrs.c_attrs.c_style_attrs_ref = ffiStyleAttrsCtor();
   ffiStyleAttrsCopy(styleAttrs.c_attrs.c_style_attrs_ref, parentStyleAttrs.c_attrs.c_style_attrs_ref);

   // Ensure that StyleEngine::style0() has not been called before for
   // this element.
   // Style computation is expensive so limit it as much as possible.
   // If this assertion is hit, you need to rearrange the code that is
   // doing styleEngine calls to call setNonCssHintOfCurrentNode() before calling
   // style() or wordStyle() for each new element.
   assert (styleNodesStack[styleNodeIndex].style == NULL);

   // reset values that are not inherited according to CSS
   styleAttrs.resetValues();
   preprocessAttrs(&styleAttrs);

   c_css_declaration_lists_t * declLists = &styleNodesStack[styleNodeIndex].declLists;

   // merge style information
   int dtnNum = styleNodesStack[styleNodeIndex].doctreeNodeIdx;

   gettimeofday(&g_apply_get_start, NULL);

   int merged_decl_set_ref = ffiCssContextApplyCssContext(this->css_context_ref,
                                                          this->doc_tree_ref, dtnNum,
                                                          declLists->main_decl_set_ref,
                                                          declLists->important_decl_set_ref,
                                                          declLists->non_css_decl_set_ref);

   gettimeofday(&g_apply_get_stop, NULL);
   struct timeval diff = {};
   timersub(&g_apply_get_stop, &g_apply_get_start, &diff);
   timeradd(&diff, &g_apply_get_acc, &g_apply_get_acc);
   fprintf(stderr, "[II] Total apply-get time increased to %ld:%06ld\n", g_apply_get_acc.tv_sec, g_apply_get_acc.tv_usec);

   // apply style
   applyStyleToGivenNode(styleNodeIndex, &parentStyleAttrs, &styleAttrs, merged_decl_set_ref, bw);

   postprocessAttrs(&styleAttrs);

   styleNodesStack[styleNodeIndex].style = Style::create(&styleAttrs);

   return styleNodesStack[styleNodeIndex].style;
}

Style * StyleEngine::makeWordStyle(BrowserWindow *bw) {
   StyleAttrs attrs = *getStyle (bw);
   // TODO: original code called this function. Retore it?
   //attrs.resetValues ();

   StyleNode * node = getCurrentNode(this);
   if (node->inheritBackgroundColor) {
      attrs.backgroundColor      = getStyle (bw)->backgroundColor;
      attrs.backgroundImage      = getStyle (bw)->backgroundImage;
      ffiStyleEngineMakeWordStyleInheritBackground(attrs.c_attrs.c_style_attrs_ref, getStyle(bw)->c_attrs.c_style_attrs_ref);
   }

   ffiStyleEngineMakeWordStyle(attrs.c_attrs.c_style_attrs_ref, getStyle(bw)->c_attrs.c_style_attrs_ref);

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
   for (int styleNodeIndex = 1; styleNodeIndex < styleNodesStackSize; styleNodeIndex++) {
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
      gettimeofday(&g_parse_start, NULL);

      CssParser parser_(origin, url, buf, buflen);
      ffiParseCss(&parser_.m_parser, &parser_.m_token, this->css_context_ref);

      gettimeofday(&g_parse_stop, NULL);
      struct timeval diff = {};
      timersub(&g_parse_stop, &g_parse_start, &diff);
      timeradd(&diff, &g_parse_acc, &g_parse_acc);
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
         gettimeofday(&g_parse_start, NULL);

         CssParser parser_(CSS_ORIGIN_USER, NULL, style->str, style->len);
         ffiParseCss(&parser_.m_parser, &parser_.m_token, context_ref);

         gettimeofday(&g_parse_stop, NULL);
         struct timeval diff = {};
         timersub(&g_parse_stop, &g_parse_start, &diff);
         timeradd(&diff, &g_parse_acc, &g_parse_acc);
         fprintf(stderr, "[II] Total parse time increased by %ld:%06ld to %ld:%06ld\n",
                 diff.tv_sec, diff.tv_usec,
                 g_parse_acc.tv_sec, g_parse_acc.tv_usec);
      }
      dStr_free (style, 1);
   }
   dFree (filename);
}

void cpp_styleEngineSetNonCssHintOfNodeLength(StyleNode * styleNode, CssDeclarationProperty property, CssLength cssLength)
{
   float lengthValue = cpp_cssLengthValue(cssLength);
   int lengthType  = (int) cpp_cssLengthType(cssLength);
   styleNode->declLists.non_css_decl_set_ref = ffiStyleEngineSetNonCssHintOfNodeLength(styleNode->declLists.non_css_decl_set_ref, property, lengthValue, lengthType);
   return;
}

void cpp_styleEngineSetNonCssHintOfNodeColor(StyleNode * styleNode, int property, int color)
{
   styleNode->declLists.non_css_decl_set_ref = ffiStyleEngineSetNonCssHintOfNodeColor(styleNode->declLists.non_css_decl_set_ref, property, color);
   return;
}

void cpp_styleEngineSetNonCssHintOfNodeString(StyleNode * styleNode, int property, const char * stringVal)
{
   styleNode->declLists.non_css_decl_set_ref = ffiStyleEngineSetNonCssHintOfNodeString(styleNode->declLists.non_css_decl_set_ref, property, stringVal);
   return;
}

void cpp_styleEngineSetNonCssHintOfNodeEnum(StyleNode * styleNode, int property, int enumVal)
{
   styleNode->declLists.non_css_decl_set_ref = ffiStyleEngineSetNonCssHintOfNodeEnum(styleNode->declLists.non_css_decl_set_ref, property, enumVal);
   return;
}




void cpp_styleEngineSetXImgOfNode(StyleNode * styleNode, int intVal)
{
   styleNode->declLists.non_css_decl_set_ref = ffiStyleEngineSetXImgOfNode(styleNode->declLists.non_css_decl_set_ref, intVal);
   return;
}

void cpp_styleEngineSetXLangOfNode(StyleNode * styleNode, const char * stringVal)
{
   styleNode->declLists.non_css_decl_set_ref = ffiStyleEngineSetXLangOfNode(styleNode->declLists.non_css_decl_set_ref, stringVal);
   return;
}

void cpp_styleEngineSetXLinkOfNode(StyleNode * styleNode, int intVal)
{
   styleNode->declLists.non_css_decl_set_ref = ffiStyleEngineSetXLinkOfNode(styleNode->declLists.non_css_decl_set_ref, intVal);
   return;
}

void cpp_styleEngineSetXTooltipOfNode(StyleNode * styleNode, const char * stringVal)
{
   styleNode->declLists.non_css_decl_set_ref = ffiStyleEngineSetXTooltipOfNode(styleNode->declLists.non_css_decl_set_ref, stringVal);
   return;
}

