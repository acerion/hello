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

   stackPush ();
   StyleNode *n = &styleNodesStack[styleNodesStackSize - 1];

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

   n->style = Style::create (&style_attrs);
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

void StyleEngine::stackPush () {
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

   stackPush ();
   StyleNode *n = &styleNodesStack[styleNodesStackSize - 1];

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

      ffiCssParseElementStyleAttribute(baseUrl, cssStyleAttribute, strlen (cssStyleAttribute),
                                       currentNode->declLists.main_decl_set_ref,
                                       currentNode->declLists.important_decl_set_ref);
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
         *bgRepeat     = n->style->backgroundRepeat;
         *bgAttachment = n->style->backgroundAttachment;
         *bgPositionX  = n->style->backgroundPositionX;
         *bgPositionY  = n->style->backgroundPositionY;
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
      attrs->backgroundRepeat     = parentNode->style->backgroundRepeat;
      attrs->backgroundAttachment = parentNode->style->backgroundAttachment;
      attrs->backgroundPositionX  = parentNode->style->backgroundPositionX;
      attrs->backgroundPositionY  = parentNode->style->backgroundPositionY;
      attrs->verticalAlign        = parentNode->style->verticalAlign;
   }
   attrs->borderColor.top = (Color *) -1;
   attrs->borderColor.bottom = (Color *) -1;
   attrs->borderColor.left = (Color *) -1;
   attrs->borderColor.right = (Color *) -1;
   /* initial value of border-width is 'medium' */
   attrs->borderWidth.top = 2;
   attrs->borderWidth.bottom = 2;
   attrs->borderWidth.left = 2;
   attrs->borderWidth.right = 2;
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
   /* computed value of border-width is 0 if border-style
      is 'none' or 'hidden' */
   if (attrs->borderStyle.top == BORDER_NONE ||
       attrs->borderStyle.top == BORDER_HIDDEN)
      attrs->borderWidth.top = 0;
   if (attrs->borderStyle.bottom == BORDER_NONE ||
       attrs->borderStyle.bottom == BORDER_HIDDEN)
      attrs->borderWidth.bottom = 0;
   if (attrs->borderStyle.left == BORDER_NONE ||
       attrs->borderStyle.left == BORDER_HIDDEN)
      attrs->borderWidth.left = 0;
   if (attrs->borderStyle.right == BORDER_NONE ||
       attrs->borderStyle.right == BORDER_HIDDEN)
      attrs->borderWidth.right = 0;
}

c_style_attrs_t * c_style_attrs_calloc(void)
{
   c_style_attrs_t * style_attrs = (c_style_attrs_t *) calloc(1, sizeof (c_style_attrs_t));
   style_attrs->c_border_width = (c_border_width_t *) calloc(1, sizeof (c_border_width_t));
   style_attrs->c_border_style = (c_border_style_t *) calloc(1, sizeof (c_border_style_t));
   style_attrs->c_border_color = (c_border_color_t *) calloc(1, sizeof (c_border_color_t));
   style_attrs->c_margin       = (c_style_margin_t *) calloc(1, sizeof (c_style_margin_t));
   style_attrs->c_padding      = (c_style_padding_t *) calloc(1, sizeof (c_style_padding_t));
   style_attrs->c_font_attrs   = (c_font_attrs_t *) calloc(1, sizeof (c_font_attrs_t));
   style_attrs->c_text_indent  = (DwLength *) calloc(1, sizeof (DwLength));
   style_attrs->c_width        = (DwLength *) calloc(1, sizeof (DwLength));
   style_attrs->c_height       = (DwLength *) calloc(1, sizeof (DwLength));
   style_attrs->c_line_height  = (DwLength *) calloc(1, sizeof (DwLength));

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
   free((*style_attrs)->c_border_width);
   free((*style_attrs)->c_border_style);
   free((*style_attrs)->c_border_color);
   free((*style_attrs)->c_margin);
   free((*style_attrs)->c_padding);

   if ((*style_attrs)->c_font_attrs) {
      if ((*style_attrs)->c_font_attrs->name) {
         free((*style_attrs)->c_font_attrs->name);
      }
   }

   free((*style_attrs)->c_font_attrs);
   free((*style_attrs)->c_text_indent);
   free((*style_attrs)->c_width);
   free((*style_attrs)->c_height);
   free((*style_attrs));
}

void c_style_attrs_init(c_style_attrs_t * style_attrs)
{
   // *(style_attrs->c_border_color)     = { -1, -1, -1, -1 }; TODO: uncommenting this line breaks block-quote markings in comments on SoylentNews
   style_attrs->c_color               = -1; // TODO: this probably should be moved to style_attrs' constructor
   style_attrs->c_background_color    = -1; // TODO: this probably should be moved to style_attrs' constructor
   style_attrs->c_x_tooltip           = nullptr; // TODO: this probably should be moved to style_attrs' constructor
}

void c_style_attrs_copy_from(c_style_attrs_t * style_attrs, StyleAttrs *attrs)
{
   style_attrs->c_border_collapse = attrs->borderCollapse;
   *(style_attrs->c_border_width) = attrs->borderWidth;
   *(style_attrs->c_border_style) = attrs->borderStyle;

   if (attrs->borderColor.top != nullptr && attrs->borderColor.top != (Color *) -1) {
      style_attrs->c_border_color->top    = attrs->borderColor.top->color;
   }
   if (attrs->borderColor.right != nullptr && attrs->borderColor.right != (Color *) -1) {
      style_attrs->c_border_color->right  = attrs->borderColor.right->color;
   }
   if (attrs->borderColor.left != nullptr && attrs->borderColor.left != (Color *) -1) {
      style_attrs->c_border_color->left   = attrs->borderColor.left->color;
   }
   if (attrs->borderColor.bottom != nullptr && attrs->borderColor.bottom != (Color *) -1) {
      style_attrs->c_border_color->bottom = attrs->borderColor.bottom->color;
   }

   *(style_attrs->c_margin) = attrs->margin;
   *(style_attrs->c_padding) = attrs->padding;

   *(style_attrs->c_font_attrs) = attrs->font->font_attrs;
   if (attrs->font->font_attrs.name) {
      style_attrs->c_font_attrs->name = strdup(attrs->font->font_attrs.name);
   }

   style_attrs->c_text_align      = attrs->textAlign;
   style_attrs->c_text_decoration = attrs->textDecoration;
   *(style_attrs->c_text_indent)  = attrs->textIndent;
   style_attrs->c_text_transform  = attrs->textTransform;
   style_attrs->c_vertical_align  = attrs->verticalAlign;
   style_attrs->c_white_space     = attrs->whiteSpace;
   *(style_attrs->c_width)        = attrs->width;
   *(style_attrs->c_height)       = attrs->height;
   *(style_attrs->c_line_height)  = attrs->lineHeight;
   style_attrs->c_list_style_position = attrs->listStylePosition;
   style_attrs->c_list_style_type     = attrs->listStyleType;

   style_attrs->c_display     = attrs->display;
   style_attrs->c_cursor      = attrs->cursor;
   style_attrs->c_h_border_spacing      = attrs->hBorderSpacing;
   style_attrs->c_v_border_spacing      = attrs->vBorderSpacing;
   style_attrs->c_word_spacing          = attrs->wordSpacing;

   style_attrs->c_x_link    = attrs->x_link;
   memcpy(style_attrs->c_x_lang, attrs->x_lang, sizeof (style_attrs->c_x_lang));
   style_attrs->c_x_img     = attrs->x_img;
}

void c_style_attrs_copy_to(StyleAttrs * attrs, c_style_attrs_t * style_attrs, dw::core::Layout * layout)
{
   attrs->borderCollapse = style_attrs->c_border_collapse;
   attrs->borderWidth = *(style_attrs->c_border_width);
   attrs->borderStyle = *(style_attrs->c_border_style);

   attrs->borderColor.top    = style_attrs->c_border_color->top == -1    ? NULL : Color::create(layout, style_attrs->c_border_color->top);
   attrs->borderColor.right  = style_attrs->c_border_color->right == -1  ? NULL : Color::create(layout, style_attrs->c_border_color->right);
   attrs->borderColor.bottom = style_attrs->c_border_color->bottom == -1 ? NULL : Color::create(layout, style_attrs->c_border_color->bottom);
   attrs->borderColor.left   = style_attrs->c_border_color->left == -1   ? NULL : Color::create(layout, style_attrs->c_border_color->left);

   attrs->margin  = *(style_attrs->c_margin);
   attrs->padding = *(style_attrs->c_padding);

   attrs->textAlign      = style_attrs->c_text_align;
   attrs->textDecoration = style_attrs->c_text_decoration;
   attrs->textIndent     = *(style_attrs->c_text_indent);
   attrs->textTransform  = style_attrs->c_text_transform;

   attrs->verticalAlign = style_attrs->c_vertical_align;
   attrs->whiteSpace    = style_attrs->c_white_space;
   attrs->width  = *(style_attrs->c_width);
   attrs->height = *(style_attrs->c_height);
   attrs->lineHeight = *(style_attrs->c_line_height);
   attrs->listStylePosition = style_attrs->c_list_style_position;
   attrs->listStyleType     = style_attrs->c_list_style_type;
   attrs->display           = style_attrs->c_display;
   if (style_attrs->c_color != -1) {
      // -1 is a special initial value set on top of this function
      attrs->color = Color::create(layout, style_attrs->c_color);
   }
   if (style_attrs->c_background_color != -1) {
      // -1 is a special initial value set on top of this function
      //
      // TODO: check the logic in if(). Wouldn't it be more natural to write it this way?
      // if (color attribute == white && don't allow white bg
      //    then use white bg replacement
      // else
      //    use given color attribute
      if (prefs.allow_white_bg || style_attrs->c_background_color != 0xffffff) {
         attrs->backgroundColor = Color::create(layout, style_attrs->c_background_color);
      } else {
         attrs->backgroundColor = Color::create(layout, prefs.white_bg_replacement);
      }
   }
   attrs->cursor            = style_attrs->c_cursor;
   attrs->hBorderSpacing    = style_attrs->c_h_border_spacing;
   attrs->vBorderSpacing    = style_attrs->c_v_border_spacing;
   attrs->wordSpacing       = style_attrs->c_word_spacing;

   attrs->x_link            = style_attrs->c_x_link;
   memcpy(attrs->x_lang, style_attrs->c_x_lang, sizeof (attrs->x_lang));
   attrs->x_img             = style_attrs->c_x_img;
   if (style_attrs->c_x_tooltip) {
      attrs->x_tooltip = Tooltip::create(layout, style_attrs->c_x_tooltip);
      // Here we should free() style_attrs->c_x_tooltip, but it has been
      // allocated in Haskell so I don't want to dig into freeing of such
      // pointers. As in other cases where FFI code is leaking memory: this
      // is only temporary, until all code is moved to Haskell.
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
   Font * parentFont = styleNodesStack[styleNodeIndex - 1].style->font;

   // TODO: this should be set from style_attrs calculated by
   // ffiStyleEngineApplyStyleToGivenNode for CSS_PROPERTY_BACKGROUND_IMAGE
   // property.
   DilloUrl *imgUrl = nullptr;

   c_style_attrs_t * style_attrs = c_style_attrs_calloc();
   c_style_attrs_init(style_attrs);
   c_style_attrs_copy_from(style_attrs, attrs);

   c_style_attrs_t * parent_style_attrs = c_style_attrs_calloc();
   c_style_attrs_init(parent_style_attrs);
   c_style_attrs_copy_from(parent_style_attrs, parentAttrs);

   ffiStyleEngineApplyStyleToGivenNode(merged_decl_set_ref, &prefs.preferences, layout->dpiX(), layout->dpiY(), parent_style_attrs, style_attrs);

   c_style_attrs_copy_to(attrs, style_attrs, this->layout);
   c_style_attrs_dealloc(&style_attrs);

   c_style_attrs_dealloc(&parent_style_attrs);

   /* Handle additional things that were not handled in Haskell. */
   if (style_attrs->c_display == DISPLAY_NONE) {
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
Style * StyleEngine::getStyle0(int some_idx, BrowserWindow *bw)
{
   int styleNodeIndex = some_idx;

   StyleAttrs parentStyleAttrs = *styleNodesStack[styleNodeIndex - 1].style;

   // Here "attrs" are the style attributes of previous/parent node, but
   // after they are passed to applyStyleToGivenNode and processed in the
   // function, they are then used to create new style of current node when
   // the "attrs" are passed to "Style::create(&attrs)".
   StyleAttrs styleAttrs = parentStyleAttrs;

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
   int merged_decl_set_ref = ffiCssContextApplyCssContext(this->css_context_ref,
                                                          this->doc_tree_ref, dtnNum,
                                                          declLists->main_decl_set_ref,
                                                          declLists->important_decl_set_ref,
                                                          declLists->non_css_decl_set_ref);

   // apply style
   applyStyleToGivenNode(styleNodeIndex, &parentStyleAttrs, &styleAttrs, merged_decl_set_ref, bw);

   postprocessAttrs(&styleAttrs);

   styleNodesStack[styleNodeIndex].style = Style::create(&styleAttrs);

   return styleNodesStack[styleNodeIndex].style;
}

Style * StyleEngine::getWordStyle0 (BrowserWindow *bw) {
   StyleAttrs attrs = *getStyle (bw);
   attrs.resetValues ();

   StyleNode * node = getCurrentNode(this);
   if (node->inheritBackgroundColor) {
      attrs.backgroundColor      = getStyle (bw)->backgroundColor;
      attrs.backgroundImage      = getStyle (bw)->backgroundImage;
      attrs.backgroundRepeat     = getStyle (bw)->backgroundRepeat;
      attrs.backgroundAttachment = getStyle (bw)->backgroundAttachment;
      attrs.backgroundPositionX  = getStyle (bw)->backgroundPositionX;
      attrs.backgroundPositionY  = getStyle (bw)->backgroundPositionY;
   }

   attrs.verticalAlign = getStyle(bw)->verticalAlign;

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
   for (int some_idx = 1; some_idx < styleNodesStackSize; some_idx++) {
      StyleNode *n = &styleNodesStack[some_idx];
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

      getStyle0 (some_idx, bw);
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
      CssParser parser_(origin, url, buf, buflen);
      ffiParseCss(&parser_.m_parser, &parser_.m_token, this->css_context_ref);
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
         CssParser parser_(CSS_ORIGIN_USER, NULL, style->str, style->len);
         ffiParseCss(&parser_.m_parser, &parser_.m_token, context_ref);
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

