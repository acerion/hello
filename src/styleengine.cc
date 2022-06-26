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
   this->doc_tree_ref = hll_doctreeCtor();

   //styleNodesStack = new lout::misc::SimpleVector <StyleNode> (1);

   {
      this->css_context_ref = hll_cssContextCtor();
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
      NULL, NULL, NULL, NULL, NULL, NULL, false, false, 0
   };

   memcpy(&styleNodesStack[styleNodesStackSize], &emptyNode, sizeof (emptyNode));
   styleNodesStackSize++;
}

void StyleEngine::stackPop () {
   StyleNode * currentNode = getCurrentNode(this);

   delete currentNode->declLists.main;
   delete currentNode->declLists.important;
   delete currentNode->declLists.nonCss;
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
   //n->doctreeNodeIdx = hll_doctreePushNode(this->doc_tree_ref, html_element_idx);
   hll_doctreePushNode(this->doc_tree_ref, html_element_idx);

   if (styleNodesStackSize > 1) {
      StyleNode * parentNode = getParentNode(this);
      n->displayNone = parentNode->displayNone;
   }
}

void StyleEngine::startElement (const char *tagname, BrowserWindow *bw) {
   startElement (hll_htmlTagIndex(tagname), bw);
}

void StyleEngine::setElementId (const char *id) {
   c_doctree_node_t * dtn = doctreeGetTopNode(this->doc_tree_ptr);
   assert (dtn->c_element_selector_id == NULL);
   dtn->c_element_selector_id = strdup (id);

   hll_styleEngineSetElementId(this->doc_tree_ref, id);
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
   }
   dtn->c_element_selector_class_size = i;
   free(in);

   hll_styleEngineSetElementClass(this->doc_tree_ref, element_class_tokens);
}

// 'cssStyleAttribute' is contents of element's "style" attribute, describing CSS
// properties-values for a Node.
void StyleEngine::setCssStyleForCurrentNode(const char * cssStyleAttribute)
{
   StyleNode * currentNode = getCurrentNode(this);
   assert (currentNode->declLists.main == NULL);
   // parse style information from style="" attribute, if it exists
   if (cssStyleAttribute && prefs.parse_embedded_css) {
      currentNode->declLists.main      = declarationListNew();
      currentNode->declLists.important = declarationListNew();

      hll_cssParseElementStyleAttribute(baseUrl, cssStyleAttribute, strlen (cssStyleAttribute),
                                        currentNode->declLists.main, currentNode->declLists.important);
   }
}

/**
 * \brief Instruct StyleEngine to use the nonCssHints from parent element
 * This is only used for tables where nonCssHints on the TABLE-element
 * (e.g. border=1) also affect child elements like TD.
 */
void StyleEngine::inheritNonCssHints()
{
   StyleNode * parentNode = getParentNode(this);

   if (parentNode->declLists.nonCss) {
      StyleNode * currentNode = getCurrentNode(this);
      c_css_declaration_set_t * origDeclListNonCss = currentNode->declLists.nonCss;

      currentNode->declLists.nonCss = declarationListNew(parentNode->declLists.nonCss); // NOTICE: copy constructor

      if (origDeclListNonCss) {// original declListNonCss have precedence
         hll_declarationListAppend(currentNode->declLists.nonCss, origDeclListNonCss);
      }

      delete origDeclListNonCss;
   }
}

void StyleEngine::clearNonCssHints()
{
   StyleNode * currentNode = getCurrentNode(this);

   delete currentNode->declLists.nonCss;
   currentNode->declLists.nonCss = NULL;
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

   hll_styleEngineSetElementPseudoClass(this->doc_tree_ref, "link");
}

/**
 * \brief set the CSS pseudo class :visited.
 */
void StyleEngine::setPseudoVisited () {
   c_doctree_node_t * dtn = doctreeGetTopNode(this->doc_tree_ptr);
   dtn->c_element_selector_pseudo_class = "visited";

   hll_styleEngineSetElementPseudoClass(this->doc_tree_ref, "visited");
}

/**
 * \brief tell the styleEngine that a html element has ended.
 */
void StyleEngine::endElement (int element) {
   assert (element == doctreeGetTopNode(this->doc_tree_ptr)->c_html_element_idx);

   stackPop ();
   doctreePopNode(this->doc_tree_ptr);
   hll_doctreePopNode(this->doc_tree_ref);
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

/**
 * \brief Make changes to StyleAttrs attrs according to c_css_declaration_set_t props.
 */
void StyleEngine::apply(int some_idx, StyleAttrs *attrs, c_css_declaration_set_t * declList, BrowserWindow *bw)
{
   FontAttrs fontAttrs = *attrs->font;
   Font *parentFont = styleNodesStack[some_idx - 1].style->font;
   DilloUrl *imgUrl = NULL;
   double val_ = 0;
   int type_   = 0;

   c_style_attrs_t * style_attrs = (c_style_attrs_t *) calloc(1, sizeof (c_style_attrs_t));
   style_attrs->c_border_width = (c_border_width_t *) calloc(1, sizeof (c_border_width_t));
   style_attrs->c_border_style = (c_border_style_t *) calloc(1, sizeof (c_border_style_t));
   style_attrs->c_border_color = (c_border_color_t *) calloc(1, sizeof (c_border_color_t));
   style_attrs->c_margin = (c_style_margin_t *) calloc(1, sizeof (c_style_margin_t));
   style_attrs->c_padding = (c_style_padding_t *) calloc(1, sizeof (c_style_padding_t));
   style_attrs->c_text_indent = (DwLength *) calloc(1, sizeof (DwLength));
   style_attrs->c_width  = (DwLength *) calloc(1, sizeof (DwLength));
   style_attrs->c_height = (DwLength *) calloc(1, sizeof (DwLength));
   style_attrs->c_line_height = (DwLength *) calloc(1, sizeof (DwLength));

   *(style_attrs->c_border_width) = attrs->borderWidth;
   *(style_attrs->c_border_style) = attrs->borderStyle;
   //*(style_attrs->c_border_color) = { -1, -1, -1, -1 }; TODO: uncommenting this line breaks block-quote markings in comments on SoylentNews
   *(style_attrs->c_margin) = attrs->margin;
   *(style_attrs->c_padding) = attrs->padding;
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
   style_attrs->c_color       = -1;// TODO: this probably should be moved to style_attrs' constructor
   style_attrs->c_cursor      = attrs->cursor;
   style_attrs->c_h_border_spacing      = attrs->hBorderSpacing;
   style_attrs->c_v_border_spacing      = attrs->vBorderSpacing;
   style_attrs->c_word_spacing          = attrs->wordSpacing;

   /* Determine font first so it can be used to resolve relative lengths. */
   hll_styleEngineApplyStyleToFont(declList, &prefs.preferences, layout->dpiX(), layout->dpiY(), &parentFont->font_attrs, &fontAttrs.font_attrs);

   attrs->font = Font::create (layout, &fontAttrs);

   for (int j = 0; j < declList->c_declarations_size; j++) {
      c_css_declaration_t * decl = declList->c_declarations[j];

      switch (decl->c_property) {
         /* \todo missing cases */
         case CSS_PROPERTY_BACKGROUND_ATTACHMENT:
            attrs->backgroundAttachment =
               (BackgroundAttachment) decl->c_value->c_int_val;
            break;
         case CSS_PROPERTY_BACKGROUND_COLOR:
            if (prefs.allow_white_bg || decl->c_value->c_int_val != 0xffffff)
               attrs->backgroundColor = Color::create(layout, decl->c_value->c_int_val);
            else
               attrs->backgroundColor =
                  Color::create(layout, prefs.white_bg_replacement);
            break;
         case CSS_PROPERTY_BACKGROUND_IMAGE:
            // decl->value.c_text_val should be absolute, so baseUrl is not needed
            imgUrl = a_Url_new (decl->c_value->c_text_val, NULL);
            break;
         case CSS_PROPERTY_BACKGROUND_POSITION:
            CssLength cssLength;

            cssLength.length_bits = decl->c_value->c_bg_pos_x;
            val_  = (double) cpp_cssLengthValue(cssLength);
            type_ = cpp_cssLengthType(cssLength);
            hll_computeDwLength(&attrs->backgroundPositionX, val_, type_, &attrs->font->font_attrs, layout->dpiX(), layout->dpiY());

            cssLength.length_bits = decl->c_value->c_bg_pos_y;
            val_  = (double) cpp_cssLengthValue(cssLength);
            type_ = cpp_cssLengthType(cssLength);
            hll_computeDwLength(&attrs->backgroundPositionY, val_, type_, &attrs->font->font_attrs, layout->dpiX(), layout->dpiY());

            break;
         case CSS_PROPERTY_BACKGROUND_REPEAT:
            attrs->backgroundRepeat = (BackgroundRepeat) decl->c_value->c_int_val;
            break;
         case CSS_PROPERTY_BORDER_COLLAPSE:
            attrs->borderCollapse = (BorderCollapse) decl->c_value->c_int_val;
            break;

         case CSS_PROPERTY_BORDER_SPACING:
         case CSS_PROPERTY_COLOR:
         case CSS_PROPERTY_CURSOR:
         case CSS_PROPERTY_DISPLAY:
         case CSS_PROPERTY_LINE_HEIGHT:
         case CSS_PROPERTY_LIST_STYLE_POSITION:
         case CSS_PROPERTY_LIST_STYLE_TYPE:
         case CSS_PROPERTY_BORDER_BOTTOM_STYLE:
         case CSS_PROPERTY_BORDER_LEFT_STYLE:
         case CSS_PROPERTY_BORDER_RIGHT_STYLE:
         case CSS_PROPERTY_BORDER_TOP_STYLE:
         case CSS_PROPERTY_BORDER_BOTTOM_WIDTH:
         case CSS_PROPERTY_BORDER_LEFT_WIDTH:
         case CSS_PROPERTY_BORDER_RIGHT_WIDTH:
         case CSS_PROPERTY_BORDER_TOP_WIDTH:
         case CSS_PROPERTY_BORDER_TOP_COLOR:
         case CSS_PROPERTY_BORDER_BOTTOM_COLOR:
         case CSS_PROPERTY_BORDER_LEFT_COLOR:
         case CSS_PROPERTY_BORDER_RIGHT_COLOR:
         case CSS_PROPERTY_MARGIN_BOTTOM:
         case CSS_PROPERTY_MARGIN_LEFT:
         case CSS_PROPERTY_MARGIN_RIGHT:
         case CSS_PROPERTY_MARGIN_TOP:
         case CSS_PROPERTY_PADDING_TOP:
         case CSS_PROPERTY_PADDING_BOTTOM:
         case CSS_PROPERTY_PADDING_LEFT:
         case CSS_PROPERTY_PADDING_RIGHT:
         case CSS_PROPERTY_TEXT_ALIGN:
         case CSS_PROPERTY_TEXT_DECORATION:
         case CSS_PROPERTY_TEXT_INDENT:
         case CSS_PROPERTY_TEXT_TRANSFORM:
         case CSS_PROPERTY_VERTICAL_ALIGN:
         case CSS_PROPERTY_WHITE_SPACE:
         case CSS_PROPERTY_WIDTH:
         case CSS_PROPERTY_HEIGHT:
         case CSS_PROPERTY_WORD_SPACING:
            cssLength = cpp_cssCreateLength(decl->c_value->c_length_val, (CssLengthType) decl->c_value->c_length_type);
            val_  = (double) cpp_cssLengthValue(cssLength);
            type_ = cpp_cssLengthType(cssLength);
            hll_styleEngineSetStyle(decl->c_property, decl->c_value, val_, type_, &attrs->font->font_attrs, layout->dpiX(), layout->dpiY(), style_attrs);
            break;

         case PROPERTY_X_LINK:
            attrs->x_link = decl->c_value->c_int_val;
            break;
         case PROPERTY_X_LANG:
            attrs->x_lang[0] = D_ASCII_TOLOWER(decl->c_value->c_text_val[0]);
            if (attrs->x_lang[0])
               attrs->x_lang[1] = D_ASCII_TOLOWER(decl->c_value->c_text_val[1]);
            else
               attrs->x_lang[1] = 0;
            break;
         case PROPERTY_X_IMG:
            attrs->x_img = decl->c_value->c_int_val;
            break;
         case PROPERTY_X_TOOLTIP:
            attrs->x_tooltip = Tooltip::create(layout, decl->c_value->c_text_val);
            break;
         default:
            break;
      }
   }

   /* Handle additional things that were not handled in Haskell. */
   if (attrs->display == DISPLAY_NONE) {
      styleNodesStack[some_idx].displayNone = true;
   }

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
   attrs->cursor            = style_attrs->c_cursor;
   attrs->hBorderSpacing    = style_attrs->c_h_border_spacing;
   attrs->vBorderSpacing    = style_attrs->c_v_border_spacing;
   attrs->wordSpacing       = style_attrs->c_word_spacing;

   free(style_attrs->c_border_width);
   free(style_attrs->c_border_style);
   free(style_attrs->c_border_color);
   free(style_attrs->c_margin);
   free(style_attrs->c_padding);
   free(style_attrs->c_text_indent);
   free(style_attrs->c_width);
   free(style_attrs->c_height);
   free(style_attrs);


   if (imgUrl && prefs.load_background_images &&
       !styleNodesStack[some_idx].displayNone &&
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
Style * StyleEngine::getStyle0(int some_idx, BrowserWindow *bw) {

   // get previous style from the stack
   StyleAttrs attrs = * styleNodesStack[some_idx - 1].style;

   // Ensure that StyleEngine::style0() has not been called before for
   // this element.
   // Style computation is expensive so limit it as much as possible.
   // If this assertion is hit, you need to rearrange the code that is
   // doing styleEngine calls to call setNonCssHintOfCurrentNode() before calling
   // style() or wordStyle() for each new element.
   assert (styleNodesStack[some_idx].style == NULL);

   // reset values that are not inherited according to CSS
   attrs.resetValues ();
   preprocessAttrs (&attrs);

   c_css_declaration_lists_t * declLists = &styleNodesStack[some_idx].declLists;

   // merge style information
   c_css_declaration_set_t * mergedDeclList = declarationListNew();
   int dtnNum = styleNodesStack[some_idx].doctreeNodeIdx;
   hll_cssContextApplyCssContext(this->css_context_ref,
                                 mergedDeclList,
                                 this->doc_tree_ref, dtnNum,
                                 declLists->main, declLists->important, declLists->nonCss);

   // apply style
   apply(some_idx, &attrs, mergedDeclList, bw);

   postprocessAttrs(&attrs);

   styleNodesStack[some_idx].style = Style::create(&attrs);

   return styleNodesStack[some_idx].style;
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
   char path[30] = { 0 };
   snprintf(path, sizeof (path), "/tmp/hello_context_%03d", i);
   FILE * file = fopen(path, "w");
   i++;

   importDepth++;
   {
      CssParser parser_(origin, url, buf, buflen);
      hll_parseCss(&parser_.m_parser, &parser_.m_token, this->css_context_ref);
   }
   importDepth--;

   hll_cssContextPrint(path, this->css_context_ref);
   fclose(file);
}

void StyleEngine::buildUserStyle(int context_ref)
{
   Dstr *style;
   char *filename = dStrconcat(dGethomedir(), "/.dillo/style.css", NULL);

   if ((style = a_Misc_file2dstr(filename))) {
      {
         CssParser parser_(CSS_ORIGIN_USER, NULL, style->str, style->len);
         hll_parseCss(&parser_.m_parser, &parser_.m_token, context_ref);
      }
      dStr_free (style, 1);
   }
   dFree (filename);
}

void cpp_styleEngineSetNonCssHintOfNodeLength(StyleNode * styleNode, CssDeclarationProperty property, CssDeclarationValueType type, CssLength cssLength)
{
   float lengthValue = cpp_cssLengthValue(cssLength);
   int lengthType  = (int) cpp_cssLengthType(cssLength);
   cpp_styleEngineSetNonCssHintOfNodeInt(styleNode, property, type, 0, lengthValue, lengthType);
   return;
}

void cpp_styleEngineSetNonCssHintOfNodeInt(StyleNode * styleNode, int property, int valueType, int intVal, float lengthValue, int lengthType)
{
   c_css_declaration_set_t * inDeclSet = styleNode->declLists.nonCss;
   c_css_declaration_set_t * outDeclSet = hll_styleEngineSetNonCssHintOfNodeInt(inDeclSet, property, valueType, intVal, lengthValue, lengthType);
   styleNode->declLists.nonCss = outDeclSet;
   return;
}

void cpp_styleEngineSetNonCssHintOfNodeString(StyleNode * styleNode, int property, int valueType, const char * stringVal)
{
   c_css_declaration_set_t * inDeclSet = styleNode->declLists.nonCss;
   c_css_declaration_set_t * outDeclSet = hll_styleEngineSetNonCssHintOfNodeString(inDeclSet, property, valueType, stringVal);
   styleNode->declLists.nonCss = outDeclSet;
   return;
}


