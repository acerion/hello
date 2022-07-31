/*
 * File: table.cc
 *
 * Copyright 2008 Jorge Arellano Cid <jcid@dillo.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 */

#include "table.hh"
#include "html_common.hh"

#include "dw/style.hh"
#include "dw/textblock.hh"
#include "dw/table.hh"

#include "prefs.h"
#include "msg.h"
#include "css.hh"

using namespace dw;
using namespace dw::core;
using namespace dw::core::style;

/*
 * Forward declarations
 */

static void Html_tag_open_table_cell(DilloHtml *html,
                                     const char *tag, int tagsize,
                                     dw::core::style::TextAlignType text_align);
static void Html_tag_content_table_cell(DilloHtml *html,
                                        const char *tag, int tagsize);

/*
 * <TABLE>
 */
void Html_tag_open_table(DilloHtml *html, const char *tag, int tagsize)
{
   const char *attr_value;
   int32_t border = -1, cellspacing = -1, cellpadding = -1, bgcolor = -1;
   CssLength cssLength;

   if ((attr_value = html_attribute_get_value(tag, tagsize, "border")))
      border = isdigit(attr_value[0]) ? strtol (attr_value, NULL, 10) : 1;
   if ((attr_value = html_attribute_get_value(tag, tagsize, "cellspacing"))) {
      cellspacing = strtol (attr_value, NULL, 10);
      if (html->doctype.c_doc_type == DT_HTML && html->doctype.c_doc_type_version >= 5.0f)
         BUG_MSG("<table> cellspacing attribute is obsolete.");
   }

   if ((attr_value = html_attribute_get_value(tag, tagsize, "cellpadding"))) {
      cellpadding = strtol (attr_value, NULL, 10);
      if (html->doctype.c_doc_type == DT_HTML && html->doctype.c_doc_type_version >= 5.0f)
         BUG_MSG("<table> cellpadding attribute is obsolete.");
   }

   if (border != -1) {
      cssLength = cpp_cssCreateLength(border, CSS_LENGTH_TYPE_PX);
      StyleNode * currentNode = getCurrentNode(html->styleEngine);
      cpp_styleEngineSetNonCssHintOfNodeLength2(currentNode, CSS_PROPERTY_BORDER_TOP_WIDTH,    CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
      cpp_styleEngineSetNonCssHintOfNodeLength2(currentNode, CSS_PROPERTY_BORDER_BOTTOM_WIDTH, CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
      cpp_styleEngineSetNonCssHintOfNodeLength2(currentNode, CSS_PROPERTY_BORDER_LEFT_WIDTH,   CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
      cpp_styleEngineSetNonCssHintOfNodeLength2(currentNode, CSS_PROPERTY_BORDER_RIGHT_WIDTH,  CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
      cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_BORDER_TOP_STYLE,    BORDER_OUTSET);
      cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_BORDER_BOTTOM_STYLE, BORDER_OUTSET);
      cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_BORDER_LEFT_STYLE,   BORDER_OUTSET);
      cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_BORDER_RIGHT_STYLE,  BORDER_OUTSET);
   }

   if (cellspacing != -1) {
      StyleNode * currentNode = getCurrentNode(html->styleEngine);
      cssLength = cpp_cssCreateLength(cellspacing, CSS_LENGTH_TYPE_PX);
      cpp_styleEngineSetNonCssHintOfNodeLength(currentNode, CSS_PROPERTY_BORDER_SPACING, CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
   }

   if ((attr_value = html_attribute_get_value(tag, tagsize, "width"))) {
      CssLength width = html_parse_attribute_width_or_height(attr_value);
      StyleNode * currentNode = getCurrentNode(html->styleEngine);
      cpp_styleEngineSetNonCssHintOfNodeLength(currentNode, CSS_PROPERTY_WIDTH, CssDeclarationValueTypeLENGTH_PERCENTAGE, width);
      if (html->doctype.c_doc_type == DT_HTML && html->doctype.c_doc_type_version >= 5.0f)
         BUG_MSG("<table> width attribute is obsolete.");
   }

   StyleNode * currentNode = getCurrentNode(html->styleEngine);
   if ((attr_value = html_attribute_get_value(tag, tagsize, "align"))) {
      if (dStrAsciiCasecmp (attr_value, "left") == 0) {
         cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_TEXT_ALIGN, TEXT_ALIGN_LEFT);
      } else if (dStrAsciiCasecmp (attr_value, "right") == 0) {
         cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_TEXT_ALIGN, TEXT_ALIGN_RIGHT);
      } else if (dStrAsciiCasecmp (attr_value, "center") == 0) {
         cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_TEXT_ALIGN, TEXT_ALIGN_CENTER);
      }
      if (html->doctype.c_doc_type == DT_HTML && html->doctype.c_doc_type_version >= 5.0f) {
         BUG_MSG("<table> align attribute is obsolete.");
      }
   }

   if ((attr_value = html_attribute_get_value(tag, tagsize, "bgcolor"))) {
      bgcolor = a_Html_color_parse(html, attr_value, -1);
      if (bgcolor != -1) {
         StyleNode * currentNode = getCurrentNode(html->styleEngine);
         cpp_styleEngineSetNonCssHintOfNodeColor(currentNode, CSS_PROPERTY_BACKGROUND_COLOR, bgcolor);
      }
      if (html->doctype.c_doc_type == DT_HTML && html->doctype.c_doc_type_version >= 5.0f)
         BUG_MSG("<table> bgcolor attribute is obsolete.");
   }

   html->styleEngine->getStyle (html->bw); // evaluate now, so we can build non-css hints for the cells

   /* The style for the cells */
   html->styleEngine->clearNonCssHints ();
   if (border > 0) {
      cssLength = cpp_cssCreateLength(1, CSS_LENGTH_TYPE_PX);
      StyleNode * currentNode = getCurrentNode(html->styleEngine);
      cpp_styleEngineSetNonCssHintOfNodeLength2(currentNode, CSS_PROPERTY_BORDER_TOP_WIDTH,    CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
      cpp_styleEngineSetNonCssHintOfNodeLength2(currentNode, CSS_PROPERTY_BORDER_BOTTOM_WIDTH, CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
      cpp_styleEngineSetNonCssHintOfNodeLength2(currentNode, CSS_PROPERTY_BORDER_LEFT_WIDTH,   CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
      cpp_styleEngineSetNonCssHintOfNodeLength2(currentNode, CSS_PROPERTY_BORDER_RIGHT_WIDTH,  CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
      cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_BORDER_TOP_STYLE,    BORDER_INSET);
      cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_BORDER_BOTTOM_STYLE, BORDER_INSET);
      cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_BORDER_LEFT_STYLE,   BORDER_INSET);
      cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_BORDER_RIGHT_STYLE,  BORDER_INSET);
   }

   if (cellpadding != -1) {
      StyleNode * currentNode = getCurrentNode(html->styleEngine);
      cssLength = cpp_cssCreateLength(cellpadding, CSS_LENGTH_TYPE_PX);
      cpp_styleEngineSetNonCssHintOfNodeLength2(currentNode, CSS_PROPERTY_PADDING_TOP,    CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
      cpp_styleEngineSetNonCssHintOfNodeLength2(currentNode, CSS_PROPERTY_PADDING_BOTTOM, CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
      cpp_styleEngineSetNonCssHintOfNodeLength2(currentNode, CSS_PROPERTY_PADDING_LEFT,   CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
      cpp_styleEngineSetNonCssHintOfNodeLength2(currentNode, CSS_PROPERTY_PADDING_RIGHT,  CssDeclarationValueTypeLENGTH_PERCENTAGE, cssLength);
   }

}
void Html_tag_content_table(DilloHtml *html, const char *tag, int tagsize)
{
   dw::core::Widget *table;

   Html2TextBlock(html)->addParbreak (0, html->styleEngine->getWordStyle (html->bw));
   table = new dw::Table(prefs.limit_text_width);
   Html2TextBlock(html)->addWidget (table, html->styleEngine->getStyle (html->bw));
   Html2TextBlock(html)->addParbreak (0, html->styleEngine->getWordStyle (html->bw));

   TopOfParsingStack(html)->table_mode = DILLO_HTML_TABLE_MODE_TOP;
   TopOfParsingStack(html)->table_border_mode = DILLO_HTML_TABLE_BORDER_SEPARATE;
   TopOfParsingStack(html)->cell_text_align_set = FALSE;
   TopOfParsingStack(html)->table = table;

}

/*
 * <TR>
 */
void Html_tag_open_tr(DilloHtml *html, const char *tag, int tagsize)
{
   const char *attr_value;
   int32_t bgcolor = -1;

   html->styleEngine->inheritNonCssHints ();

   switch (TopOfParsingStack(html)->table_mode) {
   case DILLO_HTML_TABLE_MODE_NONE:
      _MSG("Invalid HTML syntax: <tr> outside <table>\n");
      return;

   case DILLO_HTML_TABLE_MODE_TOP:
   case DILLO_HTML_TABLE_MODE_TR:
   case DILLO_HTML_TABLE_MODE_TD:

      if ((attr_value = html_attribute_get_value(tag, tagsize, "bgcolor"))) {
         bgcolor = a_Html_color_parse(html, attr_value, -1);
         if (bgcolor != -1) {
            StyleNode * currentNode = getCurrentNode(html->styleEngine);
            cpp_styleEngineSetNonCssHintOfNodeColor(currentNode, CSS_PROPERTY_BACKGROUND_COLOR, bgcolor);
         }
         if (html->doctype.c_doc_type == DT_HTML && html->doctype.c_doc_type_version >= 5.0f) {
            BUG_MSG("<tr> bgcolor attribute is obsolete.");
         }
      }

      if (html_attribute_get_value(tag, tagsize, "align")) {
         StyleNode * currentNode = getCurrentNode(html->styleEngine);
         TopOfParsingStack(html)->cell_text_align_set = TRUE;
         a_Html_tag_set_align_attr(&html->doctype, currentNode, tag, tagsize);
      }

      html->styleEngine->inheritBackgroundColor ();

      if (bgcolor != -1) {
         StyleNode * currentNode = getCurrentNode(html->styleEngine);
         cpp_styleEngineSetNonCssHintOfNodeColor(currentNode, CSS_PROPERTY_BACKGROUND_COLOR, bgcolor);
      }
      a_Html_tag_set_valign_attr (html, tag, tagsize);
      break;
   default:
      break;
   }
}

void Html_tag_content_tr(DilloHtml *html, const char *tag, int tagsize)
{
   switch (TopOfParsingStack(html)->table_mode) {
   case DILLO_HTML_TABLE_MODE_NONE:
      return;
   case DILLO_HTML_TABLE_MODE_TOP:
   case DILLO_HTML_TABLE_MODE_TR:
   case DILLO_HTML_TABLE_MODE_TD:
      ((dw::Table*)TopOfParsingStack(html)->table)->addRow (html->styleEngine->getStyle (html->bw));
   default:
      break;
   }

   TopOfParsingStack(html)->table_mode = DILLO_HTML_TABLE_MODE_TR;
}

/*
 * <TD>
 */
void Html_tag_open_td(DilloHtml *html, const char *tag, int tagsize)
{
   Html_tag_open_table_cell (html, tag, tagsize,
                             dw::core::style::TEXT_ALIGN_LEFT);
}

void Html_tag_content_td(DilloHtml *html, const char *tag, int tagsize)
{
   Html_tag_content_table_cell (html, tag, tagsize);
}

/*
 * <TH>
 */
void Html_tag_open_th(DilloHtml *html, const char *tag, int tagsize)
{
   Html_tag_open_table_cell (html, tag, tagsize,
                             dw::core::style::TEXT_ALIGN_CENTER);
}

void Html_tag_content_th(DilloHtml *html, const char *tag, int tagsize)
{
   Html_tag_content_table_cell (html, tag, tagsize);
}

/*
 * Utilities
 */

/*
 * The table border model is stored in the table's stack item
 */
static int Html_table_get_border_model(DilloHtml *html)
{
   static int i_TABLE = -1;
   if (i_TABLE == -1)
      i_TABLE = hll_htmlTagIndex("table");

   int s_idx = html->stack->size();
   while (--s_idx > 0 && html->stack->getRef(s_idx)->tag_idx != i_TABLE)
      ;
   return html->stack->getRef(s_idx)->table_border_mode;
}

/*
 * Set current table's border model
 */
static void Html_table_set_border_model(DilloHtml *html,
                                        DilloHtmlTableBorderMode mode)
{
   int s_idx = html->stack->size();
   const int i_TABLE = hll_htmlTagIndex("table");

   while (--s_idx > 0 && html->stack->getRef(s_idx)->tag_idx != i_TABLE) ;
   if (s_idx > 0)
      html->stack->getRef(s_idx)->table_border_mode = mode;
}

/* WORKAROUND: collapsing border model requires moving rendering code from
 *             the cell to the table, and making table-code aware of each
 *             cell style.
 * This workaround mimics collapsing model within separate model. This is not
 * a complete emulation but should be enough for most cases.
 */
static void Html_set_collapsing_border_model(DilloHtml *html, Widget *col_tb)
{
   dw::core::style::Style *collapseStyle, *tableStyle;
   dw::core::style::StyleAttrs collapseCellAttrs, collapseTableAttrs;
   int borderWidth, marginWidth;

   tableStyle = ((dw::Table*)TopOfParsingStack(html)->table)->getStyle ();
   borderWidth = html->styleEngine->getStyle (html->bw)->borderWidth.top;
   marginWidth = tableStyle->margin.top;

   collapseCellAttrs = *(html->styleEngine->getStyle (html->bw));
   styleMarginSetVal(&collapseCellAttrs.margin, 0);
   collapseCellAttrs.borderWidth.left = 0;
   collapseCellAttrs.borderWidth.top = 0;
   collapseCellAttrs.borderWidth.right = borderWidth;
   collapseCellAttrs.borderWidth.bottom = borderWidth;
   collapseCellAttrs.hBorderSpacing = 0;
   collapseCellAttrs.vBorderSpacing = 0;
   collapseStyle = Style::create(&collapseCellAttrs);
   col_tb->setStyle (collapseStyle);

   if (Html_table_get_border_model(html) != DILLO_HTML_TABLE_BORDER_COLLAPSE) {
      Html_table_set_border_model(html, DILLO_HTML_TABLE_BORDER_COLLAPSE);
      collapseTableAttrs = *tableStyle;
      styleMarginSetVal(&collapseTableAttrs.margin, marginWidth);
      collapseTableAttrs.borderWidth.left = borderWidth;
      collapseTableAttrs.borderWidth.top = borderWidth;
      collapseTableAttrs.borderWidth.right = 0;
      collapseTableAttrs.borderWidth.bottom = 0;
      collapseTableAttrs.hBorderSpacing = 0;
      collapseTableAttrs.vBorderSpacing = 0;
      collapseTableAttrs.borderColor = collapseCellAttrs.borderColor;
      collapseTableAttrs.borderStyle = collapseCellAttrs.borderStyle;
      /* CSS2 17.6.2: table does not have padding (in collapsing mode) */
      stylePaddingSetVal(&collapseTableAttrs.padding, 0);
      collapseStyle = Style::create(&collapseTableAttrs);
      ((dw::Table*)TopOfParsingStack(html)->table)->setStyle (collapseStyle);
   }
}

/*
 * Adjust style for separate border model.
 * (Dw uses this model internally).
 */
static void Html_set_separate_border_model(DilloHtml *html, Widget *col_tb)
{
   dw::core::style::Style *separateStyle;
   dw::core::style::StyleAttrs separateCellAttrs;

   separateCellAttrs = *(html->styleEngine->getStyle (html->bw));
   /* CSS2 17.5: Internal table elements do not have margins */
   styleMarginSetVal(&separateCellAttrs.margin, 0);
   separateStyle = Style::create(&separateCellAttrs);
   col_tb->setStyle (separateStyle);
}

/*
 * used by <TD> and <TH>
 */
static void Html_tag_open_table_cell(DilloHtml *html,
                                     const char *tag, int tagsize,
                                     dw::core::style::TextAlignType text_align)
{
   const char *attr_value;
   int32_t bgcolor;

   html->styleEngine->inheritNonCssHints ();

   switch (TopOfParsingStack(html)->table_mode) {
   case DILLO_HTML_TABLE_MODE_NONE:
      return;

   case DILLO_HTML_TABLE_MODE_TOP:
      /* a_Dw_table_add_cell takes care that dillo does not crash. */
      /* continues */
   case DILLO_HTML_TABLE_MODE_TR:
   case DILLO_HTML_TABLE_MODE_TD:
      /* text style */
      if (!TopOfParsingStack(html)->cell_text_align_set) {
         StyleNode * currentNode = getCurrentNode(html->styleEngine);
         cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_TEXT_ALIGN, text_align);
      }
      if (html_attribute_get_value(tag, tagsize, "nowrap")) {
         if (html->doctype.c_doc_type == DT_HTML && html->doctype.c_doc_type_version >= 5.0f) {
            BUG_MSG("<t%c> nowrap attribute is obsolete.", (tagsize >=3 && (D_ASCII_TOLOWER(tag[2]) == 'd')) ? 'd' : 'h');
         }
         StyleNode * currentNode = getCurrentNode(html->styleEngine);
         cpp_styleEngineSetNonCssHintOfNodeEnum(currentNode, CSS_PROPERTY_WHITE_SPACE, WHITE_SPACE_NOWRAP);
      }

      {
         StyleNode * currentNode = getCurrentNode(html->styleEngine);
         a_Html_tag_set_align_attr(&html->doctype, currentNode, tag, tagsize);
      }

      if ((attr_value = html_attribute_get_value(tag, tagsize, "width"))) {
         CssLength width = html_parse_attribute_width_or_height(attr_value);
         StyleNode * currentNode = getCurrentNode(html->styleEngine);
         cpp_styleEngineSetNonCssHintOfNodeLength(currentNode, CSS_PROPERTY_WIDTH, CssDeclarationValueTypeLENGTH_PERCENTAGE, width);
         if (html->doctype.c_doc_type == DT_HTML && html->doctype.c_doc_type_version >= 5.0f)
            BUG_MSG("<t%c> width attribute is obsolete.",
               (tagsize >=3 && (D_ASCII_TOLOWER(tag[2]) == 'd')) ? 'd' : 'h');
      }

      a_Html_tag_set_valign_attr (html, tag, tagsize);

      if ((attr_value = html_attribute_get_value(tag, tagsize, "bgcolor"))) {
         bgcolor = a_Html_color_parse(html, attr_value, -1);
         if (bgcolor != -1) {
            StyleNode * currentNode = getCurrentNode(html->styleEngine);
            cpp_styleEngineSetNonCssHintOfNodeColor(currentNode, CSS_PROPERTY_BACKGROUND_COLOR, bgcolor);
         }
         if (html->doctype.c_doc_type == DT_HTML && html->doctype.c_doc_type_version >= 5.0f) {
            BUG_MSG("<t%c> bgcolor attribute is obsolete.", (tagsize >=3 && (D_ASCII_TOLOWER(tag[2]) == 'd')) ? 'd' : 'h');
         }
      }

   default:
      /* compiler happiness */
      break;
   }
}

static void Html_tag_content_table_cell(DilloHtml *html,
                                     const char *tag, int tagsize)
{
   int colspan = 1, rowspan = 1;
   const char *attr_value;
   Widget *col_tb;

   switch (TopOfParsingStack(html)->table_mode) {
   case DILLO_HTML_TABLE_MODE_NONE:
      BUG_MSG("<t%c> outside <table>.",
              (tagsize >=3 && (D_ASCII_TOLOWER(tag[2]) == 'd')) ? 'd' : 'h');
      return;

   case DILLO_HTML_TABLE_MODE_TOP:
      BUG_MSG("<t%c> outside <tr>.",
              (tagsize >=3 && (D_ASCII_TOLOWER(tag[2]) == 'd')) ? 'd' : 'h');
      /* a_Dw_table_add_cell takes care that dillo does not crash. */
      /* continues */
   case DILLO_HTML_TABLE_MODE_TR:
   case DILLO_HTML_TABLE_MODE_TD:
      if ((attr_value = html_attribute_get_value(tag, tagsize, "colspan"))) {
         char *invalid;
         colspan = strtol(attr_value, &invalid, 10);
         if ((colspan < 0) || (attr_value == invalid))
            colspan = 1;
      }
      /* TODO: check errors? */
      if ((attr_value = html_attribute_get_value(tag, tagsize, "rowspan")))
         rowspan = MAX(1, strtol (attr_value, NULL, 10));
      if (html->styleEngine->getStyle (html->bw)->textAlign
          == TEXT_ALIGN_STRING)
         col_tb = new dw::TableCell (
                     ((dw::Table*)TopOfParsingStack(html)->table)->getCellRef (),
                     prefs.limit_text_width);
      else
         col_tb = new Textblock (prefs.limit_text_width);

      if (html->styleEngine->getStyle(html->bw)->borderCollapse == BORDER_MODEL_COLLAPSE){
         Html_set_collapsing_border_model(html, col_tb);
      } else {
         Html_set_separate_border_model(html, col_tb);
      }

      ((dw::Table*)TopOfParsingStack(html)->table)->addCell (col_tb, colspan, rowspan);
      TopOfParsingStack(html)->textblock = html->dw = col_tb;
      break;

   default:
      /* compiler happiness */
      break;
   }

   TopOfParsingStack(html)->table_mode = DILLO_HTML_TABLE_MODE_TD;
}
