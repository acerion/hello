/*
 * File: cssparser.cc
 *
 * Copyright 2004 Sebastian Geerken <sgeerken@dillo.org>
 * Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 */

/*
 * This file is heavily based on the CSS parser of dillo-0.8.0-css-3 -
 * a dillo1 based CSS prototype written by Sebastian Geerken.
 */

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>

#include "lout/debug.hh"
#include "msg.h"
#include "html_common.hh"
#include "css.hh"
#include "cssparser.hh"
#include "haskell/hello.h"

using namespace dw::core::style;

#define MSG_CSS(A, ...) MSG(A, __VA_ARGS__)
#define DEBUG_TOKEN_LEVEL   0
#define DEBUG_PARSE_LEVEL   0
#define DEBUG_CREATE_LEVEL  0

#define DEBUG_LEVEL 10

/* The last three ones are never parsed. */
#define CSS_NUM_INTERNAL_PROPERTIES 3
#define CSS_NUM_PARSED_PROPERTIES \
   (CSS_PROPERTY_LAST - CSS_NUM_INTERNAL_PROPERTIES)


typedef struct {
   const char *symbol;
   const CssPropertyValueDataType type[3];
   const char *const *enum_symbols;
} CssPropertyInfo;

static const char *const Css_background_attachment_enum_vals[] = {
   "scroll", "fixed", NULL
};

static const char *const Css_background_repeat_enum_vals[] = {
   "repeat", "repeat-x", "repeat-y", "no-repeat", NULL
};

static const char *const Css_border_collapse_enum_vals[] = {
   "separate", "collapse", NULL
};

static const char *const Css_border_color_enum_vals[] = {
   "transparent", NULL
};

static const char *const Css_border_style_enum_vals[] = {
   "none", "hidden", "dotted", "dashed", "solid", "double", "groove",
   "ridge", "inset", "outset", NULL
};

static const char *const Css_border_width_enum_vals[] = {
   "thin", "medium", "thick", NULL
};

static const char *const Css_cursor_enum_vals[] = {
   "crosshair", "default", "pointer", "move", "e-resize", "ne-resize",
   "nw-resize", "n-resize", "se-resize", "sw-resize", "s-resize",
   "w-resize", "text", "wait", "help", NULL
};

static const char *const Css_display_enum_vals[] = {
   "block", "inline", "inline-block", "list-item", "none", "table",
   "table-row-group", "table-header-group", "table-footer-group", "table-row",
   "table-cell", NULL
};

static const char *const Css_font_size_enum_vals[] = {
   "large", "larger", "medium", "small", "smaller", "xx-large", "xx-small",
   "x-large", "x-small", NULL
};

static const char *const Css_font_style_enum_vals[] = {
   "normal", "italic", "oblique", NULL
};

static const char *const Css_font_variant_enum_vals[] = {
   "normal", "small-caps", NULL
};

static const char *const Css_font_weight_enum_vals[] = {
   "bold", "bolder", "light", "lighter", "normal", NULL
};

static const char *const Css_letter_spacing_enum_vals[] = {
   "normal", NULL
};

static const char *const Css_list_style_position_enum_vals[] = {
   "inside", "outside", NULL
};

static const char *const Css_line_height_enum_vals[] = {
   "normal", NULL
};

static const char *const Css_list_style_type_enum_vals[] = {
   "disc", "circle", "square", "decimal", "decimal-leading-zero",
   "lower-roman", "upper-roman", "lower-greek", "lower-alpha",
   "lower-latin", "upper-alpha", "upper-latin", "hebrew", "armenian",
   "georgian", "cjk-ideographic", "hiragana", "katakana", "hiragana-iroha",
   "katakana-iroha", "none", NULL
};

static const char *const Css_text_align_enum_vals[] = {
   "left", "right", "center", "justify", "string", NULL
};

static const char *const Css_text_decoration_enum_vals[] = {
   "underline", "overline", "line-through", "blink", NULL
};

static const char *const Css_text_transform_enum_vals[] = {
   "none", "capitalize", "uppercase", "lowercase", NULL
};

static const char *const Css_vertical_align_vals[] = {
   "top", "bottom", "middle", "baseline", "sub", "super", "text-top",
   "text-bottom", NULL
};

static const char *const Css_white_space_vals[] = {
   "normal", "pre", "nowrap", "pre-wrap", "pre-line", NULL
};

static const char *const Css_word_spacing_enum_vals[] = {
   "normal", NULL
};

const CssPropertyInfo Css_property_info[CSS_PROPERTY_LAST] = {
   {"background-attachment",  {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED},   Css_background_attachment_enum_vals},
   {"background-color",       {CssPropertyValueDataType::COLOR, CssPropertyValueDataType::UNUSED}, NULL},
   {"background-image",       {CssPropertyValueDataType::URI, CssPropertyValueDataType::UNUSED}, NULL},
   {"background-position",    {CssPropertyValueDataType::BACKGROUND_POSITION, CssPropertyValueDataType::UNUSED},     NULL},
   {"background-repeat",      {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED},     Css_background_repeat_enum_vals},
   {"border-bottom-color",    {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::COLOR, CssPropertyValueDataType::UNUSED},     Css_border_color_enum_vals},
   {"border-bottom-style",    {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED},     Css_border_style_enum_vals},
   {"border-bottom-width",    {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::LENGTH, CssPropertyValueDataType::UNUSED},     Css_border_width_enum_vals},
   {"border-collapse",        {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED},     Css_border_collapse_enum_vals},
   {"border-left-color",      {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::COLOR, CssPropertyValueDataType::UNUSED},     Css_border_color_enum_vals},
   {"border-left-style",      {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED},     Css_border_style_enum_vals},
   {"border-left-width",      {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::LENGTH, CssPropertyValueDataType::UNUSED},     Css_border_width_enum_vals},
   {"border-right-color",     {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::COLOR, CssPropertyValueDataType::UNUSED},     Css_border_color_enum_vals},
   {"border-right-style",     {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED},    Css_border_style_enum_vals},
   {"border-rigth-width",     {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::LENGTH, CssPropertyValueDataType::UNUSED},    Css_border_width_enum_vals},
   {"border-spacing",         {CssPropertyValueDataType::LENGTH, CssPropertyValueDataType::UNUSED}, NULL},
   {"border-top-color",       {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::COLOR, CssPropertyValueDataType::UNUSED},    Css_border_color_enum_vals},
   {"border-top-style",       {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED},    Css_border_style_enum_vals},
   {"border-top-width",       {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::LENGTH, CssPropertyValueDataType::UNUSED},    Css_border_width_enum_vals},
   {"bottom",                 {CssPropertyValueDataType::UNUSED}, NULL},
   {"caption-side",           {CssPropertyValueDataType::UNUSED}, NULL},
   {"clear",                  {CssPropertyValueDataType::UNUSED}, NULL},
   {"clip",                   {CssPropertyValueDataType::UNUSED}, NULL},
   {"color",                  {CssPropertyValueDataType::COLOR, CssPropertyValueDataType::UNUSED}, NULL},
   {"content",                {CssPropertyValueDataType::STRING, CssPropertyValueDataType::UNUSED}, NULL},
   {"counter-increment",      {CssPropertyValueDataType::UNUSED}, NULL},
   {"counter-reset",          {CssPropertyValueDataType::UNUSED}, NULL},
   {"cursor",                 {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED}, Css_cursor_enum_vals},
   {"direction",              {CssPropertyValueDataType::UNUSED}, NULL},
   {"display",                {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED}, Css_display_enum_vals},
   {"empty-cells",            {CssPropertyValueDataType::UNUSED}, NULL},
   {"float",                  {CssPropertyValueDataType::UNUSED}, NULL},
   {"font-family",            {CssPropertyValueDataType::SYMBOL, CssPropertyValueDataType::UNUSED}, NULL},
   {"font-size",              {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::LENGTH_PERCENTAGE, CssPropertyValueDataType::UNUSED},    Css_font_size_enum_vals},
   {"font-size-adjust",       {CssPropertyValueDataType::UNUSED}, NULL},
   {"font-stretch",           {CssPropertyValueDataType::UNUSED}, NULL},
   {"font-style",             {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED}, Css_font_style_enum_vals},
   {"font-variant",           {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED},    Css_font_variant_enum_vals},
   {"font-weight",            {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::FONT_WEIGHT, CssPropertyValueDataType::UNUSED},    Css_font_weight_enum_vals},
   {"height",                 {CssPropertyValueDataType::LENGTH_PERCENTAGE, CssPropertyValueDataType::AUTO, CssPropertyValueDataType::UNUSED}, NULL},
   {"left",                   {CssPropertyValueDataType::UNUSED}, NULL},
   {"letter-spacing",         {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::SIGNED_LENGTH, CssPropertyValueDataType::UNUSED},    Css_letter_spacing_enum_vals},
   {"line-height",            {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::LENGTH_PERCENTAGE_NUMBER, CssPropertyValueDataType::UNUSED},    Css_line_height_enum_vals},
   {"list-style-image",       {CssPropertyValueDataType::UNUSED}, NULL},
   {"list-style-position",    {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED},    Css_list_style_position_enum_vals},
   {"list-style-type",        {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED},    Css_list_style_type_enum_vals},
   {"margin-bottom",          {CssPropertyValueDataType::SIGNED_LENGTH, CssPropertyValueDataType::AUTO, CssPropertyValueDataType::UNUSED}, NULL},
   {"margin-left",            {CssPropertyValueDataType::SIGNED_LENGTH, CssPropertyValueDataType::AUTO, CssPropertyValueDataType::UNUSED}, NULL},
   {"margin-right",           {CssPropertyValueDataType::SIGNED_LENGTH, CssPropertyValueDataType::AUTO, CssPropertyValueDataType::UNUSED}, NULL},
   {"margin-top",             {CssPropertyValueDataType::SIGNED_LENGTH, CssPropertyValueDataType::AUTO, CssPropertyValueDataType::UNUSED}, NULL},
   {"marker-offset",          {CssPropertyValueDataType::UNUSED}, NULL},
   {"marks",                  {CssPropertyValueDataType::UNUSED}, NULL},
   {"max-height",             {CssPropertyValueDataType::UNUSED}, NULL},
   {"max-width",              {CssPropertyValueDataType::UNUSED}, NULL},
   {"min-height",             {CssPropertyValueDataType::UNUSED}, NULL},
   {"min-width",              {CssPropertyValueDataType::UNUSED}, NULL},
   {"outline-color",          {CssPropertyValueDataType::UNUSED}, NULL},
   {"outline-style",          {CssPropertyValueDataType::UNUSED}, NULL},
   {"outline-width",          {CssPropertyValueDataType::UNUSED}, NULL},
   {"overflow",               {CssPropertyValueDataType::UNUSED}, NULL},
   {"padding-bottom",         {CssPropertyValueDataType::LENGTH, CssPropertyValueDataType::UNUSED}, NULL},
   {"padding-left",           {CssPropertyValueDataType::LENGTH, CssPropertyValueDataType::UNUSED}, NULL},
   {"padding-right",          {CssPropertyValueDataType::LENGTH, CssPropertyValueDataType::UNUSED}, NULL},
   {"padding-top",            {CssPropertyValueDataType::LENGTH, CssPropertyValueDataType::UNUSED}, NULL},
   {"position",               {CssPropertyValueDataType::UNUSED}, NULL},
   {"quotes",                 {CssPropertyValueDataType::UNUSED}, NULL},
   {"right",                  {CssPropertyValueDataType::UNUSED}, NULL},
   {"text-align",             {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED}, Css_text_align_enum_vals},
   {"text-decoration",        {CssPropertyValueDataType::MULTI_ENUM, CssPropertyValueDataType::UNUSED},    Css_text_decoration_enum_vals},
   {"text-indent",            {CssPropertyValueDataType::LENGTH_PERCENTAGE, CssPropertyValueDataType::UNUSED}, NULL},
   {"text-shadow",            {CssPropertyValueDataType::UNUSED}, NULL},
   {"text-transform",         {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED},    Css_text_transform_enum_vals},
   {"top",                    {CssPropertyValueDataType::UNUSED}, NULL},
   {"unicode-bidi",           {CssPropertyValueDataType::UNUSED}, NULL},
   {"vertical-align",         {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED},Css_vertical_align_vals},
   {"visibility",             {CssPropertyValueDataType::UNUSED}, NULL},
   {"white-space",            {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::UNUSED}, Css_white_space_vals},
   {"width",                  {CssPropertyValueDataType::LENGTH_PERCENTAGE, CssPropertyValueDataType::AUTO, CssPropertyValueDataType::UNUSED}, NULL},
   {"word-spacing",           {CssPropertyValueDataType::ENUM, CssPropertyValueDataType::SIGNED_LENGTH, CssPropertyValueDataType::UNUSED},    Css_word_spacing_enum_vals},
   {"z-index",                {CssPropertyValueDataType::UNUSED}, NULL},

   /* These are extensions, for internal used, and never parsed. */
   {"x-link",                 {CssPropertyValueDataType::INTEGER, CssPropertyValueDataType::UNUSED}, NULL},
   {"x-colspan",              {CssPropertyValueDataType::INTEGER, CssPropertyValueDataType::UNUSED}, NULL},
   {"x-rowspan",              {CssPropertyValueDataType::INTEGER, CssPropertyValueDataType::UNUSED}, NULL},
   {"last",                   {CssPropertyValueDataType::UNUSED}, NULL},
};

typedef struct {
   const char *symbol;
   enum {
      CSS_SHORTHAND_MULTIPLE,   /* [ p1 || p2 || ...], the property pi is
                                 * determined  by the type */
      CSS_SHORTHAND_DIRECTIONS, /* <t>{1,4} */
      CSS_SHORTHAND_BORDER,     /* special, used for 'border' */
      CSS_SHORTHAND_FONT,       /* special, used for 'font' */
   } type;
   const CssPropertyName *properties; /* CSS_SHORTHAND_MULTIPLE:
                                       *   must be terminated by
                                       *   CSS_PROPERTY_END 
                                       * CSS_SHORTHAND_DIRECTIONS:
                                       *   must have length 4
                                       * CSS_SHORTHAND_BORDERS:
                                       *   must have length 12
                                       * CSS_SHORTHAND_FONT:
                                       *   unused */
} CssShorthandInfo;

const CssPropertyName Css_background_properties[] = {
   CSS_PROPERTY_BACKGROUND_COLOR,
   CSS_PROPERTY_BACKGROUND_IMAGE,
   CSS_PROPERTY_BACKGROUND_REPEAT,
   CSS_PROPERTY_BACKGROUND_ATTACHMENT,
   CSS_PROPERTY_BACKGROUND_POSITION,
   CSS_PROPERTY_END
};

const CssPropertyName Css_border_bottom_properties[] = {
   CSS_PROPERTY_BORDER_BOTTOM_WIDTH,
   CSS_PROPERTY_BORDER_BOTTOM_STYLE,
   CSS_PROPERTY_BORDER_BOTTOM_COLOR,
   CSS_PROPERTY_END
};

const CssPropertyName Css_border_color_properties[4] = {
   CSS_PROPERTY_BORDER_TOP_COLOR,
   CSS_PROPERTY_BORDER_BOTTOM_COLOR,
   CSS_PROPERTY_BORDER_LEFT_COLOR,
   CSS_PROPERTY_BORDER_RIGHT_COLOR
};

const CssPropertyName Css_border_left_properties[] = {
   CSS_PROPERTY_BORDER_LEFT_WIDTH,
   CSS_PROPERTY_BORDER_LEFT_STYLE,
   CSS_PROPERTY_BORDER_LEFT_COLOR,
   CSS_PROPERTY_END
};

const CssPropertyName Css_border_right_properties[] = {
   CSS_PROPERTY_BORDER_RIGHT_WIDTH,
   CSS_PROPERTY_BORDER_RIGHT_STYLE,
   CSS_PROPERTY_BORDER_RIGHT_COLOR,
   CSS_PROPERTY_END
};

const CssPropertyName Css_border_style_properties[] = {
   CSS_PROPERTY_BORDER_TOP_STYLE,
   CSS_PROPERTY_BORDER_BOTTOM_STYLE,
   CSS_PROPERTY_BORDER_LEFT_STYLE,
   CSS_PROPERTY_BORDER_RIGHT_STYLE
};

const CssPropertyName Css_border_top_properties[] = {
   CSS_PROPERTY_BORDER_TOP_WIDTH,
   CSS_PROPERTY_BORDER_TOP_STYLE,
   CSS_PROPERTY_BORDER_TOP_COLOR,
   CSS_PROPERTY_END
};

const CssPropertyName Css_border_width_properties[] = {
   CSS_PROPERTY_BORDER_TOP_WIDTH,
   CSS_PROPERTY_BORDER_BOTTOM_WIDTH,
   CSS_PROPERTY_BORDER_LEFT_WIDTH,
   CSS_PROPERTY_BORDER_RIGHT_WIDTH
};

const CssPropertyName Css_list_style_properties[] = {
   CSS_PROPERTY_LIST_STYLE_TYPE,
   CSS_PROPERTY_LIST_STYLE_POSITION,
   CSS_PROPERTY_LIST_STYLE_IMAGE,
   CSS_PROPERTY_END
};

const CssPropertyName Css_margin_properties[] = {
   CSS_PROPERTY_MARGIN_TOP,
   CSS_PROPERTY_MARGIN_BOTTOM,
   CSS_PROPERTY_MARGIN_LEFT,
   CSS_PROPERTY_MARGIN_RIGHT
};

const CssPropertyName Css_outline_properties[] = {
   CSS_PROPERTY_OUTLINE_COLOR,
   CSS_PROPERTY_OUTLINE_STYLE,
   CSS_PROPERTY_OUTLINE_WIDTH,
   CSS_PROPERTY_END
};

const CssPropertyName Css_padding_properties[] = {
   CSS_PROPERTY_PADDING_TOP,
   CSS_PROPERTY_PADDING_BOTTOM,
   CSS_PROPERTY_PADDING_LEFT,
   CSS_PROPERTY_PADDING_RIGHT
};

const CssPropertyName Css_border_properties[] = {
   CSS_PROPERTY_BORDER_TOP_WIDTH,
   CSS_PROPERTY_BORDER_TOP_STYLE,
   CSS_PROPERTY_BORDER_TOP_COLOR,
   CSS_PROPERTY_BORDER_BOTTOM_WIDTH,
   CSS_PROPERTY_BORDER_BOTTOM_STYLE,
   CSS_PROPERTY_BORDER_BOTTOM_COLOR,
   CSS_PROPERTY_BORDER_LEFT_WIDTH,
   CSS_PROPERTY_BORDER_LEFT_STYLE,
   CSS_PROPERTY_BORDER_LEFT_COLOR,
   CSS_PROPERTY_BORDER_RIGHT_WIDTH,
   CSS_PROPERTY_BORDER_RIGHT_STYLE,
   CSS_PROPERTY_BORDER_RIGHT_COLOR
};

const CssPropertyName Css_font_properties[] = {
   CSS_PROPERTY_FONT_SIZE,
   CSS_PROPERTY_FONT_STYLE,
   CSS_PROPERTY_FONT_VARIANT,
   CSS_PROPERTY_FONT_WEIGHT,
   CSS_PROPERTY_FONT_FAMILY,
   CSS_PROPERTY_END
};

static const CssShorthandInfo Css_shorthand_info[] = {
   {"background",     CssShorthandInfo::CSS_SHORTHAND_MULTIPLE,      Css_background_properties},
   {"border",         CssShorthandInfo::CSS_SHORTHAND_BORDER,        Css_border_properties},
   {"border-bottom",  CssShorthandInfo::CSS_SHORTHAND_MULTIPLE,      Css_border_bottom_properties},
   {"border-color",   CssShorthandInfo::CSS_SHORTHAND_DIRECTIONS,    Css_border_color_properties},
   {"border-left",    CssShorthandInfo::CSS_SHORTHAND_MULTIPLE,      Css_border_left_properties},
   {"border-right",   CssShorthandInfo::CSS_SHORTHAND_MULTIPLE,      Css_border_right_properties},
   {"border-style",   CssShorthandInfo::CSS_SHORTHAND_DIRECTIONS,    Css_border_style_properties},
   {"border-top",     CssShorthandInfo::CSS_SHORTHAND_MULTIPLE,      Css_border_top_properties},
   {"border-width",   CssShorthandInfo::CSS_SHORTHAND_DIRECTIONS,    Css_border_width_properties},
   {"font",           CssShorthandInfo::CSS_SHORTHAND_FONT,          Css_font_properties},
   {"list-style",     CssShorthandInfo::CSS_SHORTHAND_MULTIPLE,      Css_list_style_properties},
   {"margin",         CssShorthandInfo::CSS_SHORTHAND_DIRECTIONS,    Css_margin_properties},
   {"outline",        CssShorthandInfo::CSS_SHORTHAND_MULTIPLE,      Css_outline_properties},
   {"padding",        CssShorthandInfo::CSS_SHORTHAND_DIRECTIONS,    Css_padding_properties},
};

#define CSS_SHORTHAND_NUM \
   (sizeof(Css_shorthand_info) / sizeof(Css_shorthand_info[0]))

void tokenizerPrintCurrentToken(CssTokenizer * tokenizer);
const char * tokenizerGetTokenTypeStr(CssTokenizer * tokenizer);

const char * tokenizerGetTokenTypeStr(CssTokenizer * tokenizer)
{
   const char * typeStr = NULL;

   switch (tokenizer->type) {
   case CSS_TOKEN_TYPE_DECINT:
      typeStr = "decint";
      break;
   case CSS_TOKEN_TYPE_FLOAT:
      typeStr = "float";
      break;
   case CSS_TOKEN_TYPE_COLOR:
      typeStr = "color";
      break;
   case CSS_TOKEN_TYPE_SYMBOL:
      typeStr = "symbol";
      break;
   case CSS_TOKEN_TYPE_STRING:
      typeStr = "string";
      break;
   case CSS_TOKEN_TYPE_CHAR:
      typeStr = "char";
      break;
   case CSS_TOKEN_TYPE_END:
      typeStr = "end";
      break;
   default:
      typeStr = "unknown";
      break;
      }

   return typeStr;
}

void tokenizerPrintCurrentToken(CssTokenizer * tokenizer)
{
   fprintf(stderr, "Current token: '%s' = '%s'\n",
           tokenizerGetTokenTypeStr(tokenizer),
           tokenizer->value);
}


/* ----------------------------------------------------------------------
 *    Parsing
 * ---------------------------------------------------------------------- */

CssParser::CssParser(CssContext *context, CssOrigin origin,
                     const DilloUrl *baseUrl,
                     const char *buf, int buflen)
{
   this->context = context;
   this->origin = origin;
   this->tokenizer.buf = buf;
   this->tokenizer.buflen = buflen;
   this->tokenizer.bufOffset = 0;
   this->hll_css_parser.withinBlockC = false;
   this->hll_css_parser.spaceSeparatedC = false;
   this->hll_css_parser.bufOffsetC = 0;
   this->baseUrl = baseUrl;

   nextToken(&this->tokenizer, &this->hll_css_parser);
}

/*
 * Gets the next character from the buffer, or EOF.
 */
int getChar(CssTokenizer * tokenizer)
{
   int c;

   if (tokenizer->bufOffset >= tokenizer->buflen)
      c = EOF;
   else
      c = tokenizer->buf[tokenizer->bufOffset];

   /* The buffer pointer is increased in any case, so that ungetChar works
    * correctly at the end of the buffer. */
   tokenizer->bufOffset++;
   return c;
}

/*
 * Undoes the last getChar().
 */
void ungetChar(CssTokenizer * tokenizer)
{
   tokenizer->bufOffset--;
}

/*
 * Skip string str if it is found in the input buffer.
 * If string is found leave bufptr pointing to last matched char.
 * If not wind back. The first char is passed as parameter c
 * to avoid unnecessary getChar() / ungetChar() calls.
 */
inline bool skipString(CssTokenizer * tokenizer, int c, const char *str)
{
   for (int n = 0; str[n]; n++) {
      if (n > 0)
         c = getChar(tokenizer);

      if (str[n] != c) {
         while (n--)
            ungetChar(tokenizer);
         return false;
      }
   }

   return true;
}

static FILE * g_file = NULL;
void nextTokenInner(CssTokenizer * tokenizer, hll_CssParser * hll_css_parser);
void nextTokenInner2(CssTokenizer * tokenizer, hll_CssParser * hll_css_parser);
void nextToken(CssTokenizer * tokenizer, hll_CssParser * hll_css_parser)
{
#if 0
   if (NULL == g_file) {
      char templ[] = "css_XXXXXX";
      fprintf(stderr, "===%d\n", NULL == g_file);
      const char * path = mktemp(templ);
      fprintf(stderr, "===%d\n", NULL == g_file);
      fprintf(stderr, "%s\n", path);
      g_file = fopen(path, "w");
   }

   const size_t len_before = strlen(tok->buf + hll_css_parser->bufOffset);
   char before[128] = { 0 };
   snprintf(before, sizeof (before), "%s", tok->buf + hll_css_parser->bufOffset);
#endif

   nextTokenInner2(tokenizer, hll_css_parser);
   const char * type = tokenizerGetTokenTypeStr(tokenizer);


#if 0
   const size_t len_after = strlen(tok->buf + hll_css_parser->bufOffset);
   const size_t diff = len_before - len_after;
   char after[128] = { 0 };
   snprintf(after, sizeof (after) - diff, "%s", tok->buf + hll_css_parser->bufOffset);

   fprintf(g_file, "CSStok: ^{\n");
   fprintf(g_file, "CSStok:     \"%s\",\n", before);
   fprintf(g_file, "nextToken defaultParser {remainder = KAMIL1%sKAMIL2}\n", before);
   fprintf(g_file, "CSStok:    %*s \"%s\",\n", (int) diff, "", after);

   fprintf(g_file, "CSStok:     %d,\n", tokenizer->type);
   fprintf(g_file, "CSStok:     type=%s,\n", type);
   fprintf(g_file, "CSStok:     \"%s\",\n", tokenizer->value);

   fprintf(g_file, "CSStok: ^},\n");
#endif
}

void nextTokenInner2(CssTokenizer * tokenizer, hll_CssParser * hll_css_parser)
{
#if 0
   fprintf(stderr, "before:\n");
   fprintf(stderr, "hll_css_parser->spaceSeparated = %d\n", hll_css_parser->spaceSeparatedC);
   fprintf(stderr, "hll_css_parser->bufOffset = %d\n", hll_css_parser->bufOffsetC);
   fprintf(stderr, "hll_css_parser->tokenType = %d\n", hll_css_parser->tokenTypeC);
   fprintf(stderr, "hll_css_parser->withinBlockC = %d\n", hll_css_parser->withinBlockC);
   fprintf(stderr, "hll_css_parser->isEndC = %d\n", hll_css_parser->isEndC);
#endif
   char * tokenValue = hll_nextToken(hll_css_parser, tokenizer->buf + tokenizer->bufOffset);
#if 0
   fprintf(stderr, "after:\n");
   fprintf(stderr, "hll_css_parser->spaceSeparated = %d\n", hll_css_parser->spaceSeparatedC);
   fprintf(stderr, "hll_css_parser->bufOffset = %d\n", hll_css_parser->bufOffsetC);
   fprintf(stderr, "hll_css_parser->tokenType = %d\n", hll_css_parser->tokenTypeC);
   fprintf(stderr, "hll_css_parser->withinBlockC = %d\n", hll_css_parser->withinBlockC);
   fprintf(stderr, "hll_css_parser->isEndC = %d\n", hll_css_parser->isEndC);
   fprintf(stderr, "\n");
#endif

   tokenizer->bufOffset = hll_css_parser->bufOffsetC;

   if (NULL == tokenValue) {
      tokenizer->type = CSS_TOKEN_TYPE_END;
      tokenizer->value[0] = '\0';
   } else {
      tokenizer->type = (CssTokenType) hll_css_parser->tokenTypeC;
      snprintf(tokenizer->value, sizeof (tokenizer->value), "%s", tokenValue);
   }
}

void nextTokenInner(CssTokenizer * tokenizer, hll_CssParser * hll_css_parser)
{
   tokenizer->type = CSS_TOKEN_TYPE_CHAR; /* init */

   int c;
   while (true) {
      c = getChar(tokenizer);
      if (isspace(c)) {                    // ignore whitespace
         hll_css_parser->spaceSeparatedC = true;
      } else if (skipString(tokenizer, c, "/*")) {    // ignore comments
         do {
            c = getChar(tokenizer);
         } while (c != EOF && ! skipString(tokenizer, c, "*/"));
      } else if (skipString(tokenizer, c, "<!--")) {  // ignore XML comment markers
      } else if (skipString(tokenizer, c, "-->")) {
      } else {
         break;
      }
   }

   int i = 0;
   // handle negative numbers
   if (c == '-') {
      if (i < maxStrLen - 1)
         tokenizer->value[i++] = c;
      c = getChar(tokenizer);
   }

   if (isdigit(c)) {
      tokenizer->type = CSS_TOKEN_TYPE_DECINT;
      do {
         if (i < maxStrLen - 1) {
            tokenizer->value[i++] = c;
         }
         /* else silently truncated */
         c = getChar(tokenizer);
      } while (isdigit(c));
      if (c != '.')
         ungetChar(tokenizer);

      /* ...but keep going to see whether it's really a float */
   }

   if (c == '.') {
      c = getChar(tokenizer);
      if (isdigit(c)) {
         tokenizer->type = CSS_TOKEN_TYPE_FLOAT;
         if (i < maxStrLen - 1)
            tokenizer->value[i++] = '.';
         do {
            if (i < maxStrLen - 1)
               tokenizer->value[i++] = c;
            /* else silently truncated */
            c = getChar(tokenizer);
         } while (isdigit(c));

         ungetChar(tokenizer);
         tokenizer->value[i] = 0;
         DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token number %s\n", tokenizer->value);
         return;
      } else {
         ungetChar(tokenizer);
         if (tokenizer->type == CSS_TOKEN_TYPE_DECINT) {
            ungetChar(tokenizer);
         } else {
            c = '.';
         }
      }
   }

   if (tokenizer->type == CSS_TOKEN_TYPE_DECINT) {
      tokenizer->value[i] = 0;
      DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token number %s\n", tokenizer->value);
      return;
   }

   if (i) {
      ungetChar(tokenizer); /* ungetChar '-' */
      i--;
      c = getChar(tokenizer);
   }

   if (isalpha(c) || c == '_' || c == '-') {
      tokenizer->type = CSS_TOKEN_TYPE_SYMBOL;

      tokenizer->value[0] = c;
      i = 1;
      c = getChar(tokenizer);
      while (isalnum(c) || c == '_' || c == '-') {
         if (i < maxStrLen - 1) {
            tokenizer->value[i] = c;
            i++;
         }                      /* else silently truncated */
         c = getChar(tokenizer);
      }
      tokenizer->value[i] = 0;
      ungetChar(tokenizer);
      DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token symbol '%s'\n", tokenizer->value);
      return;
   }

   if (c == '"' || c == '\'') {
      int c1 = c;
      tokenizer->type = CSS_TOKEN_TYPE_STRING;

      i = 0;
      c = getChar(tokenizer);
      char hexbuf[5];

      while (c != EOF && c != c1) {
         if (c == '\\') {
            int d = getChar(tokenizer);
               if (isxdigit(d)) {
                  /* Read hex Unicode char. (Actually, strings are yet only 8
                   * bit.) */
                  hexbuf[0] = d;
                  int j = 1;
                  d = getChar(tokenizer);
                  while (j < 4 && isxdigit(d)) {
                     hexbuf[j] = d;
                     j++;
                     d = getChar(tokenizer);
                  }
                  hexbuf[j] = 0;
                  ungetChar(tokenizer);
                  c = strtol(hexbuf, NULL, 16);
               } else {
                  /* Take character literally. */
                  c = d;
               }
         }

         if (i < maxStrLen - 1) {
            tokenizer->value[i] = c;
            i++;
         }                      /* else silently truncated */
         c = getChar(tokenizer);
      }
      tokenizer->value[i] = 0;
      /* No ungetChar(). */
      DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token string '%s'\n", tokenizer->value);
      return;
   }

   /*
    * Within blocks, '#' starts a color, outside, it is used in selectors.
    */
   if (c == '#' && hll_css_parser->withinBlockC) {
      tokenizer->type = CSS_TOKEN_TYPE_COLOR;

      tokenizer->value[0] = c;
      i = 1;
      c = getChar(tokenizer);
      while (isxdigit(c)) {
         if (i < maxStrLen - 1) {
            tokenizer->value[i] = c;
            i++;
         }                      /* else silently truncated */
         c = getChar(tokenizer);
      }
      tokenizer->value[i] = 0;
      ungetChar(tokenizer);
      DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token color '%s'\n", tokenizer->value);
      return;
   }

   if (c == EOF) {
      DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token %s\n", "EOF");
      tokenizer->type = CSS_TOKEN_TYPE_END;
      return;
   }

   tokenizer->type = CSS_TOKEN_TYPE_CHAR;
   tokenizer->value[0] = c;
   tokenizer->value[1] = 0;
   DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token char '%c'\n", c);
}


bool CssParser::tokenMatchesProperty(CssPropertyName prop, CssPropertyValueDataType *type)
{
   int i, err = 1;
   CssPropertyValueDataType savedType = *type;

   for (int j = 0; Css_property_info[prop].type[j] != CssPropertyValueDataType::UNUSED; j++) {
      *type = Css_property_info[prop].type[j];

      switch (Css_property_info[prop].type[j]) {

      case CssPropertyValueDataType::ENUM:
         if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
            for (i = 0; Css_property_info[prop].enum_symbols[i]; i++)
               if (dStrAsciiCasecmp(tokenizer.value,
                     Css_property_info[prop].enum_symbols[i]) == 0)
                  return true;
         }
         break;

      case CssPropertyValueDataType::MULTI_ENUM:
         if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
            if (dStrAsciiCasecmp(tokenizer.value, "none") == 0) {
               return true;
            } else {
               for (i = 0; Css_property_info[prop].enum_symbols[i]; i++) {
                  if (dStrAsciiCasecmp(tokenizer.value,
                        Css_property_info[prop].enum_symbols[i]) == 0)
                     return true;
               }
            }
         }
         break;

      case CssPropertyValueDataType::BACKGROUND_POSITION:
         if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL &&
             (dStrAsciiCasecmp(tokenizer.value, "center") == 0 ||
              dStrAsciiCasecmp(tokenizer.value, "left") == 0 ||
              dStrAsciiCasecmp(tokenizer.value, "right") == 0 ||
              dStrAsciiCasecmp(tokenizer.value, "top") == 0 ||
              dStrAsciiCasecmp(tokenizer.value, "bottom") == 0))
            return true;
         // Fall Through (lenght and percentage)
      case CssPropertyValueDataType::LENGTH_PERCENTAGE:
      case CssPropertyValueDataType::LENGTH_PERCENTAGE_NUMBER:
      case CssPropertyValueDataType::LENGTH:
         if (tokenizer.value[0] == '-')
            return false;
         // Fall Through
      case CssPropertyValueDataType::SIGNED_LENGTH:
         if (tokenizer.type == CSS_TOKEN_TYPE_DECINT || tokenizer.type == CSS_TOKEN_TYPE_FLOAT)
            return true;
         break;

      case CssPropertyValueDataType::AUTO:
         if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL && dStrAsciiCasecmp(tokenizer.value, "auto") == 0)
            return true;
         break;

      case CssPropertyValueDataType::COLOR:
         if ((tokenizer.type == CSS_TOKEN_TYPE_COLOR ||
              tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) &&
            (dStrAsciiCasecmp(tokenizer.value, "rgb") == 0 ||
             hll_colorsStringToColor(tokenizer.value, -1) != -1))  /* TODO: set correct value of error flag err. */
            return true;
         break;

      case CssPropertyValueDataType::STRING:
         if (tokenizer.type == CSS_TOKEN_TYPE_STRING)
            return true;
         break;

      case CssPropertyValueDataType::SYMBOL:
         if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL ||
             tokenizer.type == CSS_TOKEN_TYPE_STRING)
            return true;
         break;

      case CssPropertyValueDataType::FONT_WEIGHT:
         if (tokenizer.type == CSS_TOKEN_TYPE_DECINT) {
            i = strtol(tokenizer.value, NULL, 10);
            if (i >= 100 && i <= 900)
               return true;
         }
         break;

      case CssPropertyValueDataType::URI:
         if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL &&
             dStrAsciiCasecmp(tokenizer.value, "url") == 0)
            return true;
         break;

      case CssPropertyValueDataType::UNUSED:
      case CssPropertyValueDataType::INTEGER:
         /* Not used for parser values. */
      default:
         assert(false);
         break;
      }
   }

   *type = savedType;
   return false;
}

bool CssParser::parseValue(CssPropertyName prop,
                           CssPropertyValueDataType type,
                           CssPropertyValue *val)
{
   CssLengthType lentype;
   bool found, ret = false;
   float fval;
   int i;
   int ival;
   Dstr *dstr;

   switch (type) {
   case CssPropertyValueDataType::ENUM:
      if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
         for (i = 0; Css_property_info[prop].enum_symbols[i]; i++)
            if (dStrAsciiCasecmp(tokenizer.value,
                            Css_property_info[prop].enum_symbols[i]) == 0) {
               val->intVal = i;
               ret = true;
               break;
            }
         nextToken(&this->tokenizer, &this->hll_css_parser);
      }
      break;

   case CssPropertyValueDataType::MULTI_ENUM:
      val->intVal = 0;
      ret = true;

      while (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
         if (dStrAsciiCasecmp(tokenizer.value, "none") != 0) {
            for (i = 0, found = false;
                 !found && Css_property_info[prop].enum_symbols[i]; i++) {
               if (dStrAsciiCasecmp(tokenizer.value,
                               Css_property_info[prop].enum_symbols[i]) == 0)
                  val->intVal |= (1 << i);
            }
         }
         nextToken(&this->tokenizer, &this->hll_css_parser);
      }
      break;

   case CssPropertyValueDataType::LENGTH_PERCENTAGE:
   case CssPropertyValueDataType::LENGTH_PERCENTAGE_NUMBER:
   case CssPropertyValueDataType::LENGTH:
   case CssPropertyValueDataType::SIGNED_LENGTH:
      if (tokenizer.type == CSS_TOKEN_TYPE_DECINT || tokenizer.type == CSS_TOKEN_TYPE_FLOAT) {
         fval = atof(tokenizer.value);
         lentype = CSS_LENGTH_TYPE_NONE;

         nextToken(&this->tokenizer, &this->hll_css_parser);
         if (!this->hll_css_parser.spaceSeparatedC && tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
            ret = true;

            if (dStrAsciiCasecmp(tokenizer.value, "px") == 0) {
               lentype = CSS_LENGTH_TYPE_PX;
               nextToken(&this->tokenizer, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(tokenizer.value, "mm") == 0) {
               lentype = CSS_LENGTH_TYPE_MM;
               nextToken(&this->tokenizer, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(tokenizer.value, "cm") == 0) {
               lentype = CSS_LENGTH_TYPE_MM;
               fval *= 10;
               nextToken(&this->tokenizer, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(tokenizer.value, "in") == 0) {
               lentype = CSS_LENGTH_TYPE_MM;
               fval *= 25.4;
               nextToken(&this->tokenizer, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(tokenizer.value, "pt") == 0) {
               lentype = CSS_LENGTH_TYPE_MM;
               fval *= (25.4 / 72);
               nextToken(&this->tokenizer, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(tokenizer.value, "pc") == 0) {
               lentype = CSS_LENGTH_TYPE_MM;
               fval *= (25.4 / 6);
               nextToken(&this->tokenizer, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(tokenizer.value, "em") == 0) {
               lentype = CSS_LENGTH_TYPE_EM;
               nextToken(&this->tokenizer, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(tokenizer.value, "ex") == 0) {
               lentype = CSS_LENGTH_TYPE_EX;
               nextToken(&this->tokenizer, &this->hll_css_parser);
            } else {
               ret = false;
            }
         } else if (!this->hll_css_parser.spaceSeparatedC &&
                    (type == CssPropertyValueDataType::LENGTH_PERCENTAGE ||
                     type == CssPropertyValueDataType::LENGTH_PERCENTAGE_NUMBER) &&
                    tokenizer.type == CSS_TOKEN_TYPE_CHAR &&
                    tokenizer.value[0] == '%') {
            fval /= 100;
            lentype = CSS_LENGTH_TYPE_PERCENTAGE;
            ret = true;
            nextToken(&this->tokenizer, &this->hll_css_parser);
         }

         /* Allow numbers without unit only for 0 or
          * CssPropertyValueDataType::LENGTH_PERCENTAGE_NUMBER
          */
         if (lentype == CSS_LENGTH_TYPE_NONE &&
            (type == CssPropertyValueDataType::LENGTH_PERCENTAGE_NUMBER || fval == 0.0))
            ret = true;

         val->intVal = CSS_CREATE_LENGTH(fval, lentype);
      }
      break;

   case CssPropertyValueDataType::AUTO:
      assert (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL && !dStrAsciiCasecmp(tokenizer.value, "auto"));
      ret = true;
      val->intVal = CSS_LENGTH_TYPE_AUTO;
      nextToken(&this->tokenizer, &this->hll_css_parser);
      break;

   case CssPropertyValueDataType::COLOR:
      {
         int color = hll_declarationValueAsColor(&this->hll_css_parser,
                                                 tokenizer.type,
                                                 tokenizer.value,
                                                 this->tokenizer.buf + this->tokenizer.bufOffset);
         this->tokenizer.bufOffset = this->hll_css_parser.bufOffsetC;
         if (999999999 != color) { // Magic value indicating error
            val->intVal = color;
            ret = true;
         } else {
            //colorError = 0;  /* TODO: set correct value of error flag colorError. */
         }
         // This takes token that is a semicolon after declaration value.
         nextToken(&this->tokenizer, &this->hll_css_parser);
      }
      break;

   case CssPropertyValueDataType::STRING:
      if (tokenizer.type == CSS_TOKEN_TYPE_STRING) {
         val->strVal = dStrdup(tokenizer.value);
         ret = true;
         nextToken(&this->tokenizer, &this->hll_css_parser);
      }
      break;

   case CssPropertyValueDataType::SYMBOL:
      /* Read comma separated list of font family names */
      dstr = dStr_new("");
      while (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL || tokenizer.type == CSS_TOKEN_TYPE_STRING ||
             (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == ',')) {
         if (this->hll_css_parser.spaceSeparatedC)
            dStr_append_c(dstr, ' ');
         dStr_append(dstr, tokenizer.value);
         ret = true;
         nextToken(&this->tokenizer, &this->hll_css_parser);
      }

      if (ret) {
         val->strVal = dStrstrip(dstr->str);
         dStr_free(dstr, 0);
      } else {
         dStr_free(dstr, 1);
      }
      break;

   case CssPropertyValueDataType::FONT_WEIGHT:
      ival = 0;
      if (tokenizer.type == CSS_TOKEN_TYPE_DECINT) {
         ival = strtol(tokenizer.value, NULL, 10);
         if (ival < 100 || ival > 900)
            /* invalid */
            ival = 0;
      }

      if (ival != 0) {
         val->intVal = ival;
         ret = true;
         nextToken(&this->tokenizer, &this->hll_css_parser);
      }
      break;

   case CssPropertyValueDataType::URI:
      if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL &&
          dStrAsciiCasecmp(tokenizer.value, "url") == 0) {
         val->strVal = parseUrl();
         nextToken(&this->tokenizer, &this->hll_css_parser);
         if (val->strVal)
            ret = true;
      }
      break;

   case CssPropertyValueDataType::BACKGROUND_POSITION:
      // 'background-position' consists of one or two values: vertical and
      // horizontal position; in most cases in this order. However, as long it
      // is unambigous, the order can be switched: "10px left" and "left 10px"
      // are both possible and have the same effect. For this reason, all
      // possibilities are tested in parallel.

      bool h[2], v[2];
      int pos[2];
      h[0] = v[0] = h[1] = v[1] = false;

      // First: collect values in pos[0] and pos[1], and determine whether
      // they can be used for a horizontal (h[i]) or vertical (v[i]) position
      // (or both). When neither h[i] or v[i] is set, pos[i] is undefined.
      for (i = 0; i < 2; i++) {
         CssPropertyValueDataType typeTmp;
         // tokenMatchesProperty will, for CSS_PROPERTY_BACKGROUND_POSITION,
         // work on both parts, since they are exchangable.
         if (tokenMatchesProperty (CSS_PROPERTY_BACKGROUND_POSITION,
                                   &typeTmp)) {
            h[i] = tokenizer.type != CSS_TOKEN_TYPE_SYMBOL ||
               (dStrAsciiCasecmp(tokenizer.value, "top") != 0 &&
                dStrAsciiCasecmp(tokenizer.value, "bottom") != 0);
            v[i] = tokenizer.type != CSS_TOKEN_TYPE_SYMBOL ||
               (dStrAsciiCasecmp(tokenizer.value, "left") != 0 &&
                dStrAsciiCasecmp(tokenizer.value, "right") != 0);
         } else
            // No match.
            h[i] = v[i] = false;

         if (h[i] || v[i]) {
            // Calculate values.
            if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
               if (dStrAsciiCasecmp(tokenizer.value, "top") == 0 ||
                   dStrAsciiCasecmp(tokenizer.value, "left") == 0) {
                  pos[i] = CSS_CREATE_LENGTH (0.0, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&this->tokenizer, &this->hll_css_parser);
               } else if (dStrAsciiCasecmp(tokenizer.value, "center") == 0) {
                  pos[i] = CSS_CREATE_LENGTH (0.5, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&this->tokenizer, &this->hll_css_parser);
               } else if (dStrAsciiCasecmp(tokenizer.value, "bottom") == 0 ||
                          dStrAsciiCasecmp(tokenizer.value, "right") == 0) {
                  pos[i] = CSS_CREATE_LENGTH (1.0, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&this->tokenizer, &this->hll_css_parser);
               } else
                  // tokenMatchesProperty should have returned "false" already.
                  lout::misc::assertNotReached ();
            } else {
               // We can assume <length> or <percentage> here ...
               CssPropertyValue valTmp;
               if (parseValue(prop, CssPropertyValueDataType::LENGTH_PERCENTAGE, &valTmp)) {
                  pos[i] = valTmp.intVal;
                  ret = true;
               } else
                  // ... but something may still fail.
                  h[i] = v[i] = false;
            }
         }

         // If the first value cannot be read, do not read the second.
         if (!h[i] && !v[i])
            break;
      }

      // Second: Create the final value. Order will be determined here.
      if (v[0] || h[0]) {
         // If second value is not set, it is set to "center", i. e. 50%, (see
         // CSS specification), which is suitable for both dimensions.
         if (!h[1] && !v[1]) {
            pos[1] = CSS_CREATE_LENGTH (0.5, CSS_LENGTH_TYPE_PERCENTAGE);
            h[1] = v[1] = true;
         }

         // Only valid, when a combination h/v or v/h is possible.
         if ((h[0] && v[1]) || (v[0] && h[1])) {
            ret = true;
            val->posVal = dNew(CssBackgroundPosition, 1);

            // Prefer combination h/v:
            if (h[0] && v[1]) {
                val->posVal->posX = pos[0];
                val->posVal->posY = pos[1];
            } else {
               // This should be v/h:
                val->posVal->posX = pos[1];
                val->posVal->posY = pos[0];
            }
         }
      }
      break;

   case CssPropertyValueDataType::UNUSED:
      /* nothing */
      break;

   case CssPropertyValueDataType::INTEGER:
      /* Not used for parser values. */
   default:
      assert(false);            /* not reached */
   }

   return ret;
}

bool CssParser::parseWeight()
{
   if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '!') {
      nextToken(&this->tokenizer, &this->hll_css_parser);
      if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL &&
          dStrAsciiCasecmp(tokenizer.value, "important") == 0) {
         nextToken(&this->tokenizer, &this->hll_css_parser);
         return true;
      }
   }

   return false;
}

/*
 * bsearch(3) compare function for searching properties
 */
static int Css_property_info_cmp(const void *a, const void *b)
{
   return dStrAsciiCasecmp(((CssPropertyInfo *) a)->symbol,
                      ((CssPropertyInfo *) b)->symbol);
}


/*
 * bsearch(3) compare function for searching shorthands
 */
static int Css_shorthand_info_cmp(const void *a, const void *b)
{
   return dStrAsciiCasecmp(((CssShorthandInfo *) a)->symbol,
                      ((CssShorthandInfo *) b)->symbol);
}

void CssParser::parseDeclaration(CssPropertyList *props,
                                 CssPropertyList *importantProps)
{
   CssPropertyInfo pi = {NULL, {CssPropertyValueDataType::UNUSED}, NULL}, *pip;
   CssShorthandInfo *sip;
   CssPropertyValueDataType type = CssPropertyValueDataType::UNUSED;

   CssPropertyName prop;
   CssPropertyValue val, dir_vals[4];
   CssPropertyValueDataType dir_types[4];
   bool found, weight;
   int sh_index, i, j, n;
   int dir_set[4][4] = {
      /* 1 value  */ {0, 0, 0, 0},
      /* 2 values */ {0, 0, 1, 1},
      /* 3 values */ {0, 2, 1, 1},
      /* 4 values */ {0, 2, 3, 1}
   };

   if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
      pi.symbol = tokenizer.value;
      pip =
          (CssPropertyInfo *) bsearch(&pi, Css_property_info,
                                      CSS_NUM_PARSED_PROPERTIES,
                                      sizeof(CssPropertyInfo),
                                      Css_property_info_cmp);
      if (pip) {
         prop = (CssPropertyName) (pip - Css_property_info);
         nextToken(&this->tokenizer, &this->hll_css_parser);
         if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == ':') {
            nextToken(&this->tokenizer, &this->hll_css_parser);
            if (tokenMatchesProperty (prop, &type) &&
                parseValue(prop, type, &val)) {
               weight = parseWeight();
               if (weight && importantProps)
                  importantProps->set(prop, type, val);
               else
                  props->set(prop, type, val);
            }
         }
      } else {
         /* Try shorthands. */
         sip =
             (CssShorthandInfo *) bsearch(&pi, Css_shorthand_info,
                                          CSS_SHORTHAND_NUM,
                                          sizeof(CssShorthandInfo),
                                          Css_shorthand_info_cmp);
         if (sip) {
            sh_index = sip - Css_shorthand_info;
            nextToken(&this->tokenizer, &this->hll_css_parser);
            if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == ':') {
               nextToken(&this->tokenizer, &this->hll_css_parser);

               switch (Css_shorthand_info[sh_index].type) {

               case CssShorthandInfo::CSS_SHORTHAND_FONT:
                  /* \todo Implement details. */
               case CssShorthandInfo::CSS_SHORTHAND_MULTIPLE:
                  do {
                     for (found = false, i = 0;
                          !found &&
                          Css_shorthand_info[sh_index].properties[i] !=
                          CSS_PROPERTY_END;
                          i++)
                        if (tokenMatchesProperty(Css_shorthand_info[sh_index].
                                                 properties[i], &type)) {
                           found = true;
                           DEBUG_MSG(DEBUG_PARSE_LEVEL,
                                     "will assign to '%s'\n",
                                     Css_property_info
                                     [Css_shorthand_info[sh_index]
                                      .properties[i]].symbol);
                           if (parseValue(Css_shorthand_info[sh_index]
                                          .properties[i], type, &val)) {
                              weight = parseWeight();
                              if (weight && importantProps)
                                 importantProps->
                                     set(Css_shorthand_info[sh_index].
                                         properties[i], type, val);
                              else
                                 props->set(Css_shorthand_info[sh_index].
                                            properties[i], type, val);
                           }
                        }
                  } while (found);
                  break;

               case CssShorthandInfo::CSS_SHORTHAND_DIRECTIONS:
                  n = 0;
                  while (n < 4) {
                     if (tokenMatchesProperty(Css_shorthand_info[sh_index].
                                              properties[0], &type) &&
                         parseValue(Css_shorthand_info[sh_index]
                                    .properties[0], type, &val)) {
                        dir_vals[n] = val;
                        dir_types[n] = type;
                        n++;
                     } else
                        break;
                  }

                  weight = parseWeight();
                  if (n > 0) {
                     for (i = 0; i < 4; i++)
                        if (weight && importantProps)
                           importantProps->set(Css_shorthand_info[sh_index]
                                               .properties[i],
                                               dir_types[dir_set[n - 1][i]],
                                               dir_vals[dir_set[n - 1][i]]);
                        else
                           props->set(Css_shorthand_info[sh_index]
                                      .properties[i],
                                      dir_types[dir_set[n - 1][i]],
                                      dir_vals[dir_set[n - 1][i]]);
                  } else
                     MSG_CSS("no values for shorthand property '%s'\n",
                             Css_shorthand_info[sh_index].symbol);

                  break;

               case CssShorthandInfo::CSS_SHORTHAND_BORDER:
                  do {
                     for (found = false, i = 0;
                          !found && i < 3;
                          i++)
                        if (tokenMatchesProperty(Css_shorthand_info[sh_index].
                                                 properties[i], &type)) {
                           found = true;
                           if (parseValue(Css_shorthand_info[sh_index]
                                          .properties[i], type, &val)) {
                              weight = parseWeight();
                              for (j = 0; j < 4; j++)
                                 if (weight && importantProps)
                                    importantProps->
                                       set(Css_shorthand_info[sh_index].
                                          properties[j * 3 + i], type, val);
                                 else
                                    props->set(Css_shorthand_info[sh_index].
                                       properties[j * 3 + i], type, val);
                           }
                        }
                  } while (found);
                  break;
               }
            }
         }
      }
   }

   /* Skip all tokens until the expected end. */
   while (!(tokenizer.type == CSS_TOKEN_TYPE_END ||
            (tokenizer.type == CSS_TOKEN_TYPE_CHAR &&
             (tokenizer.value[0] == ';' || tokenizer.value[0] == '}'))))
      nextToken(&this->tokenizer, &this->hll_css_parser);

   if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == ';')
      nextToken(&this->tokenizer, &this->hll_css_parser);
}

bool CssParser::parseSimpleSelector(CssSimpleSelector *selector)
{
   CssSelectorType selectorType;

   if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
      selector->setSelectorElement(a_Html_tag_index(tokenizer.value));
      nextToken(&this->tokenizer, &this->hll_css_parser);
      if (this->hll_css_parser.spaceSeparatedC)
         return true;
   } else if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '*') {
      selector->setSelectorElement(CssSimpleSelector::ELEMENT_ANY);
      nextToken(&this->tokenizer, &this->hll_css_parser);
      if (this->hll_css_parser.spaceSeparatedC)
         return true;
   } else if (tokenizer.type == CSS_TOKEN_TYPE_CHAR &&
              (tokenizer.value[0] == '#' ||
               tokenizer.value[0] == '.' ||
               tokenizer.value[0] == ':')) {
      // nothing to be done in this case
   } else {
      return false;
   }

   do {
      selectorType = CssSelectorType::NONE;
      if (tokenizer.type == CSS_TOKEN_TYPE_CHAR) {
         switch (tokenizer.value[0]) {
         case '#':
            selectorType = CssSelectorType::ID;
            break;
         case '.':
            selectorType = CssSelectorType::CLASS;
            break;
         case ':':
            selectorType = CssSelectorType::PSEUDO_CLASS;
            if (selector->getSelectorPseudoClass ())
               // pseudo class has been set already.
               // As dillo currently only supports :link and :visisted, a
               // selector with more than one pseudo class will never match.
               // By returning false, the whole CssRule will be dropped.
               // \todo adapt this when supporting :hover, :active...
               return false;
            break;
         }
      }

      if (selectorType != CssSelectorType::NONE) {
         nextToken(&this->tokenizer, &this->hll_css_parser);
         if (this->hll_css_parser.spaceSeparatedC)
            return false;

         if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
            selector->setSelector(selectorType, tokenizer.value);
            nextToken(&this->tokenizer, &this->hll_css_parser);
         } else {
            return false; // don't accept classes or id's starting with integer
         }
         if (this->hll_css_parser.spaceSeparatedC)
            return true;
      }
   } while (selectorType != CssSelectorType::NONE);

   DEBUG_MSG(DEBUG_PARSE_LEVEL, "end of simple selector (%s, %s, %s, %d)\n",
      selector->id, selector->klass,
      selector->pseudo, selector->element);

   return true;
}

CssSelector *CssParser::parseSelector()
{
   CssSelector *selector = new CssSelector ();

   while (true) {
      if (! parseSimpleSelector (selector->top ())) {
         delete selector;
         selector = NULL;
         break;
      }

      if (tokenizer.type == CSS_TOKEN_TYPE_CHAR &&
         (tokenizer.value[0] == ',' || tokenizer.value[0] == '{')) {
         break;
      } else if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '>') {
         selector->addSimpleSelector (CssSelector::COMB_CHILD);
         nextToken(&this->tokenizer, &this->hll_css_parser);
      } else if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '+') {
         selector->addSimpleSelector (CssSelector::COMB_ADJACENT_SIBLING);
         nextToken(&this->tokenizer, &this->hll_css_parser);
      } else if (tokenizer.type != CSS_TOKEN_TYPE_END && this->hll_css_parser.spaceSeparatedC) {
         selector->addSimpleSelector (CssSelector::COMB_DESCENDANT);
      } else {
         delete selector;
         selector = NULL;
         break;
      }
   }

   while (tokenizer.type != CSS_TOKEN_TYPE_END &&
          (tokenizer.type != CSS_TOKEN_TYPE_CHAR ||
           (tokenizer.value[0] != ',' && tokenizer.value[0] != '{')))
         nextToken(&this->tokenizer, &this->hll_css_parser);

   return selector;
}

void CssParser::parseRuleset()
{
   lout::misc::SimpleVector < CssSelector * >*list;
   CssPropertyList *props, *importantProps;
   CssSelector *selector;

   list = new lout::misc::SimpleVector < CssSelector * >(1);

   while (true) {
      selector = parseSelector();

      if (selector) {
         selector->ref();
         list->increase();
         list->set(list->size() - 1, selector);
      }

      // \todo dump whole ruleset in case of parse error as required by CSS 2.1
      //       however make sure we don't dump it if only dillo fails to parse
      //       valid CSS.

      if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == ',')
         /* To read the next tokenizer. */
         nextToken(&this->tokenizer, &this->hll_css_parser);
      else
         /* No more selectors. */
         break;
   }

   DEBUG_MSG(DEBUG_PARSE_LEVEL, "end of %s\n", "selectors");

   props = new CssPropertyList(true);
   props->ref();
   importantProps = new CssPropertyList(true);
   importantProps->ref();

   /* Read block. ('{' has already been read.) */
   if (tokenizer.type != CSS_TOKEN_TYPE_END) {
      this->hll_css_parser.withinBlockC = true;
      nextToken(&this->tokenizer, &this->hll_css_parser);
      do
         parseDeclaration(props, importantProps);
      while (!(tokenizer.type == CSS_TOKEN_TYPE_END ||
               (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '}')));
      this->hll_css_parser.withinBlockC = false;
   }

   for (int i = 0; i < list->size(); i++) {
      CssSelector *s = list->get(i);

      if (origin == CSS_ORIGIN_USER_AGENT) {
         context->addRule(s, props, CSS_PRIMARY_USER_AGENT);
      } else if (origin == CSS_ORIGIN_USER) {
         context->addRule(s, props, CSS_PRIMARY_USER);
         context->addRule(s, importantProps, CSS_PRIMARY_USER_IMPORTANT);
      } else if (origin == CSS_ORIGIN_AUTHOR) {
         context->addRule(s, props, CSS_PRIMARY_AUTHOR);
         context->addRule(s, importantProps, CSS_PRIMARY_AUTHOR_IMPORTANT);
      }

      s->unref();
   }

   props->unref();
   importantProps->unref();

   delete list;

   if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '}')
      nextToken(&this->tokenizer, &this->hll_css_parser);
}

char * CssParser::parseUrl()
{
   Dstr *urlStr = NULL;

   if (tokenizer.type != CSS_TOKEN_TYPE_SYMBOL ||
      dStrAsciiCasecmp(tokenizer.value, "url") != 0)
      return NULL;

   nextToken(&this->tokenizer, &this->hll_css_parser);

   if (tokenizer.type != CSS_TOKEN_TYPE_CHAR || tokenizer.value[0] != '(')
      return NULL;

   nextToken(&this->tokenizer, &this->hll_css_parser);

   if (tokenizer.type == CSS_TOKEN_TYPE_STRING) {
      urlStr = dStr_new(tokenizer.value);
      nextToken(&this->tokenizer, &this->hll_css_parser);
   } else {
      urlStr = dStr_new("");
      while (tokenizer.type != CSS_TOKEN_TYPE_END &&
             (tokenizer.type != CSS_TOKEN_TYPE_CHAR || tokenizer.value[0] != ')')) {
         dStr_append(urlStr, tokenizer.value);
         nextToken(&this->tokenizer, &this->hll_css_parser);
      }
   }

   if (tokenizer.type != CSS_TOKEN_TYPE_CHAR || tokenizer.value[0] != ')') {
      dStr_free(urlStr, 1);
      urlStr = NULL;
   }

   if (urlStr) {
      DilloUrl *dilloUrl = a_Url_new(urlStr->str, a_Url_str(this->baseUrl));
      char *url = dStrdup(a_Url_str(dilloUrl));
      a_Url_free(dilloUrl);
      dStr_free(urlStr, 1);
      return url;
   } else {
      return NULL;
   }
}

void CssParser::parseImport(DilloHtml *html)
{
   char *urlStr = NULL;
   bool importSyntaxIsOK = false;
   bool mediaSyntaxIsOK = true;
   bool mediaIsSelected = true;

   nextToken(&this->tokenizer, &this->hll_css_parser);

   if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL &&
       dStrAsciiCasecmp(tokenizer.value, "url") == 0)
      urlStr = parseUrl();
   else if (tokenizer.type == CSS_TOKEN_TYPE_STRING)
      urlStr = dStrdup (tokenizer.value);

   nextToken(&this->tokenizer, &this->hll_css_parser);

   /* parse a comma-separated list of media */
   if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
      mediaSyntaxIsOK = false;
      mediaIsSelected = false;
      while (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
         if (dStrAsciiCasecmp(tokenizer.value, "all") == 0 ||
             dStrAsciiCasecmp(tokenizer.value, "screen") == 0)
            mediaIsSelected = true;
         nextToken(&this->tokenizer, &this->hll_css_parser);
         if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == ',') {
            nextToken(&this->tokenizer, &this->hll_css_parser);
         } else {
            mediaSyntaxIsOK = true;
            break;
         }
      }
   }

   if (mediaSyntaxIsOK &&
       tokenizer.type == CSS_TOKEN_TYPE_CHAR &&
       tokenizer.value[0] == ';') {
      importSyntaxIsOK = true;
      nextToken(&this->tokenizer, &this->hll_css_parser);
   } else
      ignoreStatement();

   if (urlStr) {
      if (importSyntaxIsOK && mediaIsSelected) {
         MSG("CssParser::parseImport(): @import %s\n", urlStr);
         DilloUrl *url = a_Html_url_new (html, urlStr, a_Url_str(this->baseUrl),
                                         this->baseUrl ? 1 : 0);
         a_Html_load_stylesheet(html, url);
         a_Url_free(url);
      }
      dFree (urlStr);
   }
}

void CssParser::parseMedia()
{
   bool mediaSyntaxIsOK = false;
   bool mediaIsSelected = false;

   nextToken(&this->tokenizer, &this->hll_css_parser);

   /* parse a comma-separated list of media */
   while (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
      if (dStrAsciiCasecmp(tokenizer.value, "all") == 0 ||
          dStrAsciiCasecmp(tokenizer.value, "screen") == 0)
         mediaIsSelected = true;
      nextToken(&this->tokenizer, &this->hll_css_parser);
      if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == ',') {
         nextToken(&this->tokenizer, &this->hll_css_parser);
      } else {
         mediaSyntaxIsOK = true;
         break;
      }
   }

   /* check that the syntax is OK so far */
   if (!(mediaSyntaxIsOK &&
         tokenizer.type == CSS_TOKEN_TYPE_CHAR &&
         tokenizer.value[0] == '{')) {
      ignoreStatement();
      return;
   }

   /* parse/ignore the block as required */
   if (mediaIsSelected) {
      nextToken(&this->tokenizer, &this->hll_css_parser);
      while (tokenizer.type != CSS_TOKEN_TYPE_END) {
         parseRuleset();
         if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '}') {
            nextToken(&this->tokenizer, &this->hll_css_parser);
            break;
         }
      }
   } else
      ignoreBlock();
}

const char * CssParser::propertyNameString(CssPropertyName name)
{
   return Css_property_info[name].symbol;
}

void CssParser::ignoreBlock()
{
   int depth = 0;

   while (tokenizer.type != CSS_TOKEN_TYPE_END) {
      if (tokenizer.type == CSS_TOKEN_TYPE_CHAR) {
         if (tokenizer.value[0] == '{') {
            depth++;
         } else if (tokenizer.value[0] == '}') {
            depth--;
            if (depth == 0) {
               nextToken(&this->tokenizer, &this->hll_css_parser);
               return;
            }
         }
      }
      nextToken(&this->tokenizer, &this->hll_css_parser);
   }
}

void CssParser::ignoreStatement()
{
   while (tokenizer.type != CSS_TOKEN_TYPE_END) {
      if (tokenizer.type == CSS_TOKEN_TYPE_CHAR) {
         if (tokenizer.value[0] == ';') {
            nextToken(&this->tokenizer, &this->hll_css_parser);
            return;
         } else if (tokenizer.value[0] =='{') {
            ignoreBlock();
            return;
         }
      }
      nextToken(&this->tokenizer, &this->hll_css_parser);
   }
}

void CssParser::parse(DilloHtml *html, const DilloUrl *baseUrl,
                      CssContext *context,
                      const char *buf,
                      int buflen, CssOrigin origin)
{
   CssParser parser (context, origin, baseUrl, buf, buflen);
   bool importsAreAllowed = true;

   while (parser.tokenizer.type != CSS_TOKEN_TYPE_END) {
      if (parser.tokenizer.type == CSS_TOKEN_TYPE_CHAR &&
          parser.tokenizer.value[0] == '@') {
         nextToken(&parser.tokenizer, &parser.hll_css_parser);
         if (parser.tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
            if (dStrAsciiCasecmp(parser.tokenizer.value, "import") == 0 &&
                html != NULL &&
                importsAreAllowed) {
               parser.parseImport(html);
            } else if (dStrAsciiCasecmp(parser.tokenizer.value, "media") == 0) {
               parser.parseMedia();
            } else {
               parser.ignoreStatement();
            }
         } else {
            parser.ignoreStatement();
         }
      } else {
         importsAreAllowed = false;
         parser.parseRuleset();
      }
   }
}

void CssParser::parseDeclarationBlock(const DilloUrl *baseUrl,
                                      const char *buf, int buflen,
                                      CssPropertyList *props,
                                      CssPropertyList *propsImortant)
{
   CssParser parser (NULL, CSS_ORIGIN_AUTHOR, baseUrl, buf, buflen);

   parser.hll_css_parser.withinBlockC = true;

   do
      parser.parseDeclaration(props, propsImortant);
   while (!(parser.tokenizer.type == CSS_TOKEN_TYPE_END ||
         (parser.tokenizer.type == CSS_TOKEN_TYPE_CHAR && parser.tokenizer.value[0] == '}')));
}
