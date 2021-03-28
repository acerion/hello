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

/* ----------------------------------------------------------------------
 *    Parsing
 * ---------------------------------------------------------------------- */

CssParser::CssParser(CssContext *context, CssOrigin origin,
                     const DilloUrl *baseUrl,
                     const char *buf, int buflen)
{
   this->context = context;
   this->origin = origin;
   this->token.buf = buf;
   this->token.buflen = buflen;
   this->token.bufOffset = 0;
   this->hll_css_parser.withinBlockC = false;
   this->hll_css_parser.spaceSeparatedC = false;
   this->hll_css_parser.bufOffsetC = 0;
   this->baseUrl = baseUrl;

   nextToken(&this->token, &this->hll_css_parser);
}

/*
 * Gets the next character from the buffer, or EOF.
 */
int getChar(CssToken * token)
{
   int c;

   if (token->bufOffset >= token->buflen)
      c = EOF;
   else
      c = token->buf[token->bufOffset];

   /* The buffer pointer is increased in any case, so that ungetChar works
    * correctly at the end of the buffer. */
   token->bufOffset++;
   return c;
}

/*
 * Undoes the last getChar().
 */
void ungetChar(CssToken * tok)
{
   tok->bufOffset--;
}

/*
 * Skip string str if it is found in the input buffer.
 * If string is found leave bufptr pointing to last matched char.
 * If not wind back. The first char is passed as parameter c
 * to avoid unnecessary getChar() / ungetChar() calls.
 */
inline bool skipString(CssToken * tok, int c, const char *str)
{
   for (int n = 0; str[n]; n++) {
      if (n > 0)
         c = getChar(tok);

      if (str[n] != c) {
         while (n--)
            ungetChar(tok);
         return false;
      }
   }

   return true;
}

static FILE * g_file = NULL;
void nextTokenInner(CssToken * tok, hll_CssParser * hll_css_parser);
void nextTokenInner2(CssToken * tok, hll_CssParser * hll_css_parser);
void nextToken(CssToken * tok, hll_CssParser * hll_css_parser)
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

   nextTokenInner2(tok, hll_css_parser);
   const char * type = NULL;
   switch (tok->type) {
   case CSS_TOKEN_TYPE_DECINT:
      type = "decint";
      break;
   case CSS_TOKEN_TYPE_FLOAT:
      type = "float";
      break;
   case CSS_TOKEN_TYPE_COLOR:
      type = "color";
      break;
   case CSS_TOKEN_TYPE_SYMBOL:
      type = "symbol";
      break;
   case CSS_TOKEN_TYPE_STRING:
      type = "string";
      break;
   case CSS_TOKEN_TYPE_CHAR:
      type = "char";
      break;
   case CSS_TOKEN_TYPE_END:
      type = "end";
      break;
   default:
      type = "unknown";
      break;
      }

#if 0
   const size_t len_after = strlen(tok->buf + hll_css_parser->bufOffset);
   const size_t diff = len_before - len_after;
   char after[128] = { 0 };
   snprintf(after, sizeof (after) - diff, "%s", tok->buf + hll_css_parser->bufOffset);

   fprintf(g_file, "CSStok: ^{\n");
   fprintf(g_file, "CSStok:     \"%s\",\n", before);
   fprintf(g_file, "nextToken defaultParser {remainder = KAMIL1%sKAMIL2}\n", before);
   fprintf(g_file, "CSStok:    %*s \"%s\",\n", (int) diff, "", after);

   fprintf(g_file, "CSStok:     %d,\n", tok->type);
   fprintf(g_file, "CSStok:     type=%s,\n", type);
   fprintf(g_file, "CSStok:     \"%s\",\n", tok->value);

   fprintf(g_file, "CSStok: ^},\n");
#endif
}

void nextTokenInner2(CssToken * tok, hll_CssParser * hll_css_parser)
{
#if 0
   fprintf(stderr, "before:\n");
   fprintf(stderr, "hll_css_parser->spaceSeparated = %d\n", hll_css_parser->spaceSeparatedC);
   fprintf(stderr, "hll_css_parser->bufOffset = %d\n", hll_css_parser->bufOffsetC);
   fprintf(stderr, "hll_css_parser->tokenType = %d\n", hll_css_parser->tokenTypeC);
   fprintf(stderr, "hll_css_parser->withinBlockC = %d\n", hll_css_parser->withinBlockC);
#endif
   char * tokenValue = hll_nextToken(hll_css_parser, tok->buf + tok->bufOffset);
#if 0
   fprintf(stderr, "after:\n");
   fprintf(stderr, "hll_css_parser->spaceSeparated = %d\n", hll_css_parser->spaceSeparatedC);
   fprintf(stderr, "hll_css_parser->bufOffset = %d\n", hll_css_parser->bufOffsetC);
   fprintf(stderr, "hll_css_parser->tokenType = %d\n", hll_css_parser->tokenTypeC);
   fprintf(stderr, "hll_css_parser->withinBlockC = %d\n", hll_css_parser->withinBlockC);
   fprintf(stderr, "\n");
#endif

   tok->bufOffset = hll_css_parser->bufOffsetC;

   if (NULL == tokenValue) {
      tok->type = CSS_TOKEN_TYPE_END;
      tok->value[0] = '\0';
   } else {
      tok->type = (CssTokenType) hll_css_parser->tokenTypeC;
      snprintf(tok->value, sizeof (tok->value), "%s", tokenValue);
   }
}

void nextTokenInner(CssToken * tok, hll_CssParser * hll_css_parser)
{
   tok->type = CSS_TOKEN_TYPE_CHAR; /* init */

   int c;
   while (true) {
      c = getChar(tok);
      if (isspace(c)) {                    // ignore whitespace
         hll_css_parser->spaceSeparatedC = true;
      } else if (skipString(tok, c, "/*")) {    // ignore comments
         do {
            c = getChar(tok);
         } while (c != EOF && ! skipString(tok, c, "*/"));
      } else if (skipString(tok, c, "<!--")) {  // ignore XML comment markers
      } else if (skipString(tok, c, "-->")) {
      } else {
         break;
      }
   }

   int i = 0;
   // handle negative numbers
   if (c == '-') {
      if (i < maxStrLen - 1)
         tok->value[i++] = c;
      c = getChar(tok);
   }

   if (isdigit(c)) {
      tok->type = CSS_TOKEN_TYPE_DECINT;
      do {
         if (i < maxStrLen - 1) {
            tok->value[i++] = c;
         }
         /* else silently truncated */
         c = getChar(tok);
      } while (isdigit(c));
      if (c != '.')
         ungetChar(tok);

      /* ...but keep going to see whether it's really a float */
   }

   if (c == '.') {
      c = getChar(tok);
      if (isdigit(c)) {
         tok->type = CSS_TOKEN_TYPE_FLOAT;
         if (i < maxStrLen - 1)
            tok->value[i++] = '.';
         do {
            if (i < maxStrLen - 1)
               tok->value[i++] = c;
            /* else silently truncated */
            c = getChar(tok);
         } while (isdigit(c));

         ungetChar(tok);
         tok->value[i] = 0;
         DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token number %s\n", tok->value);
         return;
      } else {
         ungetChar(tok);
         if (tok->type == CSS_TOKEN_TYPE_DECINT) {
            ungetChar(tok);
         } else {
            c = '.';
         }
      }
   }

   if (tok->type == CSS_TOKEN_TYPE_DECINT) {
      tok->value[i] = 0;
      DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token number %s\n", tok->value);
      return;
   }

   if (i) {
      ungetChar(tok); /* ungetChar '-' */
      i--;
      c = getChar(tok);
   }

   if (isalpha(c) || c == '_' || c == '-') {
      tok->type = CSS_TOKEN_TYPE_SYMBOL;

      tok->value[0] = c;
      i = 1;
      c = getChar(tok);
      while (isalnum(c) || c == '_' || c == '-') {
         if (i < maxStrLen - 1) {
            tok->value[i] = c;
            i++;
         }                      /* else silently truncated */
         c = getChar(tok);
      }
      tok->value[i] = 0;
      ungetChar(tok);
      DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token symbol '%s'\n", tok->value);
      return;
   }

   if (c == '"' || c == '\'') {
      int c1 = c;
      tok->type = CSS_TOKEN_TYPE_STRING;

      i = 0;
      c = getChar(tok);
      char hexbuf[5];

      while (c != EOF && c != c1) {
         if (c == '\\') {
            int d = getChar(tok);
               if (isxdigit(d)) {
                  /* Read hex Unicode char. (Actually, strings are yet only 8
                   * bit.) */
                  hexbuf[0] = d;
                  int j = 1;
                  d = getChar(tok);
                  while (j < 4 && isxdigit(d)) {
                     hexbuf[j] = d;
                     j++;
                     d = getChar(tok);
                  }
                  hexbuf[j] = 0;
                  ungetChar(tok);
                  c = strtol(hexbuf, NULL, 16);
               } else {
                  /* Take character literally. */
                  c = d;
               }
         }

         if (i < maxStrLen - 1) {
            tok->value[i] = c;
            i++;
         }                      /* else silently truncated */
         c = getChar(tok);
      }
      tok->value[i] = 0;
      /* No ungetChar(). */
      DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token string '%s'\n", tok->value);
      return;
   }

   /*
    * Within blocks, '#' starts a color, outside, it is used in selectors.
    */
   if (c == '#' && hll_css_parser->withinBlockC) {
      tok->type = CSS_TOKEN_TYPE_COLOR;

      tok->value[0] = c;
      i = 1;
      c = getChar(tok);
      while (isxdigit(c)) {
         if (i < maxStrLen - 1) {
            tok->value[i] = c;
            i++;
         }                      /* else silently truncated */
         c = getChar(tok);
      }
      tok->value[i] = 0;
      ungetChar(tok);
      DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token color '%s'\n", tok->value);
      return;
   }

   if (c == EOF) {
      DEBUG_MSG(DEBUG_TOKEN_LEVEL, "token %s\n", "EOF");
      tok->type = CSS_TOKEN_TYPE_END;
      return;
   }

   tok->type = CSS_TOKEN_TYPE_CHAR;
   tok->value[0] = c;
   tok->value[1] = 0;
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
         if (token.type == CSS_TOKEN_TYPE_SYMBOL) {
            for (i = 0; Css_property_info[prop].enum_symbols[i]; i++)
               if (dStrAsciiCasecmp(token.value,
                     Css_property_info[prop].enum_symbols[i]) == 0)
                  return true;
         }
         break;

      case CssPropertyValueDataType::MULTI_ENUM:
         if (token.type == CSS_TOKEN_TYPE_SYMBOL) {
            if (dStrAsciiCasecmp(token.value, "none") == 0) {
               return true;
            } else {
               for (i = 0; Css_property_info[prop].enum_symbols[i]; i++) {
                  if (dStrAsciiCasecmp(token.value,
                        Css_property_info[prop].enum_symbols[i]) == 0)
                     return true;
               }
            }
         }
         break;

      case CssPropertyValueDataType::BACKGROUND_POSITION:
         if (token.type == CSS_TOKEN_TYPE_SYMBOL &&
             (dStrAsciiCasecmp(token.value, "center") == 0 ||
              dStrAsciiCasecmp(token.value, "left") == 0 ||
              dStrAsciiCasecmp(token.value, "right") == 0 ||
              dStrAsciiCasecmp(token.value, "top") == 0 ||
              dStrAsciiCasecmp(token.value, "bottom") == 0))
            return true;
         // Fall Through (lenght and percentage)
      case CssPropertyValueDataType::LENGTH_PERCENTAGE:
      case CssPropertyValueDataType::LENGTH_PERCENTAGE_NUMBER:
      case CssPropertyValueDataType::LENGTH:
         if (token.value[0] == '-')
            return false;
         // Fall Through
      case CssPropertyValueDataType::SIGNED_LENGTH:
         if (token.type == CSS_TOKEN_TYPE_DECINT || token.type == CSS_TOKEN_TYPE_FLOAT)
            return true;
         break;

      case CssPropertyValueDataType::AUTO:
         if (token.type == CSS_TOKEN_TYPE_SYMBOL && dStrAsciiCasecmp(token.value, "auto") == 0)
            return true;
         break;

      case CssPropertyValueDataType::COLOR:
         if ((token.type == CSS_TOKEN_TYPE_COLOR ||
              token.type == CSS_TOKEN_TYPE_SYMBOL) &&
            (dStrAsciiCasecmp(token.value, "rgb") == 0 ||
             hll_colorsStringToColor(token.value, -1) != -1))  /* TODO: set correct value of error flag err. */
            return true;
         break;

      case CssPropertyValueDataType::STRING:
         if (token.type == CSS_TOKEN_TYPE_STRING)
            return true;
         break;

      case CssPropertyValueDataType::SYMBOL:
         if (token.type == CSS_TOKEN_TYPE_SYMBOL ||
             token.type == CSS_TOKEN_TYPE_STRING)
            return true;
         break;

      case CssPropertyValueDataType::FONT_WEIGHT:
         if (token.type == CSS_TOKEN_TYPE_DECINT) {
            i = strtol(token.value, NULL, 10);
            if (i >= 100 && i <= 900)
               return true;
         }
         break;

      case CssPropertyValueDataType::URI:
         if (token.type == CSS_TOKEN_TYPE_SYMBOL &&
             dStrAsciiCasecmp(token.value, "url") == 0)
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

bool parseRgbFunctionComponent(CssToken * token, hll_CssParser * hll_css_parser, CssColor * color, int * component)
{
   if (token->type != CSS_TOKEN_TYPE_DECINT) {
      MSG_CSS("expected integer not found in %s color\n", "rgb");
      return false;
   }

   *component = strtol(token->value, NULL, 10);

   nextToken(token, hll_css_parser);
   if (token->type == CSS_TOKEN_TYPE_CHAR && token->value[0] == '%') {
      if (color->percentage == 0) {
         MSG_CSS("'%s' unexpected in rgb color\n", "%");
         return false;
      }
      color->percentage = 1;
      *component = *component * 255 / 100;
      nextToken(token, hll_css_parser);
   } else {
      if (color->percentage == 1) {
         MSG_CSS("expected '%s' not found in rgb color\n", "%");
         return false;
      }
      color->percentage = 0;
   }

   if (*component > 255)
      *component = 255;
   if (*component < 0)
      *component = 0;

   return true;
}

bool parseRgbFunction(CssToken * token, hll_CssParser * hll_css_parser, CssColor * color)
{
   if (token->type != CSS_TOKEN_TYPE_CHAR || token->value[0] != '(') {
      MSG_CSS("expected '%s' not found in rgb color\n", "(");
      return false;
   }

   int component = 0;
   int shift = 16;
   const char sep[] = { ',', ',', ')' };

   for (int i = 0; i < 3; i++) {
      nextToken(token, hll_css_parser);
      if (!parseRgbFunctionComponent(token, hll_css_parser, color, &component))
         return false;
      color->color |= component << shift;
      if (token->type != CSS_TOKEN_TYPE_CHAR || token->value[0] != sep[i]) {
         MSG_CSS("expected '%s' not found in rgb color\n", ",");
         return false;
      }
      shift -= 8;
   }

   return true;
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
      if (token.type == CSS_TOKEN_TYPE_SYMBOL) {
         for (i = 0; Css_property_info[prop].enum_symbols[i]; i++)
            if (dStrAsciiCasecmp(token.value,
                            Css_property_info[prop].enum_symbols[i]) == 0) {
               val->intVal = i;
               ret = true;
               break;
            }
         nextToken(&this->token, &this->hll_css_parser);
      }
      break;

   case CssPropertyValueDataType::MULTI_ENUM:
      val->intVal = 0;
      ret = true;

      while (token.type == CSS_TOKEN_TYPE_SYMBOL) {
         if (dStrAsciiCasecmp(token.value, "none") != 0) {
            for (i = 0, found = false;
                 !found && Css_property_info[prop].enum_symbols[i]; i++) {
               if (dStrAsciiCasecmp(token.value,
                               Css_property_info[prop].enum_symbols[i]) == 0)
                  val->intVal |= (1 << i);
            }
         }
         nextToken(&this->token, &this->hll_css_parser);
      }
      break;

   case CssPropertyValueDataType::LENGTH_PERCENTAGE:
   case CssPropertyValueDataType::LENGTH_PERCENTAGE_NUMBER:
   case CssPropertyValueDataType::LENGTH:
   case CssPropertyValueDataType::SIGNED_LENGTH:
      if (token.type == CSS_TOKEN_TYPE_DECINT || token.type == CSS_TOKEN_TYPE_FLOAT) {
         fval = atof(token.value);
         lentype = CSS_LENGTH_TYPE_NONE;

         nextToken(&this->token, &this->hll_css_parser);
         if (!this->hll_css_parser.spaceSeparatedC && token.type == CSS_TOKEN_TYPE_SYMBOL) {
            ret = true;

            if (dStrAsciiCasecmp(token.value, "px") == 0) {
               lentype = CSS_LENGTH_TYPE_PX;
               nextToken(&this->token, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(token.value, "mm") == 0) {
               lentype = CSS_LENGTH_TYPE_MM;
               nextToken(&this->token, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(token.value, "cm") == 0) {
               lentype = CSS_LENGTH_TYPE_MM;
               fval *= 10;
               nextToken(&this->token, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(token.value, "in") == 0) {
               lentype = CSS_LENGTH_TYPE_MM;
               fval *= 25.4;
               nextToken(&this->token, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(token.value, "pt") == 0) {
               lentype = CSS_LENGTH_TYPE_MM;
               fval *= (25.4 / 72);
               nextToken(&this->token, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(token.value, "pc") == 0) {
               lentype = CSS_LENGTH_TYPE_MM;
               fval *= (25.4 / 6);
               nextToken(&this->token, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(token.value, "em") == 0) {
               lentype = CSS_LENGTH_TYPE_EM;
               nextToken(&this->token, &this->hll_css_parser);
            } else if (dStrAsciiCasecmp(token.value, "ex") == 0) {
               lentype = CSS_LENGTH_TYPE_EX;
               nextToken(&this->token, &this->hll_css_parser);
            } else {
               ret = false;
            }
         } else if (!this->hll_css_parser.spaceSeparatedC &&
                    (type == CssPropertyValueDataType::LENGTH_PERCENTAGE ||
                     type == CssPropertyValueDataType::LENGTH_PERCENTAGE_NUMBER) &&
                    token.type == CSS_TOKEN_TYPE_CHAR &&
                    token.value[0] == '%') {
            fval /= 100;
            lentype = CSS_LENGTH_TYPE_PERCENTAGE;
            ret = true;
            nextToken(&this->token, &this->hll_css_parser);
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
      assert (token.type == CSS_TOKEN_TYPE_SYMBOL && !dStrAsciiCasecmp(token.value, "auto"));
      ret = true;
      val->intVal = CSS_LENGTH_TYPE_AUTO;
      nextToken(&this->token, &this->hll_css_parser);
      break;

   case CssPropertyValueDataType::COLOR:
      if (token.type == CSS_TOKEN_TYPE_COLOR) {
         int colorError = 1;
         val->intVal = hll_colorsStringToColor(token.value, -1); colorError = 0;  /* TODO: set correct value of error flag colorError. */
         if (colorError)
            MSG_CSS("color is not in \"%s\" format\n", "#RRGGBB");
         else
            ret = true;
         nextToken(&this->token, &this->hll_css_parser);
      } else if (token.type == CSS_TOKEN_TYPE_SYMBOL) {
         if (dStrAsciiCasecmp(token.value, "rgb") == 0) {
#if 1
            fprintf(stderr, "buf before = '%s'\n\n", this->token.buf + this->token.bufOffset);
            int color = hll_parseRgbFunction(&this->hll_css_parser, this->token.buf + this->token.bufOffset);
            //this->hll_css_parser.bufOffsetC--;
            this->token.bufOffset = this->hll_css_parser.bufOffsetC;
            fprintf(stderr, "buf after = '%s'\n\n", this->token.buf + this->token.bufOffset);
            if (999999999 != color) { // Magic value indicating error
               val->intVal = color;
               ret = true;
            } else {
            }
#else
            nextToken(&this->token, &this->hll_css_parser);
            CssColor color = { .color = 0, .percentage = -1 };
            if (parseRgbFunction(&this->token, &this->hll_css_parser, &color)) {
               val->intVal = color.color;
               ret = true;
            } else {
               MSG_CSS("Failed to parse %s color\n", "rgb(r,g,b)");
            }
#endif
            fprintf(stderr, "rgb function color = %d\n", val->intVal);
         } else {
            int colorError = 1;
            val->intVal = hll_colorsStringToColor(token.value, -1); colorError = 0; /* TODO: set correct value of error flag colorError. */
            if (colorError)
               MSG_CSS("color is not in \"%s\" format\n", "#RRGGBB");
            else
               ret = true;
         }
         nextToken(&this->token, &this->hll_css_parser);
      }
      break;

   case CssPropertyValueDataType::STRING:
      if (token.type == CSS_TOKEN_TYPE_STRING) {
         val->strVal = dStrdup(token.value);
         ret = true;
         nextToken(&this->token, &this->hll_css_parser);
      }
      break;

   case CssPropertyValueDataType::SYMBOL:
      /* Read comma separated list of font family names */
      dstr = dStr_new("");
      while (token.type == CSS_TOKEN_TYPE_SYMBOL || token.type == CSS_TOKEN_TYPE_STRING ||
             (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == ',')) {
         if (this->hll_css_parser.spaceSeparatedC)
            dStr_append_c(dstr, ' ');
         dStr_append(dstr, token.value);
         ret = true;
         nextToken(&this->token, &this->hll_css_parser);
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
      if (token.type == CSS_TOKEN_TYPE_DECINT) {
         ival = strtol(token.value, NULL, 10);
         if (ival < 100 || ival > 900)
            /* invalid */
            ival = 0;
      }

      if (ival != 0) {
         val->intVal = ival;
         ret = true;
         nextToken(&this->token, &this->hll_css_parser);
      }
      break;

   case CssPropertyValueDataType::URI:
      if (token.type == CSS_TOKEN_TYPE_SYMBOL &&
          dStrAsciiCasecmp(token.value, "url") == 0) {
         val->strVal = parseUrl();
         nextToken(&this->token, &this->hll_css_parser);
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
            h[i] = token.type != CSS_TOKEN_TYPE_SYMBOL ||
               (dStrAsciiCasecmp(token.value, "top") != 0 &&
                dStrAsciiCasecmp(token.value, "bottom") != 0);
            v[i] = token.type != CSS_TOKEN_TYPE_SYMBOL ||
               (dStrAsciiCasecmp(token.value, "left") != 0 &&
                dStrAsciiCasecmp(token.value, "right") != 0);
         } else
            // No match.
            h[i] = v[i] = false;

         if (h[i] || v[i]) {
            // Calculate values.
            if (token.type == CSS_TOKEN_TYPE_SYMBOL) {
               if (dStrAsciiCasecmp(token.value, "top") == 0 ||
                   dStrAsciiCasecmp(token.value, "left") == 0) {
                  pos[i] = CSS_CREATE_LENGTH (0.0, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&this->token, &this->hll_css_parser);
               } else if (dStrAsciiCasecmp(token.value, "center") == 0) {
                  pos[i] = CSS_CREATE_LENGTH (0.5, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&this->token, &this->hll_css_parser);
               } else if (dStrAsciiCasecmp(token.value, "bottom") == 0 ||
                          dStrAsciiCasecmp(token.value, "right") == 0) {
                  pos[i] = CSS_CREATE_LENGTH (1.0, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&this->token, &this->hll_css_parser);
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
   if (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == '!') {
      nextToken(&this->token, &this->hll_css_parser);
      if (token.type == CSS_TOKEN_TYPE_SYMBOL &&
          dStrAsciiCasecmp(token.value, "important") == 0) {
         nextToken(&this->token, &this->hll_css_parser);
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

   if (token.type == CSS_TOKEN_TYPE_SYMBOL) {
      pi.symbol = token.value;
      pip =
          (CssPropertyInfo *) bsearch(&pi, Css_property_info,
                                      CSS_NUM_PARSED_PROPERTIES,
                                      sizeof(CssPropertyInfo),
                                      Css_property_info_cmp);
      if (pip) {
         prop = (CssPropertyName) (pip - Css_property_info);
         nextToken(&this->token, &this->hll_css_parser);
         if (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == ':') {
            nextToken(&this->token, &this->hll_css_parser);
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
            nextToken(&this->token, &this->hll_css_parser);
            if (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == ':') {
               nextToken(&this->token, &this->hll_css_parser);

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
   while (!(token.type == CSS_TOKEN_TYPE_END ||
            (token.type == CSS_TOKEN_TYPE_CHAR &&
             (token.value[0] == ';' || token.value[0] == '}'))))
      nextToken(&this->token, &this->hll_css_parser);

   if (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == ';')
      nextToken(&this->token, &this->hll_css_parser);
}

bool CssParser::parseSimpleSelector(CssSimpleSelector *selector)
{
   CssSelectorType selectorType;

   if (token.type == CSS_TOKEN_TYPE_SYMBOL) {
      selector->setSelectorElement(a_Html_tag_index(token.value));
      nextToken(&this->token, &this->hll_css_parser);
      if (this->hll_css_parser.spaceSeparatedC)
         return true;
   } else if (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == '*') {
      selector->setSelectorElement(CssSimpleSelector::ELEMENT_ANY);
      nextToken(&this->token, &this->hll_css_parser);
      if (this->hll_css_parser.spaceSeparatedC)
         return true;
   } else if (token.type == CSS_TOKEN_TYPE_CHAR &&
              (token.value[0] == '#' ||
               token.value[0] == '.' ||
               token.value[0] == ':')) {
      // nothing to be done in this case
   } else {
      return false;
   }

   do {
      selectorType = CssSelectorType::NONE;
      if (token.type == CSS_TOKEN_TYPE_CHAR) {
         switch (token.value[0]) {
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
         nextToken(&this->token, &this->hll_css_parser);
         if (this->hll_css_parser.spaceSeparatedC)
            return false;

         if (token.type == CSS_TOKEN_TYPE_SYMBOL) {
            selector->setSelector(selectorType, token.value);
            nextToken(&this->token, &this->hll_css_parser);
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

      if (token.type == CSS_TOKEN_TYPE_CHAR &&
         (token.value[0] == ',' || token.value[0] == '{')) {
         break;
      } else if (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == '>') {
         selector->addSimpleSelector (CssSelector::COMB_CHILD);
         nextToken(&this->token, &this->hll_css_parser);
      } else if (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == '+') {
         selector->addSimpleSelector (CssSelector::COMB_ADJACENT_SIBLING);
         nextToken(&this->token, &this->hll_css_parser);
      } else if (token.type != CSS_TOKEN_TYPE_END && this->hll_css_parser.spaceSeparatedC) {
         selector->addSimpleSelector (CssSelector::COMB_DESCENDANT);
      } else {
         delete selector;
         selector = NULL;
         break;
      }
   }

   while (token.type != CSS_TOKEN_TYPE_END &&
          (token.type != CSS_TOKEN_TYPE_CHAR ||
           (token.value[0] != ',' && token.value[0] != '{')))
         nextToken(&this->token, &this->hll_css_parser);

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

      if (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == ',')
         /* To read the next token. */
         nextToken(&this->token, &this->hll_css_parser);
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
   if (token.type != CSS_TOKEN_TYPE_END) {
      this->hll_css_parser.withinBlockC = true;
      nextToken(&this->token, &this->hll_css_parser);
      do
         parseDeclaration(props, importantProps);
      while (!(token.type == CSS_TOKEN_TYPE_END ||
               (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == '}')));
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

   if (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == '}')
      nextToken(&this->token, &this->hll_css_parser);
}

char * CssParser::parseUrl()
{
   Dstr *urlStr = NULL;

   if (token.type != CSS_TOKEN_TYPE_SYMBOL ||
      dStrAsciiCasecmp(token.value, "url") != 0)
      return NULL;

   nextToken(&this->token, &this->hll_css_parser);

   if (token.type != CSS_TOKEN_TYPE_CHAR || token.value[0] != '(')
      return NULL;

   nextToken(&this->token, &this->hll_css_parser);

   if (token.type == CSS_TOKEN_TYPE_STRING) {
      urlStr = dStr_new(token.value);
      nextToken(&this->token, &this->hll_css_parser);
   } else {
      urlStr = dStr_new("");
      while (token.type != CSS_TOKEN_TYPE_END &&
             (token.type != CSS_TOKEN_TYPE_CHAR || token.value[0] != ')')) {
         dStr_append(urlStr, token.value);
         nextToken(&this->token, &this->hll_css_parser);
      }
   }

   if (token.type != CSS_TOKEN_TYPE_CHAR || token.value[0] != ')') {
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

   nextToken(&this->token, &this->hll_css_parser);

   if (token.type == CSS_TOKEN_TYPE_SYMBOL &&
       dStrAsciiCasecmp(token.value, "url") == 0)
      urlStr = parseUrl();
   else if (token.type == CSS_TOKEN_TYPE_STRING)
      urlStr = dStrdup (token.value);

   nextToken(&this->token, &this->hll_css_parser);

   /* parse a comma-separated list of media */
   if (token.type == CSS_TOKEN_TYPE_SYMBOL) {
      mediaSyntaxIsOK = false;
      mediaIsSelected = false;
      while (token.type == CSS_TOKEN_TYPE_SYMBOL) {
         if (dStrAsciiCasecmp(token.value, "all") == 0 ||
             dStrAsciiCasecmp(token.value, "screen") == 0)
            mediaIsSelected = true;
         nextToken(&this->token, &this->hll_css_parser);
         if (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == ',') {
            nextToken(&this->token, &this->hll_css_parser);
         } else {
            mediaSyntaxIsOK = true;
            break;
         }
      }
   }

   if (mediaSyntaxIsOK &&
       token.type == CSS_TOKEN_TYPE_CHAR &&
       token.value[0] == ';') {
      importSyntaxIsOK = true;
      nextToken(&this->token, &this->hll_css_parser);
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

   nextToken(&this->token, &this->hll_css_parser);

   /* parse a comma-separated list of media */
   while (token.type == CSS_TOKEN_TYPE_SYMBOL) {
      if (dStrAsciiCasecmp(token.value, "all") == 0 ||
          dStrAsciiCasecmp(token.value, "screen") == 0)
         mediaIsSelected = true;
      nextToken(&this->token, &this->hll_css_parser);
      if (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == ',') {
         nextToken(&this->token, &this->hll_css_parser);
      } else {
         mediaSyntaxIsOK = true;
         break;
      }
   }

   /* check that the syntax is OK so far */
   if (!(mediaSyntaxIsOK &&
         token.type == CSS_TOKEN_TYPE_CHAR &&
         token.value[0] == '{')) {
      ignoreStatement();
      return;
   }

   /* parse/ignore the block as required */
   if (mediaIsSelected) {
      nextToken(&this->token, &this->hll_css_parser);
      while (token.type != CSS_TOKEN_TYPE_END) {
         parseRuleset();
         if (token.type == CSS_TOKEN_TYPE_CHAR && token.value[0] == '}') {
            nextToken(&this->token, &this->hll_css_parser);
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

   while (token.type != CSS_TOKEN_TYPE_END) {
      if (token.type == CSS_TOKEN_TYPE_CHAR) {
         if (token.value[0] == '{') {
            depth++;
         } else if (token.value[0] == '}') {
            depth--;
            if (depth == 0) {
               nextToken(&this->token, &this->hll_css_parser);
               return;
            }
         }
      }
      nextToken(&this->token, &this->hll_css_parser);
   }
}

void CssParser::ignoreStatement()
{
   while (token.type != CSS_TOKEN_TYPE_END) {
      if (token.type == CSS_TOKEN_TYPE_CHAR) {
         if (token.value[0] == ';') {
            nextToken(&this->token, &this->hll_css_parser);
            return;
         } else if (token.value[0] =='{') {
            ignoreBlock();
            return;
         }
      }
      nextToken(&this->token, &this->hll_css_parser);
   }
}

void CssParser::parse(DilloHtml *html, const DilloUrl *baseUrl,
                      CssContext *context,
                      const char *buf,
                      int buflen, CssOrigin origin)
{
   CssParser parser (context, origin, baseUrl, buf, buflen);
   bool importsAreAllowed = true;

   while (parser.token.type != CSS_TOKEN_TYPE_END) {
      if (parser.token.type == CSS_TOKEN_TYPE_CHAR &&
          parser.token.value[0] == '@') {
         nextToken(&parser.token, &parser.hll_css_parser);
         if (parser.token.type == CSS_TOKEN_TYPE_SYMBOL) {
            if (dStrAsciiCasecmp(parser.token.value, "import") == 0 &&
                html != NULL &&
                importsAreAllowed) {
               parser.parseImport(html);
            } else if (dStrAsciiCasecmp(parser.token.value, "media") == 0) {
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
   while (!(parser.token.type == CSS_TOKEN_TYPE_END ||
         (parser.token.type == CSS_TOKEN_TYPE_CHAR && parser.token.value[0] == '}')));
}
