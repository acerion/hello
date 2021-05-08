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

typedef struct {
   const char *symbol;
   const CssDeclarationValueType accepted_value_type[3];
   const char *const *enum_symbols;
} CssPropertyInfo;

typedef struct {
   const char *symbol;
   enum {
      CSS_SHORTHAND_MULTIPLE,   /* [ p1 || p2 || ...], the property pi is
                                 * determined  by the type */
      CSS_SHORTHAND_DIRECTIONS, /* <t>{1,4} */
      CSS_SHORTHAND_BORDER,     /* special, used for 'border' */
      CSS_SHORTHAND_FONT,       /* special, used for 'font' */
   } type;
   const CssDeclarationProperty *properties; /* CSS_SHORTHAND_MULTIPLE:
                                              *   must be terminated by
                                              *   CSS_PROPERTY_END 
                                              * CSS_SHORTHAND_DIRECTIONS:
                                              *   must have length 4
                                              * CSS_SHORTHAND_BORDERS:
                                              *   must have length 12
                                              * CSS_SHORTHAND_FONT:
                                              *   unused */
} CssShorthandInfo;

const CssDeclarationProperty Css_background_properties[] = {
   CSS_PROPERTY_BACKGROUND_COLOR,
   CSS_PROPERTY_BACKGROUND_IMAGE,
   CSS_PROPERTY_BACKGROUND_REPEAT,
   CSS_PROPERTY_BACKGROUND_ATTACHMENT,
   CSS_PROPERTY_BACKGROUND_POSITION,
   CSS_PROPERTY_END
};

const CssDeclarationProperty Css_border_bottom_properties[] = {
   CSS_PROPERTY_BORDER_BOTTOM_WIDTH,
   CSS_PROPERTY_BORDER_BOTTOM_STYLE,
   CSS_PROPERTY_BORDER_BOTTOM_COLOR,
   CSS_PROPERTY_END
};

const CssDeclarationProperty Css_border_color_properties[4] = {
   CSS_PROPERTY_BORDER_TOP_COLOR,
   CSS_PROPERTY_BORDER_BOTTOM_COLOR,
   CSS_PROPERTY_BORDER_LEFT_COLOR,
   CSS_PROPERTY_BORDER_RIGHT_COLOR
};

const CssDeclarationProperty Css_border_left_properties[] = {
   CSS_PROPERTY_BORDER_LEFT_WIDTH,
   CSS_PROPERTY_BORDER_LEFT_STYLE,
   CSS_PROPERTY_BORDER_LEFT_COLOR,
   CSS_PROPERTY_END
};

const CssDeclarationProperty Css_border_right_properties[] = {
   CSS_PROPERTY_BORDER_RIGHT_WIDTH,
   CSS_PROPERTY_BORDER_RIGHT_STYLE,
   CSS_PROPERTY_BORDER_RIGHT_COLOR,
   CSS_PROPERTY_END
};

const CssDeclarationProperty Css_border_style_properties[] = {
   CSS_PROPERTY_BORDER_TOP_STYLE,
   CSS_PROPERTY_BORDER_BOTTOM_STYLE,
   CSS_PROPERTY_BORDER_LEFT_STYLE,
   CSS_PROPERTY_BORDER_RIGHT_STYLE
};

const CssDeclarationProperty Css_border_top_properties[] = {
   CSS_PROPERTY_BORDER_TOP_WIDTH,
   CSS_PROPERTY_BORDER_TOP_STYLE,
   CSS_PROPERTY_BORDER_TOP_COLOR,
   CSS_PROPERTY_END
};

const CssDeclarationProperty Css_border_width_properties[] = {
   CSS_PROPERTY_BORDER_TOP_WIDTH,
   CSS_PROPERTY_BORDER_BOTTOM_WIDTH,
   CSS_PROPERTY_BORDER_LEFT_WIDTH,
   CSS_PROPERTY_BORDER_RIGHT_WIDTH
};

const CssDeclarationProperty Css_list_style_properties[] = {
   CSS_PROPERTY_LIST_STYLE_TYPE,
   CSS_PROPERTY_LIST_STYLE_POSITION,
   CSS_PROPERTY_LIST_STYLE_IMAGE,
   CSS_PROPERTY_END
};

const CssDeclarationProperty Css_margin_properties[] = {
   CSS_PROPERTY_MARGIN_TOP,
   CSS_PROPERTY_MARGIN_BOTTOM,
   CSS_PROPERTY_MARGIN_LEFT,
   CSS_PROPERTY_MARGIN_RIGHT
};

const CssDeclarationProperty Css_outline_properties[] = {
   CSS_PROPERTY_OUTLINE_COLOR,
   CSS_PROPERTY_OUTLINE_STYLE,
   CSS_PROPERTY_OUTLINE_WIDTH,
   CSS_PROPERTY_END
};

const CssDeclarationProperty Css_padding_properties[] = {
   CSS_PROPERTY_PADDING_TOP,
   CSS_PROPERTY_PADDING_BOTTOM,
   CSS_PROPERTY_PADDING_LEFT,
   CSS_PROPERTY_PADDING_RIGHT
};

const CssDeclarationProperty Css_border_properties[] = {
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

const CssDeclarationProperty Css_font_properties[] = {
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

#define CSS_SHORTHAND_NUM (sizeof(Css_shorthand_info) / sizeof(Css_shorthand_info[0]))

void tokenizerPrintCurrentToken(CssTokenizer * tokenizer);
const char * tokenizerGetTokenTypeStr(CssTokenizer * tokenizer);
void nextToken(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser);
CssDeclarationValueType tokenMatchesProperty(CssDeclarationProperty property, const char * tokenValue, int tokenType);
void ignoreBlock(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser);
void ignoreStatement(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser);

static void parseDeclarationWrapper(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant);
static bool parseDeclarationNormal(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant);
static void parseDeclarationShorthands(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant);


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
   case CSS_TOKEN_TYPE_WHITESPACE:
      typeStr = "whitespace";
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
   fprintf(stderr, "Current token: '%s' = '%s'\n", tokenizerGetTokenTypeStr(tokenizer), tokenizer->value);
}


/* ----------------------------------------------------------------------
 *    Parsing
 * ---------------------------------------------------------------------- */

CssParser::CssParser(CssContext *context, CssOrigin origin,
                     const DilloUrl *baseUrl,
                     const char *buf, int buflen)
{
   this->context_ = context;
   this->origin = origin;
   this->tokenizer.buf = buf;
   this->tokenizer.buflen = buflen;
   this->tokenizer.bufOffset = 0;
   this->hll_css_parser.c_within_block = false;
   this->hll_css_parser.c_space_separated = false;
   this->hll_css_parser.c_buf_offset = 0;
   this->baseUrl = baseUrl;

   nextToken(&this->tokenizer, &this->hll_css_parser);
}


static FILE * g_file = NULL;
void nextTokenInner2(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser);
void nextToken(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser)
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


#if 0
   const char * type = tokenizerGetTokenTypeStr(tokenizer);
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

void nextTokenInner2(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser)
{
#if 0
   fprintf(stderr, "before:\n");
   fprintf(stderr, "hll_css_parser->c_space_separated = %d\n", hll_css_parser->c_space_separated);
   fprintf(stderr, "hll_css_parser->c_buf_offset      = %d\n", hll_css_parser->c_buf_offset);
   fprintf(stderr, "hll_css_parser->c_token_type      = %d\n", hll_css_parser->c_token_type);
   fprintf(stderr, "hll_css_parser->c_within_block    = %d\n", hll_css_parser->c_within_block);
#endif
   char * tokenValue = hll_nextToken(hll_css_parser, tokenizer->buf + tokenizer->bufOffset);
#if 0
   fprintf(stderr, "after:\n");
   fprintf(stderr, "hll_css_parser->c_space_separated = %d\n", hll_css_parser->c_space_separated);
   fprintf(stderr, "hll_css_parser->c_buf_offset      = %d\n", hll_css_parser->c_buf_offset);
   fprintf(stderr, "hll_css_parser->c_token_type      = %d\n", hll_css_parser->c_token_type);
   fprintf(stderr, "hll_css_parser->c_within_block    = %d\n", hll_css_parser->c_within_block);
   fprintf(stderr, "\n");
#endif

   tokenizer->bufOffset = hll_css_parser->c_buf_offset;

   if (NULL == tokenValue) {
      tokenizer->type = CSS_TOKEN_TYPE_END;
      tokenizer->value[0] = '\0';
   } else {
      tokenizer->type = (CssTokenType) hll_css_parser->c_token_type;
      snprintf(tokenizer->value, sizeof (tokenizer->value), "%s", tokenValue);
   }
}

CssDeclarationValueType tokenMatchesProperty(CssDeclarationProperty property, const char * tokenValue, int tokenType)
{
   const int hll_valueType = hll_tokenMatchesProperty(tokenType, tokenValue, property);
   if (-1 == hll_valueType) {
      return CssDeclarationValueTypeUNUSED;
   } else {
      return (CssDeclarationValueType) hll_valueType;
   }
}

bool parseDeclarationValue(CssParser * parser,
                           CssDeclarationProperty property,
                           CssDeclarationValueType valueType,
                           CssDeclarationValue * value)
{
   bool ret = false;

   switch (valueType) {
   case CssDeclarationValueTypeENUM:
   case CssDeclarationValueTypeCOLOR:
   case CssDeclarationValueTypeFONT_WEIGHT:
   case CssDeclarationValueTypeMULTI_ENUM:
   case CssDeclarationValueTypeAUTO:
   case CssDeclarationValueTypeLENGTH_PERCENTAGE:
   case CssDeclarationValueTypeLENGTH_PERCENTAGE_NUMBER:
   case CssDeclarationValueTypeLENGTH:
   case CssDeclarationValueTypeSIGNED_LENGTH:
   case CssDeclarationValueTypeSTRING:
   case CssDeclarationValueTypeSYMBOL:
   case CssDeclarationValueTypeURI:
      {
         c_css_declaration_value_t val;
         memset(&val, 0, sizeof (val));

         if (hll_parseDeclarationValue(&parser->hll_css_parser,
                                       parser->tokenizer.type,
                                       parser->tokenizer.value,
                                       parser->tokenizer.buf + parser->tokenizer.bufOffset,
                                       valueType,
                                       property,
                                       &val)) {

            // Property found. Value found and valid. Declaration parsed.
            value->type   = (CssDeclarationValueType) val.c_type_tag;
            value->intVal = val.c_int_val;
            value->strVal = val.c_text_val;
            ret = true;
         }

         // This takes token that is a semicolon after declaration value.
         // TODO: consider if this should be called only on success of
         // parsing value, or regardless of success/failure.
         nextToken(&parser->tokenizer, &parser->hll_css_parser);
      }
      break;
   case CssDeclarationValueTypeBACKGROUND_POSITION:
      {
      // 'background-position' consists of one or two values: vertical and
      // horizontal position; in most cases in this order. However, as long it
      // is unambigous, the order can be switched: "10px left" and "left 10px"
      // are both possible and have the same effect. For this reason, all
      // possibilities are tested in parallel.

         struct pos {
            CssLength cssLength;
            bool as_h;
            bool as_v;
         };

         struct pos positions[2] =
            {
             {0, false, false},
             {0, false, false}
            };

      // First: collect values in positions[0].value and positions[1].value, and determine whether
      // they can be used for a horizontal (positions[i].as_h) or vertical (positions[i].as_v) position
      // (or both). When neither positions[i].as_h or positions[i].as_v is set, positions[i].value is undefined.
      for (int i = 0; i < 2; i++) {
         CssDeclarationValueType typeTmp = tokenMatchesProperty(CSS_PROPERTY_BACKGROUND_POSITION, parser->tokenizer.value, parser->tokenizer.type);
         // tokenMatchesProperty will, for CSS_PROPERTY_BACKGROUND_POSITION,
         // work on both parts, since they are exchangable.
         if (CssDeclarationValueTypeUNUSED != typeTmp) {
            positions[i].as_h = parser->tokenizer.type != CSS_TOKEN_TYPE_SYMBOL || (dStrAsciiCasecmp(parser->tokenizer.value, "top") != 0  && dStrAsciiCasecmp(parser->tokenizer.value, "bottom") != 0);
            positions[i].as_v = parser->tokenizer.type != CSS_TOKEN_TYPE_SYMBOL || (dStrAsciiCasecmp(parser->tokenizer.value, "left") != 0 && dStrAsciiCasecmp(parser->tokenizer.value, "right") != 0);
            fprintf(stderr, "POSITION %s:%d: '%s' %d %d\n", __func__, __LINE__, parser->tokenizer.value, positions[i].as_h, positions[i].as_v);
         } else {
            // No match.
            positions[i].as_h = positions[i].as_v = false;
         }

         if (positions[i].as_h || positions[i].as_v) {
            // Calculate values.
            if (parser->tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
               if (dStrAsciiCasecmp(parser->tokenizer.value, "top") == 0 || dStrAsciiCasecmp(parser->tokenizer.value, "left") == 0) {
                  positions[i].cssLength = cssCreateLength(0.0, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&parser->tokenizer, &parser->hll_css_parser);
               } else if (dStrAsciiCasecmp(parser->tokenizer.value, "center") == 0) {
                  positions[i].cssLength = cssCreateLength(0.5, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&parser->tokenizer, &parser->hll_css_parser);
               } else if (dStrAsciiCasecmp(parser->tokenizer.value, "bottom") == 0 || dStrAsciiCasecmp(parser->tokenizer.value, "right") == 0) {
                  positions[i].cssLength = cssCreateLength(1.0, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&parser->tokenizer, &parser->hll_css_parser);
               } else
                  // tokenMatchesProperty should have returned CssDeclarationValueTypeUNUSED already.
                  lout::misc::assertNotReached ();
            } else {
               // We can assume <length> or <percentage> here ...
               CssDeclarationValue valTmp;
               if (parseDeclarationValue(parser, property, CssDeclarationValueTypeLENGTH_PERCENTAGE, &valTmp)) {
                  positions[i].cssLength.bits = valTmp.intVal;
                  ret = true;
               } else
                  // ... but something may still fail.
                  positions[i].as_h = positions[i].as_v = false;
            }
         }

         // If the first value cannot be read, do not read the second.
         if (!positions[i].as_h && !positions[i].as_v)
            break;
      }

      // Second: Create the final value. Order will be determined here.
      if (positions[0].as_v || positions[0].as_h) {
         // If second value is not set, it is set to "center", i. e. 50%, (see
         // CSS specification), which is suitable for both dimensions.
         if (!positions[1].as_h && !positions[1].as_v) {
            positions[1].cssLength = cssCreateLength(0.5, CSS_LENGTH_TYPE_PERCENTAGE);
            positions[1].as_h = positions[1].as_v = true;
         }

         // Only valid, when a combination h/v or v/h is possible.
         if ((positions[0].as_h && positions[1].as_v) || (positions[0].as_v && positions[1].as_h)) {
            ret = true;
            fprintf(stderr, "POSITION\n");

            // Prefer combination h/v:
            if (positions[0].as_h && positions[1].as_v) {
               value->posVal.posX = positions[0].cssLength.bits;
               value->posVal.posY = positions[1].cssLength.bits;
            } else {
               // This should be v/h:
               value->posVal.posX = positions[1].cssLength.bits;
               value->posVal.posY = positions[0].cssLength.bits;
            }
         }
      }
      }
      break;

   case CssDeclarationValueTypeUNUSED:
      /* nothing */
      break;

   case CssDeclarationValueTypeINTEGER:
      /* Not used for parser values. */
   default:
      assert(false);            /* not reached */
   }

   return ret;
}

bool CssParser::parseWeight()
{
   int ival = hll_cssParseWeight(&this->hll_css_parser,
                                 tokenizer.type,
                                 tokenizer.value,
                                 this->tokenizer.buf + this->tokenizer.bufOffset);
   this->tokenizer.bufOffset = this->hll_css_parser.c_buf_offset;
   return ival;
}


/*
 * bsearch(3) compare function for searching shorthands
 */
static int Css_shorthand_info_cmp(const void *a, const void *b)
{
   return dStrAsciiCasecmp(((CssShorthandInfo *) a)->symbol,
                      ((CssShorthandInfo *) b)->symbol);
}

void parseDeclarationWrapper(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant)
{
      if (parser->tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
         if (!parseDeclarationNormal(parser, declList, declListImportant)) {
            parseDeclarationShorthands(parser, declList, declListImportant);
         }
      }

      /* Skip all tokens until the expected end. */
      while (!(parser->tokenizer.type == CSS_TOKEN_TYPE_END ||
            (parser->tokenizer.type == CSS_TOKEN_TYPE_CHAR &&
             (parser->tokenizer.value[0] == ';' || parser->tokenizer.value[0] == '}'))))
         nextToken(&parser->tokenizer, &parser->hll_css_parser);

      if (parser->tokenizer.type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.value[0] == ';')
         nextToken(&parser->tokenizer, &parser->hll_css_parser);
}

bool parseDeclarationNormal(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant)
{
#if 1
   c_css_declaration_value_t value;
   memset(&value, 0, sizeof (value));
   int rv = hll_parseDeclarationNormal(&parser->hll_css_parser,
                                       parser->tokenizer.type, parser->tokenizer.value,
                                       parser->tokenizer.buf + parser->tokenizer.bufOffset,
                                       &value);

   if (rv >= 0) {
      // Property found. Value found and valid. Declaration parsed.
      CssDeclarationValue val;
      val.type   = (CssDeclarationValueType) value.c_type_tag;
      val.intVal = value.c_int_val;
      val.strVal = value.c_text_val;

      if (value.c_important) {
         declarationListAddOrUpdateDeclaration(declListImportant, (CssDeclarationProperty) rv, val);
      } else {
         declarationListAddOrUpdateDeclaration(declList, (CssDeclarationProperty) rv, val);
      }
      return true;
   } else if (rv == -1) {
      // Property not found. Try to find the property among shortcuts.
      return false;
   } else {
      // Property found, but some error occurred during parsing. Skip this declaration.
      return true;
   }
#else
   const int idx = hll_cssPropertyInfoIdxByName(parser->tokenizer.value);
   if (-1 != idx) {
      CssDeclarationProperty property = (CssDeclarationProperty) idx;
      nextToken(&parser->tokenizer, &parser->hll_css_parser);
      if (parser->tokenizer.type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.value[0] == ':') {
         nextToken(&parser->tokenizer, &parser->hll_css_parser);

         CssDeclarationValue val;
         CssDeclarationValueType type = tokenMatchesProperty(property, parser->tokenizer.value, parser->tokenizer.type);
         if (CssDeclarationValueTypeUNUSED != type && parseDeclarationValue(parser, property, type, &val)) {

            const bool weight = parser->parseWeight();

            val.type = type;
            if (weight)
               declarationListAddOrUpdateDeclaration(declListImportant, property, val);
            else
               declarationListAddOrUpdateDeclaration(declList, property, val);
         }
      }
      return true;
   } else {
      return false;
   }
#endif
}

void parseDeclarationShorthands(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant)
{
   /* Try shorthands. */
   CssPropertyInfo pi = { .symbol = parser->tokenizer.value, {CssDeclarationValueTypeUNUSED}, NULL};
   CssShorthandInfo * sip = (CssShorthandInfo *) bsearch(&pi, Css_shorthand_info,
                                                         CSS_SHORTHAND_NUM,
                                                         sizeof(CssShorthandInfo),
                                                         Css_shorthand_info_cmp);
   if (sip) {
      const int sh_index = sip - Css_shorthand_info;
      const CssShorthandInfo * shinfo = &Css_shorthand_info[sh_index];
      nextToken(&parser->tokenizer, &parser->hll_css_parser);
      if (parser->tokenizer.type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.value[0] == ':') {
         nextToken(&parser->tokenizer, &parser->hll_css_parser);

         switch (shinfo->type) {

         case CssShorthandInfo::CSS_SHORTHAND_FONT:
            /* \todo Implement details. */
         case CssShorthandInfo::CSS_SHORTHAND_MULTIPLE: {
            bool found;
            do {
               int i = 0;
               for (found = false, i = 0; !found && shinfo->properties[i] != CSS_PROPERTY_END; i++) {

                  CssDeclarationValue val;
                  CssDeclarationValueType type = tokenMatchesProperty(shinfo->properties[i], parser->tokenizer.value, parser->tokenizer.type);
                  if (CssDeclarationValueTypeUNUSED != type) {
                     found = true;
                     DEBUG_MSG(DEBUG_PARSE_LEVEL, "will assign to '%s'\n", hll_cssPropertyNameString(shinfo->properties[i]));
                     if (parseDeclarationValue(parser, shinfo->properties[i], type, &val)) {

                        const bool weight = parser->parseWeight();
                        val.type = type;
                        if (weight)
                           declarationListAddOrUpdateDeclaration(declListImportant, shinfo->properties[i], val);
                        else
                           declarationListAddOrUpdateDeclaration(declList, shinfo->properties[i], val);
                     }
                  }
                     }
            } while (found);
         }
            break;

         case CssShorthandInfo::CSS_SHORTHAND_DIRECTIONS: {
            int n = 0;

            CssDeclarationValue values[4];
            CssDeclarationValueType types[4];
            while (n < 4) {
               CssDeclarationValue val;
               CssDeclarationValueType type = tokenMatchesProperty(shinfo->properties[0], parser->tokenizer.value, parser->tokenizer.type);
               if (type != CssDeclarationValueTypeUNUSED && parseDeclarationValue(parser, shinfo->properties[0], type, &val)) {
                  values[n] = val;
                  types[n] = type;
                  n++;
               } else
                  break;
            }

            const bool weight = parser->parseWeight();
            if (n > 0) {
               int dir_set[4][4] = {
                                    /* 1 value  */ {0, 0, 0, 0},
                                    /* 2 values */ {0, 0, 1, 1},
                                    /* 3 values */ {0, 2, 1, 1},
                                    /* 4 values */ {0, 2, 3, 1}
               };
               for (int i = 0; i < 4; i++) {
                  const int set_idx = dir_set[n - 1][i];
                  values[set_idx].type = types[set_idx];

                  if (weight)
                     declarationListAddOrUpdateDeclaration(declListImportant, shinfo->properties[i], values[set_idx]);
                  else
                     declarationListAddOrUpdateDeclaration(declList, shinfo->properties[i], values[set_idx]);
               }
            } else
               MSG_CSS("no values for shorthand property '%s'\n", shinfo->symbol);
         }
            break;

         case CssShorthandInfo::CSS_SHORTHAND_BORDER: {
            bool found;
            do {
               int i = 0;
               for (found = false, i = 0; !found && i < 3; i++) {
                  CssDeclarationValue val;
                  CssDeclarationValueType type = tokenMatchesProperty(shinfo->properties[i], parser->tokenizer.value, parser->tokenizer.type);
                  if (CssDeclarationValueTypeUNUSED != type) {
                     found = true;
                     if (parseDeclarationValue(parser, shinfo->properties[i], type, &val)) {
                        const bool weight = parser->parseWeight();
                        for (int j = 0; j < 4; j++) {

                           val.type = type;
                           if (weight)
                              declarationListAddOrUpdateDeclaration(declListImportant, shinfo->properties[j * 3 + i], val);
                           else
                              declarationListAddOrUpdateDeclaration(declList, shinfo->properties[j * 3 + i], val);
                        }
                     }
                  }
               }
            } while (found);
         }
            break;
         }
      }
   }
}

c_css_selector_t * parseSelector(CssParser * cssParser)
{
   c_css_selector_t * selector = hll_cssParseSelector(&cssParser->hll_css_parser,
                                                      cssParser->tokenizer.type, cssParser->tokenizer.value,
                                                      cssParser->tokenizer.buf + cssParser->tokenizer.bufOffset);
   cssParser->tokenizer.bufOffset = cssParser->hll_css_parser.c_buf_offset;
   snprintf(cssParser->tokenizer.value, sizeof (cssParser->tokenizer.value), "%s", cssParser->hll_css_parser.c_token_value);
   cssParser->tokenizer.type = (CssTokenType) cssParser->hll_css_parser.c_token_type;

   while (cssParser->tokenizer.type != CSS_TOKEN_TYPE_END &&
          (cssParser->tokenizer.type != CSS_TOKEN_TYPE_CHAR ||
           (cssParser->tokenizer.value[0] != ',' && cssParser->tokenizer.value[0] != '{')))
         nextToken(&cssParser->tokenizer, &cssParser->hll_css_parser);

   return selector;
}

void parseRuleset(CssParser * parser, CssContext * context)
{
   c_css_selector_t * selectors[100] = { 0 };
   int selectors_count = 0;

   while (true) {
      c_css_selector_t * selector = parseSelector(parser);
      if (nullptr != selector) {
         selectors[selectors_count] = selector;
         selectors_count++;
      }

      // \todo dump whole ruleset in case of parse error as required by CSS 2.1
      //       however make sure we don't dump it if only dillo fails to parse
      //       valid CSS.

      if (parser->tokenizer.type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.value[0] == ',')
         /* To read the next selector. */
         nextToken(&parser->tokenizer, &parser->hll_css_parser);
      else
         /* No more selectors. */
         break;
   }

   DEBUG_MSG(DEBUG_PARSE_LEVEL, "end of %s\n", "selectors");

   CssDeclartionList * declList = new CssDeclartionList();
   CssDeclartionList * declListImportant = new CssDeclartionList();

   /* Read block. ('{' has already been read.) */
   if (parser->tokenizer.type != CSS_TOKEN_TYPE_END) {
      parser->hll_css_parser.c_within_block = true;
      nextToken(&parser->tokenizer, &parser->hll_css_parser);
      do {
         parseDeclarationWrapper(parser, declList, declListImportant);
      } while (!(parser->tokenizer.type == CSS_TOKEN_TYPE_END ||
               (parser->tokenizer.type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.value[0] == '}')));
      parser->hll_css_parser.c_within_block = false;
   }

   for (int i = 0; i < selectors_count; i++) {
      c_css_selector_t * sel = selectors[i];

      if (parser->origin == CSS_ORIGIN_USER_AGENT) {
         if (declList->declarations_count > 0) {
            CssRule * rule = new CssRule(sel, declList, context->rulePosition++);
            addRuleToContext(context, rule, CSS_PRIMARY_USER_AGENT);
         }
      } else if (parser->origin == CSS_ORIGIN_USER) {
         if (declList->declarations_count > 0) {
            CssRule * rule = new CssRule(sel, declList, context->rulePosition++);
            addRuleToContext(context, rule, CSS_PRIMARY_USER);
         }
         if (declListImportant->declarations_count > 0) {
            CssRule * rule = new CssRule(sel, declListImportant, context->rulePosition++);
            addRuleToContext(context, rule, CSS_PRIMARY_USER_IMPORTANT);
         }
      } else if (parser->origin == CSS_ORIGIN_AUTHOR) {
         if (declList->declarations_count > 0) {
            CssRule * rule = new CssRule(sel, declList, context->rulePosition++);
            addRuleToContext(context, rule, CSS_PRIMARY_AUTHOR);
         }
         if (declListImportant->declarations_count > 0) {
            CssRule * rule = new CssRule(sel, declListImportant, context->rulePosition++);
            addRuleToContext(context, rule, CSS_PRIMARY_AUTHOR_IMPORTANT);
         }
      }
   }


   if (parser->tokenizer.type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.value[0] == '}')
      nextToken(&parser->tokenizer, &parser->hll_css_parser);
}

char * CssParser::parseUrl()
{
   char * str = hll_declarationValueAsString(&this->hll_css_parser,
                                             tokenizer.type,
                                             tokenizer.value,
                                             this->tokenizer.buf + this->tokenizer.bufOffset,
                                             0, 0);
   this->tokenizer.bufOffset = this->hll_css_parser.c_buf_offset;
   snprintf(tokenizer.value, sizeof (tokenizer.value), "%s", this->hll_css_parser.c_token_value);
   tokenizer.type = (CssTokenType) this->hll_css_parser.c_token_type;


   return str;
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
      ignoreStatement(&this->tokenizer, &this->hll_css_parser);

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
      ignoreStatement(&this->tokenizer, &this->hll_css_parser);
      return;
   }

   /* parse/ignore the block as required */
   if (mediaIsSelected) {
      nextToken(&this->tokenizer, &this->hll_css_parser);
      while (tokenizer.type != CSS_TOKEN_TYPE_END) {
         parseRuleset(this, this->context_);
         if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '}') {
            nextToken(&this->tokenizer, &this->hll_css_parser);
            break;
         }
      }
   } else
      ignoreBlock(&this->tokenizer, &this->hll_css_parser);
}

void ignoreBlock(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser)
{
   hll_ignoreBlock(hll_css_parser, tokenizer->buf + tokenizer->bufOffset);
   tokenizer->bufOffset = hll_css_parser->c_buf_offset;
   snprintf(tokenizer->value, sizeof (tokenizer->value), "%s", hll_css_parser->c_token_value);
   tokenizer->type = (CssTokenType) hll_css_parser->c_token_type;

}

void ignoreStatement(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser)
{
   hll_ignoreStatement(hll_css_parser, tokenizer->buf + tokenizer->bufOffset);
   tokenizer->bufOffset = hll_css_parser->c_buf_offset;
   snprintf(tokenizer->value, sizeof (tokenizer->value), "%s", hll_css_parser->c_token_value);
   tokenizer->type = (CssTokenType) hll_css_parser->c_token_type;

}

void CssParser::parse(DilloHtml *html, const DilloUrl *baseUrl,
                      CssContext *context,
                      const char *buf,
                      int buflen, CssOrigin origin)
{
   CssParser parser (context, origin, baseUrl, buf, buflen);
   bool importsAreAllowed = true;

   fprintf(stderr, "ZZZZ %s\n", buf);

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
               ignoreStatement(&parser.tokenizer, &parser.hll_css_parser);
            }
         } else {
            ignoreStatement(&parser.tokenizer, &parser.hll_css_parser);
         }
      } else {
         importsAreAllowed = false;
         parseRuleset(&parser, parser.context_);
      }
   }
}

/* Parse CSS style information contained in "cssStyleAttribute". The buffer
   contains value of "style" attribute of a html element. */
void CssParser::parseElementStyleAttribute(const DilloUrl *baseUrl,
                                           const char * cssStyleAttribute, int buflen,
                                           CssDeclartionList * declList,
                                           CssDeclartionList * declListImportant)
{
   CssParser parser(NULL, CSS_ORIGIN_AUTHOR, baseUrl, cssStyleAttribute, buflen);

   parser.hll_css_parser.c_within_block = true;

   do {
      parseDeclarationWrapper(&parser, declList, declListImportant);
   } while (!(parser.tokenizer.type == CSS_TOKEN_TYPE_END || (parser.tokenizer.type == CSS_TOKEN_TYPE_CHAR && parser.tokenizer.value[0] == '}')));
}
