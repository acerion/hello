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

typedef enum {
              CSS_SHORTHAND_MULTIPLE,   /* [ p1 || p2 || ...], the property pi is determined  by the type; array of properties must be terminated by CSS_PROPERTY_END. */
              CSS_SHORTHAND_DIRECTIONS, /* <t>{1,4}; array of properties must have length 4. */
              CSS_SHORTHAND_BORDER,     /* special, used for 'border'; array of properties must have length 12. */
              CSS_SHORTHAND_FONT        /* special, used for 'font' */
} css_shorthand_t;


typedef struct {
   const char *symbol;
   css_shorthand_t type;
   const CssDeclarationProperty properties[12];
} CssShorthandInfo;


static const CssShorthandInfo Css_shorthand_info[] =
   {
   {"background",     CSS_SHORTHAND_MULTIPLE,      { CSS_PROPERTY_BACKGROUND_COLOR, CSS_PROPERTY_BACKGROUND_IMAGE, CSS_PROPERTY_BACKGROUND_REPEAT,
                                                     CSS_PROPERTY_BACKGROUND_ATTACHMENT, CSS_PROPERTY_BACKGROUND_POSITION, CSS_PROPERTY_END } },

   {"border",         CSS_SHORTHAND_BORDER,        { CSS_PROPERTY_BORDER_TOP_WIDTH,    CSS_PROPERTY_BORDER_TOP_STYLE,     CSS_PROPERTY_BORDER_TOP_COLOR,
                                                     CSS_PROPERTY_BORDER_BOTTOM_WIDTH, CSS_PROPERTY_BORDER_BOTTOM_STYLE,  CSS_PROPERTY_BORDER_BOTTOM_COLOR,
                                                     CSS_PROPERTY_BORDER_LEFT_WIDTH,   CSS_PROPERTY_BORDER_LEFT_STYLE,    CSS_PROPERTY_BORDER_LEFT_COLOR,
                                                     CSS_PROPERTY_BORDER_RIGHT_WIDTH,  CSS_PROPERTY_BORDER_RIGHT_STYLE,   CSS_PROPERTY_BORDER_RIGHT_COLOR } },

   {"border-bottom",  CSS_SHORTHAND_MULTIPLE,      { CSS_PROPERTY_BORDER_BOTTOM_WIDTH, CSS_PROPERTY_BORDER_BOTTOM_STYLE,  CSS_PROPERTY_BORDER_BOTTOM_COLOR, CSS_PROPERTY_END } },
   {"border-color",   CSS_SHORTHAND_DIRECTIONS,    { CSS_PROPERTY_BORDER_TOP_COLOR,    CSS_PROPERTY_BORDER_BOTTOM_COLOR,  CSS_PROPERTY_BORDER_LEFT_COLOR,   CSS_PROPERTY_BORDER_RIGHT_COLOR } },
   {"border-left",    CSS_SHORTHAND_MULTIPLE,      { CSS_PROPERTY_BORDER_LEFT_WIDTH,   CSS_PROPERTY_BORDER_LEFT_STYLE,    CSS_PROPERTY_BORDER_LEFT_COLOR,   CSS_PROPERTY_END } },
   {"border-right",   CSS_SHORTHAND_MULTIPLE,      { CSS_PROPERTY_BORDER_RIGHT_WIDTH,  CSS_PROPERTY_BORDER_RIGHT_STYLE,   CSS_PROPERTY_BORDER_RIGHT_COLOR,  CSS_PROPERTY_END } },
   {"border-style",   CSS_SHORTHAND_DIRECTIONS,    { CSS_PROPERTY_BORDER_TOP_STYLE,    CSS_PROPERTY_BORDER_BOTTOM_STYLE,  CSS_PROPERTY_BORDER_LEFT_STYLE,   CSS_PROPERTY_BORDER_RIGHT_STYLE } },
   {"border-top",     CSS_SHORTHAND_MULTIPLE,      { CSS_PROPERTY_BORDER_TOP_WIDTH,    CSS_PROPERTY_BORDER_TOP_STYLE,     CSS_PROPERTY_BORDER_TOP_COLOR,    CSS_PROPERTY_END } },

   {"border-width",   CSS_SHORTHAND_DIRECTIONS,    { CSS_PROPERTY_BORDER_TOP_WIDTH,    CSS_PROPERTY_BORDER_BOTTOM_WIDTH, CSS_PROPERTY_BORDER_LEFT_WIDTH,   CSS_PROPERTY_BORDER_RIGHT_WIDTH } },

   {"font",           CSS_SHORTHAND_FONT,          { CSS_PROPERTY_FONT_SIZE,  CSS_PROPERTY_FONT_STYLE, CSS_PROPERTY_FONT_VARIANT, CSS_PROPERTY_FONT_WEIGHT, CSS_PROPERTY_FONT_FAMILY, CSS_PROPERTY_END } },

   {"list-style",     CSS_SHORTHAND_MULTIPLE,      { CSS_PROPERTY_LIST_STYLE_TYPE, CSS_PROPERTY_LIST_STYLE_POSITION, CSS_PROPERTY_LIST_STYLE_IMAGE, CSS_PROPERTY_END } },
   {"margin",         CSS_SHORTHAND_DIRECTIONS,    { CSS_PROPERTY_MARGIN_TOP, CSS_PROPERTY_MARGIN_BOTTOM, CSS_PROPERTY_MARGIN_LEFT, CSS_PROPERTY_MARGIN_RIGHT } },
   {"outline",        CSS_SHORTHAND_MULTIPLE,      { CSS_PROPERTY_OUTLINE_COLOR, CSS_PROPERTY_OUTLINE_STYLE, CSS_PROPERTY_OUTLINE_WIDTH, CSS_PROPERTY_END } },

   {"padding",        CSS_SHORTHAND_DIRECTIONS,    { CSS_PROPERTY_PADDING_TOP, CSS_PROPERTY_PADDING_BOTTOM, CSS_PROPERTY_PADDING_LEFT, CSS_PROPERTY_PADDING_RIGHT } },
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
static void parseDeclarationMultiple(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant, const CssDeclarationProperty properties[12]);
static void parseDeclarationShorthands(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant);


const char * tokenizerGetTokenTypeStr(CssTokenizer * tokenizer)
{
   const char * typeStr = NULL;

   switch (tokenizer->token.c_type) {
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
   fprintf(stderr, "Current token: '%s' = '%s'\n", tokenizerGetTokenTypeStr(tokenizer), tokenizer->token.c_value);
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
   this->hll_css_parser.c_in_block = false;
   this->hll_css_parser.c_space_separated = false;
   this->hll_css_parser.c_buf_offset = 0;
   this->baseUrl = baseUrl;

   nextToken(&this->tokenizer, &this->hll_css_parser);
}


void nextToken(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser)
{
#if 0
   fprintf(stderr, "before:\n");
   fprintf(stderr, "hll_css_parser->c_space_separated = %d\n", hll_css_parser->c_space_separated);
   fprintf(stderr, "hll_css_parser->c_buf_offset      = %d\n", hll_css_parser->c_buf_offset);
   fprintf(stderr, "hll_css_parser->c_in_block        = %d\n", hll_css_parser->c_in_block);
#endif
   char * tokenValue = hll_nextToken(hll_css_parser, &tokenizer->token, tokenizer->buf + hll_css_parser->c_buf_offset);
#if 0
   fprintf(stderr, "after:\n");
   fprintf(stderr, "hll_css_parser->c_space_separated = %d\n", hll_css_parser->c_space_separated);
   fprintf(stderr, "hll_css_parser->c_buf_offset      = %d\n", hll_css_parser->c_buf_offset);
   fprintf(stderr, "hll_css_parser->c_in_block        = %d\n", hll_css_parser->c_in_block);
   fprintf(stderr, "\n");
#endif
}

CssDeclarationValueType tokenMatchesProperty(CssDeclarationProperty property, c_css_token_t * token)
{
   const int hll_valueType = hll_tokenMatchesProperty(token, property);
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
                                       &parser->tokenizer.token,
                                       parser->tokenizer.buf + parser->hll_css_parser.c_buf_offset,
                                       valueType,
                                       property,
                                       &val)) {

            // Property found. Value found and valid. Declaration parsed.
            value->type   = (CssDeclarationValueType) val.c_type_tag;
            value->intVal = val.c_int_val;
            value->strVal = val.c_text_val;
            ret = true;
         }


#if 0
         if (parser->tokenizer.token.c_value[0] != ';' && parser->tokenizer.token.c_value[0] != '}') {
            // This takes token that is a semicolon after declaration value.
            // TODO: consider if this should be called only on success of
            // parsing value, or regardless of success/failure.
            nextToken(&parser->tokenizer, &parser->hll_css_parser);
         } else {
            fprintf(stderr, "shorthand: zzz = '%s'\n", parser->tokenizer.token.c_value);
         }
#endif
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
         CssDeclarationValueType typeTmp = tokenMatchesProperty(CSS_PROPERTY_BACKGROUND_POSITION, &parser->tokenizer.token);
         // tokenMatchesProperty will, for CSS_PROPERTY_BACKGROUND_POSITION,
         // work on both parts, since they are exchangable.
         if (CssDeclarationValueTypeUNUSED != typeTmp) {
            positions[i].as_h = parser->tokenizer.token.c_type != CSS_TOKEN_TYPE_SYMBOL || (dStrAsciiCasecmp(parser->tokenizer.token.c_value, "top") != 0  && dStrAsciiCasecmp(parser->tokenizer.token.c_value, "bottom") != 0);
            positions[i].as_v = parser->tokenizer.token.c_type != CSS_TOKEN_TYPE_SYMBOL || (dStrAsciiCasecmp(parser->tokenizer.token.c_value, "left") != 0 && dStrAsciiCasecmp(parser->tokenizer.token.c_value, "right") != 0);
            fprintf(stderr, "POSITION %s:%d: '%s' %d %d\n", __func__, __LINE__, parser->tokenizer.token.c_value, positions[i].as_h, positions[i].as_v);
         } else {
            // No match.
            positions[i].as_h = positions[i].as_v = false;
         }

         if (positions[i].as_h || positions[i].as_v) {
            // Calculate values.
            if (parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_SYMBOL) {
               if (dStrAsciiCasecmp(parser->tokenizer.token.c_value, "top") == 0 || dStrAsciiCasecmp(parser->tokenizer.token.c_value, "left") == 0) {
                  positions[i].cssLength = cssCreateLength(0.0, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&parser->tokenizer, &parser->hll_css_parser);
               } else if (dStrAsciiCasecmp(parser->tokenizer.token.c_value, "center") == 0) {
                  positions[i].cssLength = cssCreateLength(0.5, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&parser->tokenizer, &parser->hll_css_parser);
               } else if (dStrAsciiCasecmp(parser->tokenizer.token.c_value, "bottom") == 0 || dStrAsciiCasecmp(parser->tokenizer.token.c_value, "right") == 0) {
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
                                 &tokenizer.token,
                                 this->tokenizer.buf + this->hll_css_parser.c_buf_offset);
   return ival;
}


void parseDeclarationWrapper(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant)
{
      if (parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_SYMBOL) {
         if (!parseDeclarationNormal(parser, declList, declListImportant)) {
            /* Try shorthands. */
            parseDeclarationShorthands(parser, declList, declListImportant);
         }
      }

      /* Skip all tokens until the expected end. */
      while (!(parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_END ||
            (parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR &&
             (parser->tokenizer.token.c_value[0] == ';' || parser->tokenizer.token.c_value[0] == '}'))))
         nextToken(&parser->tokenizer, &parser->hll_css_parser);

      if (parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.token.c_value[0] == ';')
         nextToken(&parser->tokenizer, &parser->hll_css_parser);
}

bool parseDeclarationNormal(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant)
{
#if 1
   c_css_declaration_value_t value;
   memset(&value, 0, sizeof (value));
   int rv = hll_parseDeclarationNormal(&parser->hll_css_parser,
                                       &parser->tokenizer.token,
                                       parser->tokenizer.buf + parser->hll_css_parser.c_buf_offset,
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
   const int idx = hll_cssPropertyInfoIdxByName(parser->tokenizer.value_);
   if (-1 != idx) {
      CssDeclarationProperty property = (CssDeclarationProperty) idx;
      nextToken(&parser->tokenizer, &parser->hll_css_parser);
      if (parser->tokenizer.type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.value_[0] == ':') {
         nextToken(&parser->tokenizer, &parser->hll_css_parser);

         CssDeclarationValue val;
         CssDeclarationValueType type = tokenMatchesProperty(property, parser->tokenizer.value_, parser->tokenizer.type);
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
   const int sh_index = hll_cssShorthandInfoIdxByName(parser->tokenizer.token.c_value);

   if (-1 != sh_index) {
      fprintf(stderr, "shorthand\n");
      fprintf(stderr, "shorthand: found shorthand for '%s'\n", parser->tokenizer.token.c_value);
      fprintf(stderr, "shorthand: current remainder 1 = '%s''\n", parser->tokenizer.buf + parser->hll_css_parser.c_buf_offset);

      const CssShorthandInfo * shinfo = &Css_shorthand_info[sh_index];
      nextToken(&parser->tokenizer, &parser->hll_css_parser); // Skip ':' after property (before value)
      fprintf(stderr, "shorthand: current remainder 2 = '%s''\n", parser->tokenizer.buf + parser->hll_css_parser.c_buf_offset);
      if (parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.token.c_value[0] == ':') {
         nextToken(&parser->tokenizer, &parser->hll_css_parser);
         fprintf(stderr, "shorthand: token in 'if' = '%s', shinfo->type = %d\n", parser->tokenizer.token.c_value, shinfo->type);

         switch (shinfo->type) {

         case CSS_SHORTHAND_FONT:
            /* \todo Implement details. */
         case CSS_SHORTHAND_MULTIPLE:
            parseDeclarationMultiple(parser, declList, declListImportant, shinfo->properties);
            break;

         case CSS_SHORTHAND_DIRECTIONS: {
            int n = 0;

            CssDeclarationValue values[4];
            CssDeclarationValueType types[4];
            while (n < 4) {
               CssDeclarationValue val;
               CssDeclarationValueType type = tokenMatchesProperty(shinfo->properties[0], &parser->tokenizer.token);
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

         case CSS_SHORTHAND_BORDER: {
            bool found;

            fprintf(stderr, "shorthand: BORDER: token = '%s'\n", parser->tokenizer.token.c_value);

            do {
               int i = 0;
               for (found = false, i = 0; !found && i < 3; i++) {
                  fprintf(stderr, "shorthand: BORDER: i = %d\n", i);
                  CssDeclarationValue val;
                  CssDeclarationValueType type = tokenMatchesProperty(shinfo->properties[i], &parser->tokenizer.token);
                  if (CssDeclarationValueTypeUNUSED != type) {
                     fprintf(stderr, "shorthand: BORDER: token = '%s' matches property #%d = '%s'\n",
                             parser->tokenizer.token.c_value,
                             i,
                             hll_cssPropertyNameString(shinfo->properties[i]));
                     found = true;

                     if (parseDeclarationValue(parser, shinfo->properties[i], type, &val)) {
                        fprintf(stderr, "shorthand: BORDER: value parsed\n");
                        const bool weight = parser->parseWeight();
                        for (int j = 0; j < 4; j++) {

                           val.type = type;
                           if (weight)
                              declarationListAddOrUpdateDeclaration(declListImportant, shinfo->properties[j * 3 + i], val);
                           else
                              declarationListAddOrUpdateDeclaration(declList, shinfo->properties[j * 3 + i], val);
                        }
                     } else {
                        fprintf(stderr, "shorthand: BORDER: value not parsed\n");
                     }

                  } else {
                     fprintf(stderr, "shorthand: BORDER: token = '%s' doesn't match property #%d = '%s'\n",
                             parser->tokenizer.token.c_value,
                             i,
                             hll_cssPropertyNameString(shinfo->properties[i]));
                  }
               }
            } while (found);
         }
            break;
         }
      }
   }
}

typedef struct {
   CssDeclarationProperty property;
   CssDeclarationValue value;
   bool important;
} inter_t;

static void parseDeclarationMultiple(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant, const CssDeclarationProperty properties[12])
{
   inter_t inter[12];
   memset(inter, 0, sizeof (inter));
   int inter_i = 0;

   bool found;
   do {
      fprintf(stderr, "shorthand: MULTIPLE\n");
      int i = 0;
      for (found = false, i = 0; !found && properties[i] != CSS_PROPERTY_END; i++) {

         const CssDeclarationProperty property = properties[i];

         fprintf(stderr, "shorthand: MULTIPLE, found token '%s'\n", parser->tokenizer.token.c_value);
         CssDeclarationValueType type = tokenMatchesProperty(property, &parser->tokenizer.token);
         if (CssDeclarationValueTypeUNUSED != type) {

            fprintf(stderr, "shorthand: MULTIPLE, token '%s' matches property '%s'\n",
                    parser->tokenizer.token.c_value,
                    hll_cssPropertyNameString(property)); // E.g. token 'blue' matches property 'border-top-color'

            found = true;
            DEBUG_MSG(DEBUG_PARSE_LEVEL, "will assign to '%s'\n", hll_cssPropertyNameString(property));
            CssDeclarationValue val;
            if (parseDeclarationValue(parser, property, type, &val)) {

               val.type = type;
               const bool important = parser->parseWeight();
#if 0
               if (important)
                  declarationListAddOrUpdateDeclaration(declListImportant, property, val);
               else
                  declarationListAddOrUpdateDeclaration(declList, property, val);
#else
               inter[inter_i].important = important;
               inter[inter_i].value = val;
               inter[inter_i].property = property;
               inter_i++;
#endif
            }
         }
      }
   } while (found); // This condition makes sure that all values match some non-shorthand property

   for (int i = 0; i < inter_i; i++) {
      if (inter[i].important)
         declarationListAddOrUpdateDeclaration(declListImportant, inter[i].property, inter[i].value);
      else
         declarationListAddOrUpdateDeclaration(declList, inter[i].property, inter[i].value);
   }
}

c_css_selector_t * parseSelector(CssParser * cssParser)
{
   c_css_selector_t * selector = hll_cssParseSelector(&cssParser->hll_css_parser,
                                                      &cssParser->tokenizer.token,
                                                      cssParser->tokenizer.buf + cssParser->hll_css_parser.c_buf_offset);

   while (cssParser->tokenizer.token.c_type != CSS_TOKEN_TYPE_END &&
          (cssParser->tokenizer.token.c_type != CSS_TOKEN_TYPE_CHAR ||
           (cssParser->tokenizer.token.c_value[0] != ',' && cssParser->tokenizer.token.c_value[0] != '{')))
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

      if (parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.token.c_value[0] == ',')
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
   if (parser->tokenizer.token.c_type != CSS_TOKEN_TYPE_END) {
      parser->hll_css_parser.c_in_block = true;
      nextToken(&parser->tokenizer, &parser->hll_css_parser);
      do {
         parseDeclarationWrapper(parser, declList, declListImportant);
      } while (!(parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_END ||
               (parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.token.c_value[0] == '}')));
      parser->hll_css_parser.c_in_block = false;
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


   if (parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.token.c_value[0] == '}')
      nextToken(&parser->tokenizer, &parser->hll_css_parser);
}

char * CssParser::parseUrl()
{
   char * str = hll_declarationValueAsString(&this->hll_css_parser,
                                             &tokenizer.token,
                                             this->tokenizer.buf + this->hll_css_parser.c_buf_offset,
                                             0, 0);
   return str;
}

void CssParser::parseImport(DilloHtml *html)
{
   char *urlStr = NULL;
   bool importSyntaxIsOK = false;
   bool mediaSyntaxIsOK = true;
   bool mediaIsSelected = true;

   nextToken(&this->tokenizer, &this->hll_css_parser);

   if (tokenizer.token.c_type == CSS_TOKEN_TYPE_SYMBOL &&
       dStrAsciiCasecmp(tokenizer.token.c_value, "url") == 0)
      urlStr = parseUrl();
   else if (tokenizer.token.c_type == CSS_TOKEN_TYPE_STRING)
      urlStr = dStrdup (tokenizer.token.c_value);

   nextToken(&this->tokenizer, &this->hll_css_parser);

   /* parse a comma-separated list of media */
   if (tokenizer.token.c_type == CSS_TOKEN_TYPE_SYMBOL) {
      mediaSyntaxIsOK = false;
      mediaIsSelected = false;
      while (tokenizer.token.c_type == CSS_TOKEN_TYPE_SYMBOL) {
         if (dStrAsciiCasecmp(tokenizer.token.c_value, "all") == 0 ||
             dStrAsciiCasecmp(tokenizer.token.c_value, "screen") == 0)
            mediaIsSelected = true;
         nextToken(&this->tokenizer, &this->hll_css_parser);
         if (tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR && tokenizer.token.c_value[0] == ',') {
            nextToken(&this->tokenizer, &this->hll_css_parser);
         } else {
            mediaSyntaxIsOK = true;
            break;
         }
      }
   }

   if (mediaSyntaxIsOK &&
       tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR &&
       tokenizer.token.c_value[0] == ';') {
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
   while (tokenizer.token.c_type == CSS_TOKEN_TYPE_SYMBOL) {
      if (dStrAsciiCasecmp(tokenizer.token.c_value, "all") == 0 ||
          dStrAsciiCasecmp(tokenizer.token.c_value, "screen") == 0)
         mediaIsSelected = true;
      nextToken(&this->tokenizer, &this->hll_css_parser);
      if (tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR && tokenizer.token.c_value[0] == ',') {
         nextToken(&this->tokenizer, &this->hll_css_parser);
      } else {
         mediaSyntaxIsOK = true;
         break;
      }
   }

   /* check that the syntax is OK so far */
   if (!(mediaSyntaxIsOK &&
         tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR &&
         tokenizer.token.c_value[0] == '{')) {
      ignoreStatement(&this->tokenizer, &this->hll_css_parser);
      return;
   }

   /* parse/ignore the block as required */
   if (mediaIsSelected) {
      nextToken(&this->tokenizer, &this->hll_css_parser);
      while (tokenizer.token.c_type != CSS_TOKEN_TYPE_END) {
         parseRuleset(this, this->context_);
         if (tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR && tokenizer.token.c_value[0] == '}') {
            nextToken(&this->tokenizer, &this->hll_css_parser);
            break;
         }
      }
   } else
      ignoreBlock(&this->tokenizer, &this->hll_css_parser);
}

void ignoreBlock(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser)
{
   hll_ignoreBlock(hll_css_parser, &tokenizer->token, tokenizer->buf + hll_css_parser->c_buf_offset);
}

void ignoreStatement(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser)
{
   hll_ignoreStatement(hll_css_parser, &tokenizer->token, tokenizer->buf + hll_css_parser->c_buf_offset);
}

void CssParser::parse(DilloHtml *html, const DilloUrl *baseUrl,
                      CssContext *context,
                      const char *buf,
                      int buflen, CssOrigin origin)
{
   CssParser parser (context, origin, baseUrl, buf, buflen);
   bool importsAreAllowed = true;

   fprintf(stderr, "ZZZZ %s\n", buf);

   while (parser.tokenizer.token.c_type != CSS_TOKEN_TYPE_END) {
      if (parser.tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR &&
          parser.tokenizer.token.c_value[0] == '@') {
         nextToken(&parser.tokenizer, &parser.hll_css_parser);
         if (parser.tokenizer.token.c_type == CSS_TOKEN_TYPE_SYMBOL) {
            if (dStrAsciiCasecmp(parser.tokenizer.token.c_value, "import") == 0 &&
                html != NULL &&
                importsAreAllowed) {
               parser.parseImport(html);
            } else if (dStrAsciiCasecmp(parser.tokenizer.token.c_value, "media") == 0) {
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

   parser.hll_css_parser.c_in_block = true;

   do {
      parseDeclarationWrapper(&parser, declList, declListImportant);
   } while (!(parser.tokenizer.token.c_type == CSS_TOKEN_TYPE_END || (parser.tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR && parser.tokenizer.token.c_value[0] == '}')));
}

