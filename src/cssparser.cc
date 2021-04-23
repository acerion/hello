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
void nextToken(CssTokenizer * tokenizer, hll_CssParser * hll_css_parser);
bool tokenMatchesProperty(CssDeclarationProperty property, CssDeclarationValueType * type, const char * tokenValue, int tokenType);
void ignoreBlock(CssTokenizer * tokenizer, hll_CssParser * hll_css_parser);
void ignoreStatement(CssTokenizer * tokenizer, hll_CssParser * hll_css_parser);



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
   fprintf(stderr, "Current token: '%s' = '%s'\n", tokenizerGetTokenTypeStr(tokenizer), tokenizer->value);
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


static FILE * g_file = NULL;
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

bool tokenMatchesProperty(CssDeclarationProperty property, CssDeclarationValueType * valueType, const char * tokenValue, int tokenType)
{
   const int hll_valueType = hll_tokenMatchesProperty(tokenType, tokenValue, property);
   if (-1 == hll_valueType) {
      //*valueType = savedValueType;
      return false;
   } else {
      *valueType = (CssDeclarationValueType) hll_valueType;
      return true;
   }
}

bool CssParser::parseDeclarationValue(CssDeclarationProperty property,
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
      {
         int ival = hll_declarationValueAsInt(&this->hll_css_parser,
                                              tokenizer.type,
                                              tokenizer.value,
                                              this->tokenizer.buf + this->tokenizer.bufOffset,
                                              valueType,
                                              property);
         this->tokenizer.bufOffset = this->hll_css_parser.bufOffsetC;
         if (999999999 != ival) {
            value->intVal = ival;
            ret = true;
         } else {
            // TODO: report error
         }
         // This takes token that is a semicolon after declaration value.
         // TODO: consider if this should be called only on success of
         // parsing value, or regardless of success/failure.
         nextToken(&this->tokenizer, &this->hll_css_parser);
      }
      break;

   case CssDeclarationValueTypeSTRING:
   case CssDeclarationValueTypeSYMBOL:
   case CssDeclarationValueTypeURI:
      {
         char * str = hll_declarationValueAsString(&this->hll_css_parser,
                                                   tokenizer.type,
                                                   tokenizer.value,
                                                   this->tokenizer.buf + this->tokenizer.bufOffset,
                                                   valueType,
                                                   property);
         this->tokenizer.bufOffset = this->hll_css_parser.bufOffsetC;
         if (NULL != str) {
            value->strVal = str;
            ret = true;
         } else {
            // TODO: report error
         }

         // This takes token that is a semicolon after declaration value.
         // TODO: consider if this should be called only on success of
         // parsing value, or regardless of success/failure.
         nextToken(&this->tokenizer, &this->hll_css_parser);
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
         CssDeclarationValueType typeTmp;
         // tokenMatchesProperty will, for CSS_PROPERTY_BACKGROUND_POSITION,
         // work on both parts, since they are exchangable.
         if (tokenMatchesProperty(CSS_PROPERTY_BACKGROUND_POSITION, &typeTmp, this->tokenizer.value, this->tokenizer.type)) {
            positions[i].as_h = tokenizer.type != CSS_TOKEN_TYPE_SYMBOL || (dStrAsciiCasecmp(tokenizer.value, "top") != 0  && dStrAsciiCasecmp(tokenizer.value, "bottom") != 0);
            positions[i].as_v = tokenizer.type != CSS_TOKEN_TYPE_SYMBOL || (dStrAsciiCasecmp(tokenizer.value, "left") != 0 && dStrAsciiCasecmp(tokenizer.value, "right") != 0);
            fprintf(stderr, "POSITION %s:%d: '%s' %d %d\n", __func__, __LINE__, this->tokenizer.value, positions[i].as_h, positions[i].as_v);
         } else {
            // No match.
            positions[i].as_h = positions[i].as_v = false;
         }

         if (positions[i].as_h || positions[i].as_v) {
            // Calculate values.
            if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
               if (dStrAsciiCasecmp(tokenizer.value, "top") == 0 || dStrAsciiCasecmp(tokenizer.value, "left") == 0) {
                  positions[i].cssLength = cssCreateLength(0.0, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&this->tokenizer, &this->hll_css_parser);
               } else if (dStrAsciiCasecmp(tokenizer.value, "center") == 0) {
                  positions[i].cssLength = cssCreateLength(0.5, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&this->tokenizer, &this->hll_css_parser);
               } else if (dStrAsciiCasecmp(tokenizer.value, "bottom") == 0 || dStrAsciiCasecmp(tokenizer.value, "right") == 0) {
                  positions[i].cssLength = cssCreateLength(1.0, CSS_LENGTH_TYPE_PERCENTAGE);
                  nextToken(&this->tokenizer, &this->hll_css_parser);
               } else
                  // tokenMatchesProperty should have returned "false" already.
                  lout::misc::assertNotReached ();
            } else {
               // We can assume <length> or <percentage> here ...
               CssDeclarationValue valTmp;
               if (parseDeclarationValue(property, CssDeclarationValueTypeLENGTH_PERCENTAGE, &valTmp)) {
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
            value->posVal = dNew(CssBackgroundPosition, 1);
            fprintf(stderr, "POSITION\n");

            // Prefer combination h/v:
            if (positions[0].as_h && positions[1].as_v) {
               value->posVal->posX = positions[0].cssLength.bits;
               value->posVal->posY = positions[1].cssLength.bits;
            } else {
               // This should be v/h:
               value->posVal->posX = positions[1].cssLength.bits;
               value->posVal->posY = positions[0].cssLength.bits;
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
   this->tokenizer.bufOffset = this->hll_css_parser.bufOffsetC;
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

void CssParser::parseDeclaration(CssDeclartionList * declList,
                                 CssDeclartionList *importantProps)
{
   if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
      const int idx = hll_cssPropertyInfoIdxByName(tokenizer.value);
      if (-1 != idx) {
         CssDeclarationProperty property = (CssDeclarationProperty) idx;
         nextToken(&this->tokenizer, &this->hll_css_parser);
         if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == ':') {
            nextToken(&this->tokenizer, &this->hll_css_parser);

            CssDeclarationValue val;
            CssDeclarationValueType type = CssDeclarationValueTypeUNUSED;
            if (tokenMatchesProperty(property, &type, this->tokenizer.value, this->tokenizer.type)
                && parseDeclarationValue(property, type, &val)) {

               const bool weight = parseWeight();

               val.type = type;
               if (weight && importantProps)
                  importantProps->updateOrAddDeclaration(property, val);
               else
                  declList->updateOrAddDeclaration(property, val);
            }
         }
      } else {
         /* Try shorthands. */
         CssPropertyInfo pi = { .symbol = tokenizer.value, {CssDeclarationValueTypeUNUSED}, NULL};
         CssShorthandInfo * sip = (CssShorthandInfo *) bsearch(&pi, Css_shorthand_info,
                                                               CSS_SHORTHAND_NUM,
                                                               sizeof(CssShorthandInfo),
                                                               Css_shorthand_info_cmp);
         if (sip) {
            const int sh_index = sip - Css_shorthand_info;
            const CssShorthandInfo * shinfo = &Css_shorthand_info[sh_index];
            nextToken(&this->tokenizer, &this->hll_css_parser);
            if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == ':') {
               nextToken(&this->tokenizer, &this->hll_css_parser);

               switch (shinfo->type) {

               case CssShorthandInfo::CSS_SHORTHAND_FONT:
                  /* \todo Implement details. */
               case CssShorthandInfo::CSS_SHORTHAND_MULTIPLE: {
                  bool found;
                  do {
                     int i = 0;
                     for (found = false, i = 0; !found && shinfo->properties[i] != CSS_PROPERTY_END; i++) {

                        CssDeclarationValue val;
                        CssDeclarationValueType type = CssDeclarationValueTypeUNUSED;
                        if (tokenMatchesProperty(shinfo->properties[i], &type, this->tokenizer.value, this->tokenizer.type)) {
                           found = true;
                           DEBUG_MSG(DEBUG_PARSE_LEVEL, "will assign to '%s'\n", hll_cssPropertyNameString(shinfo->properties[i]));
                           if (parseDeclarationValue(shinfo->properties[i], type, &val)) {
                              const bool weight = parseWeight();

                              val.type = type;
                              if (weight && importantProps)
                                 importantProps->updateOrAddDeclaration(shinfo->properties[i], val);
                              else
                                 declList->updateOrAddDeclaration(shinfo->properties[i], val);
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
                     CssDeclarationValueType type = CssDeclarationValueTypeUNUSED;
                     if (tokenMatchesProperty(shinfo->properties[0], &type, this->tokenizer.value, this->tokenizer.type) &&
                         parseDeclarationValue(shinfo->properties[0], type, &val)) {
                        values[n] = val;
                        types[n] = type;
                        n++;
                     } else
                        break;
                  }

                  const bool weight = parseWeight();
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
                        if (weight && importantProps)
                           importantProps->updateOrAddDeclaration(shinfo->properties[i], values[set_idx]);
                        else
                           declList->updateOrAddDeclaration(shinfo->properties[i], values[set_idx]);
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
                        CssDeclarationValueType type = CssDeclarationValueTypeUNUSED;
                        if (tokenMatchesProperty(shinfo->properties[i], &type, this->tokenizer.value, this->tokenizer.type)) {
                           found = true;
                           if (parseDeclarationValue(shinfo->properties[i], type, &val)) {
                              const bool weight = parseWeight();
                              for (int j = 0; j < 4; j++) {

                                 val.type = type;
                                 if (weight && importantProps)
                                    importantProps->updateOrAddDeclaration(shinfo->properties[j * 3 + i], val);
                                 else
                                    declList->updateOrAddDeclaration(shinfo->properties[j * 3 + i], val);
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
   }

   /* Skip all tokens until the expected end. */
   while (!(tokenizer.type == CSS_TOKEN_TYPE_END ||
            (tokenizer.type == CSS_TOKEN_TYPE_CHAR &&
             (tokenizer.value[0] == ';' || tokenizer.value[0] == '}'))))
      nextToken(&this->tokenizer, &this->hll_css_parser);

   if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == ';')
      nextToken(&this->tokenizer, &this->hll_css_parser);
}

bool CssParser::parseSimpleSelector(CssSimpleSelector *simpleSelector)
{
   CssSelectorType selectorType;

   char val[30];
   snprintf(val, sizeof (val), "%s", tokenizer.value);
   int rv = -2;
#if 1
   hll_CssSimpleSelector * simSel = (hll_CssSimpleSelector *) simpleSelector;
   rv = hll_cssParseSimpleSelector(&this->hll_css_parser, simSel, tokenizer.type, tokenizer.value, tokenizer.buf + tokenizer.bufOffset);
   simSel->alloced = true;
   tokenizer.bufOffset = this->hll_css_parser.bufOffsetC;
   snprintf(tokenizer.value, sizeof (tokenizer.value), "%s", this->hll_css_parser.tokenValueC);
   tokenizer.type = (CssTokenType) this->hll_css_parser.tokenTypeC;

   if (rv == 0) {
      // continue;
   } else if (rv == 1) {
      fprintf(stderr, "%d: %d simpleSelector.selector_class_size = %d, simpleSelector->selector_element = %d = '%s'\n",
              rv, this->hll_css_parser.spaceSeparatedC, simpleSelector->selector_class_size, simpleSelector->selector_element, val);
      return true;
   } else {
      fprintf(stderr, "%d: %d simpleSelector.selector_class_size = %d, simpleSelector->selector_element = %d = '%s'\n",
              rv, this->hll_css_parser.spaceSeparatedC, simpleSelector->selector_class_size, simpleSelector->selector_element, val);
      return false;
   }
#else
   if (tokenizer.type == CSS_TOKEN_TYPE_SYMBOL) {
      simpleSelector->selector_element = a_Html_tag_index(tokenizer.value);
      nextToken(&this->tokenizer, &this->hll_css_parser);
      if (this->hll_css_parser.spaceSeparatedC) {
         rv = 1;
         fprintf(stderr, "%d: %d simpleSelector.selector_class_size = %d, simpleSelector->selector_element = %d = '%s'\n",
                 rv, this->hll_css_parser.spaceSeparatedC, simpleSelector->selector_class_size, simpleSelector->selector_element, val);
         return true;
      } else {
         rv = 0;
      }

   } else if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '*') {
      simpleSelector->selector_element = CssSimpleSelectorElementAny;
      nextToken(&this->tokenizer, &this->hll_css_parser);
      if (this->hll_css_parser.spaceSeparatedC) {
         rv = 1;
         fprintf(stderr, "%d: %d simpleSelector.selector_class_size = %d, simpleSelector->selector_element = %d = '%s'\n",
                 rv, this->hll_css_parser.spaceSeparatedC, simpleSelector->selector_class_size, simpleSelector->selector_element, val);
         return true;
      } else {
         rv = 0;
      }
   } else if (tokenizer.type == CSS_TOKEN_TYPE_CHAR &&
              (tokenizer.value[0] == '#' ||
               tokenizer.value[0] == '.' ||
               tokenizer.value[0] == ':')) {
      rv = 0;
      // nothing to be done in this case
   } else {
      rv = 2;
      fprintf(stderr, "%d: %d simpleSelector.selector_class_size = %d, simpleSelector->selector_element = %d = '%s'\n",
              rv, this->hll_css_parser.spaceSeparatedC, simpleSelector->selector_class_size, simpleSelector->selector_element, val);
      return false;
   }

#endif
   fprintf(stderr, "%d: %d simpleSelector.selector_class_size = %d, simpleSelector->selector_element = %d = '%s'\n",
           rv, this->hll_css_parser.spaceSeparatedC, simpleSelector->selector_class_size, simpleSelector->selector_element, val);

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
            if (nullptr != simpleSelector->selector_pseudo_class)
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
            setSimpleSelector(simpleSelector, selectorType, tokenizer.value);
            nextToken(&this->tokenizer, &this->hll_css_parser);
         } else {
            return false; // don't accept classes or id's starting with integer
         }
         if (this->hll_css_parser.spaceSeparatedC)
            return true;
      }
   } while (selectorType != CssSelectorType::NONE);

   DEBUG_MSG(DEBUG_PARSE_LEVEL, "end of simple selector (%s, %s, %s, %d)\n",
      selector->id, selector->css_selector_class,
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
         cssSelectorAddSimpleSelector(selector, CssSelectorCombinatorChild);
         nextToken(&this->tokenizer, &this->hll_css_parser);
      } else if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '+') {
         cssSelectorAddSimpleSelector(selector, CssSelectorCombinatorAdjacentSibling);
         nextToken(&this->tokenizer, &this->hll_css_parser);
      } else if (tokenizer.type != CSS_TOKEN_TYPE_END && this->hll_css_parser.spaceSeparatedC) {
         cssSelectorAddSimpleSelector(selector, CssSelectorCombinatorDescendant);
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
   lout::misc::SimpleVector < CssSelector * > * selectors = new lout::misc::SimpleVector < CssSelector * >(1);

   while (true) {
      CssSelector * selector = parseSelector();

      if (selector) {
         selector->ref();
         selectors->increase();
         selectors->set(selectors->size() - 1, selector);
      }

      // \todo dump whole ruleset in case of parse error as required by CSS 2.1
      //       however make sure we don't dump it if only dillo fails to parse
      //       valid CSS.

      if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == ',')
         /* To read the next selector. */
         nextToken(&this->tokenizer, &this->hll_css_parser);
      else
         /* No more selectors. */
         break;
   }

   DEBUG_MSG(DEBUG_PARSE_LEVEL, "end of %s\n", "selectors");

   CssDeclartionList * declList = new CssDeclartionList(true);
   CssDeclartionList * importantProps = new CssDeclartionList(true);

   /* Read block. ('{' has already been read.) */
   if (tokenizer.type != CSS_TOKEN_TYPE_END) {
      this->hll_css_parser.withinBlockC = true;
      nextToken(&this->tokenizer, &this->hll_css_parser);
      do
         parseDeclaration(declList, importantProps);
      while (!(tokenizer.type == CSS_TOKEN_TYPE_END ||
               (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '}')));
      this->hll_css_parser.withinBlockC = false;
   }

   for (int i = 0; i < selectors->size(); i++) {
      CssSelector * sel = selectors->get(i);

      if (origin == CSS_ORIGIN_USER_AGENT) {
         context->addRule(sel, declList, CSS_PRIMARY_USER_AGENT);
      } else if (origin == CSS_ORIGIN_USER) {
         context->addRule(sel, declList, CSS_PRIMARY_USER);
         context->addRule(sel, importantProps, CSS_PRIMARY_USER_IMPORTANT);
      } else if (origin == CSS_ORIGIN_AUTHOR) {
         context->addRule(sel, declList, CSS_PRIMARY_AUTHOR);
         context->addRule(sel, importantProps, CSS_PRIMARY_AUTHOR_IMPORTANT);
      }

      sel->unref();
   }


   delete selectors;

   if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '}')
      nextToken(&this->tokenizer, &this->hll_css_parser);
}

char * CssParser::parseUrl()
{
   char * str = hll_declarationValueAsString(&this->hll_css_parser,
                                             tokenizer.type,
                                             tokenizer.value,
                                             this->tokenizer.buf + this->tokenizer.bufOffset,
                                             0, 0);
   this->tokenizer.bufOffset = this->hll_css_parser.bufOffsetC;
   snprintf(tokenizer.value, sizeof (tokenizer.value), "%s", this->hll_css_parser.tokenValueC);
   tokenizer.type = (CssTokenType) this->hll_css_parser.tokenTypeC;


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
         parseRuleset();
         if (tokenizer.type == CSS_TOKEN_TYPE_CHAR && tokenizer.value[0] == '}') {
            nextToken(&this->tokenizer, &this->hll_css_parser);
            break;
         }
      }
   } else
      ignoreBlock(&this->tokenizer, &this->hll_css_parser);
}

void ignoreBlock(CssTokenizer * tokenizer, hll_CssParser * hll_css_parser)
{
   hll_ignoreBlock(hll_css_parser, tokenizer->buf + tokenizer->bufOffset);
   tokenizer->bufOffset = hll_css_parser->bufOffsetC;
   snprintf(tokenizer->value, sizeof (tokenizer->value), "%s", hll_css_parser->tokenValueC);
   tokenizer->type = (CssTokenType) hll_css_parser->tokenTypeC;

}

void ignoreStatement(CssTokenizer * tokenizer, hll_CssParser * hll_css_parser)
{
   hll_ignoreStatement(hll_css_parser, tokenizer->buf + tokenizer->bufOffset);
   tokenizer->bufOffset = hll_css_parser->bufOffsetC;
   snprintf(tokenizer->value, sizeof (tokenizer->value), "%s", hll_css_parser->tokenValueC);
   tokenizer->type = (CssTokenType) hll_css_parser->tokenTypeC;

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
         parser.parseRuleset();
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

   parser.hll_css_parser.withinBlockC = true;

   do
      parser.parseDeclaration(declList, declListImportant);
   while (!(parser.tokenizer.type == CSS_TOKEN_TYPE_END || (parser.tokenizer.type == CSS_TOKEN_TYPE_CHAR && parser.tokenizer.value[0] == '}')));
}
