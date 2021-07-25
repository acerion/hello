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
#include "Hello/hello.h"

using namespace dw::core::style;

#define MSG_CSS(A, ...) MSG(A, __VA_ARGS__)
#define DEBUG_TOKEN_LEVEL   0
#define DEBUG_PARSE_LEVEL   0
#define DEBUG_CREATE_LEVEL  0

#define DEBUG_LEVEL 10


void nextToken(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser);
void ignoreBlock(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser);
void ignoreStatement(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser);


/* ----------------------------------------------------------------------
 *    Parsing
 * ---------------------------------------------------------------------- */

CssParser::CssParser(c_css_context_t * context, CssOrigin origin,
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

void parseRuleset(CssParser * parser, c_css_context_t * context)
{
#define SELECTORS_MAX 100
   c_css_selector_t ** selectors = (c_css_selector_t **) calloc(SELECTORS_MAX, sizeof (c_css_selector_t *));
   for (int s = 0; s < SELECTORS_MAX; s++) {
      selectors[s] = (c_css_selector_t *) calloc(1, sizeof (c_css_selector_t *));
   }
   int selectors_count = hll_cssParseSelectors(&parser->hll_css_parser,
                                               &parser->tokenizer.token,
                                               parser->tokenizer.buf + parser->hll_css_parser.c_buf_offset,
                                               selectors);

   c_css_declaration_set_t * declList = declarationListNew();
   c_css_declaration_set_t * declListImportant = declarationListNew();

   /* Read block. ('{' has already been read.) */
   if (parser->tokenizer.token.c_type != CSS_TOKEN_TYPE_END) {

#if 0
      if (parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR) {
         fprintf(stderr, "++++++++++++++++ char '%c'\n", parser->tokenizer.token.c_value[0]);
      } else if (parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_IDENT) {
         fprintf(stderr, "++++++++++++++++ ident '%s'\n", parser->tokenizer.token.c_value);
      } else {
         fprintf(stderr, "++++++++++++++++ unknown '%d'\n", parser->tokenizer.token.c_type);
      }
#endif
      parser->hll_css_parser.c_in_block = true;
      nextToken(&parser->tokenizer, &parser->hll_css_parser);
      do {
         hll_parseDeclarationWrapper(&parser->hll_css_parser, &parser->tokenizer.token, parser->tokenizer.buf + parser->hll_css_parser.c_buf_offset,
                                     declList, declListImportant);
      } while (!(parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_END || (parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.token.c_value[0] == '}')));
      parser->hll_css_parser.c_in_block = false;
   }

   for (int i = 0; i < selectors_count; i++) {
      c_css_selector_t * sel = selectors[i];

      switch (parser->origin) {
      case CSS_ORIGIN_USER_AGENT:
         if (declList->c_declarations_size > 0) {
            c_css_rule_t * rule = css_rule_new(sel, declList, context->c_rule_position);
            hll_cssContextAddRule(context, rule, CSS_PRIMARY_USER_AGENT);
         }
         break;
      case CSS_ORIGIN_USER:
         if (declList->c_declarations_size > 0) {
            c_css_rule_t * rule = css_rule_new(sel, declList, context->c_rule_position);
            hll_cssContextAddRule(context, rule, CSS_PRIMARY_USER);
         }
         if (declListImportant->c_declarations_size > 0) {
            c_css_rule_t * rule = css_rule_new(sel, declListImportant, context->c_rule_position);
            hll_cssContextAddRule(context, rule, CSS_PRIMARY_USER_IMPORTANT);
         }
         break;
      case CSS_ORIGIN_AUTHOR:
         if (declList->c_declarations_size > 0) {
            c_css_rule_t * rule = css_rule_new(sel, declList, context->c_rule_position);
            hll_cssContextAddRule(context, rule, CSS_PRIMARY_AUTHOR);
         }
         if (declListImportant->c_declarations_size > 0) {
            c_css_rule_t * rule = css_rule_new(sel, declListImportant, context->c_rule_position);
            hll_cssContextAddRule(context, rule, CSS_PRIMARY_AUTHOR_IMPORTANT);
         }
         break;
      default:
         break;
      }
   }

   if (parser->tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR && parser->tokenizer.token.c_value[0] == '}')
      nextToken(&parser->tokenizer, &parser->hll_css_parser);
}

void CssParser::parseImport(DilloHtml *html)
{
   char *urlStr = NULL;
   bool importSyntaxIsOK = false;
   bool mediaSyntaxIsOK = true;
   bool mediaIsSelected = true;

   nextToken(&this->tokenizer, &this->hll_css_parser);

   if (tokenizer.token.c_type == CSS_TOKEN_TYPE_IDENT &&
       dStrAsciiCasecmp(tokenizer.token.c_value, "url") == 0)
      urlStr = hll_declarationValueAsString(&this->hll_css_parser,
                                            &tokenizer.token,
                                            this->tokenizer.buf + this->hll_css_parser.c_buf_offset,
                                            0, 0);
   else if (tokenizer.token.c_type == CSS_TOKEN_TYPE_STRING)
      urlStr = dStrdup (tokenizer.token.c_value);

   nextToken(&this->tokenizer, &this->hll_css_parser);

   /* parse a comma-separated list of media */
   if (tokenizer.token.c_type == CSS_TOKEN_TYPE_IDENT) {
      mediaSyntaxIsOK = false;
      mediaIsSelected = false;
      while (tokenizer.token.c_type == CSS_TOKEN_TYPE_IDENT) {
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
   while (tokenizer.token.c_type == CSS_TOKEN_TYPE_IDENT) {
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
                      c_css_context_t * context,
                      const char *buf,
                      int buflen, CssOrigin origin)
{
   CssParser parser (context, origin, baseUrl, buf, buflen);
   bool importsAreAllowed = true;

   while (parser.tokenizer.token.c_type != CSS_TOKEN_TYPE_END) {
      if (parser.tokenizer.token.c_type == CSS_TOKEN_TYPE_CHAR &&
          parser.tokenizer.token.c_value[0] == '@') {
         nextToken(&parser.tokenizer, &parser.hll_css_parser);
         if (parser.tokenizer.token.c_type == CSS_TOKEN_TYPE_IDENT) {
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
