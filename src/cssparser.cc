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


void nextToken(c_css_parser_t * hll_parser, c_css_token_t * token);
void parseDeclaration(CssParser * parser, c_css_declaration_set_t * declList, c_css_declaration_set_t * declListImportant);
void parseRuleset(CssParser * parser, c_css_context_t * context);
void parseImport(DilloHtml *html, c_css_parser_t * parser, c_css_token_t * token, const DilloUrl * base_url);
void parseMedia(c_css_parser_t * parser, c_css_token_t * token, c_css_context_t * context);



/* ----------------------------------------------------------------------
 *    Parsing
 * ---------------------------------------------------------------------- */

CssParser::CssParser(CssOrigin origin,
                     const DilloUrl *baseUrl,
                     const char *buf, int buflen)
{
   this->m_parser.c_parser_buf = buf;
   this->m_parser.c_parser_buflen = buflen;
   this->m_parser.c_in_block = false;
   this->m_parser.c_space_separated = false;
   this->m_parser.c_buf_offset = 0;
   this->m_parser.c_origin = origin;
   this->m_base_url = baseUrl;

   nextToken(&this->m_parser, &this->m_token);
}


void nextToken(c_css_parser_t * hll_parser, c_css_token_t * token)
{
#if 0
   fprintf(stderr, "before:\n");
   fprintf(stderr, "hll_css_parser->c_space_separated = %d\n", hll_css_parser->c_space_separated);
   fprintf(stderr, "hll_css_parser->c_buf_offset      = %d\n", hll_css_parser->c_buf_offset);
   fprintf(stderr, "hll_css_parser->c_in_block        = %d\n", hll_css_parser->c_in_block);
#endif
   char * tokenValue = hll_nextToken(hll_parser, token);
#if 0
   fprintf(stderr, "after:\n");
   fprintf(stderr, "hll_css_parser->c_space_separated = %d\n", hll_css_parser->c_space_separated);
   fprintf(stderr, "hll_css_parser->c_buf_offset      = %d\n", hll_css_parser->c_buf_offset);
   fprintf(stderr, "hll_css_parser->c_in_block        = %d\n", hll_css_parser->c_in_block);
   fprintf(stderr, "\n");
#endif
}

void parseRuleset(c_css_parser_t * parser, c_css_token_t * token, c_css_context_t * context)
{
#define SELECTORS_MAX 100
   c_css_selector_t ** selectors = (c_css_selector_t **) calloc(SELECTORS_MAX, sizeof (c_css_selector_t *));
   for (int s = 0; s < SELECTORS_MAX; s++) {
      selectors[s] = (c_css_selector_t *) calloc(1, sizeof (c_css_selector_t *));
   }
   int selectors_count = hll_cssParseSelectors(parser, token, selectors);

   c_css_declaration_set_t * declList = declarationListNew();
   c_css_declaration_set_t * declListImportant = declarationListNew();

   /* Read block. ('{' has already been read.) */
   if (token->c_type != CSS_TOKEN_TYPE_END) {

      parser->c_in_block = true;
      nextToken(parser, token);
      do {
         hll_parseDeclarationWrapper(parser, token, declList, declListImportant);
      } while (!(token->c_type == CSS_TOKEN_TYPE_END || (token->c_type == CSS_TOKEN_TYPE_CHAR && token->c_value[0] == '}')));
      parser->c_in_block = false;
   }

   // Construct rules from selectors and delcarations, and add them to context
   hll_constructAndAddRules(context, selectors, selectors_count, declList, declListImportant, (CssOrigin) parser->c_origin);

   if (token->c_type == CSS_TOKEN_TYPE_CHAR && token->c_value[0] == '}') {
      nextToken(parser, token);
   }
}

void parseImport(DilloHtml *html, c_css_parser_t * parser, c_css_token_t * token, const DilloUrl * base_url)
{
   char *urlStr = NULL;
   bool importSyntaxIsOK = false;
   bool mediaSyntaxIsOK = true;
   bool mediaIsSelected = true;

   nextToken(parser, token);

   if (token->c_type == CSS_TOKEN_TYPE_IDENT &&
       dStrAsciiCasecmp(token->c_value, "url") == 0)
      urlStr = hll_declarationValueAsString(parser, token, 0, 0);
   else if (token->c_type == CSS_TOKEN_TYPE_STRING)
      urlStr = dStrdup (token->c_value);

   nextToken(parser, token);

   /* parse a comma-separated list of media */
   if (token->c_type == CSS_TOKEN_TYPE_IDENT) {
      mediaSyntaxIsOK = false;
      mediaIsSelected = false;
      while (token->c_type == CSS_TOKEN_TYPE_IDENT) {
         if (dStrAsciiCasecmp(token->c_value, "all") == 0 ||
             dStrAsciiCasecmp(token->c_value, "screen") == 0)
            mediaIsSelected = true;
         nextToken(parser, token);
         if (token->c_type == CSS_TOKEN_TYPE_CHAR && token->c_value[0] == ',') {
            nextToken(parser, token);
         } else {
            mediaSyntaxIsOK = true;
            break;
         }
      }
   }

   if (mediaSyntaxIsOK &&
       token->c_type == CSS_TOKEN_TYPE_CHAR &&
       token->c_value[0] == ';') {
      importSyntaxIsOK = true;
      nextToken(parser, token);
   } else
      hll_ignoreStatement(parser, token);

   if (urlStr) {
      if (importSyntaxIsOK && mediaIsSelected) {
         MSG("CssParser::parseImport(): @import %s\n", urlStr);
         DilloUrl *url = a_Html_url_new (html, urlStr, a_Url_str(base_url),
                                         base_url ? 1 : 0);
         a_Html_load_stylesheet(html, url);
         a_Url_free(url);
      }
      dFree (urlStr);
   }
}

void parseMedia(c_css_parser_t * parser, c_css_token_t * token, c_css_context_t * context)
{
   bool mediaSyntaxIsOK = false;
   bool mediaIsSelected = false;

   nextToken(parser, token);

   /* parse a comma-separated list of media */
   while (token->c_type == CSS_TOKEN_TYPE_IDENT) {
      if (dStrAsciiCasecmp(token->c_value, "all") == 0 ||
          dStrAsciiCasecmp(token->c_value, "screen") == 0)
         mediaIsSelected = true;
      nextToken(parser, token);
      if (token->c_type == CSS_TOKEN_TYPE_CHAR && token->c_value[0] == ',') {
         nextToken(parser, token);
      } else {
         mediaSyntaxIsOK = true;
         break;
      }
   }

   /* check that the syntax is OK so far */
   if (!(mediaSyntaxIsOK &&
         token->c_type == CSS_TOKEN_TYPE_CHAR &&
         token->c_value[0] == '{')) {
      hll_ignoreStatement(parser, token);
      return;
   }

   /* parse/ignore the block as required */
   if (mediaIsSelected) {
      nextToken(parser, token);
      while (token->c_type != CSS_TOKEN_TYPE_END) {
         parseRuleset(parser, token, context);
         if (token->c_type == CSS_TOKEN_TYPE_CHAR && token->c_value[0] == '}') {
            nextToken(parser, token);
            break;
         }
      }
   } else
      hll_ignoreBlock(parser, token);
}

void parseCss(DilloHtml *html, const DilloUrl * baseUrl, c_css_context_t * context, const char * buf, int buflen, CssOrigin origin)
{
   CssParser parser_(origin, baseUrl, buf, buflen);
   bool importsAreAllowed = true;

   c_css_token_t * token = &parser_.m_token;
   c_css_parser_t * parser = &parser_.m_parser;

   while (token->c_type != CSS_TOKEN_TYPE_END) {
      if (token->c_type == CSS_TOKEN_TYPE_CHAR &&
          token->c_value[0] == '@') {
         nextToken(parser, token);
         if (token->c_type == CSS_TOKEN_TYPE_IDENT) {
            if (dStrAsciiCasecmp(token->c_value, "import") == 0 &&
                html != NULL &&
                importsAreAllowed) {
               parseImport(html, parser, token, parser_.m_base_url);
            } else if (dStrAsciiCasecmp(token->c_value, "media") == 0) {
               parseMedia(parser, token, context);
            } else {
               hll_ignoreStatement(parser, token);
            }
         } else {
            hll_ignoreStatement(parser, token);
         }
      } else {
         importsAreAllowed = false;
         parseRuleset(parser, token, context);
      }
   }
}
