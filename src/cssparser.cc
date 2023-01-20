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
#include <sys/time.h>

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

void nextToken(c_css_parser_t * parser, c_css_token_t * token);
void parseImport(DilloHtml *html, c_css_parser_t * parser, c_css_token_t * token, const DilloUrl * base_url);
//void parseMedia(c_css_parser_t * parser, c_css_token_t * token, c_css_context_t * context);
static void parse_media_query(c_css_parser_t * parser, c_css_token_t * token, int * mediaSyntaxIsOk, int * mediaIsSelected);

/* ----------------------------------------------------------------------
 *    Parsing
 * ---------------------------------------------------------------------- */

CssParser::CssParser(CssOrigin origin, const DilloUrl * baseUrl, const char * buf, int buflen)
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

void nextToken(c_css_parser_t * parser, c_css_token_t * token)
{
   char * tokenValue = ffiNextToken(parser, token);
}

#if 0
void parseImport(DilloHtml *html, c_css_parser_t * parser, c_css_token_t * token, const DilloUrl * base_url)
{
   char *urlStr = NULL;
   bool importSyntaxIsOK = false;
   int mediaSyntaxIsOK = true;
   int mediaIsSelected = true;

   nextToken(parser, token);

   if (token->c_type == CSS_TOKEN_TYPE_IDENT &&
       dStrAsciiCasecmp(token->c_value, "url") == 0)
      urlStr = ffiDeclarationValueAsString(parser, token, 0);
   else if (token->c_type == CSS_TOKEN_TYPE_STRING)
      urlStr = dStrdup (token->c_value);

   nextToken(parser, token);

   /* parse a comma-separated list of media */
   if (token->c_type == CSS_TOKEN_TYPE_IDENT) {
      mediaSyntaxIsOK = false;
      mediaIsSelected = false;
      parse_media_query(parser, token, &mediaSyntaxIsOK, &mediaIsSelected);
   }

   if (mediaSyntaxIsOK && ffiIsTokenSemicolon(token)) {
      importSyntaxIsOK = true;
      nextToken(parser, token);
   } else
      ffiIgnoreStatement(parser, token);

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
#endif
#if 0
void parseMedia(c_css_parser_t * parser, c_css_token_t * token, c_css_context_t * context)
{
   nextToken(parser, token);

   int mediaSyntaxIsOK = false;
   int mediaIsSelected = false;
   parse_media_query(parser, token, &mediaSyntaxIsOK, &mediaIsSelected);

   /* check that the syntax is OK so far */
   if (!(mediaSyntaxIsOK && ffiIsTokenBraceCurlyOpen(token))) {
      ffiIgnoreStatement(parser, token);
      return;
   }

   /* parse/ignore the block as required */
   if (mediaIsSelected) {
      nextToken(parser, token);
      while (token->c_type != CSS_TOKEN_TYPE_END) {
         ffiCssParseRuleset(parser, token, context);
         if (ffiIsTokenBraceCurlyClose(token)) {
            nextToken(parser, token);
            break;
         }
      }
   } else {
      ffiIgnoreBlock(parser, token);
   }
}
#endif

static void parse_media_query(c_css_parser_t * parser, c_css_token_t * token, int * mediaSyntaxIsOK, int * mediaIsSelected)
{
   while (token->c_type == CSS_TOKEN_TYPE_IDENT) {
      if (dStrAsciiCasecmp(token->c_value, "all") == 0 || dStrAsciiCasecmp(token->c_value, "screen") == 0) {
         *mediaIsSelected = true;
      }

      fprintf(stderr, "MEDIA = '%s'\n", token->c_value);

      nextToken(parser, token);

      if (ffiIsTokenComma(token)) {
         nextToken(parser, token);
      } else {
         *mediaSyntaxIsOK = true;
         break;
      }
   }
}

