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


void nextToken(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser);
void ignoreBlock(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser);
void ignoreStatement(CssTokenizer * tokenizer, c_css_parser_t * hll_css_parser);

static void parseDeclarationWrapper(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant);



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

void parseDeclarationWrapper(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant)
{
   c_css_declaration_ffi_t * declarations = (c_css_declaration_ffi_t *) malloc(12 * sizeof (c_css_declaration_ffi_t));
   int n = hll_parseDeclaration(&parser->hll_css_parser,
                                &parser->tokenizer.token,
                                parser->tokenizer.buf + parser->hll_css_parser.c_buf_offset,
                                declarations);
   for (int v = 0; v < n; v++) {
      CssDeclarationValue val;
      val.type   = (CssDeclarationValueType) declarations[v].c_type_tag;
      val.intVal = declarations[v].c_int_val;
      val.strVal = declarations[v].c_text_val;

      if (declarations[v].c_important)
         declarationListAddOrUpdateDeclaration(declListImportant, (CssDeclarationProperty) declarations[v].c_property, val);
      else
         declarationListAddOrUpdateDeclaration(declList, (CssDeclarationProperty) declarations[v].c_property, val);
   }
}

void parseRuleset(CssParser * parser, CssContext * context)
{
   c_css_selector_t * selectors = (c_css_selector_t *) calloc(100, sizeof (c_css_selector_t));
   int selectors_count = hll_cssParseSelectors(&parser->hll_css_parser,
                                               &parser->tokenizer.token,
                                               parser->tokenizer.buf + parser->hll_css_parser.c_buf_offset,
                                               selectors);

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
      c_css_selector_t * sel = &selectors[i];

      switch (parser->origin) {
      case CSS_ORIGIN_USER_AGENT:
         if (declList->declarations_count > 0) {
            CssRule * rule = new CssRule(sel, declList, context->rulePosition++);
            addRuleToContext(context, rule, CSS_PRIMARY_USER_AGENT);
         }
         break;
      case CSS_ORIGIN_USER:
         if (declList->declarations_count > 0) {
            CssRule * rule = new CssRule(sel, declList, context->rulePosition++);
            addRuleToContext(context, rule, CSS_PRIMARY_USER);
         }
         if (declListImportant->declarations_count > 0) {
            CssRule * rule = new CssRule(sel, declListImportant, context->rulePosition++);
            addRuleToContext(context, rule, CSS_PRIMARY_USER_IMPORTANT);
         }
         break;
      case CSS_ORIGIN_AUTHOR:
         if (declList->declarations_count > 0) {
            CssRule * rule = new CssRule(sel, declList, context->rulePosition++);
            addRuleToContext(context, rule, CSS_PRIMARY_AUTHOR);
         }
         if (declListImportant->declarations_count > 0) {
            CssRule * rule = new CssRule(sel, declListImportant, context->rulePosition++);
            addRuleToContext(context, rule, CSS_PRIMARY_AUTHOR_IMPORTANT);
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

   if (tokenizer.token.c_type == CSS_TOKEN_TYPE_SYMBOL &&
       dStrAsciiCasecmp(tokenizer.token.c_value, "url") == 0)
      urlStr = hll_declarationValueAsString(&this->hll_css_parser,
                                            &tokenizer.token,
                                            this->tokenizer.buf + this->hll_css_parser.c_buf_offset,
                                            0, 0);
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

