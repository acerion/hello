/*
 * File: css.cc
 *
 * Copyright 2008-2014 Johannes Hofmann <Johannes.Hofmann@gmx.de>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 */

#include "css.hh"
#include "css.h"

CssLengthType cpp_cssLengthType(CssLength len)
{
   return (CssLengthType) len.length_type;
}

float cpp_cssLengthValue(CssLength l)
{
   return (float) l.length_value;
}

CssLength cpp_cssCreateLength(float v, CssLengthType t)
{
   CssLength retv = { .length_value = (double) v, .length_type = (int) t };
   return retv;
}




/*
 Parsing code, moved from cssparser.cc.

 This is the code that downloads stylesheet specified by @import.

 TODO: use it in the future.
*/
#if 0
void parseImport(DilloHtml *html, c_css_parser_t * parser, c_css_token_t * token, const DilloUrl * base_url)
{
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

