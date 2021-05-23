#ifndef __CSSPARSER_HH__
#define __CSSPARSER_HH__

#include "css.hh"
#include "haskell/hello.h"

class DilloHtml;

typedef enum {
              CSS_TOKEN_TYPE_SYMBOL = 0,
              CSS_TOKEN_TYPE_STRING = 1,   // ['#00000000',] -> [#00000000] (quoted (') text)     ["\25B8";] -> [¸] (quoted (") text)       [ "";}DIV] -> [] (empty)
              CSS_TOKEN_TYPE_CHAR   = 2,
              CSS_TOKEN_TYPE_END    = 3
} CssTokenType;


struct CssTokenizer {
   c_css_token_t token;

   const char *buf;
   int buflen;
};

class CssParser {
   public:
      CssContext *context_;
      CssOrigin origin;
      const DilloUrl *baseUrl;


      CssTokenizer tokenizer;
      c_css_parser_t hll_css_parser;

      CssParser(CssContext *context, CssOrigin origin, const DilloUrl *baseUrl,
                const char *buf, int buflen);

      void parseImport(DilloHtml *html);
      void parseMedia();

      static void parseElementStyleAttribute(const DilloUrl *baseUrl,
                                             const char * cssStyleAttribute, int buflen,
                                             CssDeclartionList * declList,
                                             CssDeclartionList * declListImportant);
      static void parse(DilloHtml *html, const DilloUrl *baseUrl, CssContext *context,
                        const char *buf, int buflen, CssOrigin origin);
};

void parseDeclaration(CssParser * parser, CssDeclartionList * declList, CssDeclartionList * declListImportant);
void parseRuleset(CssParser * parser, CssContext * context);

#endif
