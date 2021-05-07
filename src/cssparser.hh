#ifndef __CSSPARSER_HH__
#define __CSSPARSER_HH__

#include "css.hh"
#include "haskell/hello.h"

class DilloHtml;

#define maxStrLen 256

typedef enum {
              CSS_TOKEN_TYPE_DECINT,   // [-1em]   -> [-1]       [60%;]  -> [60]      [1;]       -> [1]          [0,0,0,.8)] -> [0]
              CSS_TOKEN_TYPE_FLOAT,    // [-0.4em] -> [-0.4]     [-.5em] -> [-.5]     [4.1667%;] -> [4.1667]
              CSS_TOKEN_TYPE_COLOR,    // [#999;border] -> [#999]    [#E6E6E6;] -> [#E6E6E6]    [#000\9}] -> [#000]      [rgba(0,0,0,.8)] is split into sub-tokens
              CSS_TOKEN_TYPE_SYMBOL,   //
              CSS_TOKEN_TYPE_STRING,   // ['#00000000',] -> [#00000000] (quoted (') text)     ["\25B8";] -> [�] (quoted (") text)       [ "";}DIV] -> [] (empty)
              CSS_TOKEN_TYPE_CHAR,
              CSS_TOKEN_TYPE_WHITESPACE,
              CSS_TOKEN_TYPE_END       // End of input, no new tokens.
} CssTokenType;

struct CssTokenizer {
   CssTokenType type;
   char value[maxStrLen];

   const char *buf;
   int buflen;
   int bufOffset;
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

      /* declarationProperty:declarationValue, e.g. color:#324156 */
      bool parseWeight();
      char *parseUrl();
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
bool parseDeclarationValue(CssParser * parser, CssDeclarationProperty property, CssDeclarationValueType type, CssDeclarationValue * val);
void parseRuleset(CssParser * parser, CssContext * context);
c_css_selector_t * parseSelector(CssParser * cssParser);

#endif
