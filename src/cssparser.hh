#ifndef __CSSPARSER_HH__
#define __CSSPARSER_HH__

#include "css.hh"
#include "Hello/hello.h"

class DilloHtml;

typedef enum {
              CSS_TOKEN_TYPE_IDENT  = 0,
              CSS_TOKEN_TYPE_STRING = 1,   // ['#00000000',] -> [#00000000] (quoted (') text)     ["\25B8";] -> [�] (quoted (") text)       [ "";}DIV] -> [] (empty)
              CSS_TOKEN_TYPE_CHAR   = 2,
              CSS_TOKEN_TYPE_END    = 3,
              CSS_TOKEN_TYPE_BRACE_CURLY_CLOSE = 4,
              CSS_TOKEN_TYPE_COLON = 5,
              CSS_TOKEN_TYPE_BRACE_SQUARE_OPEN = 6,
              CSS_TOKEN_TYPE_BRACE_SQUARE_CLOSE = 7,
              CSS_TOKEN_TYPE_HASH_UN = 8,
              CSS_TOKEN_TYPE_HASH_ID = 9,
              CSS_TOKEN_TYPE_AT_KEYWORD = 10
} CssTokenType;



#endif
