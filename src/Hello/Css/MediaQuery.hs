{-
Copyright (C) 2021 Kamil Ignacak acerion@wp.pl

This file is part of "hello" web browser.

"hello" is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

"hello" is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with "hello".  If not, see <https://www.gnu.org/licenses/>.

This file is derived from dillo-3.0.5/src/cssparser.cc.
Copyright assignments from that file:
Copyright 2004 Sebastian Geerken <sgeerken@dillo.org>
Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
Additional note in cssparser.cc:
"This file is heavily based on the CSS parser of dillo-0.8.0-css-3 -
a dillo1 based CSS prototype written by Sebastian Geerken."
-}




{-
Handling of media query tokens that are between "@media" and rules block.
Handling of media query values themselves.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Css.MediaQuery
  (
    CssMediaQuery

  , parseMediaQuery
  , mediaMatchesParser
  )
where




import Hello.Css.Tokenizer




type CssMediaQuery = [[CssToken]]




consumeMediaQueryTokens :: (CssParser, CssToken) -> [CssToken] -> ((CssParser, CssToken), [CssToken])
consumeMediaQueryTokens (parser, tok@(CssTokBraceCurlyOpen)) xs = ((parser, tok), reverse xs)
consumeMediaQueryTokens (parser, tok@CssTokEnd)              xs = ((parser, tok), reverse xs)
-- Whitespaces are probably not significant in media query string
consumeMediaQueryTokens (parser, CssTokWS)                   xs = consumeMediaQueryTokens (nextToken2 parser) (xs)
 -- The function may be called from ghci with initial None token, so this
 -- function must be prepared to handle this situation.
consumeMediaQueryTokens (parser, CssTokNone)                 xs = consumeMediaQueryTokens (nextToken2 parser) (xs)
consumeMediaQueryTokens (parser, tok)                        xs = consumeMediaQueryTokens (nextToken2 parser) (tok:xs)




parseMediaQuery :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssMediaQuery)
parseMediaQuery (parser, token) = ((p2, t2), Just media)
  where
    ((p2, t2), tokens) = consumeMediaQueryTokens (parser, token) []
    media = splitAtCommaToken tokens []
    syntaxValid = True -- TODO: implement syntax validation




mediaMatchesParser :: CssParser -> CssMediaQuery -> Bool
mediaMatchesParser _ media = elem [(CssTokIdent "all")] media || elem [(CssTokIdent "screen")] media
