{-
Copyright (C) 2021-2023 Kamil Ignacak acerion@wp.pl

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




{-# LANGUAGE OverloadedStrings #-}




{-
Code for parsing of things that are one step above of a single CSS
declaration
-}




module Hello.Css.Parser.Rule
  (
    ignoreBlock
  , ignoreStatement

  , CssRule (..)
  , CssParsedStyleRule (..)

  , getTopCompound

  , parseStyleRule
  , parseElementStyleAttribute

  -- These are exported only for tests
  , readDeclarations
  )
where




import Control.Applicative (Alternative(..))
import qualified Data.Text as T
--import Debug.Trace

import Hello.Chain
import Hello.Css.Parser.Declaration
import Hello.Css.Parser.Selector
import Hello.Css.Tokenizer
import Hello.Css.Selector
import Hello.Utils.Parser




-- TODO: rewrite with _consumeBlock?
ignoreBlock :: CssParser -> (CssParser, CssToken)
ignoreBlock parser = ignoreBlock' (parser, CssTokNone) 0
  where
    ignoreBlock' :: (CssParser, CssToken) -> Int -> (CssParser, CssToken)
    ignoreBlock' (par, tok@CssTokEnd) _             = (par, tok)
    ignoreBlock' (par, CssTokBraceCurlyOpen) depth  = ignoreBlock' (nextToken par) (depth + 1)
    ignoreBlock' (par, CssTokBraceCurlyClose) depth = if depth == 1
                                                      then nextToken par
                                                      else ignoreBlock' (nextToken par) (depth - 1)
    ignoreBlock' (par, _tok) depth                  = ignoreBlock' (nextToken par) depth





-- TODO: this function can recognize only blocks enclosed by curly braces.
-- Make the function recognize all types of Css braces.
_consumeBlock :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
_consumeBlock pat = consumeBlock' pat [] []
  where
    -- Last argument (braces) is used to keep track of opened/closed braces
    -- to know what is the current nesting level of blocks.
    consumeBlock' (parser, tok@CssTokEnd) tokens _                                   = ((parser, tok), reverse tokens)
    consumeBlock' (parser, CssTokBraceCurlyOpen) tokens braces                       = consumeBlock' (nextToken parser) (CssTokBraceCurlyOpen : tokens) (CssTokBraceCurlyOpen : braces)
    consumeBlock' (parser, CssTokBraceCurlyClose) tokens [CssTokBraceCurlyOpen]      = (nextToken parser, reverse tokens)
    consumeBlock' (parser, CssTokBraceCurlyClose) tokens (CssTokBraceCurlyOpen : xs) = consumeBlock' (nextToken parser) (CssTokBraceCurlyClose : tokens) xs
    consumeBlock' (parser, tok) tokens braces                                        = consumeBlock' (nextToken parser) (tok : tokens) braces




ignoreStatement :: CssParser -> (CssParser, CssToken)
ignoreStatement parser = ignoreStatement' (parser, CssTokNone)
  where
    ignoreStatement' (par, tok@CssTokEnd)        = (par, tok)
    ignoreStatement' (par, CssTokSemicolon)      = nextToken par
    ignoreStatement' (par, CssTokBraceCurlyOpen) = ignoreBlock par
    ignoreStatement' (par, _)                    = ignoreStatement' (nextToken par)




-- Consume input until end of {} block is encountered.
-- To be called when handling errors during parsing of {} block.
consumeRestOfCurlyBlock :: (CssParser, CssToken) -> (CssParser, CssToken)
consumeRestOfCurlyBlock pair@(_, CssTokEnd)             = pair
consumeRestOfCurlyBlock (parser, CssTokBraceCurlyClose) =
  -- Don't forget to consume the spaces after closing } too.
  -- Since we are leaving the block, set inBlock flag accordingly.
  consumeFinalSpaces . nextToken $ parser { inBlock = False }
  where
    consumeFinalSpaces (p, CssTokWS) = consumeFinalSpaces . nextToken $ p
    consumeFinalSpaces pat = pat
consumeRestOfCurlyBlock (parser, _)                     = consumeRestOfCurlyBlock . nextToken $ parser




{-
Parse CSS style information contained in "cssStyleAttribute". The buffer
contains value of "style" attribute of a html element.

import qualified Data.Sequence as S
let declSet    = CssDeclarationSet {isSafe = True, items = S.fromList []}
let declSetImp = CssDeclarationSet {isSafe = True, items = S.fromList []}
let cssStyleAttribute = "color: red !important; font-weight: bold"
parseElementStyleAttribute "" cssStyleAttribute (declSet, declSetImp)
-}
parseElementStyleAttribute :: T.Text -> T.Text -> CssDeclarationSets -> CssDeclarationSets
parseElementStyleAttribute _baseUrl cssStyleAttribute declSets = declSets'
  where
    ((_p2, _t2), declSets') = parseAllDeclarations ((p1, t1), declSets)
    (p1, t1) = nextToken parser -- Kick-off the parsing

    {-
      TODO: in original C++ code the parser was initialized like this:
      CssParser parser(NULL,              -- c_css_context_t object
                       CSS_ORIGIN_AUTHOR, -- CssOrigin enum
                       baseUrl, cssStyleAttribute, buflen);
      Be sure to recreate this in final Haskell code.
    -}
    parser = CssParser{ remainder      = cssStyleAttribute
                      , spaceSeparated = False
                      , inBlock        = True -- There is no block enclosed in {}. but parser needs to behave as if we were in the block.
                      , bufOffset      = 0
                      , cssOrigin      = CssOriginAuthor
                      }




-- TODO: this looks like a duplicate of readDeclarations.
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
--
-- parseAllDeclarations  (nextToken . defaultParserInBlock $ "border-top-color: #000001; border-right-color: #000002; border-bottom-color: #000003 !important; border-left-color: #000004;", (defaultCssDeclarationSet, defaultCssDeclarationSet))
parseAllDeclarations :: ((CssParser, CssToken), CssDeclarationSets) -> ((CssParser, CssToken), CssDeclarationSets)
parseAllDeclarations input@((_, CssTokEnd), _)             = input
parseAllDeclarations input@((_, CssTokBraceCurlyClose), _) = input
parseAllDeclarations input                                 = parseAllDeclarations . parseSingleDeclarationWrapper $ input




data CssRule = CssRule {
    complexSelector :: CssCachedComplexSelector
  , declarationSet  :: CssDeclarationSet
  , specificity     :: Int
  , position        :: Int
  } deriving (Eq)


instance Show CssRule where
  show (CssRule cs ds s p) = "Rule {" ++  show cs ++ "\n" ++
                                          show ds ++ "\n" ++
                             "spec = " ++ show s  ++ "\n" ++
                             "pos = "  ++ show p  ++ "}\n"




-- Get top compound selector
getTopCompound :: CssRule -> CssCompoundSelector
getTopCompound rule = chainGetFirstDatum . chain . complexSelector $ rule




-- A helper data type
--
-- https://www.w3.org/TR/css-syntax-3/#style-rules
--
-- "A style rule is a qualified rule that associates a selector list with a
-- list of property declarations and possibly a list of nested rules."
data CssParsedStyleRule = CssParsedStyleRule
  { -- "The prelude of the qualified rule is parsed as a <selector-list>. If
    -- this returns failure, the entire style rule is invalid."
    prelude :: [CssCachedComplexSelector]

    -- "The content of the qualified rule’s block is parsed as a style
    -- block’s contents."
  , content :: CssDeclarationSets
  } deriving (Show, Eq)




-- https://www.w3.org/TR/css-syntax-3/#style-rules
-- https://www.w3.org/TR/css-syntax-3/#qualified-rule
-- https://www.w3.org/TR/CSS22/syndata.html#rule-sets
--
-- Parse a style rule. On success get the ingredients of the rule:
--   a list of complex selectors from prelude of the rule (a <selector-list>)
--   a list of property declarations (actually two lists: normal declarations and important declarations)
--
-- https://www.w3.org/TR/css-syntax-3/#style-rules says that invalid selector
-- list invalidates entire style rule. But a single invalid property doesn't
-- invalidate the entire rule.
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
--
-- parseStyleRule (nextToken $ defaultParser "body {color:red ; background-color: #ffff00;line-height: normal h1{color:blue} h2{color: #001122} h3 {color : #998877;}")
--
-- Unit-tested: yes
parseStyleRule :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssParsedStyleRule)
parseStyleRule pat = case readSelectorList pat of
                       (pat', Nothing)           -> (pat', Nothing)
                       (pat', Just selectorList) -> case readDeclarationsBlockWithError pat' of
                                                      (pat'', Just declSets) -> (pat'', Just $ CssParsedStyleRule selectorList declSets)
                                                      (pat'', Nothing)       -> (pat'', Nothing)




-- Read declarations that are within a {} block.
--
-- The function expects some empty/initial/default declaration sets as input.
--
-- The function is not dealing with opening or closing brace.
--
-- TODO: this looks like a duplicate of parseAllDeclarations
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
-- readDeclarations (nextToken . defaultParserInBlock $ "border-top-color: #000001; border-right-color: #000002; border-bottom-color: #000003 !important; border-left-color: #000004", (defaultCssDeclarationSet, defaultCssDeclarationSet))
--
-- Unit-tested: yes
readDeclarations :: ((CssParser, CssToken), CssDeclarationSets) -> ((CssParser, CssToken), CssDeclarationSets)
readDeclarations input@((_, token), _) =
  case token of
    CssTokEnd             -> input
    CssTokBraceCurlyClose -> input
    _                     -> readDeclarations . parseSingleDeclarationWrapper $ input




-- Read a {} block with declarations.
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
--
-- readDeclarationsBlock (nextToken $ defaultParser "{} p.v")
-- readDeclarationsBlock (nextToken $ defaultParser "{color: rgb(0, 100, 0)} p.v")
-- readDeclarationsBlock (nextToken $ defaultParser " { color:rgb(0, 100, 0) !important} p.v")
readDeclarationsBlock :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssDeclarationSets)
readDeclarationsBlock = runParser $ parserOpeningBrace *> parserDeclarations <* parserClosingBrace
  where
    parserOpeningBrace :: Parser (CssParser, CssToken) CssToken
    parserOpeningBrace = Parser $ \ pat ->
      case runParser parser pat of
        Just ((p, t), declSet) -> Just ((p { inBlock = True }, t), declSet) -- inBlock = True: we enter {} block.
        Nothing                -> Nothing
      where
        parser = many parserTokenWhitespace *> parserTokenBraceCurlyOpen <* many parserTokenWhitespace


    parserClosingBrace :: Parser (CssParser, CssToken) CssToken
    parserClosingBrace = Parser $ \ pat ->
      case runParser parser pat of
        Just ((p, t), declSet) -> Just ((p { inBlock = False }, t), declSet) -- inBlock = False: we leave {} block.
        Nothing                -> Nothing
      where
        parser = many parserTokenWhitespace *> parserTokenBraceCurlyClose <* many parserTokenWhitespace


    -- Parse all declarations located between '{' and '}'.
    -- This function appears to never fail: it always returns some Just. This
    -- is because failure to parse a single declaration in declarations block
    -- result in skipping just the single declaration. Other declarations in
    -- the block may still be parsed correctly and be returned in Just.
    --
    -- TODO: check if a block with empty list of declarations is valid. If
    -- this function fails to correctly parse zero declarations, it will
    -- return empty CssDeclarationSets.
    parserDeclarations :: Parser (CssParser, CssToken) CssDeclarationSets
    parserDeclarations = Parser $ \ pat ->
      case readDeclarations (pat, (defaultCssDeclarationSet, defaultCssDeclarationSet)) of
        (pat', declSets) -> Just (pat', declSets)




-- Read a {} block with declarations. On success return the declaration sets.
-- On parse error do a recovery and move parser to end of invalid block.
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Declaration
--
-- readDeclarationsBlockWithError  (nextToken $ defaultParser " { color:rgb(0, 100, 0) !important } p.now ")   -- success
-- readDeclarationsBlockWithError  (nextToken $ defaultParser " { color:rgb(0, 100, 0) !importan } p.now ")    -- failure (invalid input)
readDeclarationsBlockWithError :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssDeclarationSets)
readDeclarationsBlockWithError pat = case readDeclarationsBlock pat of
                                       Just (pat', declSets) -> (pat', Just declSets)
                                       Nothing               -> (consumeRestOfCurlyBlock pat, Nothing)

