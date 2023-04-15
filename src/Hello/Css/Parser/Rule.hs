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

  , parseElementStyleAttribute

  -- These are exported only for tests
  , parseAllDeclarations

  , parserCssRules
  , parserImportRule
  , parserStyleRule2
  , parserMediaRule
  , parserInvalidRule
  , parserEnd
  , parseCssRules

  , parserStyleSheet
  , parserBody
  )
where




import Control.Applicative (Alternative(..))
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Text as T
-- import Debug.Trace

import Hello.Css.Declaration
import Hello.Css.MediaQuery
import Hello.Css.Parser.Declaration
import Hello.Css.Parser.Property
import Hello.Css.Parser.Selector
import Hello.Css.Parser.Value
import Hello.Css.Rule
import Hello.Css.Selector
import Hello.Css.Tokenizer
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



{-
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
-}




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
    (p1, t1) = startTokenizer parser -- Kick-off the parsing

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
                      }




{-
Read declarations that are within a {} block.

The function expects some empty/initial/default declaration sets as input.

The function is not dealing with opening or closing brace.

Unit-tested: yes

:m +Hello.Css.Parser.Declaration
:m +Hello.Css.Tokenizer
:m +Hello.Css.Parser.Property
:m +Hello.Css.Parser.Rule
:set prompt >

parseAllDeclarations  (startTokenizer . defaultParserInBlock $ "border-top-color: #000001; border-right-color: #000002; border-bottom-color: #000003 !important; border-left-color: #000004;", (defaultCssDeclarationSet, defaultCssDeclarationSet))
-}
parseAllDeclarations :: ((CssParser, CssToken), CssDeclarationSets) -> ((CssParser, CssToken), CssDeclarationSets)
parseAllDeclarations input@((_, CssTokEnd), _)             = input
parseAllDeclarations input@((_, CssTokBraceCurlyClose), _) = input
parseAllDeclarations input                                 = parseAllDeclarations . parseSingleDeclarationWrapper $ input




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
-- :m +Hello.Css.Parser.Property
--
-- parseStyleRule (startTokenizer $ defaultParser "body {color:red ; background-color: #ffff00;line-height: normal h1{color:blue} h2{color: #001122} h3 {color : #998877;}")
--
-- Unit-tested: yes
{-
parseStyleRule :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssParsedStyleRule)
parseStyleRule pat = case runParser parserStyleRule pat of
                        Nothing -> (consumeRestOfCurlyBlock pat, Nothing) -- Error recovery, skip invalid rule.
                        Just (pat', parsedStyleRule) -> (pat', Just parsedStyleRule)
-}




-- Parser of style rule: a list of complex selectors followed by {} block
-- with declarations.
{-
:m +Hello.Css.Parser.Declaration
:m +Hello.Css.Tokenizer
:m +Hello.Css.Parser.Property
:m +Hello.Css.Parser.Rule
:m +Hello.Utils.Parser
:set prompt >

runParser parserStyleRule  (startTokenizer $ defaultParser "body {color:red ; background-color: #ffff00;line-height: normal h1{color:blue} h2{color: #001122} h3 {color : #998877;}")


runParser parserStyleRule  (startTokenizer $ defaultParser "span.sd-info-block, span[class^=sd-key-] {display: none !important;}")
-}
parserStyleRule :: Parser (CssParser, CssToken) CssParsedStyleRule
parserStyleRule = Parser $ \ pat -> do
  (pat', selectorList) <- runParser parserSelectorList pat
  (pat'', declSets)    <- runParser parserDeclarationBlock pat'
  pure (pat'', CssParsedStyleRule { prelude = selectorList, content = declSets })




-- Read a {} block with declarations.
--
-- :m +Hello.Css.Parser.Declaration
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Css.Parser.Property
--
-- runParser parserDeclarationBlock (startTokenizer $ defaultParser "{} p.v")
-- runParser parserDeclarationBlock (startTokenizer $ defaultParser "{color: rgb(0, 100, 0)} p.v")
-- runParser parserDeclarationBlock (startTokenizer $ defaultParser " { color:rgb(0, 100, 0) !important} p.v")
parserDeclarationBlock :: Parser (CssParser, CssToken) CssDeclarationSets
parserDeclarationBlock = parserOpeningBrace *> parserDeclarations <* parserClosingBrace
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
      case parseAllDeclarations (pat, (defaultCssDeclarationSet, defaultCssDeclarationSet)) of
        (pat', declSets) -> Just (pat', declSets)




{-
:m +Hello.Css.Parser.Declaration
:m +Hello.Css.Tokenizer
:m +Hello.Css.Parser.Property
:m +Hello.Utils.Parser
:m +Hello.Css.Parser.Rule
:set prompt >


-- Notice that one of declarations in this line is invalid.
-- FIXME; the invalid declaration doesn't seem to be handled properly: color and background-color are attached to list of declarations of the rule.
runParser parserListOfStyleRules (startTokenizer $ defaultParser "body {color:red ; background-color: #ffff00;line-height: normal h1{color:blue} h2{color: #001122} h3 {color : #998877;}")

-- Notice that one of declarations in this line is invalid
runParser parserListOfStyleRules (startTokenizer $ defaultParser "body {color:red ; background-color: #ffff00;line-height: normal h1{color:blue} h2{color: #001122}@media print {}")

runParser parserListOfStyleRules (startTokenizer $ defaultParser "body {color:red ; background-color: #ffff00;line-height: normal !important} h1{color:blue} h2{color: #001122}@media print {}")

-- Notice that this is an invalid input for parser.
runParser parserListOfStyleRules (startTokenizer $ defaultParser "body {color:red ; background-color: #ffff00;line-")
-}
{-
parserListOfStyleRules :: Parser (CssParser, CssToken) [CssParsedStyleRule]
parserListOfStyleRules = many ((many parserTokenWhitespace) *> parserStyleRule <* (many parserTokenWhitespace))
-}



{-
Main parser for any supported CSS rule types.

It returns a list of rules because for each complex selector in selectors
list of a style rule the function will return separate rule.

TODO: handle CssTokAtKeyword tokens with values other than "media"/"import".
-}
parserCssRule2 :: Parser (CssParser, CssToken) [CssRule2]
parserCssRule2 = parserStyleRule2 <|> fmap (:[]) parserMediaRule <|> fmap (:[]) parserImportRule <|> fmap (:[]) parserInvalidRule <|> fmap (:[]) parserEnd



-- TODO: this is only temporary, until all parts of parserCssRule2 will be tested independently.
parserCssRule3 :: Parser (CssParser, CssToken) [CssRule2]
parserCssRule3 = parserStyleRule2 <|> fmap (:[]) parserMediaRule <|> fmap (:[]) parserImportRule



{-
Main parser for style rule.

It returns a list of style rules because for each complex selector in
selectors list the function will return separate style rule.
-}
parserStyleRule2 :: Parser (CssParser, CssToken) [CssRule2]
parserStyleRule2 = fmap parsedRuleToRule2 parserStyleRule
  where
    parsedRuleToRule2 parsedStyleRule = buildRules complexSelectors declSets []
      where
        complexSelectors = fmap mkComplexSelector (prelude parsedStyleRule)
        declSets         = content parsedStyleRule




{-
Main parser for media rule.

TODO: write better implementation, even if this implementation will just
consume rules' tokens.

TODO: check whether string comparison of "media" should be case-sensitive or
not.
-}
parserMediaRule :: Parser (CssParser, CssToken) CssRule2
parserMediaRule = Parser $ \ pat -> do
  (pat', _)           <- runParser (parserTokenAtKeyword "media") pat
  (pat'', mediaQuery) <- runParser parserMediaQuery pat'
  (pat''', ss)        <- runParser parserBody pat''
  pure (pat''', CssMediaRule mediaQuery ss)




{-
:m +Hello.Css.Parser.Declaration
:m +Hello.Css.Tokenizer
:m +Hello.Css.Parser.Property
:m +Hello.Utils.Parser
:m +Hello.Css.Parser.Rule
:set prompt >

runParser parserBody (startTokenizer $ defaultParser "{body {color:red;}}")
-}
parserBody :: Parser (CssParser, CssToken) [CssRule2]
parserBody = (many parserTokenWhitespace) *> parserTokenBraceCurlyOpen *> parserStyleSheet <* parserTokenBraceCurlyClose <* (many parserTokenWhitespace)





-- TODO: this is only a temporary approach to parsing a style sheet
parserStyleSheet :: Parser (CssParser, CssToken) [CssRule2]
parserStyleSheet = parserCssRules3









{-
Main parser for import rule.

TODO: write better implementation, even if this implementation will just
consume rules' tokens.

TODO: check whether string comparison of "import" should be case-sensitive or
not.
-}
parserImportRule :: Parser (CssParser, CssToken) CssRule2
parserImportRule = Parser $ \ pat -> fn pat
  where
    fn :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssRule2)
    fn pat =
      do
        (pat', _)     <- runParser (parserTokenAtKeyword "import") pat
        (pat'', url)  <- runParser (many parserTokenWhitespace *> (parserUrl <|> (fmap ParsedUrl parserTokenStringValue)) <* many parserTokenWhitespace) pat'
        (pat''', _)   <- runParser parserTokenSemicolon pat''
        pure (pat''', CssImportRule url)



{-
Main parser that consumes a rule that appears to be invalid: it's neither
style rule, nor media rule nor import rule.

TODO: write better implementation.
-}
parserInvalidRule :: Parser (CssParser, CssToken) CssRule2
parserInvalidRule = Parser $ \ (parser, token) -> case runParser ((many restOfCurlyBlock) <* parserTokenBraceCurlyClose) (parser, token) of
                                                    Just (pat', _) -> Just (pat', CssInvalidRule (T.take 100 (remainder parser)))
                                                    Nothing        -> Nothing




restOfCurlyBlock :: Parser (CssParser, CssToken) CssToken
restOfCurlyBlock = Parser $ \ (parser, token) -> case token of
                                                   CssTokBraceCurlyClose -> Nothing -- TODO: maybe this should be (Just ((parser, token), token))?
                                                   CssTokEnd             -> Nothing
                                                   t                     -> Just (nextToken parser, t)




{-
Parser needed to correctly handle end of input and terminate parsing. Or
maybe it's not so needed after all.

TODO: check if this parser is needed at all.

TODO: use better value constructor than CssInvalidRule2.
-}
parserEnd :: Parser (CssParser, CssToken) CssRule2
parserEnd = fmap CssInvalidRule2 (some (Parser $ \ (parser, token) -> if (token == CssTokEnd) then Nothing else Just ((parser, token), token)))




{-
Build rules from basic ingredients: from list of complex selectors and from a
set of declarations.

The two ingredients come from parsing a style rule that may have a list of
complex selectors, like this:

h1.x > h2.y, p.z div.x { color: blue; width: 10px; }

In this example we have a list of two complex selectors and one set of
declarations.
-}
buildRules :: [CssComplexSelector] -> CssDeclarationSets -> [CssRule2] -> [CssRule2]
buildRules []     _        acc = reverse acc
buildRules (x:xs) declSets acc | addBoth      = buildRules xs declSets ((CssStyleRule ruleImp True) : (CssStyleRule rule False) : acc)
                               | addRegular   = buildRules xs declSets ((CssStyleRule rule False) : acc)
                               | addImportant = buildRules xs declSets ((CssStyleRule ruleImp True) : acc)
                               | otherwise    = buildRules xs declSets acc

  where rule    = ruleCtor x (fst declSets)
        ruleImp = ruleCtor x (snd declSets)
        ruleCtor cplxSel decls = CssRule { complexSelector = cplxSel
                                         , declarationSet  = decls
                                         , specificity     = selectorSpecificity cplxSel
                                         , position        = 0 -- Position of a rule will be set at the moment of inserting the rule to CSS context
                                         }
        addRegular   = not . S.null . items . fst $ declSets  -- Should add a regular rule to accumulator?
        addImportant = not . S.null . items . snd $ declSets  -- Should add an important rule to accumulator?
        addBoth      = addRegular && addImportant             -- Should add both regular and imporant rules to accumulator?




{-
:m +Hello.Css.Parser.Declaration
:m +Hello.Css.Tokenizer
:m +Hello.Css.Parser.Property
:m +Hello.Utils.Parser
:m +Hello.Css.Parser.Rule
:set prompt >

-- Notice that one of declarations in this line is invalid.
-- FIXME; the invalid declaration doesn't seem to be handled properly: color and background-color are attached to list of declarations of the rule.
runParser parserCssRules (startTokenizer $ defaultParser "body {color:red ; background-color: #ffff00;line-height: normal h1{color:blue} h2{color: #001122} h3 {color : #998877;}")
-}
parserCssRules :: Parser (CssParser, CssToken) [CssRule2]
parserCssRules = fmap concat (some ((many parserTokenWhitespace) *> parserCssRule2 <* (many parserTokenWhitespace)))




parserCssRules3 :: Parser (CssParser, CssToken) [CssRule2]
parserCssRules3 = fmap concat (some ((many parserTokenWhitespace) *> parserCssRule3 <* (many parserTokenWhitespace)))




parseCssRules :: (CssParser, CssToken) -> ((CssParser, CssToken), [(CssRule, Bool)])
parseCssRules pat = (fmap . fmap) rule2ToRule (fromMaybe (pat, []) (runParser parserCssRules pat))
  where
    rule2ToRule :: CssRule2 -> (CssRule, Bool)
    rule2ToRule rule = case rule of
                         CssStyleRule r imp -> (r, imp)

                         -- These are treated as invalid rules because they aren't style rules.
                         --
                         -- TODO: figure out better way of handling Media and Import rules.
                         CssMediaRule sel ss -> wrapInvalidRule (T.pack ("MEDIA RULE: media selector = " ++ (show sel) ++ ", stylesheet = " ++ (show ss)))
                         CssImportRule x     -> wrapInvalidRule (T.pack ("IMPORT RULE" ++ (show x)))

                         CssInvalidRule x    -> wrapInvalidRule (T.append "INVALID RULE: " x)
                         CssInvalidRule2 x   -> wrapInvalidRule (T.pack ("INVALID RULE 2: " ++ (show x)))

    -- Wrap info about invalid rule in a default Css Style Rule.
    --
    -- I want to keep track of invalid rules in output of parser, and storing
    -- them like this allows me to do it.
    wrapInvalidRule text = ( defaultCssRule { declarationSet = declarationsSetUpdateOrAdd defaultCssDeclarationSet
                                              (CssDeclaration (CssPropertyXTooltip $ CssValueXTooltip text) False)
                                            }
                           , False)


