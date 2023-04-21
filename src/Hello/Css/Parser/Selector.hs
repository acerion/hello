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
{-# LANGUAGE ScopedTypeVariables #-}




module Hello.Css.Parser.Selector
  (
    parserSelectorList

  -- Just for unit tests.
  , parserCompoundSelector
  , parserComplexSelector
  )
where




import Control.Applicative
import qualified Data.Text as T
--import Debug.Trace

import Hello.Css.Tokenizer
import Hello.Css.Selector
import Hello.Html.Tag
import Hello.Utils.Parser




-- Update compound selector with given subclass selector.
setSubclassSelector :: CssCompoundSelector -> CssSubclassSelector -> CssCompoundSelector
setSubclassSelector compound subSel =
  case subSel of
    CssClassSelector ident       -> compound {selectorClass = selectorClass compound ++ [ident]}
    CssPseudoClassSelector ident -> if T.null ident
                                    then compound
                                    else compound {selectorPseudoClass = selectorPseudoClass compound ++ [ident]}
    CssIdSelector ident          -> if selectorId compound == ""
                                    then compound {selectorId = ident}
                                    else compound  -- TODO: is this valid that we ignore new value of the field without any warning?




-- https://www.w3.org/TR/selectors-4/#structure: "A complex selector is a
-- sequence of one or more compound selectors separated by combinators."
--
-- Function always consumes the group of tokens, regardless of
-- success/failure of the parsing.
--
-- :m +Hello.Css.Tokenizer
-- :m +Hello.Utils.Parser
-- :m +Hello.Css.Parser.Selector
--
-- runParser parserComplexSelector (startTokenizer . defaultParser $ "b    >   head")
-- runParser parserComplexSelector (startTokenizer . defaultParser $ "a    >   head + a")
--
-- HASKELL FEATURE: APPLICATIVE FUNCTOR
parserComplexSelector :: Parser (CssParser, CssToken) CssParsedComplexSelector
parserComplexSelector = ((:) <$> parserFirstCompound <*> fmap concat (many parserCombinatorAndCompound))
  where
    -- A first compound selector in a complex selector may be preceded with spaces.
    parserFirstCompound :: Parser (CssParser, CssToken) ComplexItem
    parserFirstCompound = many parserTokenWhitespace *> ((fmap) CompoundItem (parserCompoundSelector))

    parserCombinatorAndCompound :: Parser (CssParser, CssToken) CssParsedComplexSelector
    parserCombinatorAndCompound = (( \ a b -> [a, b]) <$> parserCombinator <*> ((fmap) CompoundItem (parserCompoundSelector)))




-- :m +Hello.Css.Tokenizer
-- :m +Hello.Utils.Parser
-- :m +Hello.Css.Parser.Selector
--
-- runParser parserCombinator  (startTokenizer . defaultParser $ "    >   head")
parserCombinator :: Parser (CssParser, CssToken) ComplexItem
parserCombinator = Parser $ \ pat -> parseCombinator pat




parseCombinator :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), ComplexItem)
parseCombinator pat = case runParser parser pat of
                        Just (pat', CssTokWS)        -> Just (pat', CombinatorItem CssCombinatorDescendant)
                        Just (pat', CssTokDelim '>') -> Just (pat', CombinatorItem CssCombinatorChild)
                        Just (pat', CssTokDelim '+') -> Just (pat', CombinatorItem CssCombinatorAdjacentSibling)
                        _                            -> Nothing
  where
    parser = many parserTokenWhitespace *> parserTokenDelim '>'  <* many parserTokenWhitespace
             <|> many parserTokenWhitespace *> parserTokenDelim '+'  <* many parserTokenWhitespace
             <|> parserTokenWhitespace <* many parserTokenWhitespace




-- Unit-tested: yes
parserCompoundSelector :: Parser (CssParser, CssToken) CssCompoundSelector
parserCompoundSelector = Parser $ \ pat -> parseCompound [] pat




-- Parse a compound selector.
-- https://www.w3.org/TR/selectors-4/#compound
--
-- TODO: type selector and explicit universal selector are allowed in a
-- compound selector only when type/universal is first on the list:
-- https://www.w3.org/TR/selectors-4/#compound: "If it contains a type
-- selector or universal selector, that selector must come first in the
-- sequence.".
parseCompound :: [CssSimpleSelector] -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssCompoundSelector)
parseCompound acc (parser, CssTokDelim '*') = parseCompound (CssSimpleSelectorType CssTypeSelectorUniversal:acc) (nextToken parser)
parseCompound acc (parser, CssTokIdent sym) = case htmlTagIndex2 sym of
                                                Just idx -> parseCompound (CssSimpleSelectorType (CssTypeSelector idx):acc) (nextToken parser)
                                                Nothing  -> parseCompound (CssSimpleSelectorType CssTypeSelectorUnknown:acc) (nextToken parser)
-- https://www.w3.org/TR/css-syntax-3/#tokenization: "Only hash tokens with
-- the "id" type are valid ID selectors."
parseCompound acc (parser, CssTokHash CssHashId ident) = parseCompound (CssSimpleSelectorSubclass (CssIdSelector ident):acc) (nextToken parser)
parseCompound acc pat@(_, CssTokComma)                 = finalizeCompound acc pat
parseCompound acc pat@(_, CssTokBraceCurlyOpen)        = finalizeCompound acc pat
parseCompound acc pat@(_, CssTokEnd)                   = finalizeCompound acc pat
parseCompound acc pat@(_, CssTokWS)                    = finalizeCompound acc pat
parseCompound acc (parser, CssTokDelim '.') = case nextToken parser of
                                                (parser', CssTokIdent sym) -> parseCompound (CssSimpleSelectorSubclass (CssClassSelector sym):acc) (nextToken parser')
                                                _                          -> Nothing
parseCompound acc (parser, CssTokColon)     = case nextToken parser of
                                                (parser', CssTokIdent sym) -> parseCompound (CssSimpleSelectorSubclass (CssPseudoClassSelector sym):acc) (nextToken parser')
                                                _                          -> Nothing
-- Don't just return Nothing: acc may contain some simple selectors, so the
-- fact that we don't match any token in this pattern doesn't mean that we
-- encounter invalid situation. Call 'finalizeCompound' instead.
parseCompound acc pat = finalizeCompound acc pat




-- Convert list of simple selectors into compound selector.
--
-- Utility function to be used in few places of parseCompound to nicely
-- check, wrap and return a value of desided type.
finalizeCompound :: [CssSimpleSelector] -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssCompoundSelector)
finalizeCompound []   _ = Nothing
finalizeCompound ss pat = Just (pat, compound)
  where
    compound = foldr f defaultCssCompoundSelector ss
    f (CssSimpleSelectorType CssTypeSelectorUniversal) cpd = cpd { selectorTagName = CssTypeSelectorUniversal }
    f (CssSimpleSelectorType (CssTypeSelector idx)) cpd    = cpd { selectorTagName = CssTypeSelector idx }
    f (CssSimpleSelectorType CssTypeSelectorUnknown) cpd   = cpd { selectorTagName = CssTypeSelectorUnknown }
    f (CssSimpleSelectorSubclass subclass) cpd             = setSubclassSelector cpd subclass




-- Parser of list of complex selectors separated with comma.
--
-- https://www.w3.org/TR/selectors-4/#list-of-simple-selectors
--
-- https://www.w3.org/TR/selectors-4/#grouping: "If just one of these
-- selectors were invalid, the entire selector list would be invalid."
--
-- Parse entire list of selectors that are separated with comma.
-- Function name is matching CSS Selectors Level 4 terminology.
--
-- https://www.w3.org/TR/selectors-4/#structure:
-- "A list of simple/compound/complex selectors is a comma-separated list of
-- simple, compound, or complex selectors. This is also called just a
-- selector list when the type is either unimportant or specified in the
-- surrounding prose; if the type is important and unspecified, it defaults
-- to meaning a list of complex selectors."
--
-- Note from dillo:
--
-- TODO: dump whole ruleset in case of parse error as required by CSS 2.1
-- however make sure we don't dump it if only dillo fails to parse valid CSS.
--
-- Unit-tested: yes, but with issues
parserSelectorList :: Parser (CssParser, CssToken) [CssParsedComplexSelector]
parserSelectorList = parserSeparatedList parserComplexSelector parserSeparator
  where
    parserSeparator = (many parserTokenWhitespace) *> parserTokenComma <* (many parserTokenWhitespace)



