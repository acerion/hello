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
    readSelectorList
  , parseComplexSelector
  )
where




import qualified Data.Text as T
--import Debug.Trace

import Hello.Chain
import Hello.Css.Tokenizer
import Hello.Css.Selector
import Hello.Html.Tag




-- Update compound selector with given subclass selector.
appendSubclassSelector :: CssCompoundSelector -> CssSubclassSelector -> CssCompoundSelector
appendSubclassSelector compound subSel =
  case subSel of
    CssClassSelector ident       -> compound {selectorClass = selectorClass compound ++ [ident]}
    CssPseudoClassSelector ident -> if T.null ident
                                    then compound
                                    else compound {selectorPseudoClass = selectorPseudoClass compound ++ [ident]}
    CssIdSelector ident          -> if selectorId compound == ""
                                    then compound {selectorId = ident}
                                    else compound  -- TODO: is this valid that we ignore new value of the field without any warning?




-- Create a comples selector from a group of tokens that are terminated by
-- ',' or '{' character.
--
-- https://www.w3.org/TR/selectors-4/#structure: "A complex selector is a
-- sequence of one or more compound selectors separated by combinators."
--
-- Function always consumes the group of tokens, regardless of
-- success/failure of the parsing.
parseComplexSelector :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssCachedComplexSelector)
parseComplexSelector (parser, token) = ((outParser, outToken), selector)
  where
    (outParser, outToken) = consumeRestOfSelector (p2, t2)
    ((p2, t2), selector) = case parseComplexSelectorTokens (removeSpaceTokens cplxSelTokens []) of
                             Just xs -> ((newParser, newToken), Just defaultComplexSelector{chain = xs})
                             Nothing -> ((newParser, newToken), Nothing)

    ((newParser, newToken), cplxSelTokens) = takeComplexSelectorTokens (parser, token)




parseCombinator :: [CssToken] -> Maybe (CssCombinator, [CssToken])
parseCombinator (CssTokDelim '>':tokens) = Just (CssCombinatorChild, tokens)
parseCombinator (CssTokDelim '+':tokens) = Just (CssCombinatorAdjacentSibling, tokens)
parseCombinator (CssTokWS:tokens)        = Just (CssCombinatorDescendant, tokens)
parseCombinator _                        = Nothing




parseCompoundSelector :: (Maybe CssCompoundSelector, [CssToken]) -> Maybe (CssCompoundSelector, [CssToken])
parseCompoundSelector (Just compound, CssTokDelim '*':tokens) = parseCompoundSelector (Just compound, tokens)
parseCompoundSelector (Just compound, CssTokIdent sym:tokens) = case htmlTagIndex2 sym of
                                                                  Just idx -> parseCompoundSelector (Just (setSelectorTagName compound (CssTypeSelector idx)), tokens)
                                                                  Nothing  -> parseCompoundSelector (Just (setSelectorTagName compound CssTypeSelectorUnknown), tokens)
  where
    setSelectorTagName :: CssCompoundSelector -> CssTypeSelector -> CssCompoundSelector
    setSelectorTagName cpd t = cpd { selectorTagName = t }
-- https://www.w3.org/TR/css-syntax-3/#tokenization: "Only hash tokens with
-- the "id" type are valid ID selectors."
parseCompoundSelector (Just compound, CssTokHash CssHashId ident:tokens)      = parseCompoundSelector (Just (appendSubclassSelector compound (CssIdSelector ident)), tokens)
parseCompoundSelector (Just compound, CssTokDelim '.':CssTokIdent sym:tokens) = parseCompoundSelector (Just (appendSubclassSelector compound (CssClassSelector sym)), tokens)
parseCompoundSelector (Just compound, CssTokColon:CssTokIdent sym:tokens)     = parseCompoundSelector (Just (appendSubclassSelector compound (CssPseudoClassSelector sym)), tokens)
parseCompoundSelector (Just compound, tokens)                                 = Just (compound, tokens)
parseCompoundSelector (Nothing, _)                                            = Nothing




{-
parseCompoundSelectorTokens :: [CssToken] -> CssCompoundSelector -> Maybe ([CssToken], CssCompoundSelector)
parseCompoundSelectorTokens (CssTokDelim '*':tokens) compound = parseCompoundSelectorTokens tokens (setSelectorTagName compound CssTypeSelectorUniversal)
parseCompoundSelectorTokens (CssTokIdent sym:tokens) compound = case htmlTagIndex2 sym of
                                                                  Just idx -> parseCompoundSelectorTokens tokens (setSelectorTagName compound (CssTypeSelector idx))
                                                                  Nothing  -> parseCompoundSelectorTokens tokens (setSelectorTagName compound CssTypeSelectorUnknown)
-- https://www.w3.org/TR/css-syntax-3/#tokenization: "Only hash tokens with
-- the "id" type are valid ID selectors."
parseCompoundSelectorTokens (CssTokHash CssHashId ident:tokens) compound      = parseCompoundSelectorTokens
                                                                                tokens (appendSubclassSelector compound (CssIdSelector ident))
parseCompoundSelectorTokens (CssTokDelim '.':CssTokIdent sym:tokens) compound = parseCompoundSelectorTokens
                                                                                tokens (appendSubclassSelector compound (CssClassSelector sym))
parseCompoundSelectorTokens (CssTokColon:CssTokIdent sym:tokens) compound     = parseCompoundSelectorTokens
                                                                                tokens (appendSubclassSelector compound (CssPseudoClassSelector sym))
parseCompoundSelectorTokens t@(CssTokDelim '>':_ts) compound = Just (t, compound)
parseCompoundSelectorTokens t@(CssTokDelim '+':_ts) compound = Just (t, compound)
parseCompoundSelectorTokens t@(CssTokWS:_ts)        compound = Just (t, compound)
parseCompoundSelectorTokens [] compound = Just ([], compound)
parseCompoundSelectorTokens _  _        = Nothing
-}




-- First take a single compound selector. Then, in properly built complex
-- selector, there should be zero or more pairs of combinator-compound. Take
-- the pairs, and combine them with the first compound into a complex
-- selector.
parseComplexSelectorTokens :: [CssToken] -> Maybe CssComplexSelector
parseComplexSelectorTokens tokens = case parseCompoundSelector (Just defaultCssCompoundSelector, tokens) of
                                      Nothing                  -> Nothing
                                      Just (compound, tokens2) -> case parsePairs tokens2 [] of
                                                                    Nothing         -> Nothing
                                                                    Just (_, pairs) -> Just (makeComplexR (Last compound) pairs)




makeComplexR :: CssComplexSelector -> [(CssCombinator, CssCompoundSelector)] -> CssComplexSelector
makeComplexR compound pairs = foldr f compound pairs
  where
    f :: (CssCombinator, CssCompoundSelector) -> CssComplexSelector -> CssComplexSelector
    f x acc = Chain (snd x) (fst x) acc




parsePairs :: [CssToken] -> [(CssCombinator, CssCompoundSelector)] -> Maybe ([CssToken], [(CssCombinator, CssCompoundSelector)])
parsePairs [] acc   = Just ([], acc) -- There is no "combinator followed by selector" data. Don't return error here.
parsePairs tokens acc = case parseCombinator tokens of
                          Nothing                    -> Nothing
                          Just (combinator, tokens2) -> case parseCompoundSelector (Just defaultCssCompoundSelector, tokens2) of
                                                          Nothing -> Nothing
                                                          Just (compound, tokens3) -> parsePairs tokens3 ((combinator, compound):acc)




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
readSelectorList :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe [CssCachedComplexSelector])
readSelectorList pat = parseSelectorWrapper pat []
  where
    parseSelectorWrapper pat' acc =
      case parseComplexSelector pat' of
        ((parser, token), Just selector) -> case token of
                                              CssTokComma -> parseSelectorWrapper (nextToken parser) (acc ++ [selector])
                                              _           -> ((parser, token), Just $ acc ++ [selector])
        _                                -> (pat', Just acc)




-- Find end of current selector (probably needed only if something goes wrong
-- during parsign of current selector).
consumeRestOfSelector :: (CssParser, CssToken) -> (CssParser, CssToken)
consumeRestOfSelector pair@(_, CssTokEnd)            = pair
consumeRestOfSelector pair@(_, CssTokBraceCurlyOpen) = pair
consumeRestOfSelector pair@(_, CssTokComma)          = pair
consumeRestOfSelector (parser, _)                    = consumeRestOfSelector . nextToken $ parser




-- Take all tokens until ',' or '{' or EOF is met. The tokens will be used to
-- create list of CssCompoundSelectors (with separating combinators). If input
-- stream starts with whitespace, discard the whitespace (don't return token
-- for it) - leading whitespace is certainly meaningless.
takeComplexSelectorTokens :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
takeComplexSelectorTokens pat = takeNext pat []
  where
    takeNext :: (CssParser, CssToken) -> [CssToken] -> ((CssParser, CssToken), [CssToken])
    takeNext (parser, token) tokens = case token of
                                        CssTokBraceCurlyOpen -> ((parser, token), tokens)
                                        CssTokComma          -> ((parser, token), tokens)
                                        CssTokEnd    -> ((parser, token), tokens)
                                        -- Ignore whitespace occurring at the
                                        -- beginning of selectors list. I
                                        -- could filter it out later with
                                        -- removeSpaceTokens, but it's easier
                                        -- to not to add it at all.
                                        CssTokWS   -> if null tokens
                                                      then takeNext (nextToken parser) tokens
                                                      else takeNext (nextToken parser) (tokens ++ [token])
                                        CssTokNone -> takeNext (nextToken parser) tokens -- This token can be used to 'kick-start' of parsing
                                        _          -> takeNext (nextToken parser) (tokens ++ [token])




-- A dumb way to remove spaces that are adjactent to '+' or '>' combinators.
-- In such situations the spaces aren't combinators themselves but are just
-- separators.
removeSpaceTokens :: [CssToken] -> [CssToken] -> [CssToken]
removeSpaceTokens (CssTokWS:(CssTokDelim '+'):xs) acc = removeSpaceTokens (CssTokDelim '+':xs) acc
removeSpaceTokens ((CssTokDelim '+'):CssTokWS:xs) acc = removeSpaceTokens (CssTokDelim '+':xs) acc
removeSpaceTokens (CssTokWS:(CssTokDelim '>'):xs) acc = removeSpaceTokens (CssTokDelim '>':xs) acc
removeSpaceTokens ((CssTokDelim '>'):CssTokWS:xs) acc = removeSpaceTokens (CssTokDelim '>':xs) acc
removeSpaceTokens [x, CssTokWS] acc                   = acc ++ [x] -- Don't forget to remove ending whitespace too.
removeSpaceTokens (x:xs) acc                          = removeSpaceTokens xs (acc ++ [x])
removeSpaceTokens [] acc                              = acc




