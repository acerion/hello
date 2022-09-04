{-
Copyright (C) 2021-2022 Kamil Ignacak acerion@wp.pl

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




module Hello.Css.Parser
  (
    ignoreBlock
  , consumeBlock
  , ignoreStatement

    -- Parsing of selector
  , parseCompoundSelectorTokens
  , parseCombinator
  , parseCompoundSelector
  , parseComplexSelectorTokens
  , takeComplexSelectorTokens
  , parseComplexSelector
  , parsePairs
  , makeComplexR
  , readSelectorList
  , removeSpaceTokens

  , declValueAsString
  , declValueAsURI

  , CssCombinator (..)

  , parseDeclarationWrapper
  , takePropertyNameToken

  , parseElementStyleAttribute
  , parseAllDeclarations
  , parseSingleDeclaration

  , declarationsSetUpdateOrAdd
  , declarationsSetAppend
  , CssDeclarationSet (..)
  , defaultCssDeclarationSet

  , CssRule (..)

  , getTopCompound
  )
  where




import Data.Data (toConstr)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Bits
import Debug.Trace

import Hello.Chain
import Hello.Utils
import Hello.Css.Declaration
import Hello.Css.Distance
import Hello.Css.ParserHelpers
import Hello.Css.Tokenizer
import Hello.Css.Selector
import Hello.Css.Value
import Colors
import HtmlTag




-- Only some of these properties are supported by this implementation.
cssPropertyCtors = M.fromList [
     ("background-attachment",  makeCssDeclarationBackgroundAttachment)
   , ("background-color",       makeCssDeclarationBackgroundColor)
   , ("background-image",       makeCssDeclarationBackgroundImage)
   , ("background-position",    makeCssDeclarationBackgroundPosition)
   , ("background-repeat",      makeCssDeclarationBackgroundRepeat)

   , ("border-collapse",        makeCssDeclarationBorderCollapse)
   , ("border-spacing",         makeCssDeclarationBorderSpacing)

   , ("border-top-color",       makeCssDeclarationBorderTopColor)
   , ("border-right-color",     makeCssDeclarationBorderRightColor)
   , ("border-bottom-color",    makeCssDeclarationBorderBottomColor)
   , ("border-left-color",      makeCssDeclarationBorderLeftColor)

   , ("border-top-style",       makeCssDeclarationBorderTopStyle)
   , ("border-right-style",     makeCssDeclarationBorderRightStyle)
   , ("border-bottom-style",    makeCssDeclarationBorderBottomStyle)
   , ("border-left-style",      makeCssDeclarationBorderLeftStyle)

   , ("border-top-width",       makeCssDeclarationBorderTopWidth)
   , ("border-right-width",     makeCssDeclarationBorderRightWidth)
   , ("border-bottom-width",    makeCssDeclarationBorderBottomWidth)
   , ("border-left-width",      makeCssDeclarationBorderLeftWidth)

   --, ("bottom",                 Nothing)
   --, ("caption-side",           Nothing)
   --, ("clear",                  Nothing)
   --, ("clip",                   Nothing)
   , ("color",                  makeCssDeclarationColor)
   , ("content",                makeCssDeclarationContent)
   --, ("counter-increment",      Nothing)
   --, ("counter-reset",          Nothing)
   , ("cursor",                 makeCssDeclarationCursor)
   --, ("direction",              Nothing)
   , ("display",                makeCssDeclarationDisplay)
   --, ("empty-cells",            Nothing)
   --, ("float",                  Nothing)
   , ("font-family",            makeCssDeclarationFontFamily)
   , ("font-size",              makeCssDeclarationFontSize)
   --, ("font-size-adjust",       Nothing)
   --, ("font-stretch",           Nothing)
   , ("font-style",             makeCssDeclarationFontStyle)
   , ("font-variant",           makeCssDeclarationFontVariant)
   , ("font-weight",            makeCssDeclarationFontWeight)
   , ("height",                 makeCssDeclarationHeight)
   --, ("left",                   Nothing)
   , ("letter-spacing",         makeCssDeclarationLetterSpacing)
   , ("line-height",            makeCssDeclarationLineHeight)
   , ("list-style-image",       makeCssDeclarationListStyleImage)
   , ("list-style-position",    makeCssDeclarationListStylePosition)
   , ("list-style-type",        makeCssDeclarationListStyleType)

   , ("margin-top",             makeCssDeclarationMarginTop)
   , ("margin-right",           makeCssDeclarationMarginRight)
   , ("margin-bottom",          makeCssDeclarationMarginBottom)
   , ("margin-left",            makeCssDeclarationMarginLeft)

   --, ("marker-offset",          Nothing)
   --, ("marks",                  Nothing)
   --, ("max-height",             Nothing)
   --, ("max-width",              Nothing)
   --, ("min-height",             Nothing)
   --, ("min-width",              Nothing)
   --, ("outline-color",          Nothing)
   --, ("outline-style",          Nothing)
   --, ("outline-width",          Nothing)
   --, ("overflow",               Nothing)
   , ("padding-bottom",         makeCssDeclarationPaddingBottom)
   , ("padding-left",           makeCssDeclarationPaddingLeft)
   , ("padding-right",          makeCssDeclarationPaddingRight)
   , ("padding-top",            makeCssDeclarationPaddingTop)
   --, ("position",               Nothing)
   --, ("quotes",                 Nothing)
   --, ("right",                  Nothing)
   , ("text-align",             makeCssDeclarationTextAlign)
   , ("text-decoration",        makeCssDeclarationTextDecoration)
   , ("text-indent",            makeCssDeclarationTextIndent)
   --, ("text-shadow",            Nothing)
   , ("text-transform",         makeCssDeclarationTextTransform)
   --, ("top",                    Nothing)
   --, ("unicode-bidi",           Nothing)
   , ("vertical-align",         makeCssDeclarationVerticalAlign)
   --, ("visibility",             Nothing)
   , ("white-space",            makeCssDeclarationWhitespace)
   , ("width",                  makeCssDeclarationWidth)
   , ("word-spacing",           makeCssDeclarationWordSpacing)
   --, ("z-index",                Nothing)
   ] :: M.Map T.Text DeclarationCtor




-- Mapping between shorthand name of property and a constructor
cssShorthandInfo :: M.Map T.Text DeclarationShorthandCtor
cssShorthandInfo = M.fromList [
    ("background",         makeCssDeclarationBackground)

    -- Parsing of this property is unit-tested.
  , ("border",             makeCssDeclarationBorder)

    -- Parsing of this property is unit-tested.
  , ("border-top",         makeCssDeclarationBorderTop)
    -- Parsing of this property is unit-tested.
  , ("border-right",       makeCssDeclarationBorderRight)
    -- Parsing of this property is unit-tested.
  , ("border-bottom",      makeCssDeclarationBorderBottom)
    -- Parsing of this property is unit-tested.
  , ("border-left",        makeCssDeclarationBorderLeft)

    -- Parsing of this property is unit-tested.
  , ("border-width",       makeCssDeclarationBorderWidth)
    -- Parsing of this property is unit-tested.
  , ("border-style",       makeCssDeclarationBorderStyle)
    -- Parsing of this property is unit-tested.
  , ("border-color",       makeCssDeclarationBorderColor)

  --, ("font",           makeCssDeclarationFont)

    -- Parsing of this property is unit-tested (poorly).
  , ("list-style",         makeCssDeclarationListStyle)

    -- Parsing of this property is unit-tested.
  , ("margin",             makeCssDeclarationMargin)

    -- Parsing of this property is unit-tested (poorly).
  , ("padding",            makeCssDeclarationPadding)
  ]




-- Use property name string to look up constructor of CSS declaration.
--
-- TODO: case-insensitive search?
getDeclarationCtorByName :: T.Text -> Maybe DeclarationCtor
getDeclarationCtorByName propertyName = M.lookup propertyName cssPropertyCtors




-- Use name of shorthand property to look up a helper structure used to parse
-- the shorthand property and the property's value.
--
-- TODO: case-insensitive search?
getShorthandCtorByName :: T.Text -> Maybe DeclarationShorthandCtor
getShorthandCtorByName shorthandName = M.lookup shorthandName cssShorthandInfo




{-
-- TODO: move getting leading minus to takeNumber. Have a clearer distinction
-- between minus being a part of a number and minus being a part of other
-- type of token. Make sure that this call: "nextToken
-- defaultParser{remainder="/* hello */ -"}" returns token type == TokenChar.
-- Or should it be treated as invalid?
takeLeadingMinus :: CssParser -> (CssParser, CssToken)
takeLeadingMinus parser = case T.uncons (remainder parser) of
                            Just (c, rem) | c == '-'  -> (parser{remainder = rem}, CssToken (T.singleton c) Nothing)
                                          | otherwise -> (parser, CssTokNone)
                            Nothing -> (parser, CssTokNone)
-}




declValueAsString :: Int -> (CssParser, CssToken) -> ((CssParser, CssToken), Maybe T.Text)
declValueAsString id (parser, token) = case ((retParser, retToken), value) of
                                         ((p, t), Just (CssValueTypeString s))  -> ((p, t), Just s)
                                         ((p, t), Nothing) -> ((p, t), Nothing)
  where
    ((retParser, retToken), value) | id == 10  = tokensAsValueString (parser, token) [] -- TODO: magic value
                                   | id == 12  = declValueAsURI (parser, token) []      -- TODO: magic value
                                   | otherwise = ((parser, token), Nothing)




-- Interpret current token as "string" value (value of type CssValueTypeString).
--
-- In case of "string" value there is no need to consume more than current
-- token to build the String, but for consistency with other similar
-- functions the function is still called "tokensAs...".
tokensAsValueString :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueString (p, (CssTokStr s)) _ = (nextToken1 p, Just (CssValueTypeString s))
tokensAsValueString (p, t) _             = ((p, t), Nothing)




declValueAsURI :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
declValueAsURI (parser, token) enums = case parseUrl (parser, token) of
                                         ((newParser, newToken), Just url) -> ((newParser, newToken), Just (CssValueTypeURI url))
                                         ((newParser, newToken), Nothing)  -> ((newParser, newToken), Nothing)




{-
tokenMatchesProperty :: CssToken -> CssPropertyInfo -> Maybe CssValueType
tokenMatchesProperty token propInfo = tokenMatchesProperty' token acceptedValueTypes enums
  where
    acceptedValueTypes = tripletSnd propInfo
    enums = tripletThrd propInfo

    tokenMatchesProperty' :: CssToken -> [CssValueType] -> [T.Text] -> Maybe CssValueType
    tokenMatchesProperty' token (t:ts) enums
                                             | t == CssValueTypeBgPosition =
                                                 case token of
                                                   CssTokIdent s -> if s == "center" || s == "left" || s == "right" || s == "top" || s == "bottom"
                                                                  then Just t
                                                                  else tokenMatchesProperty' token ts enums
                                                   CssTokNum n -> Just t   -- TODO: here we should better handle numeric background positions
                                                   _           -> tokenMatchesProperty' token ts enums

                                             | t == CssValueTypeLengthPercent || t == CssValueTypeLength || t == CssValueTypeLengthPercentNumber =
                                                 case token of
                                                   CssTokNum (CssNumF f)   -> if f < 0 then Nothing else Just t
                                                   CssTokNum (CssNumI i)   -> if i < 0 then Nothing else Just t
                                                   CssTokPerc (CssNumF f)  -> if f < 0 then Nothing else Just t
                                                   CssTokPerc (CssNumI i)  -> if i < 0 then Nothing else Just t
                                                   CssTokDim (CssNumF f) _ -> if f < 0 then Nothing else Just t
                                                   CssTokDim (CssNumI i) _ -> if i < 0 then Nothing else Just t
                                                   _              -> Nothing

                                             | t == CssValueTypeSignedLength =
                                                 case token of
                                                   CssTokNum _   -> Just t
                                                   CssTokPerc _  -> Just t
                                                   CssTokDim _ _ -> Just t
                                                   _             -> tokenMatchesProperty' token ts enums

                                             | t == CssValueTypeFontWeight =
                                                 case token of
                                                   CssTokNum (CssNumI i) -> if i >= 100 && i <= 900 -- TODO: this test of range is repeated in this file
                                                                            then Just t
                                                                            else tokenMatchesProperty' token ts enums
                                                   _                     -> tokenMatchesProperty' token ts enums
                                             | t == CssValueTypeURI =
                                                 case token of
                                                   CssTokIdent s -> if s == "url"
                                                                  then Just t
                                                                  else tokenMatchesProperty' token ts enums
                                                   _           -> tokenMatchesProperty' token ts enums
-}




-- TODO: rewrite with consumeBlock?
ignoreBlock :: CssParser -> (CssParser, CssToken)
ignoreBlock parser = ignoreBlock' (parser, CssTokNone) 0
  where
    ignoreBlock' (parser, tok@CssTokEnd) depth    = (parser, tok)
    ignoreBlock' (parser, CssTokBraceCurlyOpen) depth  = ignoreBlock' (nextToken1 parser) (depth + 1)
    ignoreBlock' (parser, CssTokBraceCurlyClose) depth = if depth == 1
                                                         then nextToken1 parser
                                                         else ignoreBlock' (nextToken1 parser) (depth - 1)
    ignoreBlock' (parser, tok) depth              = ignoreBlock' (nextToken1 parser) depth
{-
   while (tokenizer->type != CSS_TOKEN_TYPE_END) {
      if (tokenizer->type == CSS_TOKEN_TYPE_CHAR) {
         if (tokenizer->value[0] == '{') {
            depth++;
         } else if (tokenizer->value[0] == '}') {
            depth--;
            if (depth == 0) {
               nextToken(tokenizer, hll_css_parser);
               return;
            }
         }
      }
      nextToken(tokenizer, hll_css_parser);
   }
-}




-- TODO: this function can recognize only blocks enclosed by curly braces.
-- Make the function recognize all types of Css braces.
consumeBlock :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
consumeBlock (parser, token) = consumeBlock' (parser, token) [] []
  where
    -- Last argument (braces) is used to keep track of opened/closed braces
    -- to know what is the current nesting level of blocks.
    consumeBlock' (parser, tok@CssTokEnd) tokens braces                              = ((parser, tok), reverse tokens)
    consumeBlock' (parser, CssTokBraceCurlyOpen) tokens braces                       = consumeBlock' (nextToken1 parser) (CssTokBraceCurlyOpen : tokens) (CssTokBraceCurlyOpen : braces)
    consumeBlock' (parser, CssTokBraceCurlyClose) tokens (CssTokBraceCurlyOpen : []) = ((nextToken1 parser), reverse tokens)
    consumeBlock' (parser, CssTokBraceCurlyClose) tokens (CssTokBraceCurlyOpen : xs) = consumeBlock' (nextToken1 parser) (CssTokBraceCurlyClose : tokens) xs
    consumeBlock' (parser, tok) tokens braces                                        = consumeBlock' (nextToken1 parser) (tok : tokens) braces




ignoreStatement :: CssParser -> (CssParser, CssToken)
ignoreStatement parser = ignoreStatement' (parser, CssTokNone)
  where
    ignoreStatement' (parser, tok@CssTokEnd)    = (parser, tok)
    ignoreStatement' (parser, CssTokSemicolon)      = nextToken1 parser
    ignoreStatement' (parser, CssTokBraceCurlyOpen) = ignoreBlock parser
    ignoreStatement' (parser, tok)              = ignoreStatement' (nextToken1 parser)
{-
   while (tokenizer->type != CSS_TOKEN_TYPE_END) {
      if (tokenizer->type == CSS_TOKEN_TYPE_CHAR) {
         if (tokenizer->value[0] == ';') {
            nextToken(tokenizer, hll_css_parser);
            return;
         } else if (tokenizer->value[0] =='{') {
            ignoreBlock(tokenizer, hll_css_parser);
            return;
         }
      }
      nextToken(tokenizer, hll_css_parser);
   }
-}




cssParseWeight :: (CssParser, CssToken) -> ((CssParser, CssToken), Bool)
cssParseWeight (parser, CssTokDelim '!') = case nextToken1 parser of
                                             (newParser, CssTokIdent "important") -> (nextToken1 newParser, True)
                                             (newParser, tok)                     -> ((newParser, tok), False)
cssParseWeight (parser, tok)          = ((parser, tok), False)




-- Update compound selector with given subclass selector.
appendSubclassSelector :: CssCompoundSelector -> CssSubclassSelector -> CssCompoundSelector
appendSubclassSelector compound subSel =
  case subSel of
    CssClassSelector ident       -> compound {selectorClass = (selectorClass compound) ++ [ident]}
    CssPseudoClassSelector ident -> if T.null ident
                                    then compound
                                    else compound {selectorPseudoClass = (selectorPseudoClass compound) ++ [ident]}
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
parseCompoundSelector (Just compound, (CssTokDelim '*':tokens)) = parseCompoundSelector (Just compound, tokens)
parseCompoundSelector (Just compound, (CssTokIdent sym:tokens)) = case htmlTagIndex2 sym of
                                                                    Just idx -> parseCompoundSelector (Just (setSelectorTagName compound (CssTypeSelector idx)), tokens)
                                                                    Nothing  -> parseCompoundSelector (Just (setSelectorTagName compound (CssTypeSelectorUnknown)), tokens)
-- https://www.w3.org/TR/css-syntax-3/#tokenization: "Only hash tokens with
-- the "id" type are valid ID selectors."
parseCompoundSelector (Just compound, (CssTokHash CssHashId ident:tokens))      = parseCompoundSelector
                                                                                  (Just (appendSubclassSelector compound (CssIdSelector ident)), tokens)
parseCompoundSelector (Just compound, (CssTokDelim '.':CssTokIdent sym:tokens)) = parseCompoundSelector
                                                                                  (Just (appendSubclassSelector compound (CssClassSelector sym)), tokens)
parseCompoundSelector (Just compound, (CssTokColon:CssTokIdent sym:tokens))     = parseCompoundSelector
                                                                                  (Just (appendSubclassSelector compound (CssPseudoClassSelector sym)), tokens)
parseCompoundSelector (Just compound, tokens)                                   = Just (compound, tokens)
parseCompoundSelector (Nothing, _)                                              = Nothing




-- First take a single compound selector. Then, in properly built complex
-- selector, there should be zero or more pairs of combinator-compound. Take
-- the pairs, and combine them with the first compound into a complex
-- selector.
parseComplexSelectorTokens :: [CssToken] -> Maybe CssComplexSelector
parseComplexSelectorTokens tokens = case parseCompoundSelector (Just defaultCssCompoundSelector, tokens) of
                                      Nothing                  -> Nothing
                                      Just (compound, tokens2) -> case parsePairs tokens2 [] of
                                                                    Nothing         -> Nothing
                                                                    Just (_, pairs) -> Just (makeComplexR (Datum compound) pairs)




makeComplexR :: CssComplexSelector -> [(CssCombinator, CssCompoundSelector)] -> CssComplexSelector
makeComplexR compound pairs = foldr f compound pairs
  where
    f :: (CssCombinator, CssCompoundSelector) -> CssComplexSelector -> CssComplexSelector
    f x acc = Link (Datum (snd x)) (fst x) acc




parsePairs :: [CssToken] -> [(CssCombinator, CssCompoundSelector)] -> Maybe ([CssToken], [(CssCombinator, CssCompoundSelector)])
parsePairs [] acc   = Just ([], acc) -- There is no "combinator followed by selector" data. Don't return error here.
parsePairs tokens acc = case parseCombinator tokens of
                          Nothing                    -> Nothing
                          Just (combinator, tokens2) -> case parseCompoundSelector (Just defaultCssCompoundSelector, tokens2) of
                                                          Nothing -> Nothing
                                                          Just (compound, tokens3) -> parsePairs tokens3 ((combinator, compound):acc)




parseCompoundSelectorTokens :: [CssToken] -> CssCompoundSelector -> Maybe ([CssToken], CssCompoundSelector)
parseCompoundSelectorTokens (CssTokDelim '*':tokens) compound = parseCompoundSelectorTokens tokens (setSelectorTagName compound CssTypeSelectorUniv)
parseCompoundSelectorTokens (CssTokIdent sym:tokens) compound = case htmlTagIndex2 sym of
                                                                  Just idx -> parseCompoundSelectorTokens tokens (setSelectorTagName compound (CssTypeSelector idx))
                                                                  Nothing  -> parseCompoundSelectorTokens tokens (setSelectorTagName compound (CssTypeSelectorUnknown))
-- https://www.w3.org/TR/css-syntax-3/#tokenization: "Only hash tokens with
-- the "id" type are valid ID selectors."
parseCompoundSelectorTokens (CssTokHash CssHashId ident:tokens) compound      = parseCompoundSelectorTokens
                                                                                tokens (appendSubclassSelector compound (CssIdSelector ident))
parseCompoundSelectorTokens (CssTokDelim '.':CssTokIdent sym:tokens) compound = parseCompoundSelectorTokens
                                                                                tokens (appendSubclassSelector compound (CssClassSelector sym))
parseCompoundSelectorTokens (CssTokColon:CssTokIdent sym:tokens) compound     = parseCompoundSelectorTokens
                                                                                tokens (appendSubclassSelector compound (CssPseudoClassSelector sym))
parseCompoundSelectorTokens t@(CssTokDelim '>':tokens) compound = Just (t, compound)
parseCompoundSelectorTokens t@(CssTokDelim '+':tokens) compound = Just (t, compound)
parseCompoundSelectorTokens t@(CssTokWS:tokens)        compound = Just (t, compound)

parseCompoundSelectorTokens [] compound = Just ([], compound)
parseCompoundSelectorTokens _  compound = Nothing



setSelectorTagName compound t = compound { selectorTagName = t }


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
readSelectorList :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssCachedComplexSelector])
readSelectorList (parser, token) = parseSelectorWrapper (parser, token) []
  where
    parseSelectorWrapper (parser, token) acc =
      case parseComplexSelector (parser, token) of
        ((parser, token), Just selector) -> case token of
                                              CssTokComma -> parseSelectorWrapper (nextToken1 parser) (acc ++ [selector])
                                              otherwise   -> ((parser, token), acc ++ [selector])
        _                                -> ((parser, token), acc)




-- Find end of current selector (probably needed only if something goes wrong
-- during parsign of current selector).
consumeRestOfSelector pair@(parser, CssTokEnd)            = pair
consumeRestOfSelector pair@(parser, CssTokBraceCurlyOpen) = pair
consumeRestOfSelector pair@(parser, CssTokComma)          = pair
consumeRestOfSelector (parser, _)                         = consumeRestOfSelector . nextToken1 $ parser




-- Take all tokens until ',' or '{' or EOF is met. The tokens will be used to
-- create list of CssCompoundSelectors (with separating combinators). If input
-- stream starts with whitespace, discard the whitespace (don't return token
-- for it) - leading whitespace is certainly meaningless.
takeComplexSelectorTokens :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
takeComplexSelectorTokens (parser, token) = takeNext (parser, token) []
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
                                        CssTokWS   -> if length tokens == 0
                                                      then takeNext (nextToken2 parser) tokens
                                                      else takeNext (nextToken2 parser) (tokens ++ [token])
                                        CssTokNone -> takeNext (nextToken2 parser) tokens -- This token can be used to 'kick-start' of parsing
                                        otherwise  -> takeNext (nextToken2 parser) (tokens ++ [token])




-- A dumb way to remove spaces that are adjactent to '+' or '>' combinators.
-- In such situations the spaces aren't combinators themselves but are just
-- separators.
removeSpaceTokens :: [CssToken] -> [CssToken] -> [CssToken]
removeSpaceTokens ((CssTokWS):(CssTokDelim '+'):xs) acc = removeSpaceTokens ((CssTokDelim '+'):xs) acc
removeSpaceTokens ((CssTokDelim '+'):(CssTokWS):xs) acc = removeSpaceTokens ((CssTokDelim '+'):xs) acc
removeSpaceTokens ((CssTokWS):(CssTokDelim '>'):xs) acc = removeSpaceTokens ((CssTokDelim '>'):xs) acc
removeSpaceTokens ((CssTokDelim '>'):(CssTokWS):xs) acc = removeSpaceTokens ((CssTokDelim '>'):xs) acc
removeSpaceTokens (x:(CssTokWS):[]) acc              = acc ++ [x] -- Don't forget to remove ending whitespace too.
removeSpaceTokens (x:xs) acc                         = removeSpaceTokens xs (acc ++ [x])
removeSpaceTokens [] acc                             = acc




-- The isSafe flag compilcates this data type. I have to declare a new "Set"
-- type that is a wrapper around list of declarations + that one boolean
-- flag.
data CssDeclarationSet = CssDeclarationSet
  { isSafe :: Bool
  , items  :: S.Seq CssDeclWrapper
  } deriving (Show, Eq)


defaultCssDeclarationSet = CssDeclarationSet
  { isSafe = True
  , items  = S.fromList []
  }




parseDeclarationNormal :: (CssParser, CssToken) -> DeclarationCtor -> ((CssParser, CssToken), [CssDeclWrapper])
parseDeclarationNormal pat ctor = case ctor pat of
                                    (pat', Just declaration) -> (pat', [defaultDeclaration{property = declaration}])
                                    (pat', Nothing)          -> (pat', []) -- TODO: return here "pat'" or "pat"?




parseDeclarationShorthand :: (CssParser, CssToken) -> DeclarationShorthandCtor -> ((CssParser, CssToken), [CssDeclWrapper])
parseDeclarationShorthand pat ctor = ctor pat




takePropertyNameToken :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssToken)
takePropertyNameToken (parser, nameToken) =
  let (colonParser, colonToken) = nextToken1 parser
      (retParser, retToken) = nextToken1 colonParser
  in
    case (nameToken, colonToken) of
      (CssTokIdent _, CssTokColon) -> ((retParser, retToken), Just nameToken) -- Don't return ':' token. Only 'property name' token is significant to caller.
      _                            -> ((parser, nameToken), Nothing)




-- The function returns a list of declarations because a line in CSS with a
-- shorthand declaration will be translated in N corresponding "normal"
-- declarations. E.g. "border-color: red" shorthand will be translated into a
-- list of "normal" declarations that will look like this:
-- ["border-top-color: red"; "border-right-color: red"; "border-bottom-color: red"; "border-left-color: red"]
parseSingleDeclarationNormalOrShorthand :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclWrapper])
parseSingleDeclarationNormalOrShorthand pat = case takePropertyNameToken pat of
                                                (pat', Just (CssTokIdent sym)) -> case getDeclarationCtorByName sym of
                                                                                    Just ctor -> parseDeclarationNormal pat' ctor
                                                                                    Nothing   -> case getShorthandCtorByName sym of
                                                                                                   Just ctor -> parseDeclarationShorthand pat' ctor
                                                                                                   Nothing   -> (pat', [])
                                                (pat', _)                 -> (pat', [])




-- For non-shorthand declaration, this function should produce one-element
-- list. But a shorthand declaration translates into two or more regular
-- declarations, hence the return type contains a list of declarations.
parseSingleDeclaration :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclWrapper])
parseSingleDeclaration (p1, t1) = ((outParser, outToken), declarationsWithImportant)
  where
    ((p2, t2), declarations) = parseSingleDeclarationNormalOrShorthand (p1, t1)
    ((p3, t3), important) = cssParseWeight (p2, t2)
    declarationsWithImportant = if important
                                then markAsImportant <$> declarations
                                else declarations
    (outParser, outToken) = consumeRestOfDeclaration (p3, t3)

    markAsImportant inDecl = inDecl{important = True}




parseDeclarationWrapper :: (CssParser, CssToken) -> (CssDeclarationSet, CssDeclarationSet) -> ((CssParser, CssToken), (CssDeclarationSet, CssDeclarationSet))
parseDeclarationWrapper pat (inSet, inSetImp) = (pat', (outSet, outSetImp))
  where
    (pat', declarations) = parseSingleDeclaration pat
    (outSet, outSetImp)  = appendDeclarations declarations inSet inSetImp

    appendDeclarations :: [CssDeclWrapper] -> CssDeclarationSet -> CssDeclarationSet -> (CssDeclarationSet, CssDeclarationSet)
    appendDeclarations [] set setImp     = (set, setImp)
    appendDeclarations (d:ds) set setImp = if important d
                                           then appendDeclarations ds set (declarationsSetUpdateOrAdd setImp d)
                                           else appendDeclarations ds (declarationsSetUpdateOrAdd set d) setImp




-- Find end of current declaration (probably needed only if something goes
-- wrong during parsign of current declaration).
consumeRestOfDeclaration pair@(parser, CssTokEnd)             = pair
consumeRestOfDeclaration pair@(parser, CssTokBraceCurlyClose) = pair -- '}' is not a part of declaration, so don't go past it. Return '}' as current token.
consumeRestOfDeclaration (parser, CssTokSemicolon)            = nextToken1 parser
consumeRestOfDeclaration (parser, _)                          = consumeRestOfDeclaration . nextToken1 $ parser




{-
takeAllTokens :: (CssParser, CssToken) -> IO CssParser
takeAllTokens (parser,token) = do
  T.IO.putStrLn (remainder parser)
  let (p, t) = nextToken parser
  if cssTokenType t == CssTokEnd
    then return p
    else takeAllTokens . nextToken $ p
-}




declarationsSetUpdateOrAdd :: CssDeclarationSet -> CssDeclWrapper -> CssDeclarationSet
declarationsSetUpdateOrAdd declSet decl =
  case S.findIndexL pred seq of
    Just idx -> CssDeclarationSet {items = S.update idx decl seq, isSafe = newSafe declSet decl}
    Nothing  -> CssDeclarationSet {items = seq S.|> decl,         isSafe = newSafe declSet decl}
  where
    -- Use 'toConstr' to compare constructors, but values without passed to constructors.
    -- https://stackoverflow.com/questions/47861648/a-general-way-of-comparing-constructors-of-two-terms-in-haskell
    pred :: CssDeclWrapper -> Bool
    pred x = (toConstr . property $ x) == (toConstr . property $ decl)

    seq = items declSet

    newSafe :: CssDeclarationSet -> CssDeclWrapper -> Bool
    newSafe declSet decl = (isSafe declSet) && case property decl of
                                                 CssDeclarationDisplay _         -> False
                                                 CssDeclarationBackgroundImage _ -> False
                                                 otherwise                       -> True




{-
Merge values from incoming into target, return result of merging

I can't use a concatenation operator because the merging is not that
simple: it has to use declarationsSetUpdateOrAdd function.
-}
declarationsSetAppend :: CssDeclarationSet -> CssDeclarationSet -> CssDeclarationSet
declarationsSetAppend target incoming = if S.null . items $ incoming
                                        then target
                                        else declarationsSetAppend (declarationsSetUpdateOrAdd target iHead) iTail
  where
    iHead = S.index (items incoming) 0
    iTail = incoming {items = S.drop 1 (items incoming)}




{-
Parse CSS style information contained in "cssStyleAttribute". The buffer
contains value of "style" attribute of a html element.

import qualified Data.Sequence as S
let declSet    = CssDeclarationSet {isSafe = True, items = S.fromList []}
let declSetImp = CssDeclarationSet {isSafe = True, items = S.fromList []}
let cssStyleAttribute = "color: red !important; font-weight: bold"
parseElementStyleAttribute "" cssStyleAttribute (declSet, declSetImp)
-}
parseElementStyleAttribute :: T.Text -> T.Text -> (CssDeclarationSet, CssDeclarationSet) -> (CssDeclarationSet, CssDeclarationSet)
parseElementStyleAttribute baseUrl cssStyleAttribute (declSet, declSetImp) = (outDeclSet, outDeclSetImp)
  where
    ((p2, t2), (outDeclSet, outDeclSetImp)) = parseAllDeclarations ((p1, t1), (declSet, declSetImp))
    (p1, t1) = nextToken1 parser -- Kick-off the parsing

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




parseAllDeclarations :: ((CssParser, CssToken), (CssDeclarationSet, CssDeclarationSet)) -> ((CssParser, CssToken), (CssDeclarationSet, CssDeclarationSet))
parseAllDeclarations ((p1, t1), (declSet, declSetImp)) | t1 == CssTokEnd             = ((p1, t1), (declSet, declSetImp))
                                                       | t1 == CssTokBraceCurlyClose = ((p1, t1), (declSet, declSetImp))
                                                       | otherwise = parseAllDeclarations (parseDeclarationWrapper (p1, t1) (declSet, declSetImp))




data CssRule = CssRule {
    complexSelector :: CssCachedComplexSelector
  , declarationSet  :: CssDeclarationSet
  , specificity     :: Int
  , position        :: Int
  } deriving (Eq)


instance Show CssRule where
  show (CssRule cs ds s p) = "Rule {" ++  (show cs) ++ "\n" ++
                                          (show ds) ++ "\n" ++
                             "spec = " ++ (show s)  ++ "\n" ++
                             "pos = "  ++ (show p)  ++ "}\n"


-- Get top compound selector
getTopCompound :: CssRule -> CssCompoundSelector
getTopCompound rule = getTopCompound' . chain . complexSelector $ rule
getTopCompound' (Link (Datum c) _ remainder) = c
getTopCompound' (Datum c)                    = c

