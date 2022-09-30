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
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Sequence as S
--import Debug.Trace

import Hello.Chain
import Hello.Css.Declaration
import Hello.Css.ParserHelpers
import Hello.Css.Tokenizer
import Hello.Css.Selector
import Hello.Css.Value
import Hello.Html.Tag




-- Mapping between name of non-shorthand property and a constructor of the
-- property.
--
-- Only a subset of CSS2.2 properties is supported by this implementation.
cssPropertyCtors = M.fromList [
     ("background-attachment",  makeCssPropertyBackgroundAttachment)
   , ("background-color",       makeCssPropertyBackgroundColor)
   , ("background-image",       makeCssPropertyBackgroundImage)
   , ("background-position",    makeCssPropertyBackgroundPosition)
   , ("background-repeat",      makeCssPropertyBackgroundRepeat)

   , ("border-collapse",        makeCssPropertyBorderCollapse)
   , ("border-spacing",         makeCssPropertyBorderSpacing)

   , ("border-top-color",       makeCssPropertyBorderTopColor)
   , ("border-right-color",     makeCssPropertyBorderRightColor)
   , ("border-bottom-color",    makeCssPropertyBorderBottomColor)
   , ("border-left-color",      makeCssPropertyBorderLeftColor)

   , ("border-top-style",       makeCssPropertyBorderTopStyle)
   , ("border-right-style",     makeCssPropertyBorderRightStyle)
   , ("border-bottom-style",    makeCssPropertyBorderBottomStyle)
   , ("border-left-style",      makeCssPropertyBorderLeftStyle)

   , ("border-top-width",       makeCssPropertyBorderTopWidth)
   , ("border-right-width",     makeCssPropertyBorderRightWidth)
   , ("border-bottom-width",    makeCssPropertyBorderBottomWidth)
   , ("border-left-width",      makeCssPropertyBorderLeftWidth)

   --, ("bottom",                 Nothing)
   --, ("caption-side",           Nothing)
   --, ("clear",                  Nothing)
   --, ("clip",                   Nothing)
   , ("color",                  makeCssPropertyColor)
   , ("content",                makeCssPropertyContent)
   --, ("counter-increment",      Nothing)
   --, ("counter-reset",          Nothing)
   , ("cursor",                 makeCssPropertyCursor)
   --, ("direction",              Nothing)
   , ("display",                makeCssPropertyDisplay)
   --, ("empty-cells",            Nothing)
   --, ("float",                  Nothing)
   , ("font-family",            makeCssPropertyFontFamily)
   , ("font-size",              makeCssPropertyFontSize)
   --, ("font-size-adjust",       Nothing)
   --, ("font-stretch",           Nothing)
   , ("font-style",             makeCssPropertyFontStyle)
   , ("font-variant",           makeCssPropertyFontVariant)
   , ("font-weight",            makeCssPropertyFontWeight)
   , ("height",                 makeCssPropertyHeight)
   --, ("left",                   Nothing)
   , ("letter-spacing",         makeCssPropertyLetterSpacing)
   , ("line-height",            makeCssPropertyLineHeight)
   , ("list-style-image",       makeCssPropertyListStyleImage)
   , ("list-style-position",    makeCssPropertyListStylePosition)
   , ("list-style-type",        makeCssPropertyListStyleType)

   , ("margin-top",             makeCssPropertyMarginTop)
   , ("margin-right",           makeCssPropertyMarginRight)
   , ("margin-bottom",          makeCssPropertyMarginBottom)
   , ("margin-left",            makeCssPropertyMarginLeft)

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
   , ("padding-bottom",         makeCssPropertyPaddingBottom)
   , ("padding-left",           makeCssPropertyPaddingLeft)
   , ("padding-right",          makeCssPropertyPaddingRight)
   , ("padding-top",            makeCssPropertyPaddingTop)
   --, ("position",               Nothing)
   --, ("quotes",                 Nothing)
   --, ("right",                  Nothing)
   , ("text-align",             makeCssPropertyTextAlign)
   , ("text-decoration",        makeCssPropertyTextDecoration)
   , ("text-indent",            makeCssPropertyTextIndent)
   --, ("text-shadow",            Nothing)
   , ("text-transform",         makeCssPropertyTextTransform)
   --, ("top",                    Nothing)
   --, ("unicode-bidi",           Nothing)
   , ("vertical-align",         makeCssPropertyVerticalAlign)
   --, ("visibility",             Nothing)
   , ("white-space",            makeCssPropertyWhitespace)
   , ("width",                  makeCssPropertyWidth)
   , ("word-spacing",           makeCssPropertyWordSpacing)
   --, ("z-index",                Nothing)
   ] :: M.Map T.Text PropertyCtor




-- Mapping between name of shorthand property and a constructor of the
-- property.
--
-- Only a subset of CSS2.2 properties is supported by this implementation.
cssShorthandInfo :: M.Map T.Text ShorthandPropertyCtor
cssShorthandInfo = M.fromList [
    ("background",         makeCssPropertyBackground)

    -- Parsing of this property is unit-tested.
  , ("border",             makeCssPropertyBorder)

    -- Parsing of this property is unit-tested.
  , ("border-top",         makeCssPropertyBorderTop)
    -- Parsing of this property is unit-tested.
  , ("border-right",       makeCssPropertyBorderRight)
    -- Parsing of this property is unit-tested.
  , ("border-bottom",      makeCssPropertyBorderBottom)
    -- Parsing of this property is unit-tested.
  , ("border-left",        makeCssPropertyBorderLeft)

    -- Parsing of this property is unit-tested.
  , ("border-width",       makeCssPropertyBorderWidth)
    -- Parsing of this property is unit-tested.
  , ("border-style",       makeCssPropertyBorderStyle)
    -- Parsing of this property is unit-tested.
  , ("border-color",       makeCssPropertyBorderColor)

  , ("font",               makeCssPropertyFont)

    -- Parsing of this property is unit-tested (poorly).
  , ("list-style",         makeCssPropertyListStyle)

    -- Parsing of this property is unit-tested.
  , ("margin",             makeCssPropertyMargin)

    -- Parsing of this property is unit-tested (poorly).
  , ("padding",            makeCssPropertyPadding)
  ]




-- Use name of property to look up a constructor used to parse the property
-- and the property's value.
--
-- TODO: case-insensitive search?
getPropertyCtorByName :: T.Text -> Maybe PropertyCtor
getPropertyCtorByName propertyName = M.lookup propertyName cssPropertyCtors




-- Use name of shorthand property to look up a constructor used to parse the
-- shorthand property and the property's value.
--
-- TODO: case-insensitive search?
getShorthandCtorByName :: T.Text -> Maybe ShorthandPropertyCtor
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
declValueAsString propId (parser, token) = case ((retParser, retToken), value) of
                                             ((p, t), Just (CssValueTypeString s)) -> ((p, t), Just s)
                                             ((p, t), Just (CssValueTypeURI s))    -> ((p, t), Just s)
                                             ((p, t), Nothing)                     -> ((p, t), Nothing)
  where
    ((retParser, retToken), value) | propId == 10  = tokensAsValueString (parser, token) [] -- TODO: magic value
                                   | propId == 12  = declValueAsURI (parser, token)      -- TODO: magic value
                                   | otherwise     = ((parser, token), Nothing)




-- Interpret current token as "string" value (value of type CssValueTypeString).
--
-- In case of "string" value there is no need to consume more than current
-- token to build the String, but for consistency with other similar
-- functions the function is still called "tokensAs...".
tokensAsValueString :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), Maybe CssValue)
tokensAsValueString (p, (CssTokStr s)) _ = (nextToken1 p, Just (CssValueTypeString s))
tokensAsValueString (p, t) _             = ((p, t), Nothing)




declValueAsURI :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe CssValue)
declValueAsURI (parser, token) = case parseUrl (parser, token) of
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
    ignoreBlock' :: (CssParser, CssToken) -> Int -> (CssParser, CssToken)
    ignoreBlock' (par, tok@CssTokEnd) _             = (par, tok)
    ignoreBlock' (par, CssTokBraceCurlyOpen) depth  = ignoreBlock' (nextToken1 par) (depth + 1)
    ignoreBlock' (par, CssTokBraceCurlyClose) depth = if depth == 1
                                                      then nextToken1 par
                                                      else ignoreBlock' (nextToken1 par) (depth - 1)
    ignoreBlock' (par, _tok) depth                  = ignoreBlock' (nextToken1 par) depth
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
consumeBlock pat = consumeBlock' pat [] []
  where
    -- Last argument (braces) is used to keep track of opened/closed braces
    -- to know what is the current nesting level of blocks.
    consumeBlock' (parser, tok@CssTokEnd) tokens _                                   = ((parser, tok), reverse tokens)
    consumeBlock' (parser, CssTokBraceCurlyOpen) tokens braces                       = consumeBlock' (nextToken1 parser) (CssTokBraceCurlyOpen : tokens) (CssTokBraceCurlyOpen : braces)
    consumeBlock' (parser, CssTokBraceCurlyClose) tokens (CssTokBraceCurlyOpen : []) = ((nextToken1 parser), reverse tokens)
    consumeBlock' (parser, CssTokBraceCurlyClose) tokens (CssTokBraceCurlyOpen : xs) = consumeBlock' (nextToken1 parser) (CssTokBraceCurlyClose : tokens) xs
    consumeBlock' (parser, tok) tokens braces                                        = consumeBlock' (nextToken1 parser) (tok : tokens) braces




ignoreStatement :: CssParser -> (CssParser, CssToken)
ignoreStatement parser = ignoreStatement' (parser, CssTokNone)
  where
    ignoreStatement' (par, tok@CssTokEnd)        = (par, tok)
    ignoreStatement' (par, CssTokSemicolon)      = nextToken1 par
    ignoreStatement' (par, CssTokBraceCurlyOpen) = ignoreBlock par
    ignoreStatement' (par, _)                    = ignoreStatement' (nextToken1 par)
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




-- https://www.w3.org/TR/CSS22/cascade.html#important-rules
-- https://www.w3.org/TR/css-cascade-5/#importance
--
-- https://www.w3.org/TR/css-syntax-3/#consume-declaration: "If the last two
-- non-<whitespace-token>s in the declaration’s value are a <delim-token>
-- with the value "!" followed by an <ident-token> with a value that is an
-- ASCII case-insensitive match for "important", remove them from the
-- declaration’s value and set the declaration’s important flag to true."
cssParseImportance :: (CssParser, CssToken) -> ((CssParser, CssToken), Bool)
cssParseImportance (parser, CssTokDelim '!') = case nextToken1 parser of
                                                 (newParser, CssTokIdent "important") -> (nextToken1 newParser, True)
                                                 (newParser, tok)                     -> ((newParser, tok), False)
cssParseImportance (parser, tok)             = ((parser, tok), False)




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
parseCompoundSelectorTokens t@(CssTokDelim '>':_ts) compound = Just (t, compound)
parseCompoundSelectorTokens t@(CssTokDelim '+':_ts) compound = Just (t, compound)
parseCompoundSelectorTokens t@(CssTokWS:_ts)        compound = Just (t, compound)
parseCompoundSelectorTokens [] compound = Just ([], compound)
parseCompoundSelectorTokens _  _        = Nothing



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
readSelectorList pat = parseSelectorWrapper pat []
  where
    parseSelectorWrapper pat' acc =
      case parseComplexSelector pat' of
        ((parser, token), Just selector) -> case token of
                                              CssTokComma -> parseSelectorWrapper (nextToken1 parser) (acc ++ [selector])
                                              _           -> ((parser, token), acc ++ [selector])
        _                                -> (pat', acc)




-- Find end of current selector (probably needed only if something goes wrong
-- during parsign of current selector).
consumeRestOfSelector pair@(_, CssTokEnd)            = pair
consumeRestOfSelector pair@(_, CssTokBraceCurlyOpen) = pair
consumeRestOfSelector pair@(_, CssTokComma)          = pair
consumeRestOfSelector (parser, _)                    = consumeRestOfSelector . nextToken1 $ parser




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
                                        CssTokWS   -> if length tokens == 0
                                                      then takeNext (nextToken2 parser) tokens
                                                      else takeNext (nextToken2 parser) (tokens ++ [token])
                                        CssTokNone -> takeNext (nextToken2 parser) tokens -- This token can be used to 'kick-start' of parsing
                                        _          -> takeNext (nextToken2 parser) (tokens ++ [token])




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
  , items  :: S.Seq CssDeclaration
  } deriving (Show, Eq)


defaultCssDeclarationSet = CssDeclarationSet
  { isSafe = True
  , items  = S.fromList []
  }




parseDeclarationNormal :: (CssParser, CssToken) -> PropertyCtor -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationNormal pat propCtor = case propCtor pat of
                                        (pat', Just prop) -> (pat', [defaultDeclaration{property = prop}])
                                        (pat', Nothing)   -> (pat', []) -- TODO: return here "pat'" or "pat"?




parseDeclarationShorthand :: (CssParser, CssToken) -> ShorthandPropertyCtor -> ((CssParser, CssToken), [CssDeclaration])
parseDeclarationShorthand pat ctor = (pat', decls)
  where
    (pat', properties) = ctor pat
    decls = fmap (\x -> defaultDeclaration { property = x }) properties




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
parseSingleDeclarationNormalOrShorthand :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclaration])
parseSingleDeclarationNormalOrShorthand pat = case takePropertyNameToken pat of
                                                (pat', Just (CssTokIdent sym)) -> case getPropertyCtorByName sym of
                                                                                    Just ctor -> parseDeclarationNormal pat' ctor
                                                                                    Nothing   -> case getShorthandCtorByName sym of
                                                                                                   Just ctor -> parseDeclarationShorthand pat' ctor
                                                                                                   Nothing   -> (pat', [])
                                                (pat', _)                 -> (pat', [])




-- For non-shorthand declaration, this function should produce one-element
-- list. But a shorthand declaration translates into two or more regular
-- declarations, hence the return type contains a list of declarations.
parseSingleDeclaration :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssDeclaration])
parseSingleDeclaration (p1, t1) = ((outParser, outToken), declarationsWithImportant)
  where
    ((p2, t2), declarations) = parseSingleDeclarationNormalOrShorthand (p1, t1)
    ((p3, t3), isImportant) = cssParseImportance (p2, t2)
    declarationsWithImportant = if isImportant
                                then markAsImportant <$> declarations
                                else declarations
    (outParser, outToken) = consumeRestOfDeclaration (p3, t3)

    markAsImportant inDecl = inDecl{important = True}




parseDeclarationWrapper :: (CssParser, CssToken) -> (CssDeclarationSet, CssDeclarationSet) -> ((CssParser, CssToken), (CssDeclarationSet, CssDeclarationSet))
parseDeclarationWrapper pat (inSet, inSetImp) = (pat', (outSet, outSetImp))
  where
    (pat', declarations) = parseSingleDeclaration pat
    (outSet, outSetImp)  = appendDeclarations declarations inSet inSetImp

    appendDeclarations :: [CssDeclaration] -> CssDeclarationSet -> CssDeclarationSet -> (CssDeclarationSet, CssDeclarationSet)
    appendDeclarations [] set setImp     = (set, setImp)
    appendDeclarations (d:ds) set setImp = if important d
                                           then appendDeclarations ds set (declarationsSetUpdateOrAdd setImp d)
                                           else appendDeclarations ds (declarationsSetUpdateOrAdd set d) setImp




-- Find end of current declaration (probably needed only if something goes
-- wrong during parsign of current declaration).
consumeRestOfDeclaration pair@(_, CssTokEnd)             = pair
consumeRestOfDeclaration pair@(_, CssTokBraceCurlyClose) = pair -- '}' is not a part of declaration, so don't go past it. Return '}' as current token.
consumeRestOfDeclaration (parser, CssTokSemicolon)       = nextToken1 parser
consumeRestOfDeclaration (parser, _)                     = consumeRestOfDeclaration . nextToken1 $ parser




{-
takeAllTokens :: (CssParser, CssToken) -> IO CssParser
takeAllTokens (parser,token) = do
  T.IO.putStrLn (remainder parser)
  let (p, t) = nextToken parser
  if cssTokenType t == CssTokEnd
    then return p
    else takeAllTokens . nextToken $ p
-}




declarationsSetUpdateOrAdd :: CssDeclarationSet -> CssDeclaration -> CssDeclarationSet
declarationsSetUpdateOrAdd declSet decl =
  case S.findIndexL predicate ix of
    Just idx -> CssDeclarationSet {items = S.update idx decl ix, isSafe = newSafe declSet decl}
    Nothing  -> CssDeclarationSet {items = ix S.|> decl,         isSafe = newSafe declSet decl}
  where
    -- Use 'toConstr' to compare constructors, but values without passed to constructors.
    -- https://stackoverflow.com/questions/47861648/a-general-way-of-comparing-constructors-of-two-terms-in-haskell
    predicate :: CssDeclaration -> Bool
    predicate x = (toConstr . property $ x) == (toConstr . property $ decl)

    ix = items declSet

    newSafe :: CssDeclarationSet -> CssDeclaration -> Bool
    newSafe declSet' decl' = (isSafe declSet') && case property decl' of
                                                    CssPropertyDisplay _         -> False
                                                    CssPropertyBackgroundImage _ -> False
                                                    _                            -> True




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
parseElementStyleAttribute _baseUrl cssStyleAttribute (declSet, declSetImp) = (outDeclSet, outDeclSetImp)
  where
    ((_p2, _t2), (outDeclSet, outDeclSetImp)) = parseAllDeclarations ((p1, t1), (declSet, declSetImp))
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
getTopCompound rule = chainGetFirstDatum . chain . complexSelector $ rule

