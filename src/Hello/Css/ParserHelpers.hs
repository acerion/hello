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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-} -- For 'Data'.




module Hello.Css.ParserHelpers
  (
    interpretRgbFunctionTokens
  , rgbFunctionToColor
  , consumeFunctionBody

  , takeLengthTokens
  , lengthValueToDistance

  , mkParserEnum
  , interpretTokensAsMultiEnum
  , mkParserLength
  , mkParserRangeInteger
  , parserColor
  , interpretTokensAsBgPosition
  , parserDistanceAuto

  , ParsedUrl (..)
  , Image (..)
  , parserImageUrl
  , parserImageGradient
  )
where




import Control.Applicative (Alternative(..), many)
import Data.Bits
import Data.Data
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Text as T
-- import Debug.Trace

import Hello.Colors
import Hello.Css.Distance
import Hello.Css.Tokenizer
import Hello.Utils.Parser




{-
Consume body of "rgb(" function. Return list of tokens representing r/g/b/
component values of a color.

Closing paren is consumed, but not added to output list.

-- https://www.w3.org/TR/css-syntax-3/#consume-function:
:m +Hello.Css.Tokenizer
:m +Hello.Utils.Parser
:m +Hello.Css.ParserHelpers
:set prompt >
consumeRgbFunctionTokens ((startTokenizer . defaultParser $ "100, 100, 100 )"))
-}
consumeRgbFunctionTokens :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), [CssToken])
consumeRgbFunctionTokens pat = runParser (rgb <* parserFunctionTrailer) pat
  where
    rgb = (:) <$> parserRgbItem <*> (sequence (L.replicate 2 (parserWsComma *> parserRgbItem))) <|> pure []
    parserFunctionTrailer = many parserTokenWhitespace <* parserTokenParenClose
    parserRgbItem = many parserTokenWhitespace *> (parserTokenPerc <|> parserTokenNum)
    parserWsComma = many parserTokenWhitespace *> parserTokenComma <* many parserTokenWhitespace




-- Interpret list of tokens in body of rgb functions. Extract r/g/b values
-- from the body. Let caller know if the values are in percents (0-100 range)
-- or not (0-255 range).
--
-- The last token in the list should be a paren that closes the function's
-- body - this is paren is used to recognize valid end of valid body of a
-- function.
--
-- "100%,90%,0%)" -> Just (r, g, b, True)
-- "255,14,91)"   -> Just (r, g, b, False)
interpretRgbFunctionTokens :: [CssToken] -> Maybe (Int, Int, Int, Bool)
interpretRgbFunctionTokens tokens =
  case tokens of
    -- "either three integer values or three percentage values" in https://www.w3.org/TR/css-color-3/
    [CssTokPerc (CssNumI r), CssTokPerc (CssNumI g), CssTokPerc (CssNumI b)] -> Just (r, g, b, True)
    [CssTokNum (CssNumI r),  CssTokNum (CssNumI g),  CssTokNum (CssNumI b) ] -> Just (r, g, b, False)
    _                                                                        -> Nothing




-- Return integer representing a color. The color is built from body of "rgb"
-- function.
rgbFunctionToColor :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), Color)
rgbFunctionToColor pat =
  consumeRgbFunctionTokens pat
  >>= (\ (pat', tokens) -> interpretRgbFunctionTokens tokens
        >>= (\ (r, g, b, isPercent) -> Just (pat', rgbToColor r g b isPercent)))




rgbToColor :: Int -> Int -> Int -> Bool -> Color
rgbToColor red green blue isPercent = (r `shiftL` 16) .|. (g `shiftL` 8) .|. b
  where
    r = toColorComponent isPercent red
    g = toColorComponent isPercent green
    b = toColorComponent isPercent blue

    -- Convert given color integer (which may or may not be a percentage)
    -- into an integer in range 0x00-0xFF.
    toColorComponent :: Bool -> Int -> Int
    toColorComponent True  = clipFF . round . (\ (x :: Float) -> (x * 255.0) / 100.0) . fromIntegral
    toColorComponent False = clipFF

    -- Ensure that given integer is in range 0x00-0xFF. Clip values that
    -- are outside of this range.
    clipFF :: Int -> Int
    clipFF x | x > 0xFF  = 0xFF
             | x < 0     = 0
             | otherwise = x




-- Make a parser that parses current 'Ident' token using a provided dictionary.
--
-- The parser checks if current 'Ident' token has one of allowed values and
-- returns a corresponding Haskell enum value.
--
-- The mapping between string identifiers and Haskell enums is specified by
-- 'dict'.
mkParserEnum :: [(T.Text, value)] -> Parser (CssParser, CssToken) value
mkParserEnum dict = Parser $ \ pat -> do
  (pat', ident) <- runParser parserTokenIdentValue pat
  propValue     <- L.lookup (T.toLower ident) dict -- TODO: should we use toLower when putting string in token or can we use it here?
  pure (pat', propValue)
  -- TODO: is this the right place to reject everything else other than CssTokIdent?
  -- Shouldn't we do it somewhere else?




-- Interpret current token (and possibly more following tokens) as color
-- value.
--
-- If current token is a Hash token, then there will be no need to take more
-- tokens. If current token is e.g. "rgb(" function, then the function should
-- (TODO) take as many tokens as necessary to build, parse and convert the
-- function into color value.
parserColor :: Parser (CssParser, CssToken) Color
parserColor = Parser $ \ pat -> fn pat
  where
    fn :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), Color)
    fn (p1, CssTokHash _ str)  = case colorsHexStringToColor str of
                                   Just c  -> Just (nextToken p1, c)
                                   Nothing -> Nothing
    fn (p1, CssTokFunc "rgb")  = rgbFunctionToColor . nextToken $ p1
    fn (p1, CssTokIdent ident) = case colorsStringToColor ident of
                                   Just c  -> Just (nextToken p1, c)
                                   Nothing -> Nothing
    fn _                       = Nothing




-- allowUnitlessDistance: are distance values without unit (e.g. "1.0", as
-- opposed to "1.0px") allowed/accepted for this property value?
mkParserLength :: Bool -> Parser (CssParser, CssToken) CssDistance
mkParserLength allowUnitlessDistance = Parser $ \ pat -> fn allowUnitlessDistance pat
  where
    fn :: Bool -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), CssDistance)
    fn allowUnitless (parser, token) =
      case tokens of
        [CssTokDim cssNum ident] -> Just ((newParser, newToken), (unitValue cssNum ident))
        [CssTokPerc cssNum]      -> Just ((newParser, newToken), (percentValue cssNum))
        [CssTokNum cssNum]       -> case ((newParser, newToken), unitlessValue cssNum allowUnitless) of
                                      ((p2, t2), Just i) -> Just ((p2, t2), i)
                                      (_,       Nothing) -> Nothing
        _                        -> Nothing
        where
          ((newParser, newToken), tokens) = takeLengthTokens (parser, token)

    percentValue :: CssNum -> CssDistance
    percentValue cssNum = distance
      where
        fval = cssNumToFloat cssNum
        distance = CssNumericPercentage (fval / 100.0)

    unitValue :: CssNum -> T.Text -> CssDistance
    unitValue cssNum unitString = distance
      where
        fval = cssNumToFloat cssNum
        distance = lengthValueToDistance fval (T.toLower unitString)

    unitlessValue :: CssNum -> Bool -> Maybe CssDistance
    -- Allow numbers without unit only for 0 or LengthPercentNumber.
    -- TODO: why?
    --
    -- TODO: original code allowed a value to be unitless if value type was
    -- CssValueTypeLengthPercentNumber or value was 0.0. Do we need to
    -- restore the condition on value type, or can we use the boolean flag?
    unitlessValue cssNum allow = if allow || fval == 0.0
                                 then Just distance
                                 else Nothing
      where
        fval = cssNumToFloat cssNum
        distance = CssNumericNone fval




-- Parse current token as integer
--
-- Value of integer that is outside of integersRange is rejected.
--
-- Notice that thanks to matching on "CssTokNum CssNumI i" value, the
-- function can reject property values that start as a number, e.g. "100px".
-- Tokenizing of "100px" input string by tokenizer will not result in
-- "CssTokNum CssNumI i" value.
--
-- TODO: restrict the integer values only to multiples of hundreds?
--
-- (Int, Int): Lower and upper value (inclusive) of allowed integer values.
-- Used to parse e.g. font weight.
--
-- HASKELL FEATURE: LAMBDA CASE
mkParserRangeInteger :: (Int, Int) -> Parser (CssParser, CssToken) Int
mkParserRangeInteger integersRange = Parser $ \ case
  (parser, CssTokNum (CssNumI i)) | i >= fst integersRange && i <= snd integersRange -> Just (nextToken parser, i)
                                  | otherwise                                        -> Nothing
  _                               -> Nothing





-- TODO: this function should handle multiple values per property, like here:
-- "th{border-width:0 0 1px;".
takeLengthTokens :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
takeLengthTokens (parser, token) = case token of
                                     CssTokNum  _   -> (nextToken parser, [token])
                                     CssTokPerc  _  -> (nextToken parser, [token])
                                     CssTokDim  _ _ -> (nextToken parser, [token])
                                     CssTokSemicolon       -> ((parser, token), [])
                                     CssTokBraceCurlyClose -> ((parser, token), [])
                                     CssTokEnd      -> ((parser, token), [])
                                     _              -> ((parser, token), [])

{-
  where
    numberWithSomething (parser, numberToken) = case nextToken parser of
                                                  pair@(p3, CssTokIdent sym)  -> if unitStringIsValid sym
                                                                               then (nextToken p3, [numberToken, snd pair])
                                                                               else (nextToken p3, []) -- TODO: how to handle unrecognized symbol?
                                                  pair@(p3, CssTokPercI i) -> (nextToken p3, [numberToken, snd pair])
                                                  pair@(p3, CssTokPercF f) -> (nextToken p3, [numberToken, snd pair])
                                                  pair@(p3, CssTokDelim ';') -> (pair, [numberToken])
                                                  pair@(p3, CssTokDelim '}') -> (pair, [numberToken])
                                                  pair@(p3, CssTokEnd)     -> (pair, [numberToken])
                                                  pair                     -> ((parser, token), [])

    unitStringIsValid str = str == "px" || str == "mm" || str == "cm" || str == "in" || str == "pt" || str == "pc" || str == "em" || str == "ex"
-}




lengthValueToDistance :: Float -> T.Text -> CssDistance
lengthValueToDistance fval unitStr | unitStr == "px" = CssDistanceAbsPx fval
                                   | unitStr == "mm" = CssDistanceAbsMm fval
                                   | unitStr == "cm" = CssDistanceAbsMm (fval * 10)
                                   | unitStr == "in" = CssDistanceAbsMm (fval * 25.4)
                                   | unitStr == "pt" = CssDistanceAbsMm (fval * (25.4/72.0))
                                   | unitStr == "pc" = CssDistanceAbsMm (fval * (25.4/6.0))
                                   | unitStr == "em" = CssDistanceRelEm fval
                                   | unitStr == "ex" = CssDistanceRelEx fval
                                   | otherwise       = CssNumericNone   fval




-- Match current CssTokIdent token (and possibly more following CssTokIdent
-- tokens) agains a dictonary of (ident, 'property value') items. Return list
-- of 'property value' items for which a match was successful.
--
-- If input stream contains CssTokIdent tokens with values not present in the
-- dictionary (perhaps they come from newer version of standard or perhaps
-- contain typos), then the function returns Nothing. Rationale: Firefox 78
-- and Chromium 90 don't apply this style:
-- "text-decoration: overline underline frog line-through;"
--
-- TODO: the function should be even stricter: the function should return
-- Nothing if any token in 'value' part of declaration *is not a CssTokIdent
-- token*. In such case entire declaration should be rejected. This is
-- suggested by behaviour of FF and Chromium. Perhaps we should take a list
-- of tokens until end of value (until '}', ';' or EOF) and parse it as a
-- whole, to see if all value tokens are symbols/strings/identifiers.
--
-- TODO: if none of tokens match given dictionary then the function doesn't
-- consume any tokens and returns Nothing. I'm not entirely sure that this is
-- a good approach. Perhaps the function should return empty list and consume
-- the tokens? But for consistency with other 'tokensAsValue*' functions this
-- function should return Nothing and don't consume any tokens.
--
-- TODO: check in spec if the dictionary should always include an implicit
-- "none" value. Original C++ code indicates that "none" was treated in
-- special way.
interpretTokensAsMultiEnum :: [(T.Text, value)] -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), [value])
interpretTokensAsMultiEnum dict (parser, token@(CssTokIdent _)) =
  case matchSymbolTokensWithListRigid (parser, token) dict [] of
    (_, [])     -> Nothing -- None of input tokens were matched agains list of enums.
    (pat', val) -> Just (pat', val)
interpretTokensAsMultiEnum _ _ = Nothing




-- Match current CssTokIdent token and any following CssTokIdent tokens
-- against list of (key, value) items. Each item that had matching token is
-- added to accumulator.
--
-- 'Rigid' means that all keys must be present in dictionary. Trying to match
-- a key not found in the dictionary will result in returning empty value.
--
-- Return the accumulator.
--
-- TODO: write unit tests for this function if it ever gets used outside of
-- tokensAsValueMultiEnum. For now tests of tokensAsValueMultiEnum should be
-- enough, but if this function becomes more widely used, then it will
-- deserve its own tests set.
--
-- A non-rigid version of the function could perhaps be reimplemented with
-- "fmap snd (L.filter (\x -> L.elem (fst x)) dict))". But that
-- implementation would not catch keys from outside of allowed set of keys.
matchSymbolTokensWithListRigid :: (CssParser, CssToken) -> [(T.Text, b)] -> [b] -> ((CssParser, CssToken), [b])
matchSymbolTokensWithListRigid (p, t@(CssTokIdent key)) dictionary acc =
  case L.lookup key dictionary of -- TODO: should we use toLower when putting string in token or can we use it here?
    Just value -> matchSymbolTokensWithListRigid (nextToken p) dictionary (acc ++ [value])
    Nothing    -> ((p, t), []) -- Given token does not match a set of allowed
                               -- strings. Since this function is "rigid", we
                               -- must return empty result.
matchSymbolTokensWithListRigid (p, t) _ acc                      = ((p, t), acc)




interpretTokensAsBgPosition :: (Int -> Int -> value) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), value)
interpretTokensAsBgPosition bgPositionValueCtor pat = Just (pat', propValue)
  where
    (pat', _) = takeBgTokens pat
    propValue = bgPositionValueCtor 0 0
    -- TODO: right now the original dillo doesn't seem to display background
    -- images at all, so I will stop the work on this function for now.
    -- Later, as I get to know dillo better, I will resume work on this
    -- functionality. Look at "case CSS_TYPE_BACKGROUND_POSITION" in
    -- src/cssparser.cc in original dillo code.
    --
    -- This functionality will require adding posX/posY fields to CssValue.




takeBgTokens :: (CssParser, CssToken) -> ((CssParser, CssToken), [CssToken])
takeBgTokens (parser, token) = ((outParser, outToken), outTokens)

  where
    ((outParser, outToken), tokens) = takeBgTokens' (parser, token) []
    outTokens = remapToken <$> reorderTokens tokens

    -- Make sure that list of tokens always contains two tokens that are
    -- properly ordered: [horiz, vert].
    reorderTokens :: [CssToken] -> [CssToken]
    reorderTokens ts@[CssTokIdent "top", _]    = L.reverse ts -- First token should be horiz, second should be vert.
    reorderTokens ts@[CssTokIdent "bottom", _] = L.reverse ts -- First token should be horiz, second should be vert.
    reorderTokens ts@[CssTokIdent "initial"]   = ts -- Handle single-element "initial" first, before other single-element lists.
    reorderTokens ts@[CssTokIdent "inherit"]   = ts -- Handle single-element "inherit" first, before other single-element lists.
    -- After "initial" and "inherit" are handled, you can now add 50% as
    -- missing second element. Also call reorderTokens recursively to handle
    -- this input string correctly: "top;".
    -- You have to ensure two things for this case:
    -- 1. output list has two members: one of them is the "top" and the other
    --    is default "50%"), therefore we add "50%" token
    -- 2. horiz/vert tokens are in proper order (horiz first, vert second),
    --    therefore we do recursive call to reorderTokens.
    reorderTokens [tok1]                       = reorderTokens [tok1, CssTokPerc $ CssNumI 50]
    reorderTokens ts@[_, _]                    = ts
    reorderTokens _                            = [] -- TODO: Perhas this is not needed and we should trust that caller will pass non-empty list?


    -- Change CssTokIdent tokens for top/left/center etc. into <percentage-token>s.
    -- TODO: this function doesn't handle "initial" and "inherit" - what do we do with them?
    --
    -- TODO: map lookup will return Nothing for "initial" and "inherit"
    -- tokens, which aren't really handled here.
    remapToken :: CssToken -> CssToken
    remapToken tok@(CssTokIdent sym) = fromMaybe tok (M.lookup sym posMap)
      where posMap = M.fromList [ ("left",   CssTokPerc $ CssNumI 0)
                                , ("right",  CssTokPerc $ CssNumI 100)
                                , ("top",    CssTokPerc $ CssNumI 0)
                                , ("bottom", CssTokPerc $ CssNumI 100)
                                , ("center", CssTokPerc $ CssNumI 50) ]
    remapToken tok@(CssTokPerc _)    = tok
    remapToken tok@(CssTokDim _ _)   = tok
    remapToken tok                   = tok -- TODO: This pattern is added to catch other token types, but is it really valid?




takeBgTokens' :: (CssParser, CssToken) -> [CssToken] -> ((CssParser, CssToken), [CssToken])
takeBgTokens' (parser, token) tokens = ((outParser, outToken), outTokens)

  where
    ((outParser, outToken), outTokens) | doContinue tokens token =
                                           case token of
                                             CssTokNone -> takeBgTokens' (nextToken parser) tokens -- Take the token, but don't append it to result
                                             _          -> takeBgTokens' (nextToken parser) (tokens ++ [token])
                                       | otherwise = if tokensValid tokens
                                                     then ((parser, token), tokens)
                                                     else ((parser, token), [])



    doContinue ts t = L.length ts < 2 && tokValid t

    tokValid CssTokNone          = True -- used to kick-start parsing of stream
    tokValid (CssTokIdent ident) = elem ident horizVals || elem ident vertVals || elem ident otherVals || ident == "center" -- TODO: or $ map (elem ident) [horizVals, vertVals, otherVals, ["center"]]
    tokValid (CssTokNum _)       = True
    tokValid (CssTokDim _ _)     = True
    tokValid (CssTokPerc _)      = True
    tokValid _                   = False

    horizVals = ["left", "right"]
    vertVals  = ["top", "bottom"]
    otherVals = ["initial", "inherit"]

    tokensValid [CssTokIdent sym1, CssTokIdent sym2] = cond1 && cond2 && cond3
      where
        cond1 = not (elem sym1 otherVals && elem sym2 otherVals) -- "initial" or "inherit" isn't used twice.
        cond2 = not (elem sym1 horizVals && elem sym2 horizVals) -- Both symbols aren't from the same list of horizontal tokens.
        cond3 = not (elem sym1 vertVals  && elem sym2 vertVals)  -- Both symbols aren't from the same list of vertical tokens.
    tokensValid [_, _]       = True
    tokensValid [_]          = True -- Single-token list is valid: token's value will be used as X, and Y will be set to 50%.
    tokensValid _            = False




data ParsedUrl = ParsedUrl
  { parsedUrl  :: T.Text
  } deriving (Data, Eq, Show)




parserUrl :: Parser (CssParser, CssToken) ParsedUrl
parserUrl = Parser parseUrl



-- https://www.w3.org/TR/css-syntax-3/#consume-url-token
-- Unquoted URL is parsed into url-token (CssTokUrl).
-- Quoted URL is parsed into function-token (CssTokFunc).
parseUrl :: (CssParser, CssToken) -> Maybe ((CssParser, CssToken), ParsedUrl)
parseUrl (p1, CssTokUrl url)    = Just (nextToken p1, ParsedUrl url)
parseUrl (p1, CssTokFunc "url") = case body of
                                    [(CssTokStr url), _] -> Just ((p2, t2), ParsedUrl url)
                                    _                    -> Nothing
  where
    ((p2, t2), body) = consumeFunctionBody p1 []
parseUrl _                      = Nothing




consumeFunctionBody :: CssParser -> [CssToken] -> ((CssParser, CssToken), [CssToken])
consumeFunctionBody p1 acc = case nextToken p1 of
                               (p2, t2@CssTokParenClose) -> (nextToken p2, L.reverse (t2:acc))
                               (p2, CssTokEnd)           -> (nextToken p2, L.reverse acc) -- TODO: this is a parse error, handle the error
                               (p2, t2)                  -> consumeFunctionBody p2 (t2:acc)




{-
Try to parse current token as CssDistanceAuto value

TODO: the tests show that the function will interpret "auto italic" as
CssDistanceAuto, but this is problematic because "italic" doesn't seem to be
something expected after "auto". Should we reject such input string here, or
in higher layer?
-}
parserDistanceAuto :: Parser (CssParser, CssToken) CssDistance
parserDistanceAuto = fmap (const CssDistanceAuto) (parserTokenIdent "auto")




data Image
  = ImageUrl ParsedUrl
  | ImageGradient
  deriving (Data, Eq, Show)




parserImageUrl :: Parser (CssParser, CssToken) Image
parserImageUrl = ImageUrl <$> parserUrl




-- TODO: implement
parserImageGradient :: Parser (CssParser, CssToken) Image
parserImageGradient = Parser (const Nothing)

