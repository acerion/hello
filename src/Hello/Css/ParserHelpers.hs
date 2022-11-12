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




module Hello.Css.ParserHelpers
  (
    consumeFunctionTokens
  , interpretRgbFunctionTokens
  , rgbFunctionToColor
  , parseUrl
  , consumeFunctionBody

  , takeLengthTokens
  , lengthValueToDistance

  , interpretTokensAsEnum
  , interpretTokensAsMultiEnum
  , interpretTokensAsLength
  , interpretTokensAsInteger
  , interpretTokensAsColor
  , interpretTokensAsBgPosition
  , interpretTokensAsStringList
  , interpretTokensAsURI
  , interpretTokensAsAuto
  , interpretTokensAsString
  )
where




import Data.Bits
import Data.List as L
import Data.Map as M
import Data.Text as T

import Hello.Colors
import Hello.Css.Distance
import Hello.Css.Tokenizer




-- Take all tokens (after initial "function-name(" tokens) that belong to
-- function's body. Closing paren is added to output list (if it was present
-- in input stream) - with the closing paren you can recognize if the body is
-- complete.
--
-- https://www.w3.org/TR/css-syntax-3/#consume-function
--
-- If `limit` is non-zero, take up to `limit` tokens (excluding closing
-- paren). This is a safety feature to avoid problems with malformed input.
consumeFunctionTokens :: Int -> CssParser -> ((CssParser, CssToken), [CssToken])
consumeFunctionTokens limit parser = (pat', L.reverse tokens)
  where
    (pat', tokens) = takeNext (nextToken parser) []

    takeNext :: (CssParser, CssToken) -> [CssToken] -> ((CssParser, CssToken), [CssToken])
    takeNext (p2, t2@CssTokParenClose) list = (nextToken p2, t2:list) -- Add closing paren to result, it will be used to check if function body is valid.
    takeNext (p2, CssTokEnd) list           = ((p2, CssTokEnd), list) -- https://www.w3.org/TR/css-syntax-3/#consume-function: "This is a parse error".
    takeNext (p2, t2) list                  = if limit > 0 && L.length list >= limit
                                              then ((p2, t2), list)
                                              else takeNext (nextToken p2) (t2:list)




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
    [CssTokPerc (CssNumI r), CssTokComma, CssTokPerc (CssNumI g), CssTokComma, CssTokPerc (CssNumI b), CssTokParenClose] -> Just (r, g, b, True)
    [CssTokNum (CssNumI r),  CssTokComma, CssTokNum (CssNumI g),  CssTokComma, CssTokNum (CssNumI b),  CssTokParenClose] -> Just (r, g, b, False)
    _                                                                                                                    -> Nothing




-- Return integer representing a color. The color is built from body of "rgb"
-- function.
rgbFunctionToColor :: CssParser -> ((CssParser, CssToken), Maybe Int)
rgbFunctionToColor p1 = let
  consumeRgbFunctionTokens = consumeFunctionTokens 5 -- 5 == count of tokens in "10%,20%,30%)", excluding closing paren.
  ((p2, t2), tokens) = consumeRgbFunctionTokens p1
  in
    case interpretRgbFunctionTokens tokens of
      Nothing                            -> ((p2, t2), Nothing)
      Just (red, green, blue, isPercent) -> ((p2, t2), Just color)
        where
          color = (r `shiftL` 16) .|. (g `shiftL` 8) .|. b
          r = toColorComponent isPercent (fromIntegral red)
          g = toColorComponent isPercent (fromIntegral green)
          b = toColorComponent isPercent (fromIntegral blue)

          -- Convert given float (which may or may not be a percentage) into
          -- an integer in range 0x00-0xFF.
          toColorComponent :: Bool -> Float -> Int
          toColorComponent True  = clipFF . round . (\x -> (x * 255.0) / 100.0)
          toColorComponent False = clipFF . round

          -- Ensure that given integer is in range 0x00-0xFF. Clip values that
          -- are outside of this range.
          clipFF :: Int -> Int
          clipFF x | x > 0xFF  = 0xFF
                   | x < 0     = 0
                   | otherwise = x




-- See if current 'Ident' token has one of allowed values and return a
-- corresponding Haskell enum value.
--
-- The mapping between string identifiers and Haskell enums is specified by
-- 'dict'.
--
-- In case of enum value there is no need to consume more than current token
-- to recognize the enum, but for consistency with other similar functions
-- the function is still called "tokenS as".
interpretTokensAsEnum :: [(T.Text, value)] -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), value)
interpretTokensAsEnum dict (parser, CssTokIdent sym) =
  case L.lookup sym' dict of
    Just propValue -> Just ((nextToken parser), propValue)
    Nothing        -> Nothing
  where
    sym' = T.toLower sym  -- TODO: should we use toLower when putting string in token or can we use it here?
interpretTokensAsEnum _ _ = Nothing
                                   -- TODO: is this the right place to reject everything else other than CssTokIdent?
                                   -- Shouldn't we do it somewhere else?




-- Interpret current token (and possibly more following tokens) as color
-- value.
--
-- If current token is a Hash token, then there will be no need to take more
-- tokens. If current token is e.g. "rgb(" function, then the function should
-- (TODO) take as many tokens as necessary to build, parse and convert the
-- function into color value.
interpretTokensAsColor :: (Int -> value) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), value)
interpretTokensAsColor colorValueCtor3 (p1, CssTokHash _ str)  = case colorsHexStringToColor str of
                                                                   Just c  -> Just (nextToken p1, colorValueCtor3 c)
                                                                   Nothing -> Nothing
interpretTokensAsColor colorValueCtor3 (p1, CssTokFunc "rgb")  = case rgbFunctionToColor p1 of
                                                                    (pat', Just c) -> Just (pat', colorValueCtor3 c)
                                                                    (_, Nothing)   -> Nothing
interpretTokensAsColor colorValueCtor3 (p1, CssTokIdent ident) = case colorsStringToColor ident of
                                                                   Just c  -> Just (nextToken p1, colorValueCtor3 c)
                                                                   Nothing -> Nothing
interpretTokensAsColor _ _                                     = Nothing




-- allowUnitlessDistance: are distance values without unit (e.g. "1.0", as
-- opposed to "1.0px") allowed/accepted for this property value?
interpretTokensAsLength :: Bool -> (CssDistance -> value) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), value)
interpretTokensAsLength allowUnitlessDistance distanceValueCtor (parser, token) =
  case tokens of
    [CssTokDim cssNum ident] -> Just ((newParser, newToken), distanceValueCtor (unitValue cssNum ident))
    [CssTokPerc cssNum]      -> Just ((newParser, newToken), distanceValueCtor (percentValue cssNum))
    [CssTokNum cssNum]       -> case ((newParser, newToken), unitlessValue cssNum) of
                                  ((p2, t2), Just i) -> Just ((p2, t2), distanceValueCtor i)
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

    unitlessValue :: CssNum -> Maybe CssDistance
    -- Allow numbers without unit only for 0 or LengthPercentNumber.
    -- TODO: why?
    --
    -- TODO: original code allowed a value to be unitless if value type was
    -- CssValueTypeLengthPercentNumber or value was 0.0. Do we need to
    -- restore the condition on value type, or can we use the boolean flag?
    unitlessValue cssNum = if allowUnitlessDistance || fval == 0.0
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
interpretTokensAsInteger :: (Int -> a) -> (Int, Int) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), a)
interpretTokensAsInteger integerValueCtor integersRange (parser, CssTokNum (CssNumI i)) = if i >= fst integersRange && i <= snd integersRange
                                                                                          then Just (nextToken parser, integerValueCtor i)
                                                                                          else Nothing
interpretTokensAsInteger _ _ _                                                          = Nothing





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
    remapToken :: CssToken -> CssToken
    remapToken tok@(CssTokIdent sym) = case M.lookup sym posMap of
                                         Just percToken -> percToken
                                         Nothing        -> tok -- TODO: this will happen for "initial" and "inherit" tokens, which aren't really handled here.
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
    ((outParser, outToken), outTokens) = if doContinue tokens token
                                         then case token of
                                                CssTokNone -> takeBgTokens' (nextToken parser) tokens -- Take the token, but don't append it to result
                                                _          -> takeBgTokens' (nextToken parser) (tokens ++ [token])
                                         else if tokensValid tokens
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




interpretTokensAsURI :: (Text -> value) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), value)
interpretTokensAsURI uriValueCtor pat = case parseUrl pat of
                                          (pat', Just url) -> Just (pat', uriValueCtor url)
                                          -- TODO: should we assign here pat' or pat?
                                          -- A token that is not an URI should be
                                          -- re-parsed by another function, not skipped.
                                          (_, Nothing)     -> Nothing




parseUrl :: (CssParser, CssToken) -> ((CssParser, CssToken), Maybe T.Text)
parseUrl (p1, CssTokUrl url)    = (nextToken p1, Just url)
parseUrl (p1, CssTokFunc "url") = ((p2, t2), Just $ T.pack (show body))
  where
    ((p2, t2), body) = consumeFunctionBody p1 []
parseUrl (p1, token)            = ((p1, token), Nothing)




consumeFunctionBody p1 acc = case nextToken p1 of
                               (p2, t2@CssTokParenClose) -> (nextToken p2, L.reverse (t2:acc))
                               (p2, CssTokEnd)           -> (nextToken p2, L.reverse acc) -- TODO: this is a parse error, handle the error
                               (p2, t2)                  -> consumeFunctionBody p2 (t2:acc)




-- Interpret current CssTokIdent/CssTokStr token (and possibly more following
-- CssTokIdent and CssTokStr tokens) as list of strings. The tokens should be
-- separated by comma tokens. Returned value is a string of items separated
-- by commas.
--
-- TODO: how we should handle list separated by spaces instead of commas? How
-- should we handle multiple consecutive commas?
--
-- TODO: all tokens in property's value should be
-- strings/symbols/commas/spaces. There can be no other tokens (e.g. numeric
-- or hash). Such declaration should be rejected:
-- 'font-family: "URW Gothic L", "Courier New", monospace, 90mph'
-- Rationale: behaviour of FF and Chromium.
--
-- Read comma-separated list of items, e.g. font family names. The items can
-- be strings with spaces, therefore the function consumes both CssTokIdent and
-- CssTokStr tokens. TODO: test the code for list of symbols separated by
-- space or comma.
tokensAsValueStringList :: (CssParser, CssToken) -> ((CssParser, CssToken), [T.Text])
tokensAsValueStringList pat = (pat', L.reverse list)
  where
    (pat', list) = asList pat []

    asList :: (CssParser, CssToken) -> [T.Text] -> ((CssParser, CssToken), [T.Text])
    asList (p, CssTokIdent sym) acc         = asList (nextToken p) (sym:acc)
    asList (p, CssTokStr str) acc           = asList (nextToken p) (str:acc)
    asList (p, CssTokComma) acc             = asList (nextToken p) acc
    asList (p, t@CssTokSemicolon) acc       = ((p, t), acc)
    asList (p, t@CssTokBraceCurlyClose) acc = ((p, t), acc)
    asList (p, t@CssTokEnd) acc             = ((p, t), acc)
    asList (_, _) _                         = (pat, []) -- TODO: this implmentation does not allow for final "!important" token.




interpretTokensAsStringList :: ([T.Text] -> value) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), value)
interpretTokensAsStringList stringListCtor pat = if L.null list
                                                 then Nothing
                                                 else Just (pat', stringListCtor list)
  where
    (pat', list) = tokensAsValueStringList pat




-- Interpret current token as "auto" value (value of type CssDistanceAuto).
--
-- In case of "auto" value there is no need to consume more than current
-- token to build the Auto, but for consistency with other similar functions
-- the function is still called "tokenS as".
--
-- The tests show that the function will interpret "auto italic" as
-- CssDistanceAuto, but this is problematic because "italic" doesn't seem to
-- be something expected after "auto". Should we reject such input string
-- here, or in higher layer?
interpretTokensAsAuto :: (CssDistance -> value) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), value)
interpretTokensAsAuto distanceValueCtor (parser, CssTokIdent sym) | T.toLower sym == "auto" = Just (nextToken parser
                                                                                                   , distanceValueCtor CssDistanceAuto
                                                                                                   )
                                                                      | otherwise           = Nothing
interpretTokensAsAuto _ _                                                                   = Nothing





-- Interpret current token as "string" value
--
-- In case of "string" value there is no need to consume more than current
-- token to build the String, but for consistency with other similar
-- functions the function is still called "tokenS as".
interpretTokensAsString :: (T.Text -> value) -> (CssParser, CssToken) -> Maybe ((CssParser, CssToken), value)
interpretTokensAsString stringCtor (p, CssTokStr s) = Just (nextToken p,stringCtor s)
interpretTokensAsString _ _                         = Nothing




