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
-}




{-
TODO: be careful with decoding string from external representation into
Data.Text. My original attempt to convert C string into Data.Text in
ffi/CssParser.hsc used T.E.decodeUtf8. I had to change it to T.E.decodeLatin1
because of exceptions from Data.Text module on some characters in some css
files.

Take a look at value of 'content' in this part of css:
a.navmenu::after { content: " â–¶"; }font.logo, font.logobl, img.logo {display: none;}img.sslogo
The line comes from https://lwn.net/CSS/pure-lwn, and the value would lead to
"libEval.so: Cannot decode byte '\xb6': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" error.
-}




{-# LANGUAGE OverloadedStrings #-}




module CssParser(nextToken
                , takeSymbol
                , takeInt
                , CssParser (..)
                , CssToken (..)
                , tryTakingRgbFunction
                , parseRgbFunction
                , parseRgbFunctionInt
                , declarationValueAsColor
                , defaultParser) where




import Prelude
import Data.Maybe
import Foreign
import qualified Data.Char as D.C
import qualified Data.Text as T
import qualified Data.Text.Read as T.R
import qualified Data.Text.IO as T.IO
import qualified HelloUtils as HU
import Colors




data CssToken =
    CssTokI   Int
  | CssTokF   Float
  | CssTokCol T.Text
  | CssTokSym T.Text
  | CssTokStr T.Text
  | CssTokCh  Char
  | CssTokEnd         -- End of input. No new tokens will appear in input.
  | CssTokNone        -- No token was taken, proceed with parsing input data to try to take some token.
  deriving (Show)




data CssParser = CssParser {
    remainder      :: T.Text
  , spaceSeparated :: Bool
  , withinBlock    :: Bool
  , bufOffset      :: Int
  } deriving (Show)




defaultParser = CssParser {
    remainder = ""
  , withinBlock = False
  , spaceSeparated = False
  , bufOffset = 0
  }




nextToken :: CssParser -> (CssParser, CssToken)
nextToken parser = (updatedParser{bufOffset = increasedBufOffset parser}, token)
  where
    (updatedParser, token) = nextToken' parser{spaceSeparated = False}
    increasedBufOffset parser = (bufOffset parser) + (T.length . remainder $ parser) - (T.length . remainder $ updatedParser)




-- This function is based on function with the same name from Real World
-- Haskell, chapter 10.
--
-- These lines are most awesome piece of code that I've written so far, in
-- any project.
(>>?) :: (CssParser, CssToken) -> (CssParser -> (CssParser, CssToken)) -> (CssParser, CssToken)
(parser, CssTokNone) >>? f = f parser
pair@(parser, _) >>? _     = pair





-- Try taking Float before trying to take Int, because otherwise you may take
-- only an initial (integral) part of Float as an Int, and leave fractional
-- part in remainder.
nextToken' :: CssParser -> (CssParser, CssToken)
nextToken' parser = takeLeadingWhite parser >>?
                    takeFloat               >>?
                    takeInt                 >>?
                    takeSymbol              >>?
                    takeString              >>?
                    takeColor               >>?
                    takeCharacter




-- Symbol must not start with a digit, therefore we have to have some kind of
-- test at the beginning. TODO: can we do the test in a better way?
--
-- TODO: Original C code parsed symbols starting with '-' (such as
-- "-webkit-user-select") in a way that resulted in token without the leading
-- '-' (so the resulting token was "webkit-user-select"). Haskell code keeps
-- the leading '-' character.
takeSymbol :: CssParser -> (CssParser, CssToken)
takeSymbol parser = if predNonNumeric . T.head . remainder $ parser
                    then (parserAppend parser tok, CssTokSym tok)
                    else (parser, CssTokNone)
  where tok = T.takeWhile pred (remainder parser)
        predNonNumeric = (\c -> D.C.isAlpha c || c == '_' || c == '-')
        pred = (\c -> D.C.isAlphaNum c || c == '_' || c == '-')




{-
TODO: handle escaped sequences in string.
Examples of escaped sequences:

before{content:"Poka\00017C  wi\000119cej";position:relative;top:-5px}
token: "Poka7C  wi19cej", [50 6f 6b 61 01 37 43 20 20 77 69 01 31 39 63 65 6a ]

before{content:"Na \00017Bywo"}
token: "Na 7Bywo", [4e 61 20 01 37 42 79 77 6f ]

.icon--arrow-right:before{content:"\f107"}
token  = "", [07 ]
-}
takeString :: CssParser -> (CssParser, CssToken)
takeString parser = case HU.takeEnclosed (remainder parser) "\"" "\"" True of
                      (Just string, rem) -> parseString parser string rem
                      (Nothing, _) -> case HU.takeEnclosed (remainder parser) "'" "'" True of
                                       (Just string, rem) -> parseString parser string rem
                                       (Nothing, _) -> (parser, CssTokNone)
  where
    parseString :: CssParser -> T.Text -> T.Text -> (CssParser, CssToken)
    parseString parser string rem = (parser{remainder = rem}, CssTokStr (escapedString string))
    escapedString str = case T.findIndex (== '\\') str of
                          Just i -> ""
                          Nothing -> str




-- TODO: think about performance of using isPrefixOf to get just one
-- character, here and elsewhere.
takeColor :: CssParser -> (CssParser, CssToken)
takeColor parser = if T.isPrefixOf "#" (remainder parser) && (withinBlock parser)
                   then takeColor' parser
                   else (parser, CssTokNone) -- Don't take the leading '#' if we are not in a block

  where
    takeColor' :: CssParser -> (CssParser, CssToken)
    takeColor' parser = (parser{ remainder = newRem }
                        , CssTokCol (T.concat ["#", newValue ]))
                        -- TODO: verify if we really need the leading '#' in token.
                        -- TODO: what if there are no digits after '#'?
                        -- TODO: add better handling of '#' followed by non-hex string.

      where
        newValue = T.takeWhile D.C.isHexDigit digitsString
        newRem = T.dropWhile D.C.isHexDigit digitsString
        digitsString = T.drop 1 (remainder parser)




takeCharacter :: CssParser -> (CssParser, CssToken)
takeCharacter parser = if T.null . remainder $ parser
                       then (parser, CssTokNone)
                       else (parserAppend parser (T.singleton . T.head . remainder $ parser),
                             CssTokCh (T.head . remainder $ parser))




-- This function does not return a token. Discarding meaningless data from
-- beginning of text would not create a valid token.
takeLeadingWhite :: CssParser -> (CssParser, CssToken)
takeLeadingWhite parser
  | T.null rem                 = (parser, CssTokEnd)
  | D.C.isSpace . T.head $ rem = takeLeadingWhite parser { remainder = T.tail rem, spaceSeparated = True }
  | T.isPrefixOf "/*" rem      = takeLeadingWhite parser { remainder = HU.skipEnclosed rem "/*" "*/" }
  | T.isPrefixOf "<!--" rem    = takeLeadingWhite parser { remainder = HU.skipEnclosed rem "<!--" "-->" }
  | otherwise                  = (parser, CssTokNone)
  where rem = remainder parser




parserAppend :: CssParser -> T.Text -> CssParser
parserAppend parser tok = parser { remainder = T.drop (T.length tok) (remainder parser) }




-- TODO: this function doesn't recognize some float formats that are valid in
-- CSS, e.g. ".5".
--
-- T.R.rational is happy to interpret "100" as float, but we want to treat is
-- as int. Therefore we have to search for '.' in taken sub-string :(
takeFloat :: CssParser -> (CssParser, CssToken)
takeFloat parser = case T.R.signed T.R.rational (remainder parser) of
                     Right pair -> case T.find (== '.') val of
                                     Just c    -> (parserAppend parser val, CssTokF (fst pair))
                                     otherwise -> (parser, CssTokNone)
                       where
                         val = T.take diff (remainder parser)
                         newRem = snd pair
                         diff = (T.length . remainder $ parser) - (T.length newRem)
                     Left pair -> (parser, CssTokNone)




-- This function is very similar to takeFloat, but I don't want to write a
-- common function just yet. takeFloat will have to be updated to read all
-- formats of float value, and that change may make it more complicated and
-- less similar to takeInt.
takeInt :: CssParser -> (CssParser, CssToken)
takeInt parser = case T.R.signed T.R.decimal (remainder parser) of
                   Right pair -> (parserAppend parser val, CssTokI (fst pair))
                     where
                       val = T.take diff (remainder parser)
                       newRem = snd pair
                       diff = (T.length . remainder $ parser) - (T.length newRem)
                   Left pair -> (parser, CssTokNone)




{-
takeAllTokens :: (CssParser, CssToken) -> IO CssParser
takeAllTokens (parser,token) = do
  T.IO.putStrLn (remainder parser)
  let (p, t) = nextToken parser
  if cssTokenType t == CssTokEnd
    then return p
    else takeAllTokens . nextToken $ p
-}




parseRgbFunctionInt :: CssParser -> (CssParser, Maybe Int)
parseRgbFunctionInt parser =
  case parseRgbFunction parser of
    (parser', Nothing) -> (parser', Nothing)
    (parser', Just (tr, tg, tb, ta, isPercent)) -> (parser', Just color)
      where
        color = (r `shiftL` 16) .|. (g `shiftL` 8) .|. b
        r = getInt tr isPercent
        g = getInt tg isPercent
        b = getInt tb isPercent

        -- TODO: make sure that r/g/b values are in range 0-255.
        getInt :: Int -> Bool -> Int
        getInt i isPercent = if isPercent then (i * 255) `div` 100 else i




parseRgbFunction :: CssParser -> (CssParser, Maybe (Int, Int, Int, T.Text, Bool))
parseRgbFunction parser =
  let fun = tryTakingRgbFunction $ parser
      parser' = fst fun
      tokens = snd fun :: [CssToken]
  in
    case tokens of
      (CssTokCh par2:CssTokCh perc3:CssTokI b:CssTokCh comma2:CssTokCh perc2:CssTokI g:CssTokCh comma1:CssTokCh perc1:CssTokI r:CssTokCh par1:[]) ->
        if par1 == '(' && par2 == ')' && perc3 == '%' && perc2 == '%' && perc1 == '%' && comma2 == ',' && comma1 == ','
        then (parser', Just (r, g, b, "%", True))
        else (parser', Nothing)
      (CssTokCh par2:CssTokI b:CssTokCh comma2:CssTokI g:CssTokCh comma1:CssTokI r:CssTokCh par1:[]) ->
        if par1 == '(' && par2 == ')' && comma2 == ',' && comma1 == ','
        then (parser', Just (r, g, b, "/100", False))
        else (parser', Nothing)
      otherwise -> (parser', Nothing)




tryTakingRgbFunction :: CssParser -> (CssParser, [CssToken])
tryTakingRgbFunction parser = takeNext parser []
  where
    takeNext :: CssParser -> [CssToken] -> (CssParser, [CssToken])
    takeNext parser list@(CssTokCh c:xs) = if length list == 10 || (c == ')' && length list == 7)
                                           then (nextParser, list)
                                           else takeNext nextParser (tok:list)
      where (nextParser, tok) = nextToken parser

    takeNext parser []        = takeNext nextParser [tok]
      where (nextParser, tok) = nextToken parser

    takeNext parser list                 = if length list == 10
                                           then (nextParser, list)
                                           else takeNext nextParser (tok:list)
      where (nextParser, tok) = nextToken parser
-- TODO: handle invalid token type here




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





declarationValueAsColor :: CssParser -> CssToken -> (CssParser, Maybe Int)
declarationValueAsColor parser (CssTokCol c) = case colorsStringToColor c of -- TODO: we know here that color should have form #RRGGBB. Call function that accepts only this format.
                                                 Just i  -> (parser, Just i)
                                                 Nothing -> (parser, Nothing)
declarationValueAsColor parser (CssTokSym s) | s == "rgb" = parseRgbFunctionInt parser
                                             | otherwise = case colorsStringToColor s of
                                                             Just i  -> (parser, Just i)
                                                             Nothing -> (parser, Nothing)
declarationValueAsColor parser _             = (parser, Nothing)
