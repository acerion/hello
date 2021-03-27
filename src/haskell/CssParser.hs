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
                , takeAllTokens
                , CssParser (..)
                , CssTokenType (..)
                , defaultParser) where




import Prelude
import Data.Maybe
import qualified Data.Char as D.C
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified HelloUtils as HU




data CssParser = CssParser {
    tokenValue     :: T.Text
  , tokenType      :: Maybe CssTokenType
  , remainder      :: T.Text
  , spaceSeparated :: Bool
  , withinBlock    :: Bool
  } deriving (Show)




data CssTokenType = TokenInt | TokenFloat | TokenColor | TokenSymbol | TokenString | TokenChar | TokenEnd
                  deriving (Show, Eq)




defaultParser = CssParser { tokenValue = ""
                          , tokenType = Nothing
                          , remainder = ""
                          , withinBlock = False
                          , spaceSeparated = False
                          }




nextToken :: CssParser -> CssParser
nextToken parser = nextToken' parser{spaceSeparated = False}




-- TODO: another place that looks like that example from chapter about
-- parsing in Real World Haskell with control structure nested few times.
nextToken' :: CssParser -> CssParser
nextToken' parser = case takeNumber . takeLeadingMinus . takeLeadingEmpty $ parser of
                      par@CssParser{tokenType = Just _} -> par
                      par -> case takeSymbol par of
                               par@CssParser{tokenType = Just _} -> par
                               par -> case takeString par of
                                        par@CssParser{tokenType = Just _} -> par
                                        par -> case takeColor par of
                                                 par@CssParser{tokenType = Just _} -> par
                                                 par -> case takeCharacter par of
                                                          par@CssParser{tokenType = Just _} -> par
                                                          par -> par




-- Symbol must not start with a digit, therefore we have to have some kind of
-- test at the beginning. TODO: can we do the test in a better way?
--
-- TODO: Original C code parsed symbols starting with '-' (such as
-- "-webkit-user-select") in a way that resulted in token without the leading
-- '-' (so the resulting token was "webkit-user-select"). Haskell code keeps
-- the leading '-' character.
takeSymbol parser = if predNonNumeric . T.head . remainder $ parser
                    then parserAppend parser tok TokenSymbol
                    else parser
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
takeString parser = case HU.takeEnclosed (remainder parser) "\"" "\"" True of
                      (Just string, rem) -> parseString parser string rem
                      (Nothing, _) -> case HU.takeEnclosed (remainder parser) "'" "'" True of
                                       (Just string, rem) -> parseString parser string rem
                                       (Nothing, _) -> parser
  where
    parseString :: CssParser -> T.Text -> T.Text -> CssParser
    parseString parser string rem = parser{tokenValue = escapedString string, tokenType = Just TokenString, remainder = rem}
    escapedString str = case T.findIndex (== '\\') str of
                          Just i -> ""
                          Nothing -> str




-- TODO: think about performance of using isPrefixOf to get just one
-- character, here and elsewhere.
takeColor parser = if T.isPrefixOf "#" (remainder parser) && (withinBlock parser)
                   then takeColor' parser
                   else parser -- Don't take the leading '#' if we are not in a block
  where takeColor' parser = parser{ tokenValue = T.concat ["#", newValue ] -- TODO: verify if we really need the leading '#' in token.
                                  , tokenType = newType
                                  , remainder = newRem }
          where
            newValue = T.takeWhile D.C.isHexDigit digitsString
            newRem = T.dropWhile D.C.isHexDigit digitsString
            newType = if T.length newValue > 0 then Just TokenColor else Nothing -- TODO: add better handling of '#' followed by non-hex string.
            digitsString = T.drop 1 (remainder parser)




takeCharacter parser = if T.null . remainder $ parser
                       then parser
                       else parserAppend parser (T.singleton . T.head . remainder $ parser) TokenChar




takeLeadingEmpty parser
  | T.null rem                 = parser { tokenType = Just TokenEnd }
  | D.C.isSpace . T.head $ rem = takeLeadingEmpty parser { remainder = T.tail rem, spaceSeparated = True }
  | T.isPrefixOf "/*" rem      = takeLeadingEmpty parser { remainder = HU.skipEnclosed rem "/*" "*/" }
  | T.isPrefixOf "<!--" rem    = takeLeadingEmpty parser { remainder = HU.skipEnclosed rem "<!--" "-->" }
  | otherwise = parser
  where rem = remainder parser



-- TODO: move getting leading minus to takeNumber. Have a clearer distinction
-- between minus being a part of a number and minus being a part of other
-- type of token. Make sure that this call: "nextToken
-- defaultParser{remainder="/* hello */ -"}" returns token type == TokenChar.
-- Or should it be treated as invalid?
takeLeadingMinus :: CssParser -> CssParser
takeLeadingMinus parser = case T.uncons (remainder parser) of
                            Just (c, rem) | c == '-'  -> parser { tokenValue = T.snoc (tokenValue parser) c
                                                                , remainder = rem
                                                                }
                                          | otherwise -> parser
                            Nothing -> parser




parserAppend :: CssParser -> T.Text -> CssTokenType -> CssParser
parserAppend parser tok tokType = parser { tokenValue = T.append (tokenValue parser) tok
                                         , tokenType = if T.length tok > 0 then (Just tokType) else (tokenType parser)
                                         , remainder = T.drop (T.length tok) (remainder parser)
                                         }




-- TODO: this function is beyond ugly
takeNumber :: CssParser -> CssParser
takeNumber parser | (not (T.null dot)) && (not (T.null fractional)) = parserAppend parser tokenFloat TokenFloat
                  | (not (T.null integral))                         = parserAppend parser tokenInt TokenInt
                  | otherwise                                       = parser
  where tokenFloat = T.concat [ integral, dot, fractional ]
        tokenInt = integral
        triplet = (fst pair1, fst pair2, fst pair3)
        pair1 = takeDec (remainder parser)
        pair2 = takeDot . snd $ pair1
        pair3 = takeDec . snd $ pair2
        takeDec text = (T.takeWhile D.C.isDigit text, T.dropWhile D.C.isDigit text)
        takeDot text = if T.isPrefixOf "." text then (".", T.tail text) else ("", text)
        integral = HU.tripletFst triplet
        dot = HU.tripletSnd triplet
        fractional = HU.tripletThrd triplet




takeAllTokens :: CssParser -> IO CssParser
takeAllTokens parser = do
  T.IO.putStrLn (remainder parser)
  let p = nextToken parser
  if tokenType p == Just TokenEnd
    then return p
    else takeAllTokens . nextToken $ p{tokenValue = "", tokenType = Nothing}

