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




{-# LANGUAGE OverloadedStrings #-}




module CssParser(nextToken
                , CssParser (..)
                , defaultParser) where




import Prelude
import Foreign.C.String
import Foreign
import Data.Maybe
import qualified Data.Char as D.C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.Read as T.R
import qualified Data.Vector as V
import qualified HelloUtils as HU




data CssParser = CssParser {
    tokenValue  :: T.Text
  , tokenType   :: Maybe CssTokenType
  , remainder   :: T.Text
  , withinBlock :: Bool
  } deriving (Show)




data CssTokenType = TokenInt | TokenFloat | TokenColor | TokenSymbol | TokenString | TokenChar | TokenEnd deriving (Show)




defaultParser = CssParser { tokenValue = "", tokenType = Nothing, remainder = "", withinBlock = True }




-- TODO: another place that looks like that example from chapter about
-- parsing in Real World Haskell with control structure nested few times.
nextToken :: CssParser -> CssParser
nextToken parser = case takeNumber . takeLeadingMinus . takeLeadingEmpty $ parser of
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




takeSymbol parser = parserAppend parser tok TokenSymbol
  where tok = T.takeWhile pred (remainder parser)
        pred = (\c -> D.C.isAlpha c || c == '_' || c == '-')




takeString parser = case HU.takeEnclosed (remainder parser) "\"" "\"" True of
                      (Just string, rem) -> parseString parser string rem
                      (Nothing, _) -> case HU.takeEnclosed (remainder parser) "'" "'" True of
                                       (Just string, rem) -> parseString parser string rem
                                       (Nothing, _) -> parser
  where
    -- TODO: handle special cases in string
    parseString :: CssParser -> T.Text -> T.Text -> CssParser
    parseString parser string rem = parser{tokenValue = string, tokenType = Just TokenString, remainder = rem}




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
  | D.C.isSpace . T.head $ rem = takeLeadingEmpty parser { remainder = T.tail rem }
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





