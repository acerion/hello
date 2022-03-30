{-
Copyright (C) 2022 Kamil Ignacak acerion@wp.pl

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

This file is derived from dillo-3.0.5/src/html.cc.
Copyright assignments from that file:
Copyright (C) 2005-2007 Jorge Arellano Cid <jcid@dillo.org>
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hello.Html.Attribute(
    parseLengthOrMultiLength2

  -- Only for tests
  , parseLengthOrMultiLength
  ) where




import Prelude
import Foreign.C.String
import Foreign
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.Read as T.R
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Map as M

import Hello.Css.Parser



cssCreateLength :: Float -> Int -> CssLength
cssCreateLength f lenType | lenType == cssLengthTypePX = CssLength word1 lenType
                          | lenType == cssLengthTypeNone
                            || lenType == cssLengthTypeMM
                            || lenType == cssLengthTypeEM
                            || lenType == cssLengthTypeEX
                            || lenType == cssLengthTypePercentage
                            || lenType == cssLengthTypeRelative = CssLength word2 lenType
                          | lenType == cssLengthTypeAuto = CssLength lenType lenType
                          | otherwise = CssLength cssLengthTypeAuto cssLengthTypeAuto

  where
    word1 = (((asInt1 (round f)) `shiftL` 3) .|. lenType)
    word2 = (((round ((asInt2 f) * (fromIntegral shift15L))) .&. (complement 7)) .|. lenType)

    shift15L = (1 `shiftL` 15) :: Int

    asInt1 :: Int -> Int
    asInt1 f = if f > css_LENGTH_INT_MAX
               then css_LENGTH_INT_MAX
               else if f < (-css_LENGTH_INT_MAX)
                    then (-css_LENGTH_INT_MAX)
                    else f

    asInt2 :: Float -> Float
    asInt2 f = if f > fromIntegral css_LENGTH_FRAC_MAX
               then fromIntegral css_LENGTH_FRAC_MAX
               else if f < fromIntegral (-css_LENGTH_FRAC_MAX)
                    then fromIntegral (-css_LENGTH_FRAC_MAX)
                    else f

    autoAsWord = cssLengthTypeAuto





takeValue text = case T.R.double text of
                   Right (f, rem) -> Just (f, rem)
                   Left _         -> Nothing




parseLengthOrMultiLength2 :: T.Text -> Maybe CssLength
parseLengthOrMultiLength2 attribute =
  case parseLengthOrMultiLength attribute of
    Just (f, t) -> Just (cssCreateLength f t)
    Nothing     -> Nothing




parseLengthOrMultiLength :: T.Text -> Maybe (Float, Int)
parseLengthOrMultiLength attribute =
  case takeValue attribute of
    Just (f, rem) -> case T.uncons rem of
                       -- The "px" suffix seems not allowed by HTML4.01 SPEC.
                       Nothing       -> Just (realToFrac f, cssLengthTypePX) -- empty remainder: attribute string is just a number
                       Just ('%', _) -> Just (realToFrac (f / 100), cssLengthTypePercentage) -- not sure why we divide by 100, but the dillo c++ code did this for percentage
                       Just ('*', _) -> Just (0.0, cssLengthTypeAuto)
                       Just (c, _)   -> if Data.Char.isSpace c
                                        then Just (realToFrac f, cssLengthTypePX)
                                        else Nothing -- Don't accept garbage attached to a number
    otherwise     -> Nothing


