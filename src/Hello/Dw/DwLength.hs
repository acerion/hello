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

This file is derived from dillo-3.0.5/dw/style.*
Copyright assignments from style.cc file:
Copyright 2005-2007 Sebastian Geerken <sgeerken@dillo.org>
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}




module Hello.Dw.DwLength
  (
    DwLength (..)
  , createPercentageDwLength
  , createAbsoluteDwLength
  , createAutoDwLength

  , isPercentageDwLength
  , isAbsoluteDwLength
  , isAutoDwLength

  , getPercentageDwLengthValue
  , getAbsoluteDwLengthValue

  , getDwLengthHash

  , dwTypePerc
  , dwTypeAbs
  , dwTypeAuto
  )
where




import Data.Bits

import Hello.Utils




-- Magic values
dwTypePerc :: Int
dwTypePerc = 2

dwTypeAbs :: Int
dwTypeAbs = 1

dwTypeAuto :: Int
dwTypeAuto = 0




data DwLength
  = DwLengthPercentage Double
  | DwLengthAbsolute Int
  | DwLengthAuto
  deriving (Eq, Show)




-- Returns a percentage, v is relative to 1, not to 100.
createPercentageDwLength :: Double -> DwLength
createPercentageDwLength value = DwLengthPercentage value




-- Returns a length of n pixels
createAbsoluteDwLength :: Int -> DwLength
createAbsoluteDwLength value = DwLengthAbsolute . fromIntegral $ value




createAutoDwLength :: DwLength
createAutoDwLength = DwLengthAuto




-- Returns true if l is a percentage.
isPercentageDwLength :: DwLength -> Bool
isPercentageDwLength (DwLengthPercentage _) = True
isPercentageDwLength _                      = False




-- Returns true if l is an absolute length.
isAbsoluteDwLength :: DwLength -> Bool
isAbsoluteDwLength (DwLengthAbsolute _) = True
isAbsoluteDwLength _                    = False




-- Returns true if l is an auto length.
isAutoDwLength :: DwLength -> Bool
isAutoDwLength DwLengthAuto = True
isAutoDwLength _            = False




-- Returns the value of a percentage, relative to 1, as a double.
--
-- When possible, do not use this function directly; it may be removed soon.
-- Instead, use multiplyWithPercentageDwLength or
-- multiplyWithPercentageDwLengthRounded.
getPercentageDwLengthValue :: DwLength -> Maybe Double
getPercentageDwLengthValue (DwLengthPercentage v) = Just v
getPercentageDwLengthValue _                      = Nothing




-- Returns the value of a length in pixels, as an integer
getAbsoluteDwLengthValue :: DwLength -> Maybe Int
getAbsoluteDwLengthValue (DwLengthAbsolute v) = Just v
getAbsoluteDwLengthValue _                    = Nothing




getDwLengthHash :: DwLength -> Int
getDwLengthHash (DwLengthPercentage v) = hashPerc
  where
    hashPerc = (iValue .&. complement 3) .|. dwTypePerc
    shifted    :: Int    = 1 `shiftL` 18
    multiplied :: Double = v * realToFrac shifted
    iValue     :: Int    = roundInt . realToFrac $ multiplied
getDwLengthHash (DwLengthAbsolute v) = hashAbsolute
  where
    hashAbsolute = (v `shiftL` 2) .|. dwTypeAbs
getDwLengthHash DwLengthAuto = hashAuto
  where
    hashAuto = dwTypeAuto

