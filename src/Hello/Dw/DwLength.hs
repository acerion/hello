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
  )
  where




import Data.Bits
import Data.Coerce (coerce)




data DwLength = DwLength
  {
    dwLengthValue :: Double
  , dwLengthType  :: Int
  , dwLengthHash  :: Int
  }




-- Returns a percentage, v is relative to 1, not to 100.
createPercentageDwLength :: Double -> DwLength
createPercentageDwLength value = DwLength {
    dwLengthValue = value
  , dwLengthType  = 2
  , dwLengthHash  = ((iValue * (1 `shiftL` 18)) .&. (complement 3)) .|. 2
  }
  where
    iValue = round value -- TODO: probably bad conversion from float to int




-- Returns a length of n pixels
createAbsoluteDwLength :: Int -> DwLength
createAbsoluteDwLength value = DwLength {
    dwLengthValue = fromIntegral value -- TODO: probably bad conversion from int to float
  , dwLengthType  = 1
  , dwLengthHash  = (value `shiftL` 2) .|. 1
  }




createAutoDwLength :: DwLength
createAutoDwLength = DwLength {
    dwLengthValue = 0.0
  , dwLengthType  = 0
  , dwLengthHash  = 0
  }



