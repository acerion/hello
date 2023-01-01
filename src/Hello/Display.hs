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
-}




-- Display: a place where web document will be displayed. A screen or a PDF document.




module Hello.Display
  (
    Display (..)
  , defaultDisplay
  )
where




--import qualified Data.Text as T




data Display = Display
  { dpiX :: Float
  , dpiY :: Float
  } deriving (Show)




defaultDisplay = Display
  { dpiX = 0
  , dpiY = 0
  }


