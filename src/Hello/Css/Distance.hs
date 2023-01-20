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




{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-} -- For 'Data'.




module Hello.Css.Distance
  (
    CssDistance (..)
  )
where




import Data.Data

--import Debug.Trace




-- This data type is not meant (yet) to be a good reflection of CSS standard.
-- For now it is only a better replacement for CssLength type. The CssLenght
-- type used an integer to encode (as bits in a bit word) different types of
-- length, with three leftmost bits used for a type tag.
--
-- This data type is a step in a better direction.
data CssDistance =
    CssDistanceRelEm Float       -- CSS_LENGTH_TYPE_EM
  | CssDistanceRelEx Float       -- CSS_LENGTH_TYPE_EX
  | CssDistanceAbsMm Float       -- CSS_LENGTH_TYPE_MM; "cm", "in", "pt" and "pc" are converted into millimeters.
  | CssDistanceAbsPx Float       -- CSS_LENGTH_TYPE_PX

  | CssNumericPercentage Float   -- CSS_LENGTH_TYPE_PERCENTAGE

  | CssNumericNone     Float     -- CSS_LENGTH_TYPE_NONE
  | CssNumericRelative Float     -- CSS_LENGTH_TYPE_RELATIVE; This does not exist in CSS but is used in HTML
    -- Value corresponding to "auto" value of "height", "width", "margin-*" property.
  | CssDistanceAuto              -- CSS_LENGTH_TYPE_AUTO; This can be used as a simple value.
  deriving (Show, Eq, Data)




