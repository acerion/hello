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
{-# LANGUAGE DeriveDataTypeable #-} -- For 'Data'.




module Hello.Css.Value
  (
    CssValue (..)
  )
where




import Debug.Trace

import Data.Data
import Data.Text as T

import Hello.Css.Distance





data CssValue =
  -- CssValueTypeInt Int             -- This type is only used internally, for x-* properties.
  -- CssValueTypeEnum Int            -- Value is i, if represented by enum_symbols[i].
  -- CssValueTypeMultiEnum Int       -- For all enum_symbols[i], 1 << i are combined.
  CssValueTypeLengthPercent CssDistance   -- <length> or <percentage>. Represented by CssDistance.
  | CssValueTypeLength CssDistance          -- <length>, represented as CssDistance.
                                    -- Note: In some cases, CSS_TYPE_LENGTH
                                    -- is used instead of
                                    -- CSS_TYPE_LENGTH_PERCENTAGE, only
                                    -- because Dw cannot handle percentages
                                    -- in this particular case (e.g.
                                    -- 'margin-*-width').
  | CssValueTypeSignedLength CssDistance    -- As CSS_TYPE_LENGTH but may be negative.
  | CssValueTypeLengthPercentNumber CssDistance -- <length> or <percentage>, or <number>
  | CssValueTypeAuto CssDistance    -- Represented as CssDistance of type CssDistanceAuto
  --  | CssValueTypeColor Int           -- Represented as integer.
  --  | CssValueTypeFontWeight Int      -- This very special and only used by 'font-weight'
  | CssValueTypeString T.Text       -- <string>
  --  | CssValueTypeStringList [T.Text] -- List of symbols, which are directly
                                    -- copied (as opposed to
                                    -- CSS_PROPERTY_DATA_TYPE_ENUM and
                                    -- CSS_PROPERTY_DATA_TYPE_MULTI_ENUM).
                                    -- Used for 'font-family'.
  | CssValueTypeURI T.Text          -- <uri>
  --  | CssValueTypeBgPosition          -- TODO: add values to this constructor
  --  | CssValueTypeUnused              -- Not yet used. Will itself get unused some day.
  deriving (Show, Eq, Data)




