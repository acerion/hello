{-
Copyright (C) 2022-2023 Kamil Ignacak acerion@wp.pl

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




module Hello.Html.Attribute
  (
    parseLengthOrMultiLength
  , validateNameOrIdValue
  )
where




import Prelude
import qualified Data.Char as D.C
import qualified Data.Text as T
import qualified Data.Text.Read as T.R

import Hello.Css.Distance
import Hello.Html.Doctype




takeValue :: T.Text -> Maybe (Double, T.Text)
takeValue text = case T.R.double text of
                   Right (f, remainder) -> Just (f, remainder)
                   Left _               -> Nothing




-- Parsing of values of "height" or "width" attributes in some of html tags.
--
-- Reference: https://www.w3.org/TR/html4/types.html#h-6.6.
-- In particular look at meaning of asterisk.
parseLengthOrMultiLength :: T.Text -> Maybe CssDistance
parseLengthOrMultiLength attribute =
  case takeValue attribute of
    Just (f, remainder) -> case T.uncons remainder of
                             -- The "px" suffix seems not allowed by HTML4.01 SPEC.
                             Nothing       -> Just $ CssDistanceAbsPx (realToFrac f) -- empty remainder: attribute string is just a number
                             Just ('%', _) -> Just $ CssNumericPercentage (realToFrac (f / 100)) -- not sure why we divide by 100, but the dillo c++ code did this for percentage
                             Just ('*', _) -> Just CssDistanceAuto -- TODO: test this case.
                             Just (c, _)   -> if D.C.isSpace c
                                              then Just $ CssDistanceAbsPx (realToFrac f)
                                              else Nothing -- Don't accept garbage attached to a number
    Nothing      -> Nothing




{-
HTML4:
Check that 'val' is composed of characters inside [A-Za-z0-9:_.-]
Note: ID can't have entities, but this check is enough (no '&').

HTML5:
TODO: check spec and add info


Return value: true if OK, false otherwise.

TODO: this function is written in terrible style
-}
validateNameOrIdValue :: HtmlDoctype -> T.Text -> T.Text -> Bool
validateNameOrIdValue doctype _attrNameArg attrValueArg =
  case doctype of
    HtmlDoctypeHtml v -> if v >= 5.0
                         then for5 attrValueArg
                         else forOlder attrValueArg
    _                 -> forOlder attrValueArg

  where
    for5 attrValue | T.length attrValue == 0          = False -- TODO: log error that value of attribute attrName must not be empty
                   | T.any (\c -> c == ' ') attrValue = False -- TODO: log error that value of attribute attrName must not contain spaces
                   | otherwise                        = True

    forOlder attrValue = case T.uncons attrValue of
                           Nothing       -> False -- TODO: log error that value of attribute attrName must not be empty
                           Just (char, remainder) | not (D.C.isLatin1 char && D.C.isLetter char) -> False -- TODO: log error that first char is out of range
                                                  | not $ T.all (\c -> (D.C.isLatin1 c && D.C.isLetter c) || D.C.isDigit c || c `elem` [':', '_', ',', '-']) remainder -> False -- TODO: log error that some char is out of range
                                                  | otherwise -> True

