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

This file is derived from dillo-3.0.5/src/css.cc.
Copyright assignments from css.cc file:
Copyright 2008-2014 Johannes Hofmann <Johannes.Hofmann@gmx.de>

This file is derived from dillo-3.0.5/src/cssparser.cc.
Copyright assignments from that file:
Copyright 2004 Sebastian Geerken <sgeerken@dillo.org>
Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
Additional note in cssparser.cc:
"This file is heavily based on the CSS parser of dillo-0.8.0-css-3 -
a dillo1 based CSS prototype written by Sebastian Geerken."
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Css.MatchCache
  (
    CssMatchCacheWrapper

  , matchCacheSize
  , matchCacheResize

  , matchCacheSetItem
  , matchCacheGetItem

  , matchCacheFromList
  , matchCacheToList

  ) where




import Hello.Utils




newtype CssMatchCacheWrapper a = CssMatchCacheWrapper { getCache :: [a] }
  deriving (Show)




matchCacheFromList :: [a] -> CssMatchCacheWrapper a
matchCacheFromList = CssMatchCacheWrapper




matchCacheResize :: Num a => CssMatchCacheWrapper a -> Int -> CssMatchCacheWrapper a
matchCacheResize cache size = newCache
  where
    oldList  = getCache cache
    oldSize  = length oldList
    newCache = CssMatchCacheWrapper (oldList ++ (replicate (size - oldSize) (-1)))




matchCacheSetItem :: CssMatchCacheWrapper a -> a -> Int -> CssMatchCacheWrapper a
matchCacheSetItem mc dtnNum elemIdx = CssMatchCacheWrapper $ listReplaceElem (getCache mc) dtnNum elemIdx




matchCacheGetItem :: CssMatchCacheWrapper a -> Int -> a
matchCacheGetItem mc selectorIdx = (getCache mc) !! selectorIdx




matchCacheToList :: CssMatchCacheWrapper a -> [a]
matchCacheToList = getCache




matchCacheSize :: CssMatchCacheWrapper a -> Int
matchCacheSize = length . getCache


