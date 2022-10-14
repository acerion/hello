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
  , CssMatchCache

  , matchCacheSize
  , matchCacheIncreaseTo
  , matchCacheIncreaseBy

  , matchCacheSetItem
  , matchCacheGetItem

  , matchCacheFromList
  , matchCacheToList

  )
where




import Hello.Utils




{-
A list of DoctreeNodes' unique numbers, indexed by a number calcuated from
ComplexSelector's properties.

Used for caching of some state during matching of specific CSS rules against
doctree. 'specific' probably means that a rule contains compound selector(s)
with 'descendant' combinator(s).
-}



newtype CssMatchCacheWrapper a = CssMatchCacheWrapper { getCache :: [a] }
  deriving (Show)




type CssMatchCache = CssMatchCacheWrapper Int




-- Increase (to 'newSize') a size of match cache. The increased size of match
-- cache be necessary after adding rule to a css context.
--
-- With (almost) each added rule the size of match cache must be increased by
-- specific amount.
matchCacheIncreaseTo :: Num a => CssMatchCacheWrapper a -> Int -> CssMatchCacheWrapper a
matchCacheIncreaseTo cache newSize = newCache
  where
    oldList  = getCache cache
    oldSize  = length oldList
    newCache = CssMatchCacheWrapper (oldList ++ replicate (newSize - oldSize) (-1))




-- Increase (by 'delta') a size of match cache. The increased size of match
-- cache be necessary after adding rule to a css context.
--
-- With (almost) each added rule the size of match cache must be increased by
-- specific amount.
matchCacheIncreaseBy :: Num a => CssMatchCacheWrapper a -> Int -> CssMatchCacheWrapper a
matchCacheIncreaseBy matchCache delta = if requiredCacheSize > currentCacheSize
                                        then matchCacheIncreaseTo matchCache requiredCacheSize
                                        else matchCache
  where
    currentCacheSize  = matchCacheSize matchCache
    requiredCacheSize = currentCacheSize + delta




-- Set value of one specific item (at given index) in match cache.
-- Invalid index results (probably) in runtime error.
matchCacheSetItem :: CssMatchCacheWrapper a -> a -> Int -> CssMatchCacheWrapper a
matchCacheSetItem mc dtnNum elemIdx = CssMatchCacheWrapper $ listReplaceElem (getCache mc) dtnNum elemIdx




-- Get one specific item (at given index) from match cache.
-- Invalid index results in runtime error.
matchCacheGetItem :: CssMatchCacheWrapper a -> Int -> a
matchCacheGetItem mc selectorIdx = (getCache mc) !! selectorIdx




-- Convert list of items to match cache.
matchCacheFromList :: [a] -> CssMatchCacheWrapper a
matchCacheFromList = CssMatchCacheWrapper




-- Convert match cache to list of items.
matchCacheToList :: CssMatchCacheWrapper a -> [a]
matchCacheToList = getCache




-- Get size of match cache.
matchCacheSize :: CssMatchCacheWrapper a -> Int
matchCacheSize = length . getCache


