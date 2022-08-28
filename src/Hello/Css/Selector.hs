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
Additional note in cssparser.cc:
"This file is heavily based on the CSS parser of dillo-0.8.0-css-3 -
a dillo1 based CSS prototype written by Sebastian Geerken."
-}




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Hello.Css.Selector
  (
    CssTypeSelector (..) -- TODO: don't export value constructors
  , unCssTypeSelector
  , mkCssTypeSelector

  , CssCompoundSelector (..)
  , defaultCssCompoundSelector
  , compoundTagName
  , compoundPseudoClass
  , compoundClass
  , compoundId
  , compoundHasUniversalType
  , compoundHasUnexpectedType
  , compoundHasSpecificType
  , compoundSpecificType
  , compoundHasClass
  , compoundHasId

  , Chain (..)
  , chainLength
  , chainAny

  , CssSubclassSelector (..)

  , CssCachedComplexSelector (..)
  , defaultComplexSelector

  , CssComplexSelector

  , CssCombinator (..)

  , styleSheetElementCount -- TODO: this constant doesn't belong to this module

  , selectorSpecificity
  )
  where




import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Bits
import Debug.Trace



{-
  TODO: don't hardcode the value.

  90 is the full number of html4 elements, including those which we have
  implemented. From html5, let's add: article, header, footer, mark, nav,
  section, aside, figure, figcaption, wbr, audio, video, source, embed.

  TODO: make it a constant imported from other (Html?) module
-}
styleSheetElementCount = (90 + 14) :: Int




-- https://www.w3.org/TR/selectors-4/#structure: "A compound selector is a
-- sequence of simple selectors that are not separated by a combinator, and
-- represents a set of simultaneous conditions on a single element."
data CssCompoundSelector = CssCompoundSelector
  { selectorPseudoClass :: [T.Text]        -- https://www.w3.org/TR/selectors-4/#pseudo-class
  , selectorId          :: T.Text          -- https://www.w3.org/TR/selectors-4/#id-selector
  , selectorClass       :: [T.Text]        -- https://www.w3.org/TR/selectors-4/#class-selector
  , selectorTagName     :: CssTypeSelector -- https://www.w3.org/TR/selectors-4/#type-selectors
                                           -- TODO: add https://www.w3.org/TR/selectors-4/#attribute-selector
  } deriving (Show, Eq)




data CssCachedComplexSelector = CssCachedComplexSelector {
    matchCacheOffset :: Int
  , chain            :: CssComplexSelector
  } deriving (Show, Eq)




-- Type (tag name) selector
-- https://www.w3.org/TR/selectors-4/#type-selectors
data CssTypeSelector
    -- Type selectors: "regular" and universal
  = CssTypeSelector Int  --   https://www.w3.org/TR/selectors-4/#type-selector; Use htmlTagIndex "text" to get the integer value.
  | CssTypeSelectorUniv  --   https://www.w3.org/TR/selectors-4/#the-universal-selector
  | CssTypeSelectorUnknown
  deriving (Show, Eq)




unCssTypeSelector :: CssTypeSelector -> Int
unCssTypeSelector (CssTypeSelector t)    = t
unCssTypeSelector CssTypeSelectorUniv    = (-2)
unCssTypeSelector CssTypeSelectorUnknown = (-1)



mkCssTypeSelector :: Int -> CssTypeSelector
mkCssTypeSelector t | t >= 0 && t < styleSheetElementCount = CssTypeSelector t
                    | t == (-2)                            = CssTypeSelectorUniv
                    | otherwise                            = CssTypeSelectorUnknown



-- https://www.w3.org/TR/selectors-4/#typedef-subclass-selector
data CssSubclassSelector
 = CssIdSelector T.Text
 | CssClassSelector T.Text
 --  | CssAttrSelector -- Unsupported for now
 | CssPseudoClassSelector T.Text
 deriving (Show, Eq)



compoundTagName :: CssCompoundSelector -> CssTypeSelector
compoundTagName = selectorTagName



compoundPseudoClass :: CssCompoundSelector -> [CssSubclassSelector]
compoundPseudoClass compound = map (\x -> CssPseudoClassSelector x) (selectorPseudoClass compound)




compoundClass :: CssCompoundSelector -> [CssSubclassSelector]
compoundClass compound = map (\x -> CssClassSelector x) (selectorClass compound)




compoundId :: CssCompoundSelector -> [CssSubclassSelector]
compoundId (CssCompoundSelector{selectorId = ""}) = []
compoundId (CssCompoundSelector{selectorId = i})  = [CssIdSelector i]




-- Is a compound selector an 'Any' HTML tag?
compoundHasUniversalType (CssCompoundSelector{selectorTagName = CssTypeSelectorUniv}) = True
compoundHasUniversalType _                                                            = False




compoundHasUnexpectedType :: CssCompoundSelector -> Bool
compoundHasUnexpectedType (CssCompoundSelector{selectorTagName = CssTypeSelectorUnknown}) = True
compoundHasUnexpectedType _                                                               = False




compoundHasSpecificType :: CssCompoundSelector -> Bool
compoundHasSpecificType = isJust . compoundSpecificType




-- What is the element in compound selector? Either some specific HTML tag
-- (then 'Maybe t') or Any or None (then 'Nothing').
compoundSpecificType :: CssCompoundSelector -> Maybe Int
compoundSpecificType (CssCompoundSelector{selectorTagName = CssTypeSelector t}) = Just t
compoundSpecificType _                                                          = Nothing




compoundHasClass :: CssCompoundSelector -> Bool
compoundHasClass = not . null . selectorClass




compoundHasId :: CssCompoundSelector -> Bool
compoundHasId = not . T.null . selectorId




data CssCombinator =
    CssCombinatorDescendant        -- ' '      "x y"   selects y that is a direct or indirect child of x
  | CssCombinatorChild             -- '>'      "x > y" selects y that is a direct child of x
  | CssCombinatorAdjacentSibling   -- '+'
  deriving (Show, Eq)




defaultCssCompoundSelector = CssCompoundSelector
  { selectorPseudoClass = []
  , selectorId          = ""
  , selectorClass       = []
  , selectorTagName     = CssTypeSelectorUniv
  }




defaultComplexSelector = CssCachedComplexSelector {
    matchCacheOffset = -1
  , chain            = Datum defaultCssCompoundSelector
  }




data Chain a b
  = Datum a
  | Link (Chain a b) b (Chain a b)
  deriving (Show, Eq)



{-
ch41 =                                                                             Datum "one"
ch42 =                                                     Link (Datum "two") '+' (Datum "one")
ch43 =                           Link (Datum "three") '-' (Link (Datum "two") '+' (Datum "one"))
ch44 = Link (Datum "four") '/'  (Link (Datum "three") '-' (Link (Datum "two") '+' (Datum "one")))
-}




-- https://www.w3.org/TR/selectors-4/#structure: "A complex selector is a
-- sequence of one or more compound selectors separated by combinators."
type CssComplexSelector = Chain CssCompoundSelector CssCombinator




chainLength chain = chainLength' chain 0
chainLength' (Link (Datum _) _ remainder) acc = chainLength' remainder (acc + 1)
chainLength' (Datum _)                    acc =                        (acc + 1)




chainAny :: (CssCompoundSelector -> Bool) -> CssComplexSelector -> Bool
chainAny fn (Datum compound)                             = fn compound
chainAny fn (Link (Datum compound) combinator remainder) = (fn compound) || (chainAny fn remainder)




{-
Return the specificity of the selector.

The specificity of a CSS selector is defined in
http://www.w3.org/TR/CSS21/cascade.html#specificity
-}
selectorSpecificity :: CssComplexSelector -> Int
selectorSpecificity complex = selectorSpecificity' complex 0
  where
    selectorSpecificity' :: CssComplexSelector -> Int -> Int
    selectorSpecificity' (Link (Datum c1) combinator remainder) acc = selectorSpecificity' remainder (acc + (compoundSelectorSpecificity c1))
    selectorSpecificity' (Datum c1)                             acc =                                 acc + (compoundSelectorSpecificity c1)




-- Return the specificity of compound selector
compoundSelectorSpecificity :: CssCompoundSelector -> Int
compoundSelectorSpecificity compound = (fromId compound) + (fromClass compound) + (fromPseudoClass compound) + (fromElement compound)
  where
    fromId compound          = if (not . T.null . selectorId $ compound) then (1 `shiftL` 20) else 0
    fromClass compound       = (length . selectorClass $ compound) `shiftL` 10
    fromPseudoClass compound = if (not . null . selectorPseudoClass $ compound) then (1 `shiftL` 10) else 0 -- Remember that C/C++ code can use only first pseudo code.
    fromElement compound     = if compoundHasUniversalType compound then 0 else 1
