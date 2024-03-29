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




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Hello.Css.Selector
  (
    CssTypeSelector (..) -- TODO: don't export value constructors
  , unCssTypeSelector

  , CssSimpleSelector (..)

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

  , CssSubclassSelector (..)

  , CssComplexSelector
  , ComplexItem (..)

  , CssCombinator (..)

  , styleSheetElementCount -- TODO: this constant doesn't belong to this module

  , selectorSpecificity
  )
where




import Data.Maybe
import qualified Data.Text as T
import Data.Bits
--import Debug.Trace




{-
  TODO: don't hardcode the value.

  90 is the full number of html4 elements, including those which we have
  implemented. From html5, let's add: article, header, footer, mark, nav,
  section, aside, figure, figcaption, wbr, audio, video, source, embed.

  TODO: make it a constant imported from other (Html?) module
-}
styleSheetElementCount :: Int
styleSheetElementCount = 90 + 14




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




-- Type (tag name) selector
-- https://www.w3.org/TR/selectors-4/#type-selectors
data CssTypeSelector
    -- Type selectors: "regular" and universal
  = CssTypeSelector Int       --   https://www.w3.org/TR/selectors-4/#type-selector; Use htmlTagIndex "text" to get the integer value.
  | CssTypeSelectorUniversal  --   https://www.w3.org/TR/selectors-4/#the-universal-selector
  | CssTypeSelectorUnknown
  deriving (Show, Eq)




unCssTypeSelector :: CssTypeSelector -> Int
unCssTypeSelector (CssTypeSelector t)      = t
unCssTypeSelector CssTypeSelectorUniversal = -2
unCssTypeSelector CssTypeSelectorUnknown   = -1




-- https://www.w3.org/TR/selectors-4/#typedef-subclass-selector
data CssSubclassSelector
 = CssIdSelector T.Text
 | CssClassSelector T.Text
 --  | CssAttrSelector -- Unsupported for now
 | CssPseudoClassSelector T.Text
 deriving (Show, Eq)




-- Wrapper type for both kinds of simple selectors: type selector or subclass selector.
-- https://www.w3.org/TR/selectors-4/#typedef-simple-selector
data CssSimpleSelector
 = CssSimpleSelectorType CssTypeSelector
 | CssSimpleSelectorSubclass CssSubclassSelector
 deriving (Eq, Show)




compoundTagName :: CssCompoundSelector -> CssTypeSelector
compoundTagName = selectorTagName



compoundPseudoClass :: CssCompoundSelector -> [CssSubclassSelector]
compoundPseudoClass compound = map CssPseudoClassSelector (selectorPseudoClass compound)




compoundClass :: CssCompoundSelector -> [CssSubclassSelector]
compoundClass compound = map CssClassSelector (selectorClass compound)




compoundId :: CssCompoundSelector -> [CssSubclassSelector]
compoundId CssCompoundSelector{selectorId = ""} = []
compoundId CssCompoundSelector{selectorId = i}  = [CssIdSelector i]




-- Is a compound selector an 'Any' HTML tag?
compoundHasUniversalType :: CssCompoundSelector -> Bool
compoundHasUniversalType CssCompoundSelector{selectorTagName = CssTypeSelectorUniversal} = True
compoundHasUniversalType _                                                          = False




compoundHasUnexpectedType :: CssCompoundSelector -> Bool
compoundHasUnexpectedType CssCompoundSelector{selectorTagName = CssTypeSelectorUnknown} = True
compoundHasUnexpectedType _                                                             = False




compoundHasSpecificType :: CssCompoundSelector -> Bool
compoundHasSpecificType = isJust . compoundSpecificType




-- What is the element in compound selector? Either some specific HTML tag
-- (then 'Maybe t') or Any or None (then 'Nothing').
compoundSpecificType :: CssCompoundSelector -> Maybe Int
compoundSpecificType CssCompoundSelector{selectorTagName = CssTypeSelector t} = Just t
compoundSpecificType _                                                        = Nothing




compoundHasClass :: CssCompoundSelector -> Bool
compoundHasClass = not . null . selectorClass




compoundHasId :: CssCompoundSelector -> Bool
compoundHasId = not . T.null . selectorId




data CssCombinator =
    CssCombinatorDescendant        -- ' '      "x y"   selects y that is a direct or indirect child of x
  | CssCombinatorChild             -- '>'      "x > y" selects y that is a direct child of x
  | CssCombinatorAdjacentSibling   -- '+'
  deriving (Show, Eq)



defaultCssCompoundSelector :: CssCompoundSelector
defaultCssCompoundSelector = CssCompoundSelector
  { selectorPseudoClass = []
  , selectorId          = ""
  , selectorClass       = []
  , selectorTagName     = CssTypeSelectorUniversal
  }




-- Simple type allowing me to put compound selectors and the combinators in
-- the same list.
data ComplexItem
  = CompoundItem CssCompoundSelector
  | CombinatorItem CssCombinator
  deriving (Show, Eq)




-- https://www.w3.org/TR/selectors-4/#structure: "A complex selector is a
-- sequence of one or more compound selectors separated by combinators."
type CssComplexSelector = [ComplexItem]




{-
Return the specificity of the selector.

The specificity of a CSS selector is defined in
http://www.w3.org/TR/CSS21/cascade.html#specificity
-}
selectorSpecificity :: CssComplexSelector -> Int
selectorSpecificity complex = selectorSpecificity' complex 0
  where
    selectorSpecificity' :: CssComplexSelector -> Int -> Int
    selectorSpecificity' (CompoundItem compound : _ : remainder)       acc = selectorSpecificity' remainder (acc + compoundSelectorSpecificity compound)
    selectorSpecificity' (CompoundItem compound : _)                   acc = acc + compoundSelectorSpecificity compound
    selectorSpecificity' [CompoundItem compound]                       acc = acc + compoundSelectorSpecificity compound
    selectorSpecificity' (CombinatorItem _:CompoundItem compound:remd) acc = selectorSpecificity' remd (acc + compoundSelectorSpecificity compound)
    selectorSpecificity' []                                            acc = acc




-- Return the specificity of compound selector
compoundSelectorSpecificity :: CssCompoundSelector -> Int
compoundSelectorSpecificity compound = fromId compound + fromClass compound + fromPseudoClass compound + fromElement compound
  where
    fromId cpd          = if not . T.null . selectorId $ cpd then 1 `shiftL` 20 else 0
    fromClass cpd       = (length . selectorClass $ cpd) `shiftL` 10
    fromPseudoClass cpd = if not . null . selectorPseudoClass $ cpd then 1 `shiftL` 10 else 0 -- Remember that C/C++ code can use only first pseudo code.
    fromElement cpd     = if compoundHasUniversalType cpd then 0 else 1


