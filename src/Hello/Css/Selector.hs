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

  , Chain2 (..)
  , linksToChain
  , chainToLinks
  , chainLength

  , CssSubclassSelector (..)

  , CssCachedComplexSelector (..)
  , defaultComplexSelector

  , CssComplexSelectorLink (..)
  , defaultComplexSelectorLink

  , CssComplexSelector

  , CssCombinator (..)

  , styleSheetElementCount -- TODO: this constant doesn't belong to this module
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





data CssCompoundSelector = CssCompoundSelector
  { selectorPseudoClass :: [T.Text]        -- https://www.w3.org/TR/selectors-4/#pseudo-class
  , selectorId          :: T.Text          -- https://www.w3.org/TR/selectors-4/#id-selector
  , selectorClass       :: [T.Text]        -- https://www.w3.org/TR/selectors-4/#class-selector
  , selectorTagName     :: CssTypeSelector
                                           -- TODO: add https://www.w3.org/TR/selectors-4/#attribute-selector
  } deriving (Show, Eq)



data CssComplexSelectorLink = CssComplexSelectorLink
  { compound   :: CssCompoundSelector
  , combinator :: CssCombinator
  } deriving (Show, Eq)




data CssCachedComplexSelector = CssCachedComplexSelector {
    matchCacheOffset :: Int
  , chain            :: CssComplexSelector
  } deriving (Show, Eq)




-- https://www.w3.org/TR/selectors-4/#typedef-type-selector
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
    CssCombinatorNone
  | CssCombinatorDescendant        -- ' '
  | CssCombinatorChild             -- '>'
  | CssCombinatorAdjacentSibling   -- '+'
  deriving (Show, Eq)




defaultCssCompoundSelector = CssCompoundSelector
  { selectorPseudoClass = []
  , selectorId          = ""
  , selectorClass       = []
  , selectorTagName     = CssTypeSelectorUniv
  }




defaultComplexSelectorLink = CssComplexSelectorLink
  { compound = defaultCssCompoundSelector

  -- Combinator that combines this compound selector and the previous one
  -- (previous one == compound selector to the left of current compound
  -- selector). For a compound selector that is first on the list (or the only
  -- on the list), the combinator will be None.
  , combinator          = CssCombinatorNone
  }




defaultComplexSelector = CssCachedComplexSelector {
    matchCacheOffset = -1
  , chain            = Datum defaultCssCompoundSelector
  }




data Chain2 a b
  = Datum a
  | Link b (Chain2 a b) (Chain2 a b)
  deriving (Show, Read, Eq, Ord)




{-
data Chain5 a b
  = Datum5 a
  | (:.) (Chain5 a b) (Chain5 a b)


data Chain4 a b
  = Datum4 a
  | Link4 (Chain4 a b) b (Chain4 a b)

ch41 =                                                                                   Datum4 "one"
ch42 =                                                         Link4 (Datum4 "two") '+' (Datum4 "one")
ch43 =                             Link4 (Datum4 "three") '-' (Link4 (Datum4 "two") '+' (Datum4 "one"))
ch44 = Link4 (Datum4 "four") '/'  (Link4 (Datum4 "three") '-' (Link4 (Datum4 "two") '+' (Datum4 "one")))
-}




type CssComplexSelector = Chain2 CssCompoundSelector CssCombinator




chainToLinks :: CssComplexSelector -> [CssComplexSelectorLink] -> [CssComplexSelectorLink]
chainToLinks (Link comb (Datum compound1) remainder) acc      = chainToLinks remainder (defaultComplexSelectorLink { compound = compound1, combinator = comb } : acc)
chainToLinks (Datum compound1) acc                 = defaultComplexSelectorLink { compound = compound1, combinator = CssCombinatorNone } : acc




linksToChain :: [CssComplexSelectorLink] -> CssComplexSelector
linksToChain = linksToChain' . reverse




linksToChain' :: [CssComplexSelectorLink] -> CssComplexSelector
linksToChain' list@(CssComplexSelectorLink{combinator=CssCombinatorChild}:xs)           = Link CssCombinatorChild           (Datum (compound . head $ list)) (linksToChain' xs)
linksToChain' list@(CssComplexSelectorLink{combinator=CssCombinatorAdjacentSibling}:xs) = Link CssCombinatorAdjacentSibling (Datum (compound . head $ list)) (linksToChain' xs)
linksToChain' list@(CssComplexSelectorLink{combinator=CssCombinatorDescendant}:xs)      = Link CssCombinatorDescendant      (Datum (compound . head $ list)) (linksToChain' xs)
linksToChain' list@(CssComplexSelectorLink{combinator=CssCombinatorNone}:xs)            = Datum . compound . head $ list
linksToChain' []                                                                        = Datum defaultCssCompoundSelector




chainLength chain = chainLength' chain 0
chainLength' (Link _ (Datum _) remainder) acc = chainLength' remainder (acc + 1)
chainLength' (Datum _)                    acc =                        (acc + 1)

