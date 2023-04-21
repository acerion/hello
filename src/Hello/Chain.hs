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




{-# LANGUAGE ScopedTypeVariables #-}




{-
A "dual-genous" list, i.e. a list of values of two types. Main type of chain
is D: a type of datum elements. Each two datum values of type D are alwyas
separated by single link value of type L. The chain may contain single value
of type D, in this case there are no values of type L in the chain.

The data structure is used to describe CSS selectors:
selector1 combinatorA selector2 combinatorB selector3 combinatorC selector4

The selectors are the datums, and the combinators are the links.



Example usages of the data type:
a = Last "sel1"
b = Chain "sel1" '+' (Last "sel2")
c = Chain "sel1" '+' (Chain "sel2" '>' (Last "sel3"))
-}




module Hello.Chain
  (
    Chain (..)

  , chainDatumLength
  , chainAnyDatum
  , chainGetFirstDatum
  )
where




-- Chain of datums and links
data Chain d l
  = Last d                 -- Last element in the chain. Also used to construct single-datum chain.
  | Chain d l (Chain d l)  -- Multi-datum chain constructor.
  deriving (Show, Eq)




-- Get count of datums in chain.
--
-- The function DOES NOT count amount of links (items between datums).
--
-- Unit tested: yes
chainDatumLength :: Chain d l -> Int
chainDatumLength chain = length' chain 0



length' :: Chain d l -> Int -> Int
length' (Chain _ _ remainder) acc = length' remainder (acc + 1)
length' (Last _)              acc =                    acc + 1




-- Check if any datum in the given chain satisfies given predicate.
--
-- Unit tested: yes
chainAnyDatum :: (d -> Bool) -> Chain d l -> Bool
chainAnyDatum predicate (Last datum)              = predicate datum
chainAnyDatum predicate (Chain datum _ remainder) = predicate datum || chainAnyDatum predicate remainder




-- Get first datum in the chain.
--
-- Since it is impossible (for now) to create empty chain (a chain without
-- datums), the function returns "datum" and not "Maybe datum". This may
-- change in the future.
--
-- Unit tested: yes
chainGetFirstDatum :: Chain d l -> d
chainGetFirstDatum (Last datum)      = datum
chainGetFirstDatum (Chain datum _ _) = datum




{-

-- TODO: this function has missing cases for pattern matching.
listToChain :: CssComplexSelector -> Chain CssCompoundSelector CssCombinator
listToChain [CompoundItem compound] = Last compound
listToChain (CompoundItem compound:CombinatorItem combi:xs) = Chain compound combi (listToChain xs)
--listToChain [] = (Last defaultCssCompoundSelector)




{-
:m +Hello.Css.Parser.Declaration
:m +Hello.Css.Tokenizer
:m +Hello.Css.Parser.Property

:m +Hello.Css.Selector
:m +Hello.Chain
:set prompt >

chainToList (Last (CssCompoundSelector {selectorPseudoClass = [], selectorId = "", selectorClass = [], selectorTagName = CssTypeSelector 0})) []
chainToList (Chain ( (CssCompoundSelector {selectorPseudoClass = [], selectorId = "", selectorClass = [], selectorTagName = CssTypeSelector 85})) CssCombinatorDescendant (Chain ( (CssCompoundSelector {selectorPseudoClass = [], selectorId = "", selectorClass = [], selectorTagName = CssTypeSelector 85})) CssCombinatorDescendant (Chain ( (CssCompoundSelector {selectorPseudoClass = [], selectorId = "", selectorClass = [], selectorTagName = CssTypeSelector 85})) CssCombinatorDescendant (Last (CssCompoundSelector {selectorPseudoClass = [], selectorId = "", selectorClass = [], selectorTagName = CssTypeSelector 85}))))) []
-}
chainToList :: Chain CssCompoundSelector CssCombinator -> CssComplexSelector -> CssComplexSelector
chainToList (Last compound)                  acc = acc ++ [CompoundItem compound]
chainToList (Chain compound combinator remd) acc = chainToList remd (acc ++ [CompoundItem compound] ++ [CombinatorItem combinator])
-}

