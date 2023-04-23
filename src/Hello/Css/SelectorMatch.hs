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

This file is derived from dillo-3.0.5/src/css.cc.
Copyright assignments from css.cc file:
Copyright 2008-2014 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}




{-
Code in this file is implementing matching of CSS complex selector against
doctree elements.

References:

-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Css.SelectorMatch
  (
    complexSelectorMatches

    -- Only for tests.
  , compoundSelectorMatches'
  , CssCompoundSelectorMatch (..)
  )
where




import qualified Data.Text as T
--import Debug.Trace

import Hello.Css.Selector
import Hello.Html.Doctree
import Hello.Html.DoctreeNode



complexSelectorMatches :: CssComplexSelector -> Doctree -> DoctreeNode -> Bool
complexSelectorMatches complexSelector doctree dtn = complexSelectorMatches2 complexSelector (Just dtn) doctree



complexSelectorMatches2 :: CssComplexSelector -> Maybe DoctreeNode -> Doctree -> Bool
complexSelectorMatches2 _                                     Nothing    _       = False
complexSelectorMatches2 [CompoundItem compound]                       (Just dtn) _       = compoundSelectorMatches compound dtn
complexSelectorMatches2 (CompoundItem compound : CombinatorItem combinator : remainder) (Just dtn) doctree =
  if compoundSelectorMatches compound dtn
  then matchCombinatorAndRemainder combinator remainder dtn doctree
  else False
complexSelectorMatches2 []                                     _    _       = False
complexSelectorMatches2 _                                     _    _       = True





-- Test whether a pair of <combinator> + <remainder of complex selector>
-- matches a doctree.
matchCombinatorAndRemainder :: CssCombinator -> CssComplexSelector -> DoctreeNode -> Doctree -> Bool
matchCombinatorAndRemainder CssCombinatorDescendant      complex dtn doctree = matchDescendant         complex (getDtnParent doctree dtn)  doctree
matchCombinatorAndRemainder CssCombinatorChild           complex dtn doctree = complexSelectorMatches2 complex (getDtnParent doctree dtn)  doctree
matchCombinatorAndRemainder CssCombinatorAdjacentSibling complex dtn doctree = complexSelectorMatches2 complex (getDtnSibling doctree dtn) doctree




-- Go upwards of DocTree looking for a matching parent (because of Descendant
-- combinator), and then try to match remainder of Complex Selector.
findMatchingParentAndFollowers  :: CssComplexSelector -> Maybe DoctreeNode -> Doctree -> Bool
findMatchingParentAndFollowers _       Nothing    _       = False
findMatchingParentAndFollowers complex (Just dtn) doctree =
  if True -- uniqueNum dtn > dtnNumForCompound -- TODO: restore?
  then case complexSelectorMatches2 complex (Just dtn) doctree of
         -- This dtn node matched innermost Compound of Complex, and the rest
         -- of tree matched remainder of Complex.
         True  -> True
         -- Go up the tree searching for another candidate node that would
         -- match the innermost Compound of Complex (and the rest of tree
         -- would also match the remainder of Complex).
         False -> findMatchingParentAndFollowers complex parentDtn doctree
  else False

  where
    parentDtn = getDtnParent doctree dtn




-- Try to match inntermost Compound of Complex agains given node (which is a
-- Parent of some other node). If this fails, try to match agains parent, and
-- grandparent, until you find a match. On success, try to match remainder of
-- Complex against remainder of tree.
matchDescendant :: CssComplexSelector -> Maybe DoctreeNode -> Doctree -> Bool
matchDescendant complex mDtn doctree =
  case findMatchingParentAndFollowers complex mDtn doctree of
    True  -> True
    False -> case mDtn of
               Nothing  -> False
               Just dtn -> False




{-
Return whether compound selector matches at a given node of the document tree.

Right now this is a naive re-write of simple_selector_matches() C function.

TODO: in C++ code the string comparisons were case-insensitive.
-}
compoundSelectorMatches :: CssCompoundSelector -> DoctreeNode -> Bool
compoundSelectorMatches compound dtn = compoundSelectorMatches' compound dtn == CssCompoundSelectorMatch




data CssCompoundSelectorMatch
  = CssCompoundSelectorMatch
  | CssCompoundSelectorMismatchElement
  | CssCompoundSelectorMismatchPseudoClass
  | CssCompoundSelectorMismatchId
  | CssCompoundSelectorMismatchClass
  deriving (Eq)




-- https://www.w3.org/TR/selectors-4/#structure: "A given element is said to
-- match a compound selector when it matches all simple selectors in the
-- compound selector."
compoundSelectorMatches' :: CssCompoundSelector -> DoctreeNode -> CssCompoundSelectorMatch
compoundSelectorMatches' compound dtnArg | not $ matchOnElement compound dtnArg     = CssCompoundSelectorMismatchElement
                                         | not $ matchOnPseudoClass compound dtnArg = CssCompoundSelectorMismatchPseudoClass
                                         | mismatchOnId compound dtnArg          = CssCompoundSelectorMismatchId
                                         | mismatchOnClass compound dtnArg       = CssCompoundSelectorMismatchClass
                                         | otherwise                             = CssCompoundSelectorMatch
  where
    matchOnElement :: CssCompoundSelector -> DoctreeNode -> Bool
    matchOnElement csel dtn = case compoundTagName csel of
                                CssTypeSelector t        -> t == htmlElementIdx dtn
                                CssTypeSelectorUniversal -> True
                                CssTypeSelectorUnknown   -> False

    matchOnPseudoClass :: CssCompoundSelector -> DoctreeNode -> Bool
    matchOnPseudoClass csel dtn = all (`elem` pseudos) (compoundPseudoClass csel)
      where
        pseudos = fmap CssPseudoClassSelector (selPseudoClass dtn)

    mismatchOnId :: CssCompoundSelector -> DoctreeNode -> Bool
    mismatchOnId csel dtn = (not . null . compoundId $ csel) && ((T.null . selId $ dtn) || (compoundId csel /= [CssIdSelector . selId $ dtn]))
    -- if (selector->c_selector_id != NULL && (dtn->c_element_selector_id == NULL || dStrAsciiCasecmp (selector->c_selector_id, dtn->c_element_selector_id) != 0))
    --     return false;

    -- All class items of a compound selector must be found in dtn's class set
    mismatchOnClass :: CssCompoundSelector -> DoctreeNode -> Bool
    mismatchOnClass csel dtn = not allCompoundClassInNodeClass
      where
        allCompoundClassInNodeClass = all (`elem` classes) (compoundClass csel)
        classes = map CssClassSelector (selClass dtn)
    -- for (int i = 0; i < selector->c_selector_class_size; i++) {
    -- bool found = false;
    -- for (size_t j = 0; j < dtn->c_element_selector_class_size; j++) {
    --     if (dStrAsciiCasecmp (selector->c_selector_class[i], dtn->c_element_selector_class[j]) == 0) {
    --         found = true;
    --         break;
    --     }
    -- }
    -- if (!found) {
    --     return false;
    -- }




