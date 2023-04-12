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
Code in this file is implementing CSS cascading.

References:

https://www.w3.org/TR/CSS22/cascade.html#cascade
https://www.w3.org/TR/css-cascade-3/
https://www.w3.org/TR/css-cascade-4/
-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Css.Cascade
  (
    applyCssStyleSheets
  )
where




import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
--import Debug.Trace

import Hello.Css.Declaration
import Hello.Css.Rule
import Hello.Css.StyleNode
import Hello.Css.StyleSheet
import Hello.Css.SelectorMatch
import Hello.Html.Doctree
import Hello.Html.DoctreeNode
import Hello.Utils




type MatchingRules = [CssRule]




applyCssRule :: CssDeclarationSet -> Doctree -> DoctreeNode -> CssRule -> CssDeclarationSet
applyCssRule declSet doctree dtn rule =
  if complexSelectorMatches (complexSelector rule) doctree dtn
  then declarationsSetAppend declSet (declarationSet rule)
  else declSet




-- Apply potentially matching rules from matchingRules with ascending
-- specificity. If specificity is equal, rules are applied in order of
-- appearance. Each matchingRules is sorted already.
applyMatchingRules :: MatchingRules -> Doctree -> DoctreeNode -> (CssDeclarationSet, [String]) -> (CssDeclarationSet, [String])
applyMatchingRules matchingRules doctree dtn (declSet, logs) = (declSet', logs')
  where
    -- TODO: uncomment this line and observe value of tree's top node num. It's
    -- constantly increasing, as if the function was called with constantly
    -- updated doctree, each time a new element is added to the doctree. This
    -- may be a great waste of resources: to call matching function on
    -- constantly updated doctree.
    --putStrLn ("Is topNodeNum increasing? " ++ (show . topNodeNum $ doctree))

    sortedRules = L.sortBy compareRules matchingRules

    declSet' = foldr (\ rule ds -> applyCssRule ds doctree dtn rule) declSet sortedRules

    debugString1 = "dtn = " ++ (show dtn) ++ "\n\n"
    debugString2 = "sorted rules = " ++ (show sortedRules) ++ "\n\n"
    debugString3 = "updated declSet = " ++ (show declSet') ++ "\n\n\n\n\n"
    logs' = debugString3:debugString2:debugString1:logs




-- TODO: check if all the conditions are included, and whether they are used
-- for calculations correctly.
compareRules :: CssRule -> CssRule -> Ordering
compareRules r1 r2 | (specificity r1) < (specificity r2) = GT
                   | (specificity r1) > (specificity r2) = LT
                   | (position r1)    < (position r2)    = GT
                   | (position r1)    > (position r2)    = LT
                   | otherwise                           = EQ




-- Apply a stylesheet to a list of declarations.
--
-- The declarations (list property+value) are set as defined by the rules in
-- the stylesheet that match at the given node in the document tree.
applyStyleSheet :: CssStyleSheet -> Doctree -> DoctreeNode -> (CssDeclarationSet, [String]) -> (CssDeclarationSet, [String])
applyStyleSheet styleSheet doctree dtn (declSet, logs) = applyMatchingRules matchingRules doctree dtn (declSet, logs)
  where
    matchingRules = buildMatchingRulesForDtn styleSheet dtn




-- This function appears to be implementing (in its own way) this part of CSS
-- 2.2 cascading: "Find all declarations that apply to the element and
-- property in question...".
--
-- The function doesn't seem to fully implement this part: "Declarations
-- apply if the associated selector matches the element in question" because
-- matching of complex selector is not done here. Only a first selector in a
-- complex selector of a rule is compared, and if it matches, the rule is
-- appended to result.
--
-- So this function pre-selects rules, and full matching of complex selectors
-- is done elsewhere (in complexSelectorMatches*?).
buildMatchingRulesForDtn :: CssStyleSheet -> DoctreeNode -> MatchingRules
buildMatchingRulesForDtn styleSheet dtn = concat rulesLists
  where
    rulesLists = byAnyElement . byElementId . byClass . bySelId $ []


    {-
    if (dtn->c_element_selector_id) {
       rules_lists[numLists] = ffiRulesMapGetList(style_sheet->c_rules_by_id, dtn->c_element_selector_id);
       if (rules_lists[numLists]) {
          numLists++;
       }
    }
    -}
    bySelId lists = if T.null . selId $ dtn
                    then lists
                    else case M.lookup (selId dtn) (rulesById styleSheet) of
                           Just l  -> l:lists
                           Nothing -> lists


    {-
    for (int i = 0; i < dtn->c_element_selector_class_size; i++) {
       if (i >= maxLists - 4) {
          MSG_WARN("Maximum number of classes per element exceeded.\n");
          break;
       }

       rules_lists[numLists] = ffiRulesMapGetList(style_sheet->c_rules_by_class, dtn->c_element_selector_class[i]);
       if (rules_lists[numLists]) {
          numLists++;
       }
    }
    -}
    byClass lists = getSelectorClassLists (selClass dtn) lists
      where
        getSelectorClassLists []     acc = acc
        getSelectorClassLists (c:cs) acc = case M.lookup c (rulesByClass styleSheet) of
                                             Just l  -> getSelectorClassLists cs (l:acc)
                                             Nothing -> getSelectorClassLists cs acc


    {-
    rules_lists[numLists] = style_sheet->c_rules_by_type[dtn->c_html_element_idx];
    if (rules_lists[numLists])
       numLists++;
    -}
    byElementId lists = if htmlElementIdx dtn == (-1)
                        then lists
                        else (rulesByType styleSheet !! htmlElementIdx dtn):lists


    {-
    rules_lists[numLists] = style_sheet->c_rules_by_any_element;
    if (rules_lists[numLists])
       numLists++;
    -}
    byAnyElement lists = if null . rulesByAnyElement $ styleSheet
                         then lists
                         else rulesByAnyElement styleSheet : lists




-- Apply a CSS context to a property list.
--
-- The stylesheets in the context are applied one after the other in the
-- ordering defined by CSS 2.1. Stylesheets that are applied later can
-- overwrite properties set by previous stylesheets. This allows e.g. user
-- styles to overwrite author styles.
--
-- Remember that styleNode and dtn aren't necessarily the top/current
-- elements of style node stack or doctree. This function may be called for
-- any element of style node stack and doctree during restyling of entire
-- tree. The restyling is done by C++ code when <body> is opened, see
-- "html->styleEngine->restyle (html->bw);" in Html_tag_open_body(). Caller
-- of this function should always use some styleNodeIndex as a starting point
-- to get a proper styleNode and dtn.
applyCssStyleSheets :: [String] -> CssContext -> Doctree -> DoctreeNode -> StyleNode -> (CssDeclarationSet, [String])
applyCssStyleSheets logs context doctree dtn styleNode = compose fs (defaultCssDeclarationSet, logs)
  where
    -- TODO: check the performance impact of using 'compose'.
    -- Will direct composition of these functions be faster?
    fs = [ (applyStyleSheet (getSheet context CssPrimaryUserAgent) doctree dtn)
         , (applyStyleSheet (getSheet context CssPrimaryUser) doctree dtn)
         , (applyDeclSet (nonCssDeclSet styleNode))
         , (applyStyleSheet (getSheet context CssPrimaryAuthor) doctree dtn)
         , (applyDeclSet (mainDeclSet styleNode))
         , (applyStyleSheet (getSheet context CssPrimaryAuthorImportant) doctree dtn)
         , (applyDeclSet (importantDeclSet styleNode))
         , (applyStyleSheet (getSheet context CssPrimaryUserImportant) doctree dtn)
         ]
    applyDeclSet declSet (declSetAcc, ls) = (declarationsSetAppend declSetAcc declSet, ls)


