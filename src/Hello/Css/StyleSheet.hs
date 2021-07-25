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




module Hello.Css.StyleSheet( CssStyleSheet (..)
                           , CssRulesMap (..)
                           , addRuleToStyleSheet

                           , CssMatchCache (..)
                           , matchCacheResize

                           , CssContext (..)
                           , cssContextAddRule

                           , styleSheetElementCount

                           , makeAndDispatchRule
                           ) where




import Prelude
import Data.List
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L
import Debug.Trace
import Control.Monad -- when

import CssParser
import Css
import Hello.Utils




type CssRulesMap = M.Map T.Text [CssRule]




{-
  TODO: don't hardcode the value.

  90 is the full number of html4 elements, including those which we have
  implemented. From html5, let's add: article, header, footer, mark, nav,
  section, aside, figure, figcaption, wbr, audio, video, source, embed.

  TODO: make it a constant imported from other (Html?) module
-}
styleSheetElementCount = (90 + 14)




data CssStyleSheet = CssStyleSheet {
    rulesById          :: CssRulesMap -- CSS rules, in which topmost simple selector is "id".
  , rulesByClass       :: CssRulesMap -- CSS rules, in which topmost simple selector is "class".
  -- TODO: list of lists to be replaced with vector indexed by element.
  , rulesByElement     :: [[CssRule]] -- CSS rules, in which topmost simple selector is "specific html element".
  , rulesByAnyElement  :: [CssRule]   -- CSS rules, in which topmost simple selector is "any html element".

  , requiredMatchCache :: Int
  } deriving (Show)




{-
Insert a rule into style sheet.

To improve matching performance the rules are organized into rule lists based
on the topmost simple selector of their selector.
-}
addRuleToStyleSheet :: CssStyleSheet -> CssRule -> CssStyleSheet
addRuleToStyleSheet sheet rule = case insertRuleToStyleSheet rule sheet of
                                   (0, outSheet) -> outSheet
                                   (i, outSheet) -> outSheet { requiredMatchCache = newRequiredMatchCache sheet rule }

  where
    newRequiredMatchCache s r = if getRequiredMatchCache r > requiredMatchCache s
                                then trace ("Updating from " ++ (show . requiredMatchCache $ s) ++ " to " ++ (show . getRequiredMatchCache $ r)) (getRequiredMatchCache r)
                                else requiredMatchCache s




{-
The returned int value is needed only in FFI code, so that the FFI code knows
which field of style sheet has changed and which input/output pointer to
poke.
-}
insertRuleToStyleSheet :: CssRule -> CssStyleSheet -> (Int, CssStyleSheet)
insertRuleToStyleSheet rule sheet
  -- Put a rule in a bucket. Decide which bucket to choose by looking at
  -- topmost Simple Selector of the rule.
  | not . T.null . selectorId $ topSimSel  = (1, sheet { rulesById         = updatedRulesById })
  | not . null . selectorClass $ topSimSel = (2, sheet { rulesByClass      = updatedRulesByClass })
  | element >= 0 && element < elementCount = (3, sheet { rulesByElement    = updatedRulesByElement })
  | element == cssSimpleSelectorElementAny = (4, sheet { rulesByAnyElement = updatedRulesByAnyElement })
  | otherwise                              = if (element /= cssSimpleSelectorElementNone)
                                             then (trace ("[EE] insert rule: unexpected element: " ++ (show element)) (0, sheet))
                                             else (0, sheet)

  where
    topSimSel = getTopSimSel rule

    element      = selectorElement topSimSel
    elementCount = styleSheetElementCount

    updatedRulesById    = updateMapOfLists (rulesById sheet) (selectorId topSimSel) rule
    updatedRulesByClass = updateMapOfLists (rulesByClass sheet) (head . selectorClass $ topSimSel) rule

    listOfLists = rulesByElement sheet
    thisElementRules = if element >= 0 && element < elementCount
                       then listOfLists !! element
                       else []

    updatedThisElementRules = insertRuleInListOfRules thisElementRules rule
    updatedRulesByElement = listReplaceElem listOfLists updatedThisElementRules element

    updatedRulesByAnyElement = insertRuleInListOfRules (rulesByAnyElement sheet) rule




{-
Update map of lists of rules (update by inserting the rule into specific
list).

A map can be indexed by either CSS ID or CSS class.

Value of a map, associated with the index (map key) is a list of rules, each
of the rules having given id/class in topmost Simple Selector.
-}
updateMapOfLists :: CssRulesMap -> T.Text -> CssRule -> CssRulesMap
updateMapOfLists map key rule = case M.lookup key map of
                                  Just list -> M.insert key (insertRuleInListOfRules list rule) map
                                  Nothing   -> M.insert key (insertRuleInListOfRules []   rule) map




{-
Insert rule with increasing specificity.

If two rules have the same specificity, the one that was added later will be
added behind the others. This gives later added rules more weight.

TODO:
The goal of proper ordering of rules where newer rules go at the end of slice
of rules with the same specificity can be achieved also with this
implementation:

insertRuleInListOfRules list rule = reverse $ L.insertBy ord rule (reverse list)
  where ord r2 r1 = compare (specificity r1) (specificity r2)

But I don't know which version is more effective: with two reverses or with
span + concat.
-}
insertRuleInListOfRules :: [CssRule] -> CssRule -> [CssRule]
insertRuleInListOfRules list rule = L.concat [smallerOrEqual, [rule], larger]
  where
    (smallerOrEqual, larger) = L.span (\r -> (specificity rule) >= (specificity r)) list




type CssMatchCache = [Int]




data CssContext = CssContext {
    sheets       :: [CssStyleSheet]
  , matchCache   :: CssMatchCache
  , rulePosition :: Int
  } deriving (Show)





cssPrimaryUserAgent       = 0
cssPrimaryUser            = 1
cssPrimaryAuthor          = 2
cssPrimaryAuthorImportant = 3
cssPrimaryUserImportant   = 4
cssPrimaryOrderSize       = 5




cssOriginUserAgent = 0
cssOriginUser      = 1
cssOriginAuthor    = 2



cssRuleIsSafe rule = (not . cssSelectorHasPseudoClass . selector $ rule) || (isSafe . declarationSet $ rule)



-- Does any selector have non-empty list of pseudo class simple selectors?
-- Remember that C/C++ code can use only first pseudo class.
cssSelectorHasPseudoClass :: CssSelector -> Bool
cssSelectorHasPseudoClass selector = any (\simSel -> not . null . selectorPseudoClass $ simSel) (simpleSelectors selector)




cssContextAddRule :: CssContext -> CssRule -> Int -> CssContext
cssContextAddRule context rule order =

  -- TODO: should we increpement rulePosition in a context, to which a rule
  -- is not being added (in "then" branch)?
  if (order == cssPrimaryAuthor || order == cssPrimaryAuthorImportant) && (not . cssRuleIsSafe $ rule)
  then trace ("[WW] Ignoring unsafe author style that might reveal browsing history") (context{rulePosition = (rulePosition context) + 1})
  else cssContextAddRule' context{rulePosition = (rulePosition context) + 1} ruleWithOffset order

  where
    -- Set match cache offset of selector.
    ruleWithOffset :: CssRule
    ruleWithOffset = if (-1) == (matchCacheOffset . selector $ rule)
                     then rule {selector = newSelector . selector $ rule}
                     else rule
      where
        newSelector sel = sel{matchCacheOffset = length . matchCache $ context}





-- Add given rule to a style sheet in given context. The style sheet is
-- selected by 'order' argument.
cssContextAddRule' :: CssContext -> CssRule -> Int -> CssContext
cssContextAddRule' context rule order = context{ sheets     = listReplaceElem (sheets context) updatedSheet order
                                               , matchCache = if requiredCacheSize > existingCacheSize
                                                              then matchCacheResize (matchCache context) requiredCacheSize
                                                              else (matchCache context)
                                               }
  where
    updatedSheet      = addRuleToStyleSheet ((sheets $ context) !! order) rule
    existingCacheSize = length . matchCache $ context
    requiredCacheSize = getRequiredMatchCache rule




matchCacheResize cache size = newCache
  where
    oldSize = length cache
    newCache = cache ++ (replicate (size - oldSize) (-1))




-- TODO: Name of this function is sub-optimal.
--
-- Given list of selectors, and given declaration sets (regular and
-- important), for each of the selectors create one rule and add it to
-- context.
--
-- Each rule can have only one selector, so this function works like this:
-- "for each selector create a rule with given selector and some
-- declarations, and put it in appropriate style sheet in the context".
makeAndDispatchRule :: CssContext -> [CssSelector] -> CssDeclarationSet -> CssDeclarationSet -> Int -> IO CssContext
makeAndDispatchRule context []     declSet declSetImp _      = return context
makeAndDispatchRule context (x:xs) declSet declSetImp origin
  | origin == cssOriginUserAgent = do
       let updatedContext = if (length . items $ declSet) > 0
                            then cssContextAddRule context (ruleCtor x declSet (rulePosition context)) cssPrimaryUserAgent
                            else context
       makeAndDispatchRule updatedContext xs declSet declSetImp origin

  | origin == cssOriginUser = do
      let updatedContext = if (length . items $ declSet) > 0
                           then cssContextAddRule context (ruleCtor x declSet (rulePosition context)) cssPrimaryUser
                           else context

      let updatedContext2 = if (length . items $ declSetImp) > 0
                            then cssContextAddRule updatedContext (ruleCtor x declSetImp (rulePosition context)) cssPrimaryUserImportant
                            else updatedContext
      makeAndDispatchRule updatedContext2 xs declSet declSetImp origin

  | origin == cssOriginAuthor = do
      let updatedContext = if (length . items $ declSet) > 0
                           then cssContextAddRule context (ruleCtor x declSet (rulePosition context)) cssPrimaryAuthor
                           else context

      let updatedContext2 = if (length . items $ declSetImp) > 0
                            then cssContextAddRule updatedContext (ruleCtor x declSetImp (rulePosition context)) cssPrimaryAuthorImportant
                            else updatedContext
      makeAndDispatchRule updatedContext2 xs declSet declSetImp origin

  where
    ruleCtor sel declSet pos = CssRule { selector = sel, declarationSet = declSet, specificity = selectorSpecificity sel, position = pos }
