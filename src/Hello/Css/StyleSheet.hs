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
-}




module Hello.Css.StyleSheet( CssStyleSheet
                           , insertRuleToStyleSheet
                           ) where




import Prelude
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Read as T.R
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Map as M
import Control.Applicative
import Control.Monad -- when
import Debug.Trace

import CssParser
import Css
import Hello.Utils



type CssStyleSheet = (Maybe CssRulesMap, Maybe CssRulesMap, Maybe ([[CssRule]], [CssRule]), Maybe [CssRule])




insertRuleToStyleSheet :: CssSimpleSelector -> CssRule -> CssStyleSheet -> CssStyleSheet
insertRuleToStyleSheet topSimSel rule (Just mapId, Just mapC, Just (inListOfLists, inListOfRules), Just listAE) =
  case selectTargetRulesList topSimSel of
    1 -> (Just map2, Nothing, Nothing, Nothing)
      where
        map2 = updateMap mapId (selectorId topSimSel) rule
    2 -> (Nothing, Just map2, Nothing, Nothing)
      where
        map2 = updateMap mapC (head . selectorClass $ topSimSel) rule
    3 -> (Nothing, Nothing, Just (updatedListOfLists, updatedListOfRules), Nothing)
      where
        element = selectorElement topSimSel
        updatedListOfRules = rulesListInsertRuleBySpecificity inListOfRules rule
        updatedListOfLists = listReplaceElem inListOfLists updatedListOfRules element -- TODO: list of lists to be replaced with vector indexed by element
    4 -> (Nothing, Nothing, Nothing, Just list2)
      where
        list2 = rulesListInsertRuleBySpecificity listAE rule
    _ -> (Nothing, Nothing, Nothing, Nothing)




selectTargetRulesList :: CssSimpleSelector -> Int
selectTargetRulesList topSimSel | not . T.null . selectorId $ topSimSel  = 1
                                | not . null . selectorClass $ topSimSel = 2
                                | element >= 0 && element < elementCount = 3
                                | element == cssSimpleSelectorElementAny = 4
                                | otherwise                              = 5
  where
    elementCount = (90 + 14)
    element = selectorElement topSimSel




updateMap :: CssRulesMap -> T.Text -> CssRule -> CssRulesMap
updateMap map key rule = case M.lookup key map of
                           Just list -> M.insert key (rulesListInsertRuleBySpecificity list rule) map
                           Nothing   -> M.insert key (rulesListInsertRuleBySpecificity []   rule) map

