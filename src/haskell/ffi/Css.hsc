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
-}


{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


module Hello.Ffi.Css.Misc() where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Read as T.R
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.Encoding.Error as T.E.E
import qualified Data.Text.IO as T.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.Foldable as Foldable
import qualified Data.Map as M
import Control.Applicative
import Control.Monad -- when
import Text.Printf
import Debug.Trace

import CssParser
import Css
import Hello.Ffi.Css.Parser
import Hello.Ffi.Utils




#include "../hello.h"




data FfiDoctreeNode = FfiDoctreeNode {
    uniqueNumC      :: CInt -- unique ascending id
  , htmlElementIdxC :: CInt -- Index to html.cc::Tags

  , elementSelectorPseudoClassC     :: CString
  , elementSelectorIdC              :: CString
  , elementSelectorClassC           :: CString
  , elementSelectorClassSizeC       :: CInt

  , parentC    :: Ptr FfiDoctreeNode
  , siblingC   :: Ptr FfiDoctreeNode
  , lastChildC :: Ptr FfiDoctreeNode
  } deriving (Show)




instance Storable FfiDoctreeNode where
  sizeOf    _ = #{size c_doctree_node_t}
  alignment _ = #{alignment c_doctree_node_t}

  poke ptr (FfiDoctreeNode a b c d e f g h i) = do
    #{poke c_doctree_node_t, c_unique_num}                    ptr a
    #{poke c_doctree_node_t, c_html_element_idx}              ptr b
    #{poke c_doctree_node_t, c_element_selector_pseudo_class} ptr c
    #{poke c_doctree_node_t, c_element_selector_id}           ptr d
    #{poke c_doctree_node_t, c_element_selector_class}        ptr e
    #{poke c_doctree_node_t, c_element_selector_class_size}   ptr f
    #{poke c_doctree_node_t, c_parent}                        ptr g
    #{poke c_doctree_node_t, c_sibling}                       ptr h
    #{poke c_doctree_node_t, c_last_child}                    ptr i

  peek ptr = do
    a <- #{peek c_doctree_node_t, c_unique_num}                    ptr
    b <- #{peek c_doctree_node_t, c_html_element_idx}              ptr
    c <- #{peek c_doctree_node_t, c_element_selector_pseudo_class} ptr
    d <- #{peek c_doctree_node_t, c_element_selector_id}           ptr
    e <- #{peek c_doctree_node_t, c_element_selector_class}        ptr
    f <- #{peek c_doctree_node_t, c_element_selector_class_size}   ptr
    g <- #{peek c_doctree_node_t, c_parent}                        ptr
    h <- #{peek c_doctree_node_t, c_sibling}                       ptr
    i <- #{peek c_doctree_node_t, c_last_child}                    ptr
    return (FfiDoctreeNode a b c d e f g h i)




peekDoctreeNode :: Ptr FfiDoctreeNode -> IO DoctreeNode
peekDoctreeNode ptrStructDoctreeNode = do

  ffiDtn <- peek ptrStructDoctreeNode
  pc <- ptrCCharToText . elementSelectorPseudoClassC $ ffiDtn
  i  <- ptrCCharToText . elementSelectorIdC $ ffiDtn

  let cOffset = (#offset c_doctree_node_t, c_element_selector_class)
  let cStringArray :: Ptr CString = plusPtr ptrStructDoctreeNode cOffset
  c  <- peekArrayOfPointers cStringArray (fromIntegral . elementSelectorClassSizeC $ ffiDtn) ptrCCharToText

  return DoctreeNode{ uniqueNum = fromIntegral . uniqueNumC $ ffiDtn

                    , htmlElementIdx = fromIntegral . htmlElementIdxC $ ffiDtn
                    , selPseudoClass = pc
                    , selId          = i
                    , selClass       = c

                    , parent = undefined
                    , sibling = undefined
                    , lastChild = undefined
                    }




foreign export ccall "hll_simpleSelectorMatches" hll_simpleSelectorMatches :: Ptr FfiCssSimpleSelector -> Ptr FfiDoctreeNode -> IO Int
foreign export ccall "hll_selectorSpecificity" hll_selectorSpecificity :: Ptr FfiCssSelector -> IO Int
foreign export ccall "hll_rulesMapGetList" hll_rulesMapGetList :: Ptr FfiCssRulesMap -> CString -> IO (Ptr FfiCssRulesList)
foreign export ccall "hll_rulesMapPutList" hll_rulesMapPutList :: Ptr FfiCssRulesMap -> CString -> Ptr FfiCssRulesList -> IO ()
foreign export ccall "hll_insertRuleToStyleSheet" hll_insertRuleToStyleSheet :: Ptr FfiCssRule -> Ptr FfiCssSimpleSelector -> Ptr FfiCssRulesMap -> Ptr FfiCssRulesMap -> Ptr FfiCssRulesList -> Ptr FfiCssRulesList -> IO CInt



hll_simpleSelectorMatches :: Ptr FfiCssSimpleSelector -> Ptr FfiDoctreeNode -> IO Int
hll_simpleSelectorMatches ptrStructSimpleSelector ptrStructDoctreeNode = do

  simSel :: CssSimpleSelector <- peekCssSimpleSelector ptrStructSimpleSelector
  dtn    :: DoctreeNode       <- peekDoctreeNode ptrStructDoctreeNode
{-
  putStrLn ("FFI: simSel: " ++ show simSel)
  putStrLn ("FFI: dtn: "
            ++ "htmlElementIdx = " ++ (show $ htmlElementIdx dtn)
            ++ ", selPseudoClass = " ++ (show $ selPseudoClass dtn)
            ++ ", selId = " ++ (show $ selId dtn)
            ++ ", selClass = " ++ (show $ selClass dtn))
-}
  if simpleSelectorMatches simSel dtn
    then return 1 -- True
    else return 0 -- False




hll_selectorSpecificity :: Ptr FfiCssSelector -> IO Int
hll_selectorSpecificity ptrStructCssSelector = do
  sel <- peekCssSelector ptrStructCssSelector
  return . selectorSpecificity $ sel





data FfiCssRulesList = FfiCssRulesList {
    rulesC     :: Ptr (Ptr FfiCssRule)
  , rulesSizeC :: CInt
  } deriving (Show)



instance Storable FfiCssRulesList where
  sizeOf    _ = #{size c_css_rules_list_t}
  alignment _ = #{alignment c_css_rules_list_t}

  peek ptr = do
    let a = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_rules_list_t, c_rules}) ptr
    b <- #{peek c_css_rules_list_t, c_rules_size} ptr
    return (FfiCssRulesList a b)

  poke ptr (FfiCssRulesList a b) = do
    #{poke c_css_rules_list_t, c_rules}      ptr a
    #{poke c_css_rules_list_t, c_rules_size} ptr b




peekCssRulesList :: Ptr FfiCssRulesList -> IO [CssRule]
peekCssRulesList ptrStructRulesList = do
  ffiRulesList <- peek ptrStructRulesList

  let array :: Ptr (Ptr FfiCssRule) = rulesC ffiRulesList
  let size  :: Int                  = fromIntegral . rulesSizeC $ ffiRulesList

  rules <- peekArrayOfPointers array size peekCssRule

  return rules



pokeCssRulesList :: Ptr FfiCssRulesList -> [CssRule] -> IO ()
pokeCssRulesList ptrStructRulesList list = do
  ffiRulesList <- peek ptrStructRulesList

  let array :: Ptr (Ptr FfiCssRule) = rulesC ffiRulesList

  pokeArrayOfPointers list pokeCssRule array
  pokeByteOff ptrStructRulesList (#offset c_css_rules_list_t, c_rules_size) (length list)




aPokeCssRulesList :: [CssRule] -> IO (Ptr FfiCssRulesList)
aPokeCssRulesList list = do
  ptrStructCssRulesList <- callocBytes #{size c_css_rules_list_t}
  pokeCssRulesList ptrStructCssRulesList list
  return ptrStructCssRulesList




data FfiCssRule = FfiCssRule {
    selectorC       :: Ptr FfiCssSelector
  , declarationSetC :: Ptr FfiCssDeclarationSet
  , specificityC    :: CInt
  , positionC       :: CInt
  } deriving (Show)



instance Storable FfiCssRule where
  sizeOf    _ = #{size c_css_value_t}
  alignment _ = #{alignment c_css_value_t}

  poke ptr (FfiCssRule a b c d) = do
    #{poke c_css_rule_t, c_selector}    ptr a
    #{poke c_css_rule_t, c_decl_set}    ptr b
    #{poke c_css_rule_t, c_specificity} ptr c
    #{poke c_css_rule_t, c_position}    ptr d

  peek ptr = do
    a <- #{peek c_css_rule_t, c_selector}    ptr
    b <- #{peek c_css_rule_t, c_decl_set}    ptr
    c <- #{peek c_css_rule_t, c_specificity} ptr
    d <- #{peek c_css_rule_t, c_position}    ptr
    return (FfiCssRule a b c d)



peekCssRule :: Ptr FfiCssRule -> IO CssRule
peekCssRule ptrStructCssRule = do

  ffiCssRule <- peek ptrStructCssRule

  sel     <- peekCssSelector (selectorC ffiCssRule)
  declSet <- peekCssDeclarationSet (declarationSetC ffiCssRule)

  return CssRule { selector       = sel
                 , declarationSet = declSet
                 , specificity    = fromIntegral . specificityC $ ffiCssRule
                 , position       = fromIntegral . positionC $ ffiCssRule
                 }




pokeCssRule :: CssRule -> IO (Ptr FfiCssRule)
pokeCssRule rule = do

  ptrStructCssRule <- callocBytes #{size c_css_rule_t}

  ptrSelector <- callocBytes #{size c_css_selector_t}
  updateSelectors ptrSelector [(selector rule)]
  pokeByteOff ptrStructCssRule #{offset c_css_rule_t, c_selector} ptrSelector

  ptrDeclSet <- callocBytes #{size c_css_declaration_set_t}
  pokeCssDeclarationSet ptrDeclSet (declarationSet rule)
  pokeByteOff ptrStructCssRule #{offset c_css_rule_t, c_decl_set} ptrDeclSet

  let spec :: CInt = fromIntegral . specificity $ rule
  pokeByteOff ptrStructCssRule #{offset c_css_rule_t, c_specificity} spec

  let pos :: CInt = fromIntegral . position $ rule
  pokeByteOff ptrStructCssRule #{offset c_css_rule_t, c_position} pos

  return ptrStructCssRule



data FfiCssRulesMap = FfiCssRulesMap {
    stringsC      :: Ptr (Ptr CChar)
  , rulesListC    :: Ptr (Ptr FfiCssRulesList)
  , rulesMapSizeC :: CInt
  } deriving (Show)




instance Storable FfiCssRulesMap where
  sizeOf    _ = #{size c_css_rules_map_t}
  alignment _ = #{alignment c_css_rules_map_t}

  peek ptr = do
    let a = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_rules_map_t, c_strings}) ptr
    let b = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_rules_map_t, c_rl})      ptr
    c <- #{peek c_css_rules_map_t, c_rules_map_size} ptr
    return (FfiCssRulesMap a b c)

  poke ptr (FfiCssRulesMap a b c) = do
    #{poke c_css_rules_map_t, c_strings}        ptr a
    #{poke c_css_rules_map_t, c_rl}             ptr b
    #{poke c_css_rules_map_t, c_rules_map_size} ptr c




peekCssRulesMap :: Ptr FfiCssRulesMap -> IO CssRulesMap
peekCssRulesMap ptrStructRulesMap = do
  ffiRulesMap <- peek ptrStructRulesMap

  let size :: Int = fromIntegral . rulesMapSizeC $ ffiRulesMap
  keys   <- peekArrayOfPointers (stringsC ffiRulesMap) size ptrCCharToText
  values <- peekArrayOfPointers (rulesListC ffiRulesMap) size peekCssRulesList

  let map = M.fromList $ zip keys values

  -- putStrLn ("FFI: map = " ++ (show map))

  return map




pokeCssRulesMap :: Ptr FfiCssRulesMap -> CssRulesMap -> IO ()
pokeCssRulesMap ptrStructRulesMap map = do
  ffiRulesMap <- peek ptrStructRulesMap

  let keys = M.keys map
  let values = M.elems map
  let len :: CInt = (fromIntegral . length $ keys)

  pokeArrayOfPointers keys textToPtrCChar (stringsC ffiRulesMap)
  pokeArrayOfPointers values aPokeCssRulesList (rulesListC ffiRulesMap)
  pokeByteOff ptrStructRulesMap (#offset c_css_rules_map_t, c_rules_map_size) len

  return ()




hll_rulesMapGetList :: Ptr FfiCssRulesMap -> CString -> IO (Ptr FfiCssRulesList)
hll_rulesMapGetList ptrStructRulesMap cStringKey = do
  ffiRulesMap :: FfiCssRulesMap <- peek ptrStructRulesMap
  key :: T.Text                 <- ptrCCharToText cStringKey

  let stringsArray      = stringsC ffiRulesMap
  let listsOfRulesArray = rulesListC ffiRulesMap
  let size :: Int       = fromIntegral . rulesMapSizeC $ ffiRulesMap

  idx <- findString stringsArray size key
  case idx of
    (-1) -> return nullPtr
    _    -> do
      e <- peekElemOff listsOfRulesArray idx
      return e




hll_rulesMapGetList2 :: Ptr FfiCssRulesMap -> T.Text -> IO (Ptr FfiCssRulesList)
hll_rulesMapGetList2 ptrStructRulesMap key = do
  ffiRulesMap :: FfiCssRulesMap <- peek ptrStructRulesMap

  let stringsArray      = stringsC ffiRulesMap
  let listsOfRulesArray = rulesListC ffiRulesMap
  let size :: Int       = fromIntegral . rulesMapSizeC $ ffiRulesMap

  idx <- findString stringsArray size key
  case idx of
    (-1) -> return nullPtr
    _    -> do
      e <- peekElemOff listsOfRulesArray idx
      return e




hll_rulesMapPutList :: Ptr FfiCssRulesMap -> CString -> Ptr FfiCssRulesList -> IO ()
hll_rulesMapPutList ptrStructRulesMap cStringKey ptrStructRulesList = do
  ffiRulesMap :: FfiCssRulesMap <- peek ptrStructRulesMap
  key :: T.Text                 <- ptrCCharToText cStringKey

  let stringsArray      = stringsC ffiRulesMap
  let listsOfRulesArray = rulesListC ffiRulesMap
  let oldSize :: Int    = fromIntegral . rulesMapSizeC $ ffiRulesMap

  idx <- findString stringsArray oldSize key
  case idx of
    (-1) -> do
      let newSize :: CInt = (fromIntegral oldSize) + 1
      pokeElemOff listsOfRulesArray oldSize ptrStructRulesList
      newCStringKey <- textToPtrCChar key -- textToPtrCChar serves as strdup()
      pokeElemOff stringsArray oldSize newCStringKey
      pokeByteOff ptrStructRulesMap (#offset c_css_rules_map_t, c_rules_map_size) newSize
      return ()
    _    -> do
      pokeElemOff listsOfRulesArray idx ptrStructRulesList -- replace old element
      return ()




hll_rulesMapPutList2 :: Ptr FfiCssRulesMap -> T.Text -> Ptr FfiCssRulesList -> IO ()
hll_rulesMapPutList2 ptrStructRulesMap key ptrStructRulesList = do
  ffiRulesMap :: FfiCssRulesMap <- peek ptrStructRulesMap

  let stringsArray      = stringsC ffiRulesMap
  let listsOfRulesArray = rulesListC ffiRulesMap
  let oldSize :: Int    = fromIntegral . rulesMapSizeC $ ffiRulesMap

  idx <- findString stringsArray oldSize key
  case idx of
    (-1) -> do
      let newSize :: CInt = (fromIntegral oldSize) + 1
      pokeElemOff listsOfRulesArray oldSize ptrStructRulesList
      newCStringKey <- textToPtrCChar key -- textToPtrCChar serves as strdup()
      pokeElemOff stringsArray oldSize newCStringKey
      pokeByteOff ptrStructRulesMap (#offset c_css_rules_map_t, c_rules_map_size) newSize
      return ()
    _    -> do
      pokeElemOff listsOfRulesArray idx ptrStructRulesList -- replace old element
      return ()






hll_insertRuleToStyleSheet :: Ptr FfiCssRule -> Ptr FfiCssSimpleSelector -> Ptr FfiCssRulesMap -> Ptr FfiCssRulesMap -> Ptr FfiCssRulesList -> Ptr FfiCssRulesList -> IO CInt
hll_insertRuleToStyleSheet ptrStructCssRule ptrStructSimSel ptrStructIdRulesMap ptrStructClassRulesMap ptrStructElementRulesList ptrStructAnyElementRulesList = do
  topSimSel <- peekCssSimpleSelector ptrStructSimSel
  rule      <- peekCssRule ptrStructCssRule

  mapId  <- peekCssRulesMap ptrStructIdRulesMap
  mapC   <- peekCssRulesMap ptrStructClassRulesMap
  listAE <- peekCssRulesList ptrStructAnyElementRulesList

  let element :: Int = selectorElement topSimSel
  inListOfLists :: [[CssRule]] <- getList ptrStructElementRulesList [] 0 (90 + 14)
  let inPtrListOfRules = (plusPtr ptrStructElementRulesList (element * #{size c_css_rules_list_t}))

  let elementCount :: Int = (90 + 14)
  inListOfRules <- if element >= 0 && element < elementCount
                   then peekCssRulesList inPtrListOfRules
                   else return []

  let styleSheet = (Just mapId, Just mapC, Just (inListOfLists, inListOfRules), Just listAE)

  case insertRuleToStyleSheet topSimSel rule styleSheet of
    (Just map, Nothing, Nothing, Nothing) -> do
      pokeCssRulesMap ptrStructIdRulesMap map
      return 1
    (Nothing, Just map, Nothing, Nothing) -> do
      pokeCssRulesMap ptrStructClassRulesMap map
      return 1
    (Nothing, Nothing, Just (listOfLists, listOfRules), Nothing) -> do
      pokeCssRulesList inPtrListOfRules listOfRules
      return 1
    (Nothing, Nothing, Nothing, Just list) -> do
      pokeCssRulesList ptrStructAnyElementRulesList list
      return 1
    _                                      -> return 0




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
        element :: Int = selectorElement topSimSel
        updatedListOfRules = rulesListInsertRuleBySpecificity inListOfRules rule
        updatedListOfLists = listReplace inListOfLists updatedListOfRules element -- TODO: list of lists to be replaced vector indexed by element
    4 -> (Nothing, Nothing, Nothing, Just list2)
      where
        list2 = rulesListInsertRuleBySpecificity listAE rule
    _ -> (Nothing, Nothing, Nothing, Nothing)




listReplace :: [a] -> a -> Int -> [a]
listReplace list new idx = concat [front, [new], back]
  where
    front = take idx list
    back  = drop (idx + 1) list



getList :: Ptr FfiCssRulesList -> [[CssRule]] -> Int -> Int -> IO [[CssRule]]
getList table acc idx count = do
  if idx == count
    then return acc
    else do
    let ptrList = (plusPtr table (idx * #{size c_css_rules_list_t}))
    listOfRules <- peekCssRulesList ptrList
    getList table (listOfRules:acc) (idx + 1) count



selectTargetRulesList :: CssSimpleSelector -> Int
selectTargetRulesList topSimSel | not . T.null . selectorId $ topSimSel  = 1
                                | not . null . selectorClass $ topSimSel = 2
                                | element >= 0 && element < elementCount = 3
                                | element == cssSimpleSelectorElementAny = 4
                                | otherwise                              = 5
  where
    elementCount :: Int = (90 + 14)
    element :: Int = selectorElement topSimSel




updateMap :: CssRulesMap -> T.Text -> CssRule -> CssRulesMap
updateMap map key rule = case M.lookup key map of
                           Just list -> M.insert key (rulesListInsertRuleBySpecificity list rule) map
                           Nothing   -> M.insert key (rulesListInsertRuleBySpecificity []   rule) map




findString :: Ptr (Ptr CChar) -> Int -> T.Text -> IO Int
findString array size string = do
  list <- peekArrayOfPointers array size ptrCCharToText
  case elemIndex string list of
    Just i  -> return i
    Nothing -> return (-1)
