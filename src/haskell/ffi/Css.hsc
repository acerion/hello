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
foreign export ccall "hll_findRuleListForInsertion" hll_findRuleListForInsertion :: Ptr FfiCssSimpleSelector -> Ptr FfiCssRulesMap -> Ptr FfiCssRulesMap -> Ptr FfiCssRulesList -> Ptr FfiCssRulesList -> IO (Ptr FfiCssRulesList)
foreign export ccall "hll_rulesListInsertRuleBySpecificity" hll_rulesListInsertRuleBySpecificity :: Ptr FfiCssRulesList -> Ptr FfiCssRule -> IO ()



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




hll_findRuleListForInsertion :: Ptr FfiCssSimpleSelector -> Ptr FfiCssRulesMap -> Ptr FfiCssRulesMap -> Ptr FfiCssRulesList -> Ptr FfiCssRulesList -> IO (Ptr FfiCssRulesList)
hll_findRuleListForInsertion ptrStructSimSel ptrStructIdRulesMap ptrStructClassRulesMap ptrStructElementRulesList ptrStructAnyElementRulesList = do
  ffiSimSel <- peek ptrStructSimSel

  let elementCount :: Int = (90 + 14)
  let element :: Int = fromIntegral . selectorElementC $ ffiSimSel

  let selectorId :: CString = selectorIdC ffiSimSel

  let ptr   | nullPtr /= selectorId = do
                list <- hll_rulesMapGetList ptrStructIdRulesMap selectorId
                if nullPtr == list
                  then do
                  newRulesList <- callocBytes #{size c_css_rules_list_t}
                  hll_rulesMapPutList ptrStructIdRulesMap selectorId newRulesList
                  return newRulesList
                  else return list

            | (fromIntegral . selectorClassSizeC $ ffiSimSel) > 0 = do
                let array = selectorClassC ffiSimSel
                elem0 :: CString <- peekElemOff array 0
                list <- hll_rulesMapGetList ptrStructClassRulesMap elem0
                if nullPtr == list
                  then do
                  newRulesList <- callocBytes #{size c_css_rules_list_t}
                  hll_rulesMapPutList ptrStructClassRulesMap elem0 newRulesList
                  return newRulesList
                  else return list

            | element >= 0 && element < elementCount = do
                let offset = element * #{size c_css_rules_list_t}
                return $ plusPtr ptrStructElementRulesList offset

            | element == cssSimpleSelectorElementAny = do return ptrStructAnyElementRulesList
            | otherwise                              = do return nullPtr
  ptr




findString :: Ptr (Ptr CChar) -> Int -> T.Text -> IO Int
findString array size string = do
  list <- peekArrayOfPointers array size ptrCCharToText
  case elemIndex string list of
    Just i  -> return i
    Nothing -> return (-1)





hll_rulesListInsertRuleBySpecificity :: Ptr FfiCssRulesList -> Ptr FfiCssRule -> IO ()
hll_rulesListInsertRuleBySpecificity ptrStructRulesList ptrStructCssRule = do

  list <- peekCssRulesList ptrStructRulesList
  rule <- peekCssRule ptrStructCssRule

  let newList = rulesListInsertRuleBySpecificity list rule

  pokeCssRulesList ptrStructRulesList newList

  return ()

  {-
void css_rules_list_insert_rule_by_specificity(c_css_rules_list_t * list, c_css_rule_t * rule)
{
   list->c_rules[list->c_rules_size] = (c_css_rule_t *) calloc(1, sizeof (c_css_rule_t));
   list->c_rules_size++;

   int i = list->c_rules_size - 1;

   while (i > 0 && rule->c_specificity < list->c_rules[i - 1]->c_specificity) {
      list->c_rules[i] = list->c_rules[i - 1];
      i--;
   }

   list->c_rules[i] = rule;
}
-}
