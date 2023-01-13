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


module Hello.Ffi.Css.Misc
  (
  )
where



{-
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Data.List
import Data.Bits
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector as V
import qualified Data.List as L
import Control.Monad -- when
import Debug.Trace

import Hello.Css.MatchCache
import Hello.Css.MediaQuery
import Hello.Css.Parser
import Hello.Css.StyleSheet
import Hello.Css.Selector
import Hello.Css.Tokenizer
import Hello.Css.UserAgentStyle
import Hello.Html.Doctree
import Hello.Html.DoctreeNode

import Hello.Utils

--import Hello.Ffi.Css.Context
import Hello.Ffi.Css.DoctreeNode
import Hello.Ffi.Css.Doctree
import Hello.Ffi.Css.Parser
--import Hello.Ffi.Css.StyleSheet
import Hello.Ffi.Utils
-}



#include "../../hello.h"




--foreign export ccall "hll_rulesMapGetList" hll_rulesMapGetList :: Ptr FfiCssRulesMap -> CString -> IO (Ptr FfiCssRulesList)
--foreign export ccall "hll_printCssDeclarationSet" hll_printCssDeclarationSet :: Ptr FfiCssDeclarationSet -> IO ()
--foreign export ccall "hll_printCssIndex" hll_printCssIndex :: Ptr CInt -> IO ()




{-
analyzeDtn ptrStructDtn tree = do
  mDtn <- ptrToMdtn ptrStructDtn
  case mDtn of
    Just dtn -> do
      let intPtr = ptrToIntPtr ptrStructDtn
      let this = case intPtr of
            IntPtr i -> i
      if M.member this tree
        then
        do
          return tree
        else
        do
          let tree2 = M.insert this dtn tree
          tree3 <- analyzeDtn (intPtrToPtr $ (IntPtr $ dtnParent dtn)) tree2
          analyzeDtn (intPtrToPtr $ (IntPtr $ dtnSibling dtn)) tree3
    Nothing  -> return tree




ptrToMdtn ptrStructDtn = do
  if (nullPtr /= ptrStructDtn)
    then
    do dtn <- peekDoctreeNode ptrStructDtn
       return $ Just dtn
    else
    return Nothing




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

  pokeArrayOfPreallocedPointers list pokeCssRule array
  pokeByteOff ptrStructRulesList (#offset c_css_rules_list_t, c_rules_size) (length list)




data FfiCssRule = FfiCssRule {
    selectorC       :: Ptr FfiCssComplexSelector
  , declarationSetC :: Ptr FfiCssDeclarationSet
  , specificityC    :: CInt
  , positionC       :: CInt
  } deriving (Show)



/**
 * \brief A pair of CSS selector and CSS declarations set.
 *
 *  The c_css_declaration_set_t is applied if the c_css_cached_complex_selector_t matches.
 */
typedef struct c_css_rule_t {
      c_css_cached_complex_selector_t * c_cached_complex_selector;
      c_css_declaration_set_t * c_decl_set;
      int c_specificity;
      int c_position;
} c_css_rule_t;



instance Storable FfiCssRule where
  sizeOf    _ = #{size c_css_value_t}
  alignment _ = #{alignment c_css_value_t}

  poke ptr (FfiCssRule a b c d) = do
    #{poke c_css_rule_t, c_cached_complex_selector}    ptr a
    #{poke c_css_rule_t, c_decl_set}    ptr b
    #{poke c_css_rule_t, c_specificity} ptr c
    #{poke c_css_rule_t, c_position}    ptr d

  peek ptr = do
    a <- #{peek c_css_rule_t, c_cached_complex_selector}    ptr
    b <- #{peek c_css_rule_t, c_decl_set}    ptr
    c <- #{peek c_css_rule_t, c_specificity} ptr
    d <- #{peek c_css_rule_t, c_position}    ptr
    return (FfiCssRule a b c d)




peekCssRule :: Ptr FfiCssRule -> IO CssRule
peekCssRule ptrStructCssRule = do

  ffiCssRule <- peek ptrStructCssRule

  cplxSel <- peekCssComplexSelector (selectorC ffiCssRule)
  declSet <- peekCssDeclarationSet (declarationSetC ffiCssRule)

  return CssRule { complexSelector = cplxSel
                 , declarationSet  = declSet
                 , specificity     = fromIntegral . specificityC $ ffiCssRule
                 , position        = fromIntegral . positionC $ ffiCssRule
                 }




-- Fill preallocated c_css_rule_t object.
-- The rule is prealloced, but its members are not (yet).
pokeCssRule :: Ptr FfiCssRule -> CssRule -> IO ()
pokeCssRule ptrStructCssRule rule = do

  ffiRule <- peek ptrStructCssRule

  let ptrSelector = selectorC ffiRule
  --ptrSelector <- callocBytes #{size c_css_cached_complex_selector_t}
  pokeCssComplexSelector ptrSelector (complexSelector rule)
  pokeByteOff ptrStructCssRule #{offset c_css_rule_t, c_cached_complex_selector} ptrSelector

  ptrDeclSet <- callocBytes #{size c_css_declaration_set_t}
  pokeCssDeclarationSet ptrDeclSet (declarationSet rule)
  pokeByteOff ptrStructCssRule #{offset c_css_rule_t, c_decl_set} ptrDeclSet

  let spec :: CInt = fromIntegral . specificity $ rule
  pokeByteOff ptrStructCssRule #{offset c_css_rule_t, c_specificity} spec

  let pos :: CInt = fromIntegral . position $ rule
  pokeByteOff ptrStructCssRule #{offset c_css_rule_t, c_position} pos

  return ()




{-
-- A version of pokeCssRule that allocates the rule. Keeping it for
-- historical reasons.
pokeCssRule :: CssRule -> IO (Ptr FfiCssRule)
pokeCssRule rule = do

  ptrStructCssRule <- callocBytes #{size c_css_rule_t}

  ptrSelector <- callocBytes #{size c_css_cached_complex_selector_t}
  pokeCssComplexSelector ptrSelector (selector rule)
  pokeByteOff ptrStructCssRule #{offset c_css_rule_t, c_cached_complex_selector} ptrSelector

  ptrDeclSet <- callocBytes #{size c_css_declaration_set_t}
  pokeCssDeclarationSet ptrDeclSet (declarationSet rule)
  pokeByteOff ptrStructCssRule #{offset c_css_rule_t, c_decl_set} ptrDeclSet

  let spec :: CInt = fromIntegral . specificity $ rule
  pokeByteOff ptrStructCssRule #{offset c_css_rule_t, c_specificity} spec

  let pos :: CInt = fromIntegral . position $ rule
  pokeByteOff ptrStructCssRule #{offset c_css_rule_t, c_position} pos

  return ptrStructCssRule
-}



#define RULES_LIST_SIZE 128
typedef struct c_css_rules_list_t {
   c_css_rule_t * c_rules[RULES_LIST_SIZE];
   int c_rules_size;
} c_css_rules_list_t;


/* Hash map: key: string, value: rules list */
   #define RULES_MAP_SIZE 256
typedef struct c_css_rules_map_t {
   char * c_strings[RULES_MAP_SIZE];
   c_css_rules_list_t * c_rules_lists[RULES_MAP_SIZE];
   int c_rules_map_size;
} c_css_rules_map_t;




data FfiCssRulesMap = FfiCssRulesMap {
    stringsC      :: Ptr (Ptr CChar)
  , rulesListC    :: Ptr (Ptr FfiCssRulesList)
  , rulesMapSizeC :: CInt
  } deriving (Show)




instance Storable FfiCssRulesMap where
  sizeOf    _ = #{size c_css_rules_map_t}
  alignment _ = #{alignment c_css_rules_map_t}

  peek ptr = do
    let a = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_rules_map_t, c_strings})     ptr
    let b = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_rules_map_t, c_rules_lists}) ptr
    c <- #{peek c_css_rules_map_t, c_rules_map_size} ptr
    return (FfiCssRulesMap a b c)

  poke ptr (FfiCssRulesMap a b c) = do
    #{poke c_css_rules_map_t, c_strings}        ptr a
    #{poke c_css_rules_map_t, c_rules_lists}    ptr b
    #{poke c_css_rules_map_t, c_rules_map_size} ptr c




peekCssRulesMap :: Ptr FfiCssRulesMap -> IO CssRulesMap
peekCssRulesMap ptrStructRulesMap = do
  ffiRulesMap <- peek ptrStructRulesMap

  let size :: Int = fromIntegral . rulesMapSizeC $ ffiRulesMap
  keys   <- peekArrayOfPointers (stringsC ffiRulesMap) size ptrCCharToText
  values <- peekArrayOfPointers (rulesListC ffiRulesMap) size peekCssRulesList

  let map = M.fromList $ zip keys values

  return map




pokeCssRulesMap :: Ptr FfiCssRulesMap -> CssRulesMap -> IO ()
pokeCssRulesMap ptrStructRulesMap map = do
  ffiRulesMap <- peek ptrStructRulesMap

  let keys = M.keys map
  let values = M.elems map
  let len :: CInt = (fromIntegral . length $ keys)

  pokeArrayOfPointersWithAlloc keys allocAndPokeCString (stringsC ffiRulesMap)
  pokeArrayOfPreallocedPointers values pokeCssRulesList (rulesListC ffiRulesMap)
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




findString :: Ptr (Ptr CChar) -> Int -> T.Text -> IO Int
findString array size string = do
  list <- peekArrayOfPointers array size ptrCCharToText
  case elemIndex string list of
    Just i  -> return i
    Nothing -> return (-1)




hll_printCssDeclarationSet :: Ptr FfiCssDeclarationSet -> IO ()
hll_printCssDeclarationSet ptrStructCssDeclarationSet = do
  declSet:: CssDeclarationSet <- peekCssDeclarationSet ptrStructCssDeclarationSet
  putStrLn . show $ declSet
  return ()




hll_printCssIndex :: Ptr CInt -> IO ()
hll_printCssIndex cPtrIndexArray = do
    index <- fmap (map fromIntegral) (peekArray 32 cPtrIndexArray)
    let v = V.fromList index
    putStrLn . show $ v
    return ()


-}

