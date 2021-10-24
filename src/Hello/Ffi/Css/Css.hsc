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


module Hello.Ffi.Css.Misc( peekCssContext
                         , pokeCssContext
                         )
  where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector as V
import qualified Data.List as L
import Control.Monad -- when
import Debug.Trace

import Hello.Css.Parser
import Css
import Hello.Css.StyleSheet
import Hello.Css.Selector
import Hello.Css.DoctreeNode
import Hello.Css.Match
import Hello.Ffi.Css.Parser
import Hello.Ffi.Utils
import Hello.Ffi.Css.DoctreeNode




#include "../../hello.h"




foreign export ccall "hll_cssComplexSelectorMatches" hll_cssComplexSelectorMatches :: Ptr FfiCssComplexSelector -> Ptr FfiDoctreeNode -> Ptr FfiCssMatchCache -> IO Bool
foreign export ccall "hll_selectorSpecificity" hll_selectorSpecificity :: Ptr FfiCssComplexSelector -> IO Int
foreign export ccall "hll_rulesMapGetList" hll_rulesMapGetList :: Ptr FfiCssRulesMap -> CString -> IO (Ptr FfiCssRulesList)
foreign export ccall "hll_matchCacheSetSize" hll_matchCacheSetSize :: Ptr FfiCssMatchCache -> CInt -> IO ()
foreign export ccall "hll_cssParseRuleset" hll_cssParseRuleset :: Ptr FfiCssParser -> Ptr FfiCssToken -> Ptr FfiCssContext -> IO ()




hll_cssComplexSelectorMatches :: Ptr FfiCssComplexSelector -> Ptr FfiDoctreeNode -> Ptr FfiCssMatchCache -> IO Bool
hll_cssComplexSelectorMatches ptrStructCachedComplexSelector ptrStructDtn ptrStructMatchCache = do

  cachedComplex <- peekCssComplexSelector ptrStructCachedComplexSelector
  mDtn <- ptrToMdtn ptrStructDtn
  tree <- analyzeDtn ptrStructDtn M.empty
  mc <- peekPtrCssMatchCache ptrStructMatchCache

  let (isMatch, outMc) = cssComplexSelectorMatches (chain cachedComplex) mDtn tree mc (matchCacheOffset cachedComplex)
  pokeCssMatchCache ptrStructMatchCache outMc

  return isMatch




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




hll_selectorSpecificity :: Ptr FfiCssComplexSelector -> IO Int
hll_selectorSpecificity ptrStructCssComplexSelector = do
  cachedComplex <- peekCssComplexSelector ptrStructCssComplexSelector
  return . selectorSpecificity . chain $ cachedComplex




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




data FfiCssStyleSheet = FfiCssStyleSheet {
    cRulesById          :: Ptr FfiCssRulesMap
  , cRulesByClass       :: Ptr FfiCssRulesMap
  , cRulesByType        :: Ptr (Ptr FfiCssRulesList)
  , cRulesByAnyElement  :: Ptr FfiCssRulesList
  , cRequiredMatchCache :: CInt
  }




instance Storable FfiCssStyleSheet where
  sizeOf    _ = #{size c_css_style_sheet_t}
  alignment _ = #{alignment c_css_style_sheet_t}

  peek ptr = do
    a <- #{peek c_css_style_sheet_t, c_rules_by_id}          ptr
    b <- #{peek c_css_style_sheet_t, c_rules_by_class}       ptr
    let c = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_style_sheet_t, c_rules_by_type}) ptr
    d <- #{peek c_css_style_sheet_t, c_rules_by_any_element} ptr
    e <- #{peek c_css_style_sheet_t, c_required_match_cache} ptr
    return (FfiCssStyleSheet a b c d e)

  poke ptr (FfiCssStyleSheet a b c d e) = do
    #{poke c_css_style_sheet_t, c_rules_by_id}          ptr a
    #{poke c_css_style_sheet_t, c_rules_by_class}       ptr b
    #{poke c_css_style_sheet_t, c_rules_by_type}     ptr c
    #{poke c_css_style_sheet_t, c_rules_by_any_element} ptr d
    #{poke c_css_style_sheet_t, c_required_match_cache} ptr e




peekCssStyleSheet :: Ptr FfiCssStyleSheet -> IO CssStyleSheet
peekCssStyleSheet ptrStructCssStyleSheet = do
  ffiStyleSheet <- peek ptrStructCssStyleSheet

  byId    <- peekCssRulesMap (cRulesById ffiStyleSheet)
  byClass <- peekCssRulesMap (cRulesByClass ffiStyleSheet)

  let elementCount = styleSheetElementCount
  let ptrStructElementRulesList :: Ptr (Ptr FfiCssRulesList) = cRulesByType ffiStyleSheet
  byType :: [[CssRule]] <- peekArrayOfPointers ptrStructElementRulesList elementCount peekCssRulesList

  byAnyElement <- peekCssRulesList (cRulesByAnyElement ffiStyleSheet)

  let rmc :: Int = fromIntegral . cRequiredMatchCache $ ffiStyleSheet

  return CssStyleSheet{ rulesById          = byId
                      , rulesByClass       = byClass
                      , rulesByType        = byType
                      , rulesByAnyElement  = byAnyElement
                      , requiredMatchCache = rmc
                      }




pokeStyleSheet :: Ptr FfiCssStyleSheet -> CssStyleSheet -> IO ()
pokeStyleSheet ptrStructStyleSheet sheet = do

  ffiStyleSheet <- peek ptrStructStyleSheet

  pokeCssRulesMap (cRulesById ffiStyleSheet) (rulesById sheet)
  pokeCssRulesMap (cRulesByClass ffiStyleSheet) (rulesByClass sheet)
  pokeArrayOfPreallocedPointers (rulesByType sheet) pokeCssRulesList (cRulesByType ffiStyleSheet)

  pokeCssRulesList (cRulesByAnyElement ffiStyleSheet) (rulesByAnyElement sheet)

  let cache :: CInt = fromIntegral . requiredMatchCache $ sheet
  pokeByteOff ptrStructStyleSheet (#offset c_css_style_sheet_t, c_required_match_cache) cache

  return ()




findString :: Ptr (Ptr CChar) -> Int -> T.Text -> IO Int
findString array size string = do
  list <- peekArrayOfPointers array size ptrCCharToText
  case elemIndex string list of
    Just i  -> return i
    Nothing -> return (-1)




data FfiCssMatchCache = FfiCssMatchCache {
    cCacheItems     :: Ptr CInt
  , cCacheItemsSize :: CInt
  } deriving (Show)




instance Storable FfiCssMatchCache where
  sizeOf    _ = #{size c_css_match_cache_t}
  alignment _ = #{alignment c_css_match_cache_t}

  peek ptr = do
    let a = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_match_cache_t, c_cache_items}) ptr
    b <- #{peek c_css_match_cache_t, c_cache_items_size} ptr
    return (FfiCssMatchCache a b)

  poke ptr (FfiCssMatchCache a b) = do
    -- #{poke c_css_match_cache_t, c_cache_items}      ptr a
    #{poke c_css_match_cache_t, c_cache_items_size} ptr b




peekPtrCssMatchCache :: Ptr FfiCssMatchCache -> IO CssMatchCache
peekPtrCssMatchCache ptrStructMatchCache = do
  ffiMatchCache <- peek ptrStructMatchCache

  let array :: Ptr CInt = cCacheItems ffiMatchCache
  let size  :: Int      = fromIntegral . cCacheItemsSize $ ffiMatchCache

  cCache :: [CInt] <- peekArray size array
  let cache = map fromIntegral cCache

  return cache




pokeCssMatchCache :: Ptr FfiCssMatchCache -> CssMatchCache -> IO ()
pokeCssMatchCache ptrStructMatchCache cache = do
  ffiMatchCache <- peek ptrStructMatchCache

  let array :: Ptr CInt = cCacheItems ffiMatchCache
  let cCache :: [CInt] = fmap fromIntegral cache
  let cLen :: CInt = fromIntegral . length $ cache
  pokeArray array cCache

  poke ptrStructMatchCache $ FfiCssMatchCache array cLen
  --pokeByteOff ptrStructMatchCache #{offset c_css_match_cache_t, c_cache_items_size} cLen




-- TODO: This function seems to just allocate a new vector of (-1) elements.
-- So far I haven't seen this function update a vector of some non-(-1)
-- elements by adding (-1)s at the end.
hll_matchCacheSetSize :: Ptr FfiCssMatchCache -> CInt -> IO ()
hll_matchCacheSetSize ptrStructMatchCache cNewSize = do
  oldMatchCache <- peekPtrCssMatchCache ptrStructMatchCache
  let newSize = fromIntegral cNewSize
  let newMatchCache = matchCacheResize oldMatchCache newSize
  pokeCssMatchCache ptrStructMatchCache newMatchCache

  return ()




data FfiCssContext = FfiCssContext {
    cSheets              :: Ptr (Ptr FfiCssStyleSheet)
  , cStructPtrMatchCache :: Ptr FfiCssMatchCache
  , cRulePosition        :: CInt
  } deriving (Show)




instance Storable FfiCssContext where
  sizeOf    _ = #{size c_css_context_t}
  alignment _ = #{alignment c_css_context_t}

  peek ptr = do
    let a = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_context_t, c_sheets}) ptr
    b <- #{peek c_css_context_t, c_match_cache}   ptr
    c <- #{peek c_css_context_t, c_rule_position} ptr
    return (FfiCssContext a b c)

  poke ptr (FfiCssContext a b c) = do
    #{poke c_css_context_t, c_sheets}        ptr a
    #{poke c_css_context_t, c_match_cache}   ptr b
    #{poke c_css_context_t, c_rule_position} ptr c




peekCssContext :: Ptr FfiCssContext -> IO CssContext
peekCssContext ptrStructContext = do
  ffiContext <- peek ptrStructContext

  s :: [CssStyleSheet] <- peekArrayOfPointers (cSheets ffiContext) 5 peekCssStyleSheet
  cache <- peekPtrCssMatchCache . cStructPtrMatchCache $ ffiContext

  return CssContext{ sheets       = s
                   , matchCache   = cache
                   , rulePosition = fromIntegral . cRulePosition $ ffiContext
                   }




pokeCssContext :: Ptr FfiCssContext -> CssContext -> IO ()
pokeCssContext ptrStructContext context = do
  ffiContext <- peek ptrStructContext

  let array :: Ptr (Ptr FfiCssStyleSheet) = cSheets ffiContext
  pokeArrayOfPreallocedPointers (sheets context) pokeStyleSheet array

  pokeCssMatchCache (cStructPtrMatchCache ffiContext) (matchCache context)

  let pos :: CInt = fromIntegral . rulePosition $ context
  pokeByteOff ptrStructContext #{offset c_css_context_t, c_rule_position} pos




hll_cssParseRuleset :: Ptr FfiCssParser -> Ptr FfiCssToken -> Ptr FfiCssContext -> IO ()
hll_cssParseRuleset ptrStructCssParser ptrStructCssToken ptrStructCssContext = do
  parser  <- peekCssParser ptrStructCssParser
  token   <- peekCssToken ptrStructCssToken
  context <- peekCssContext ptrStructCssContext

  let (p2, t2, c2) = parseRuleset parser token context

  pokeCssParser ptrStructCssParser p2
  pokeCssToken ptrStructCssToken t2
  pokeCssContext ptrStructCssContext c2

  return ()





