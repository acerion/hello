{-
Copyright (C) 2021-2022 Kamil Ignacak acerion@wp.pl

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


module Hello.Ffi.Css.StyleSheet
  (
  )
where




import Prelude
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

import Hello.Ffi.Css.DoctreeNode
import Hello.Ffi.Css.Doctree
import Hello.Ffi.Css.Parser
import Hello.Ffi.Css.MatchCache
import Hello.Ffi.Css.Misc
import Hello.Ffi.Utils




#include "../../hello.h"





{-
foreign export ccall "hll_cssStyleSheetApplyStyleSheet" hll_cssStyleSheetApplyStyleSheet :: Ptr FfiCssStyleSheet -> Ptr FfiCssDeclarationSet -> CInt -> Ptr FfiDoctreeNode -> Ptr FfiCssMatchCache -> IO ()




/**
 * \brief A list of c_css_rule_t rules.
 *
 * In apply_style_sheet() all matching rules are applied.
 */
typedef struct c_css_style_sheet_t {
   c_css_rules_map_t * c_rules_by_id;
   c_css_rules_map_t * c_rules_by_class;
   c_css_rules_list_t * c_rules_by_type[90 + 14 /* css_style_sheet_n_tags */];
   c_css_rules_list_t * c_rules_by_any_element;
} c_css_style_sheet_t;




data FfiCssStyleSheet = FfiCssStyleSheet {
    cRulesById          :: Ptr FfiCssRulesMap
  , cRulesByClass       :: Ptr FfiCssRulesMap
  , cRulesByType        :: Ptr (Ptr FfiCssRulesList)
  , cRulesByAnyElement  :: Ptr FfiCssRulesList
  }




instance Storable FfiCssStyleSheet where
  sizeOf    _ = #{size c_css_style_sheet_t}
  alignment _ = #{alignment c_css_style_sheet_t}

  peek ptr = do
    a <- #{peek c_css_style_sheet_t, c_rules_by_id}          ptr
    b <- #{peek c_css_style_sheet_t, c_rules_by_class}       ptr
    let c = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_style_sheet_t, c_rules_by_type}) ptr
    d <- #{peek c_css_style_sheet_t, c_rules_by_any_element} ptr
    return (FfiCssStyleSheet a b c d)

  poke ptr (FfiCssStyleSheet a b c d) = do
    #{poke c_css_style_sheet_t, c_rules_by_id}          ptr a
    #{poke c_css_style_sheet_t, c_rules_by_class}       ptr b
    #{poke c_css_style_sheet_t, c_rules_by_type}     ptr c
    #{poke c_css_style_sheet_t, c_rules_by_any_element} ptr d




peekCssStyleSheet :: Ptr FfiCssStyleSheet -> IO CssStyleSheet
peekCssStyleSheet ptrStructCssStyleSheet = do
  ffiStyleSheet <- peek ptrStructCssStyleSheet

  byId    <- peekCssRulesMap (cRulesById ffiStyleSheet)
  byClass <- peekCssRulesMap (cRulesByClass ffiStyleSheet)

  let elementCount = styleSheetElementCount
  let ptrStructElementRulesList :: Ptr (Ptr FfiCssRulesList) = cRulesByType ffiStyleSheet
  byType :: [[CssRule]] <- peekArrayOfPointers ptrStructElementRulesList elementCount peekCssRulesList

  byAnyElement <- peekCssRulesList (cRulesByAnyElement ffiStyleSheet)


  return CssStyleSheet{ rulesById          = byId
                      , rulesByClass       = byClass
                      , rulesByType        = byType
                      , rulesByAnyElement  = byAnyElement
                      }




pokeStyleSheet :: Ptr FfiCssStyleSheet -> CssStyleSheet -> IO ()
pokeStyleSheet ptrStructStyleSheet sheet = do

  ffiStyleSheet <- peek ptrStructStyleSheet

  pokeCssRulesMap (cRulesById ffiStyleSheet) (rulesById sheet)
  pokeCssRulesMap (cRulesByClass ffiStyleSheet) (rulesByClass sheet)
  pokeArrayOfPreallocedPointers (rulesByType sheet) pokeCssRulesList (cRulesByType ffiStyleSheet)

  pokeCssRulesList (cRulesByAnyElement ffiStyleSheet) (rulesByAnyElement sheet)

  return ()




-- Apply a stylesheet to a list of declarations.
--
-- The declarations (list property+value) are set as defined by the rules in
-- the stylesheet that match at the given node in the document tree.
hll_cssStyleSheetApplyStyleSheet :: Ptr FfiCssStyleSheet -> Ptr FfiCssDeclarationSet -> CInt -> Ptr FfiDoctreeNode -> Ptr FfiCssMatchCache -> IO ()
hll_cssStyleSheetApplyStyleSheet ptrStructCssStyleSheet ptrStructTarget cDoctreeRef ptrStructDtn ptrStructMatchCache = do

  dtn           <- peekDoctreeNode ptrStructDtn
  styleSheet    <- peekCssStyleSheet ptrStructCssStyleSheet
  let doctreeRef = fromIntegral cDoctreeRef
  doctree       <- globalDoctreeGet doctreeRef
  matchCache    <- peekPtrCssMatchCache ptrStructMatchCache
  targetDeclSet <- peekCssDeclarationSet ptrStructTarget

  (targetDeclSet', matchCache') <- cssStyleSheetApplyStyleSheet styleSheet targetDeclSet matchCache doctree dtn

  pokeCssDeclarationSet ptrStructTarget targetDeclSet'
  pokeCssMatchCache ptrStructMatchCache matchCache'

-}
