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




module Hello.Ffi.Css.DoctreeNode
  (
    FfiDoctreeNode (..)
  , ffi_dtnGetParent
  , ffi_dtnGetSibling
  , ffi_dtnGetLastChild
  , peekDoctreeNode
  )
where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.Text as T
import Debug.Trace

import Hello.Ffi.Utils
import Hello.Css.DoctreeNode




#include "../../hello.h"




foreign export ccall "hll_getDtnParent" hll_getDtnParent :: Ptr FfiDoctreeNode -> IO (Ptr FfiDoctreeNode)
foreign export ccall "hll_getDtnSibling" hll_getDtnSibling :: Ptr FfiDoctreeNode -> IO (Ptr FfiDoctreeNode)




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
  , rootNodeC  :: Ptr FfiDoctreeNode
  } deriving (Show)




instance Storable FfiDoctreeNode where
  sizeOf    _ = #{size c_doctree_node_t}
  alignment _ = #{alignment c_doctree_node_t}

  poke ptr (FfiDoctreeNode a b c d e f g h i j) = do
    #{poke c_doctree_node_t, c_unique_num}                    ptr a
    #{poke c_doctree_node_t, c_html_element_idx}              ptr b
    #{poke c_doctree_node_t, c_element_selector_pseudo_class} ptr c
    #{poke c_doctree_node_t, c_element_selector_id}           ptr d
    #{poke c_doctree_node_t, c_element_selector_class}        ptr e
    #{poke c_doctree_node_t, c_element_selector_class_size}   ptr f
    #{poke c_doctree_node_t, c_parent}                        ptr g
    #{poke c_doctree_node_t, c_sibling}                       ptr h
    #{poke c_doctree_node_t, c_last_child}                    ptr i
    #{poke c_doctree_node_t, c_root_node}                     ptr j

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
    j <- #{peek c_doctree_node_t, c_root_node}                     ptr
    return (FfiDoctreeNode a b c d e f g h i j)




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

                    , dtnParent    = case ptrToIntPtr . parentC $ ffiDtn of
                                       IntPtr i -> i
                    , dtnSibling   = case ptrToIntPtr . siblingC $ ffiDtn of
                                       IntPtr i -> i
                    , dtnLastChild = case ptrToIntPtr . lastChildC $ ffiDtn of
                                       IntPtr i -> i
                    , dtnRootNode = case ptrToIntPtr . rootNodeC $ ffiDtn of
                                      IntPtr i -> i
                    }




ffi_dtnGetParent :: DoctreeNode -> IO DoctreeNode
ffi_dtnGetParent dtn = do
  if dtnParent dtn == 0
    then return defaultDoctreeNode
    else peekDoctreeNode (intPtrToPtr (IntPtr $ dtnParent dtn))




ffi_dtnGetSibling :: DoctreeNode -> IO DoctreeNode
ffi_dtnGetSibling dtn = do
  if dtnSibling dtn == 0
    then return defaultDoctreeNode
    else peekDoctreeNode (intPtrToPtr (IntPtr $ dtnSibling dtn))




ffi_dtnGetLastChild :: DoctreeNode -> IO DoctreeNode
ffi_dtnGetLastChild dtn = do
  if dtnLastChild dtn == 0
    then return defaultDoctreeNode
    else peekDoctreeNode (intPtrToPtr (IntPtr $ dtnLastChild dtn))




hll_getDtnParent :: Ptr FfiDoctreeNode -> IO (Ptr FfiDoctreeNode)
hll_getDtnParent ptrStructDtn = do
  dtn <- peekDoctreeNode ptrStructDtn
  if dtnParent dtn /= dtnRootNode dtn
    then return (intPtrToPtr (IntPtr $ dtnParent dtn))
    else return nullPtr



hll_getDtnSibling :: Ptr FfiDoctreeNode -> IO (Ptr FfiDoctreeNode)
hll_getDtnSibling ptrStructDtn = do
  dtn <- peekDoctreeNode ptrStructDtn
  return (intPtrToPtr (IntPtr $ dtnSibling dtn))



{-

      inline c_doctree_node_t * getDtnParent(const c_doctree_node_t * dtn) {
         if (dtn->c_parent != rootNode)
            return dtn->c_parent;
         else
            return NULL;
      };

      inline c_doctree_node_t * getDtnSibling(const c_doctree_node_t * dtn) {
         return dtn->c_sibling;
      };
-}
