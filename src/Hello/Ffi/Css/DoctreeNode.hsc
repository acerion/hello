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
  , peekDoctreeNode
  )
where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.Text as T
import qualified Data.Map as M
import Debug.Trace

import Hello.Ffi.Utils
import Hello.Css.DoctreeNode




#include "../../hello.h"




foreign export ccall "hll_doctreeNodeNew" hll_doctreeNodeNew :: IO (Ptr FfiDoctreeNode)




data FfiDoctreeNode = FfiDoctreeNode {
    uniqueNumC      :: CInt -- unique ascending id
  , htmlElementIdxC :: CInt -- Index to html.cc::Tags

  , elementSelectorPseudoClassC     :: CString
  , elementSelectorIdC              :: CString
  , elementSelectorClassC           :: CString
  , elementSelectorClassSizeC       :: CInt

  , parentNumC    :: CInt
  , siblingNumC   :: CInt
  , lastChildNumC :: CInt
  } deriving (Show)




instance Storable FfiDoctreeNode where
  sizeOf    _ = #{size c_doctree_node_t}
  alignment _ = #{alignment c_doctree_node_t}

  poke ptr (FfiDoctreeNode a b d e f g h i j) = do
    #{poke c_doctree_node_t, c_unique_num}                    ptr a
    #{poke c_doctree_node_t, c_html_element_idx}              ptr b
    #{poke c_doctree_node_t, c_element_selector_pseudo_class} ptr d
    #{poke c_doctree_node_t, c_element_selector_id}           ptr e
    #{poke c_doctree_node_t, c_element_selector_class}        ptr f
    #{poke c_doctree_node_t, c_element_selector_class_size}   ptr g
    #{poke c_doctree_node_t, c_parent_num}                    ptr h
    #{poke c_doctree_node_t, c_sibling_num}                   ptr i
    #{poke c_doctree_node_t, c_last_child_num}                ptr j

  peek ptr = do
    a <- #{peek c_doctree_node_t, c_unique_num}                    ptr
    b <- #{peek c_doctree_node_t, c_html_element_idx}              ptr
    d <- #{peek c_doctree_node_t, c_element_selector_pseudo_class} ptr
    e <- #{peek c_doctree_node_t, c_element_selector_id}           ptr
    f <- #{peek c_doctree_node_t, c_element_selector_class}        ptr
    g <- #{peek c_doctree_node_t, c_element_selector_class_size}   ptr
    h <- #{peek c_doctree_node_t, c_parent_num}                    ptr
    i <- #{peek c_doctree_node_t, c_sibling_num}                   ptr
    j <- #{peek c_doctree_node_t, c_last_child_num}                ptr
    return (FfiDoctreeNode a b d e f g h i j)




peekDoctreeNode :: Ptr FfiDoctreeNode -> IO DoctreeNode
peekDoctreeNode ptrStructDoctreeNode = do

  ffiDtn <- peek ptrStructDoctreeNode
  pc <- ptrCCharToText . elementSelectorPseudoClassC $ ffiDtn
  i  <- ptrCCharToText . elementSelectorIdC $ ffiDtn

  let cOffset = (#offset c_doctree_node_t, c_element_selector_class)
  let cStringArray :: Ptr CString = plusPtr ptrStructDoctreeNode cOffset
  c  <- peekArrayOfPointers cStringArray (fromIntegral . elementSelectorClassSizeC $ ffiDtn) ptrCCharToText

  return DoctreeNode{ uniqueNum      = fromIntegral . uniqueNumC $ ffiDtn
                    , htmlElementIdx = fromIntegral . htmlElementIdxC $ ffiDtn

                    , selPseudoClass = pc
                    , selId          = i
                    , selClass       = c

                    , dtnParentNum    = fromIntegral . parentNumC $ ffiDtn
                    , dtnSiblingNum   = fromIntegral . siblingNumC $ ffiDtn
                    , dtnLastChildNum = fromIntegral . lastChildNumC $ ffiDtn
                    }






{-
pokeDoctreeNode :: DoctreeNode -> Ptr FfiDoctreeNode -> IO (Ptr FfiDoctreeNode)
pokeDoctreeNode dtn ptrDoctreeNodeRoot = do
  ptrStructDoctreeNode <- callocBytes #{size c_doctree_node_t}
  pokeByteOff ptrStructDoctreeNode #{offset c_doctree_node_t, c_root_node} ptrDoctreeNodeRoot

  return ptrStructDoctreeNode
-}



hll_doctreeNodeNew :: IO (Ptr FfiDoctreeNode)
hll_doctreeNodeNew = callocBytes #{size c_doctree_node_t}







{-
doctreeNodePush doctree htmlElementIdx = (doctree2, dtn)
  where
    nodeIdx = M.size . nodes $ doctree

    dtn = defaultDoctreeNode
          { uniqueNum      = nodeIdx
          , htmlElementIdx = htmlElementIdx

          , dtnParentNum  = topNode doctree
          , dtnSiblingNum = dtnLastChildNum top
          , dtnRootNode   = rootNode doctree
      }


    doctree2 = doctree { topNode  = nodeIdx
                       , nodes    = M.insert nodeIdx dtn (nodes doctree)
                       }

    top = case M.lookup (topNode doctree) (nodes doctree) of
            Just dtn -> dtn
            otherwise -> defaultDoctreeNode


hll_doctreeNodePush doctree htmlElementIdx = do
  ptrStructDoctreeNode <- hll_doctreeNodeNew
  let (doctree, dtn) = doctreeNodePush doctree htmlElementIdx

  pokeDoctreeNode dtn ptrStructDoctreeNode
-}
  {-
   dtn->c_parent_num = doctree->c_top_node;
   dtn->c_sibling = dtn->c_parent_num->c_last_child;
   dtn->c_parent_num->c_last_child = dtn;
   dtn->c_unique_num = doctree->c_num_nodes++;
   dtn->c_html_element_idx = element_idx;

   doctree->c_top_node = dtn;

   doctree->nodes_array[dtn->c_unique_num] = dtn;
-}
