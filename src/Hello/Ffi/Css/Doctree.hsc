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




module Hello.Ffi.Css.Doctree
  (
    FfiDoctree (..)
  , peekDoctree
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
import Hello.Ffi.Css.DoctreeNode
import Hello.Css.DoctreeNode




#include "../../hello.h"




foreign export ccall "hll_doctreePrint" hll_doctreePrint :: Ptr FfiDoctree -> IO ()




data FfiDoctree = FfiDoctree {
    topNodeC    :: Ptr FfiDoctreeNode
  , rootNode2C  :: Ptr FfiDoctreeNode
  , numNodesC   :: CInt -- nodes counter
  , nodesArrayC :: Ptr (Ptr FfiDoctreeNode)
  } deriving (Show)




instance Storable FfiDoctree where
  sizeOf    _ = #{size c_doctree_t}
  alignment _ = #{alignment c_doctree_t}

  poke ptr (FfiDoctree a b c d) = do
    #{poke c_doctree_t, c_top_node}     ptr a
    #{poke c_doctree_t, c_root_node}    ptr b
    #{poke c_doctree_t, c_num_nodes}    ptr c
    #{poke c_doctree_t, c_nodes_array}  ptr d

  peek ptr = do
    a <- #{peek c_doctree_t, c_top_node}   ptr
    b <- #{peek c_doctree_t, c_root_node}  ptr
    c <- #{peek c_doctree_t, c_num_nodes}  ptr
    let d = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_doctree_t, c_nodes_array}) ptr
    return (FfiDoctree a b c d)




peekDoctree :: Ptr FfiDoctree -> IO Doctree
peekDoctree ptrStructDoctree = do

  ffiDoctree  <- peek ptrStructDoctree
  ffiTopNode  <- peek . topNodeC $ ffiDoctree
  ffiRootNode <- peek . rootNode2C $ ffiDoctree

  let numNodes = (fromIntegral . numNodesC $ ffiDoctree)
  let array :: Ptr (Ptr FfiDoctreeNode) = nodesArrayC ffiDoctree
  list :: [DoctreeNode] <- peekArrayOfPointers array numNodes peekDoctreeNode
  let keyValueList = fmap (\x -> (thisPtr x, x)) list

  return Doctree { topNode  = fromIntegral . uniqueNumC $ ffiTopNode
                 , rootNode = fromIntegral . uniqueNumC $ ffiRootNode
                 , nodes    = M.fromList keyValueList
                 }



{-
pokeDoctreeNode :: Doctree -> Ptr FfiDoctree -> IO (Ptr FfiDoctree)
pokeDoctreeNode dtn ptrDoctreeNodeRoot = do
  ptrStructDoctreeNode <- callocBytes #{size c_doctree_node_t}
  pokeByteOff ptrStructDoctreeNode #{offset c_doctree_node_t, c_root_node} ptrDoctreeNodeRoot

  return ptrStructDoctreeNode
-}



hll_doctreePrint :: Ptr FfiDoctree -> IO ()
hll_doctreePrint ptrStructDoctree = do
  doctree <- peekDoctree ptrStructDoctree
  putStr (show doctree)


