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




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Ffi.Css.Doctree
  (
  )
where



{-
import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.Map as M
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E


--import Debug.Trace

import Hello.Css.StyleEngine
import Hello.Css.StyleEngineGlobal
import Hello.Html.Doctree
import Hello.Html.DoctreeNode
-}



-- #include "../../hello.h"




--foreign export ccall "ffiDoctreeCtor" ffiDoctreeCtor :: IO CInt
--foreign export ccall "ffiDoctreeUpdate" ffiDoctreeUpdate :: CInt -> CInt -> IO ()



{-
data FfiDoctree = FfiDoctree {
    topNodeNumC :: CInt
  , rootNode2C  :: Ptr FfiDoctreeNode
  , numNodesC   :: CInt -- nodes counter
  , nodesArrayC :: Ptr (Ptr FfiDoctreeNode)
  } deriving (Show)




instance Storable FfiDoctree where
  sizeOf    _ = #{size c_doctree_t}
  alignment _ = #{alignment c_doctree_t}

  poke ptr (FfiDoctree a b c d) = do
    #{poke c_doctree_t, c_top_node_num} ptr a
    #{poke c_doctree_t, c_root_node}    ptr b
    #{poke c_doctree_t, c_num_nodes}    ptr c
    #{poke c_doctree_t, c_nodes_array}  ptr d -- TODO: array should be set in different way

  peek ptr = do
    a <- #{peek c_doctree_t, c_top_node_num} ptr
    b <- #{peek c_doctree_t, c_root_node}    ptr
    c <- #{peek c_doctree_t, c_num_nodes}    ptr
    let d = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_doctree_t, c_nodes_array}) ptr
    return (FfiDoctree a b c d)




peekDoctree :: Ptr FfiDoctree -> IO Doctree
peekDoctree ptrStructDoctree = do

  ffiDoctree  <- peek ptrStructDoctree
  ffiRootNode <- peek . rootNode2C $ ffiDoctree

  let numNodes = fromIntegral . numNodesC $ ffiDoctree
  let array :: Ptr (Ptr FfiDoctreeNode) = nodesArrayC ffiDoctree
  list :: [DoctreeNode] <- peekArrayOfPointers array numNodes peekDoctreeNode
  let keyValueList = fmap (\x -> (uniqueNum x, x)) list

  return Doctree { topNodeNum  = fromIntegral . topNodeNumC $ ffiDoctree
                 , rootNode    = fromIntegral . uniqueNumC $ ffiRootNode
                 , root        = defaultDoctreeNode
                 , nodes       = M.fromList keyValueList
                 }




pokeDoctreeNode :: Doctree -> Ptr FfiDoctree -> IO (Ptr FfiDoctree)
pokeDoctreeNode dtn ptrDoctreeNodeRoot = do
  ptrStructDoctreeNode <- callocBytes #{size c_doctree_node_t}
  pokeByteOff ptrStructDoctreeNode #{offset c_doctree_node_t, c_root_node} ptrDoctreeNodeRoot

  return ptrStructDoctreeNode




ffiDoctreePrint :: Ptr FfiDoctree -> IO ()
ffiDoctreePrint ptrStructDoctree = do
  doctree <- peekDoctree ptrStructDoctree
  putStr ("ffiDoctreePrint: " ++ show doctree ++ "\n")




ffiDoctreeCtor :: IO CInt
ffiDoctreeCtor = fmap fromIntegral globalDoctreeCtor




ffiDoctreeUpdate :: CInt -> CInt -> IO ()
ffiDoctreeUpdate cRef cSomeVal = do
  let ref     = fromIntegral cRef
  let someVal = fromIntegral cSomeVal

  oldDoctree <- globalDoctreeGet ref
  let newDoctree = oldDoctree { rootNode = rootNode oldDoctree + someVal }
  globalDoctreeUpdate ref newDoctree
-}





{-
doctreeGetTopNode :: Int -> IO (Maybe DoctreeNode)
doctreeGetTopNode ref = do
  doctree <- globalDoctreeGet ref
  if topNodeNum doctree == (-1)
    then
    do
      return Nothing
    else
    do
      return $ Just (nodes doctree M.! topNodeNum doctree)




updateTopNodeInTrees :: Int -> (DoctreeNode -> DoctreeNode) -> IO ()
updateTopNodeInTrees ref f = do
    doctree <- globalDoctreeGet ref
    let doctree' = adjustTopNode doctree f
    globalDoctreeUpdate ref doctree'
-}



