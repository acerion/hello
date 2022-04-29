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

import Data.IORef
import System.IO.Unsafe

import Hello.Ffi.Utils
import Hello.Ffi.Css.DoctreeNode
import Hello.Css.DoctreeNode
import Hello.Utils




#include "../../hello.h"




foreign export ccall "hll_doctreePrint" hll_doctreePrint :: Ptr FfiDoctree -> IO ()
foreign export ccall "hll_doctreeModifyPair" hll_doctreeModifyPair :: CInt -> CInt -> IO ()


foreign export ccall "hll_doctreeCtor" hll_doctreeCtor :: IO (CInt)
foreign export ccall "hll_doctreeUpdate" hll_doctreeUpdate :: CInt -> CInt -> IO ()
foreign export ccall "hll_doctreePushNode" hll_doctreePushNode :: CInt -> CInt -> IO CInt
foreign export ccall "hll_doctreePopNode" hll_doctreePopNode :: CInt -> IO ()
foreign export ccall "hll_doctreeGetTopNodeElementSelectorId" hll_doctreeGetTopNodeElementSelectorId :: CInt -> IO CString



myGlobalPair :: IORef (Bool, Int)
{-# NOINLINE myGlobalPair #-}
myGlobalPair = unsafePerformIO (newIORef (True, 2))



myGlobalDoctrees :: IORef [Doctree]
{-# NOINLINE myGlobalDoctrees #-}
myGlobalDoctrees = unsafePerformIO (newIORef [])





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

  let numNodes = (fromIntegral . numNodesC $ ffiDoctree)
  let array :: Ptr (Ptr FfiDoctreeNode) = nodesArrayC ffiDoctree
  list :: [DoctreeNode] <- peekArrayOfPointers array numNodes peekDoctreeNode
  let keyValueList = fmap (\x -> (uniqueNum x, x)) list

  return Doctree { topNodeNum  = fromIntegral . topNodeNumC $ ffiDoctree
                 , rootNode    = fromIntegral . uniqueNumC $ ffiRootNode
                 , root        = defaultDoctreeNode
                 , nodes       = M.fromList keyValueList
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
  putStr ("hll_doctreePrint: " ++ (show doctree) ++ "\n")






hll_doctreeModifyPair :: CInt -> CInt -> IO ()
hll_doctreeModifyPair cRef cIncrement = do
  let increment = fromIntegral cIncrement
  old <- readIORef myGlobalPair
  let new = (not . fst $ old, increment + snd old)
  writeIORef myGlobalPair new
  putStr ("::::::::::: global pair" ++ (show new) ++ "\n")




hll_doctreeCtor :: IO (CInt)
hll_doctreeCtor = do
  old <- readIORef myGlobalDoctrees
  let new = old ++ [ defaultDoctree ]
  writeIORef myGlobalDoctrees new
  putStr ("::::::::::: ctor of new doctree updated the list to: " ++ (show new) ++ "\n")
  return $ fromIntegral ((length new) - 1)




hll_doctreeUpdate :: CInt -> CInt -> IO ()
hll_doctreeUpdate cRef cSomeVal = do
  let ref     = fromIntegral cRef
  let someVal = fromIntegral cSomeVal

  old <- readIORef myGlobalDoctrees
  let single = old !! ref
  let new = listReplaceElem old single { rootNode = (rootNode single) + someVal } ref

  writeIORef myGlobalDoctrees new
  putStr ("::::::::::: After update of " ++ (show ref) ++ " with " ++ (show someVal) ++ ": " ++ (show new) ++ "\n")






hll_doctreePushNode :: CInt -> CInt -> IO CInt
hll_doctreePushNode cRef cElementIdx = do
  let ref        = fromIntegral cRef
  let elementIdx = fromIntegral cElementIdx

  old <- readIORef myGlobalDoctrees
  let doctree = old !! ref
  (doctree2, dtn) <- doctreePushNode doctree elementIdx
  let new = listReplaceElem old doctree2 ref

  writeIORef myGlobalDoctrees new
  putStr ("::::::::::: After pushing " ++ (show elementIdx) ++ ": " ++ (show doctree2) ++ "\n")

  return $ fromIntegral (uniqueNum dtn)




doctreePushNode :: Doctree -> Int -> IO (Doctree, DoctreeNode)
doctreePushNode doctree elementIdx = do

  putStrLn ("---- push " ++ (show elementIdx) ++ "\n")

  let currentNodeNum = M.size . nodes $ doctree

  let dtn = makeNewDtn elementIdx currentNodeNum
  (doctree2, dtn2) <- setRelations doctree dtn

  -- Insert node, update reference to top node.
  let doctree3 = doctree2 { topNodeNum = uniqueNum dtn2
                          , nodes      = M.insert currentNodeNum dtn2 (nodes doctree2)
                          }

  return (doctree3, dtn2)


{-
Set properties.
dtn->c_this_ptr = dtn;
dtn->c_unique_num = this_num;
dtn->c_html_element_idx = element_idx;
-}
makeNewDtn elementIdx num = defaultDoctreeNode { uniqueNum = num
                                               , htmlElementIdx = elementIdx
                                               }



{-
   /* Set relations. */
   c_doctree_node_t * parent = NULL;
   if (doctree->c_top_node_num == ROOT_NODE_NUM) {
      /* This is a first real element in html document, placed in the tree
         under a root element. */
      dtn->c_parent_num = ROOT_NODE_NUM;
      parent = doctree->c_root_node;
      dtn->c_sibling_num = parent->c_last_child_num;
      parent->c_last_child_num = dtn->c_unique_num;

   } else {
      /* This is an n-th element in html document, placed in the tree under
         some tree node. */
      dtn->c_parent_num = doctree->c_top_node_num;
      parent = doctree->c_nodes_array[dtn->c_parent_num];
      dtn->c_sibling_num = parent->c_last_child_num;
      parent->c_last_child_num = dtn->c_unique_num;
   }
-}
setRelations doctree dtn = if topNodeNum doctree == (-1)
                           then
                             do
                               let dtn3 = dtn { dtnParentNum = (-1) }
                               let parent = root doctree
                               let dtn4 = dtn3 { dtnSiblingNum = dtnLastChildNum parent }
                               let parent2 = parent { dtnLastChildNum = uniqueNum dtn4 }
                               let tree = doctree { root = parent2 }
                               return (tree, dtn4)
                           else
                             do
                               let dtn3 = dtn { dtnParentNum = topNodeNum doctree }
                               let parent = (nodes doctree) M.! (dtnParentNum dtn3)
                               let dtn4 = dtn3 { dtnSiblingNum = dtnLastChildNum parent }
                               let parent2 = parent { dtnLastChildNum = uniqueNum dtn4 }
                               let tree = doctree { nodes = M.insert (uniqueNum parent2) parent2 (nodes doctree) }
                               return (tree, dtn4)



hll_doctreePopNode :: CInt -> IO ()
hll_doctreePopNode cRef = do
  let ref        = fromIntegral cRef

  old <- readIORef myGlobalDoctrees
  let doctree = old !! ref
  let doctree2 = doctreePopNode doctree
  let new = listReplaceElem old doctree2 ref

  writeIORef myGlobalDoctrees new
  putStr ("::::::::::: After popping: " ++ (show doctree2) ++ "\n")






doctreePopNode :: Doctree -> Doctree
doctreePopNode doctree = if uniqueNum dtn == 0 -- We are popping the element of html document that was added to the tree as the first one. What should now remain on top of the doctree is a tree's root element.
                         then doctree { topNodeNum = (-1) }
                         else doctree { topNodeNum = dtnParentNum dtn }

  where
    -- c_doctree_node_t * dtn = doctree->c_nodes_array[doctree->c_top_node_num];
    dtn = (nodes doctree) M.! (topNodeNum doctree)

{-
   if (0 == dtn->c_unique_num) {

      doctree->c_top_node_num = ROOT_NODE_NUM;
   } else {
      doctree->c_top_node_num = dtn->c_parent_num;
   }

-}




hll_doctreeGetTopNode :: CInt -> IO CInt
hll_doctreeGetTopNode cRef = do
    let ref        = fromIntegral cRef

    old <- readIORef myGlobalDoctrees
    let doctree = old !! ref
    if topNodeNum doctree /= (-1)
      then return 1
      else return 0
      {-

c_doctree_node_t * doctreeGetTopNode(c_doctree_t * doctree)
{
   if (doctree->c_top_node_num != ROOT_NODE_NUM)
      return doctree->c_nodes_array[doctree->c_top_node_num];
   else
      return NULL;
}


-}




hll_doctreeGetTopNodeHtmlElementIdx :: CInt -> IO CInt
hll_doctreeGetTopNodeHtmlElementIdx cRef = do
    let ref        = fromIntegral cRef

    old <- readIORef myGlobalDoctrees
    let doctree = old !! ref
    if topNodeNum doctree == (-1)
      then
      do
        return 0
      else
      do
        let dtn = (nodes doctree) M.! (topNodeNum doctree)
        return $ fromIntegral (htmlElementIdx dtn)




hll_doctreeGetTopNodeElementSelectorId :: CInt -> IO CString
hll_doctreeGetTopNodeElementSelectorId cRef = do
    let ref        = fromIntegral cRef

    old <- readIORef myGlobalDoctrees
    let doctree = old !! ref
    if topNodeNum doctree == (-1)
      then
      do
        return nullPtr
      else
      do
        let dtn = (nodes doctree) M.! (topNodeNum doctree)
        newCString . T.unpack . selId $ dtn





