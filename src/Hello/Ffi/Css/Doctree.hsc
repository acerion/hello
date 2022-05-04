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
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E


import Debug.Trace

import Data.IORef
import System.IO.Unsafe

import Hello.Html.Doctree
import Hello.Html.DoctreeNode
import Hello.Utils

import Hello.Ffi.Utils
import Hello.Ffi.Css.DoctreeNode




#include "../../hello.h"




foreign export ccall "hll_doctreePrint" hll_doctreePrint :: Ptr FfiDoctree -> IO ()
foreign export ccall "hll_doctreeModifyPair" hll_doctreeModifyPair :: CInt -> CInt -> IO ()


foreign export ccall "hll_doctreeCtor" hll_doctreeCtor :: IO (CInt)
foreign export ccall "hll_doctreeUpdate" hll_doctreeUpdate :: CInt -> CInt -> IO ()
foreign export ccall "hll_doctreePushNode" hll_doctreePushNode :: CInt -> CInt -> IO CInt
foreign export ccall "hll_doctreePopNode" hll_doctreePopNode :: CInt -> IO ()
foreign export ccall "hll_doctreeGetTopNodeElementSelectorId" hll_doctreeGetTopNodeElementSelectorId :: CInt -> IO CString

foreign export ccall "hll_styleEngineSetElementId" hll_styleEngineSetElementId :: CInt -> CString -> IO ()
foreign export ccall "hll_styleEngineSetElementClass" hll_styleEngineSetElementClass :: CInt -> CString -> IO ()
foreign export ccall "hll_styleEngineSetElementPseudoClass" hll_styleEngineSetElementPseudoClass :: CInt -> CString -> IO ()




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
  let doctree2 = doctreePushNode doctree elementIdx
  let new = listReplaceElem old doctree2 ref

  writeIORef myGlobalDoctrees new
  putStr ("::::::::::: After pushing " ++ (show elementIdx) ++ ": " ++ (show doctree2) ++ "\n")

  return $ fromIntegral (topNodeNum doctree2)




hll_doctreePopNode :: CInt -> IO ()
hll_doctreePopNode cRef = do
  let ref        = fromIntegral cRef

  old <- readIORef myGlobalDoctrees
  let doctree = old !! ref
  let doctree2 = doctreePopNode doctree
  let new = listReplaceElem old doctree2 ref

  writeIORef myGlobalDoctrees new
  putStr ("::::::::::: After popping: " ++ (show doctree2) ++ "\n")




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






hll_styleEngineSetElementId :: CInt -> CString -> IO ()
hll_styleEngineSetElementId cRef cElementId = do
    let ref       = fromIntegral cRef
    stringVal <- BSU.unsafePackCString $ cElementId
    let elementId  = T.E.decodeLatin1 stringVal

    old <- readIORef myGlobalDoctrees
    let doctree = old !! ref
    let dtn = (nodes doctree) M.! (topNodeNum doctree)
    let dtn2 = dtn { selId = elementId }
    let new = listReplaceElem old doctree { nodes = M.insert (uniqueNum dtn2) dtn2 (nodes doctree) } ref

    writeIORef myGlobalDoctrees new





hll_styleEngineSetElementClass :: CInt -> CString -> IO ()
hll_styleEngineSetElementClass cRef cElementClassTokens = do
    let ref = fromIntegral cRef
    tokens  <- BSU.unsafePackCString $ cElementClassTokens

    -- With ' ' character as separator of selectors, we can use 'words' to
    -- get the list of selectors.
    let ws = words . Char8.unpack $ tokens
    let classSelectors = fmap T.pack ws

    old <- readIORef myGlobalDoctrees
    let doctree = old !! ref
    let dtn = (nodes doctree) M.! (topNodeNum doctree)
    let dtn2 = dtn { selClass = classSelectors }
    let new = listReplaceElem old doctree { nodes = M.insert (uniqueNum dtn2) dtn2 (nodes doctree) } ref

    writeIORef myGlobalDoctrees new





hll_styleEngineSetElementPseudoClass :: CInt -> CString -> IO ()
hll_styleEngineSetElementPseudoClass cRef cElementPseudoClass = do
    let ref       = fromIntegral cRef
    stringVal <- BSU.unsafePackCString $ cElementPseudoClass
    let elementPseudoClass  = T.E.decodeLatin1 stringVal

    old <- readIORef myGlobalDoctrees
    let doctree = old !! ref
    let dtn = (nodes doctree) M.! (topNodeNum doctree)
    let dtn2 = dtn { selPseudoClass = elementPseudoClass }
    let new = listReplaceElem old doctree { nodes = M.insert (uniqueNum dtn2) dtn2 (nodes doctree) } ref

    writeIORef myGlobalDoctrees new

