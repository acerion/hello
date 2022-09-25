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
  , globalDoctreeGet
  )
where




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

import Hello.Html.DoctreeGlobal
import Hello.Html.Doctree
import Hello.Html.DoctreeNode

import Hello.Ffi.Utils
import Hello.Ffi.Css.DoctreeNode




#include "../../hello.h"




foreign export ccall "hll_doctreePrint" hll_doctreePrint :: Ptr FfiDoctree -> IO ()


foreign export ccall "hll_doctreeCtor" hll_doctreeCtor :: IO (CInt)
foreign export ccall "hll_doctreeUpdate" hll_doctreeUpdate :: CInt -> CInt -> IO ()
foreign export ccall "hll_doctreePushNode" hll_doctreePushNode :: CInt -> CInt -> IO CInt
foreign export ccall "hll_doctreePopNode" hll_doctreePopNode :: CInt -> IO ()
foreign export ccall "hll_doctreeGetTopNodeElementSelectorId" hll_doctreeGetTopNodeElementSelectorId :: CInt -> IO CString

foreign export ccall "hll_styleEngineSetElementId" hll_styleEngineSetElementId :: CInt -> CString -> IO ()
foreign export ccall "hll_styleEngineSetElementClass" hll_styleEngineSetElementClass :: CInt -> CString -> IO ()
foreign export ccall "hll_styleEngineSetElementPseudoClass" hll_styleEngineSetElementPseudoClass :: CInt -> CString -> IO ()




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




hll_doctreeCtor :: IO CInt
hll_doctreeCtor = fmap fromIntegral globalDoctreeCtor




hll_doctreeUpdate :: CInt -> CInt -> IO ()
hll_doctreeUpdate cRef cSomeVal = do
  let ref     = fromIntegral cRef
  let someVal = fromIntegral cSomeVal

  oldDoctree <- globalDoctreeGet ref
  let newDoctree = oldDoctree { rootNode = (rootNode oldDoctree) + someVal }
  globalDoctreeUpdate ref newDoctree




hll_doctreePushNode :: CInt -> CInt -> IO CInt
hll_doctreePushNode cRef cElementIdx = do
  let ref        = fromIntegral cRef
  let elementIdx = fromIntegral cElementIdx

  doctree <- globalDoctreeGet ref
  let doctree' = doctreePushNode doctree elementIdx
  globalDoctreeUpdate ref doctree'

  return $ fromIntegral (topNodeNum doctree')




hll_doctreePopNode :: CInt -> IO ()
hll_doctreePopNode cRef = do
  let ref = fromIntegral cRef
  doctree <- globalDoctreeGet ref
  let doctree' = doctreePopNode doctree
  globalDoctreeUpdate ref doctree'




hll_doctreeGetTopNodeHtmlElementIdx :: CInt -> IO CInt
hll_doctreeGetTopNodeHtmlElementIdx cRef = do
  mDtn <- doctreeGetTopNode . fromIntegral $ cRef
  case mDtn of
    Just dtn -> return . fromIntegral . htmlElementIdx $ dtn
    Nothing  -> return 0




hll_doctreeGetTopNodeElementSelectorId :: CInt -> IO CString
hll_doctreeGetTopNodeElementSelectorId cRef = do
  mDtn <- doctreeGetTopNode . fromIntegral $ cRef
  case mDtn of
    Just dtn -> newCString . T.unpack . selId $ dtn
    Nothing  -> return nullPtr




doctreeGetTopNode ref = do
  doctree <- globalDoctreeGet ref
  if topNodeNum doctree == (-1)
    then
    do
      return Nothing
    else
    do
      return $ Just ((nodes doctree) M.! (topNodeNum doctree))




updateTopNodeInTrees :: Int -> (DoctreeNode -> DoctreeNode) -> IO ()
updateTopNodeInTrees ref f = do
    doctree <- globalDoctreeGet ref
    let doctree' = adjustTopNode doctree f
    globalDoctreeUpdate ref doctree'




hll_styleEngineSetElementId :: CInt -> CString -> IO ()
hll_styleEngineSetElementId cDoctreeRef cElementId = do
    let doctreeRef = fromIntegral cDoctreeRef
    stringVal     <- BSU.unsafePackCString $ cElementId
    let elementId  = T.E.decodeLatin1 stringVal

    updateTopNodeInTrees doctreeRef (\x -> x { selId = elementId })




hll_styleEngineSetElementClass :: CInt -> CString -> IO ()
hll_styleEngineSetElementClass cDoctreeRef cElementClassTokens = do
    let doctreeRef = fromIntegral cDoctreeRef
    tokens        <- BSU.unsafePackCString $ cElementClassTokens
    -- With ' ' character as separator of selectors, we can use 'words' to
    -- get the list of selectors.
    let ws = words . Char8.unpack $ tokens
    let classSelectors = fmap T.pack ws

    updateTopNodeInTrees doctreeRef (\x -> x { selClass = classSelectors })




hll_styleEngineSetElementPseudoClass :: CInt -> CString -> IO ()
hll_styleEngineSetElementPseudoClass cDoctreeRef cElementPseudoClass = do
    let doctreeRef         = fromIntegral cDoctreeRef
    stringVal             <- BSU.unsafePackCString $ cElementPseudoClass
    let elementPseudoClass = T.E.decodeLatin1 stringVal

    updateTopNodeInTrees doctreeRef (\x -> x { selPseudoClass = elementPseudoClass })

