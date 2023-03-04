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




module Hello.Ffi.Dw.StyleAttrsGlobal
  (
  )
where




import Prelude
import Foreign.C.Types

--import Debug.Trace
import Data.Word

import Hello.Dw.Style
import Hello.Dw.StyleAttrsGlobal




#include "../../hello.h"




foreign export ccall "ffiStyleAttrsCtor" ffiStyleAttrsCtor :: IO CInt


foreign export ccall "ffiStyleAttrsInitValues" ffiStyleAttrsInitValues :: CInt -> IO ()
foreign export ccall "ffiStyleAttrsEqual" ffiStyleAttrsEqual :: CInt -> CInt -> IO Bool
foreign export ccall "ffiStyleAttrsHashValue" ffiStyleAttrsHashValue :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsCopy" ffiStyleAttrsCopy :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsReset" ffiStyleAttrsReset :: CInt -> IO ()

foreign export ccall "ffiStyleAttrsTextAlign" ffiStyleAttrsTextAlign :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsTextDecoration" ffiStyleAttrsTextDecoration :: CInt -> IO Word32
foreign export ccall "ffiStyleAttrsSetTextDecoration" ffiStyleAttrsSetTextDecoration :: CInt -> Word32 -> IO ()

foreign export ccall "ffiStyleAttrsTextTransform" ffiStyleAttrsTextTransform :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsCursor" ffiStyleAttrsCursor :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsSetCursor" ffiStyleAttrsSetCursor :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsWhiteSpace" ffiStyleAttrsWhiteSpace :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsListStylePosition" ffiStyleAttrsListStylePosition :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsListStyleType" ffiStyleAttrsListStyleType :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsXLink" ffiStyleAttrsXLink :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsSetXLink" ffiStyleAttrsSetXLink :: CInt -> CInt -> IO ()
foreign export ccall "ffiStyleAttrsXImg" ffiStyleAttrsXImg :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsBorderCollapse" ffiStyleAttrsBorderCollapse :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsSetCollapseTableAttrs" ffiStyleAttrsSetCollapseTableAttrs :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsBorderStyleTop" ffiStyleAttrsBorderStyleTop :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsBorderStyleRight" ffiStyleAttrsBorderStyleRight :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsBorderStyleBottom" ffiStyleAttrsBorderStyleBottom :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsBorderStyleLeft" ffiStyleAttrsBorderStyleLeft :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsSetBorderStyle" ffiStyleAttrsSetBorderStyle :: CInt -> CInt -> IO ()




ffiStyleAttrsCtor :: IO CInt
ffiStyleAttrsCtor = do
  ref <- fmap fromIntegral globalStyleAttrsCtor
  return . fromIntegral $ ref




ffiStyleAttrsInitValues :: CInt -> IO ()
ffiStyleAttrsInitValues cRef = do
  let ref = fromIntegral cRef
  old <- globalStyleAttrsGet ref
  let new = styleAttrsInitValues old
  globalStyleAttrsUpdate ref new
  return ()




ffiStyleAttrsEqual :: CInt -> CInt -> IO Bool
ffiStyleAttrsEqual cRef1 cRef2 = do
  let ref1 = fromIntegral cRef1
  let ref2 = fromIntegral cRef2
  attrs1 <- globalStyleAttrsGet ref1
  attrs2 <- globalStyleAttrsGet ref2
  return $ styleAttrsEqual attrs1 attrs2




ffiStyleAttrsHashValue :: CInt -> IO CInt
ffiStyleAttrsHashValue cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleAttrsHashValue $ attrs




ffiStyleAttrsCopy :: CInt -> CInt -> IO ()
ffiStyleAttrsCopy cRefTo cRefFrom = do
  let refTo   = fromIntegral cRefTo
  let refFrom = fromIntegral cRefFrom
  attrsTo   <- globalStyleAttrsGet refTo
  attrsFrom <- globalStyleAttrsGet refFrom
  let attrs = styleAttrsCopy attrsTo attrsFrom
  globalStyleAttrsUpdate refTo attrs
  return ()




ffiStyleAttrsTextAlign :: CInt -> IO CInt
ffiStyleAttrsTextAlign cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleTextAlign $ attrs




ffiStyleAttrsTextDecoration :: CInt -> IO Word32
ffiStyleAttrsTextDecoration cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleTextDecoration $ attrs




ffiStyleAttrsSetTextDecoration :: CInt -> Word32 -> IO ()
ffiStyleAttrsSetTextDecoration cRef val = do
  let ref = fromIntegral cRef
  --let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let new = old { styleTextDecoration = val }
  globalStyleAttrsUpdate ref new
  return ()




ffiStyleAttrsTextTransform :: CInt -> IO CInt
ffiStyleAttrsTextTransform cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleTextTransform $ attrs




ffiStyleAttrsCursor :: CInt -> IO CInt
ffiStyleAttrsCursor cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleCursor $ attrs




ffiStyleAttrsSetCursor :: CInt -> CInt -> IO ()
ffiStyleAttrsSetCursor cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let new = old { styleCursor = val }
  globalStyleAttrsUpdate ref new
  return ()





ffiStyleAttrsWhiteSpace :: CInt -> IO CInt
ffiStyleAttrsWhiteSpace cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleWhiteSpace $ attrs




ffiStyleAttrsListStylePosition :: CInt -> IO CInt
ffiStyleAttrsListStylePosition cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleListStylePosition $ attrs




ffiStyleAttrsListStyleType :: CInt -> IO CInt
ffiStyleAttrsListStyleType cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleListStyleType $ attrs




ffiStyleAttrsXLink :: CInt -> IO CInt
ffiStyleAttrsXLink cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleXLink $ attrs




ffiStyleAttrsSetXLink :: CInt -> CInt -> IO ()
ffiStyleAttrsSetXLink cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let new = old { styleXLink = val }
  globalStyleAttrsUpdate ref new
  return ()




ffiStyleAttrsXImg :: CInt -> IO CInt
ffiStyleAttrsXImg cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleXImg $ attrs




ffiStyleAttrsBorderCollapse :: CInt -> IO CInt
ffiStyleAttrsBorderCollapse cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleBorderCollapse $ attrs




ffiStyleAttrsSetCollapseTableAttrs :: CInt -> CInt -> IO ()
ffiStyleAttrsSetCollapseTableAttrs cRefTable cRefCell = do
  let refTable = fromIntegral cRefTable
  let refCell  = fromIntegral cRefCell
  attrsTable <- globalStyleAttrsGet refTable
  attrsCell  <- globalStyleAttrsGet refCell
  let attrs = styleAttrsSetCollapseTableAttrs attrsTable attrsCell
  globalStyleAttrsUpdate refTable attrs
  return ()




ffiStyleAttrsBorderStyleTop :: CInt -> IO CInt
ffiStyleAttrsBorderStyleTop cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleBorderStyleTop . styleBorderStyle $ attrs




ffiStyleAttrsBorderStyleRight :: CInt -> IO CInt
ffiStyleAttrsBorderStyleRight cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleBorderStyleRight . styleBorderStyle $ attrs




ffiStyleAttrsBorderStyleBottom :: CInt -> IO CInt
ffiStyleAttrsBorderStyleBottom cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleBorderStyleBottom . styleBorderStyle $ attrs




ffiStyleAttrsBorderStyleLeft :: CInt -> IO CInt
ffiStyleAttrsBorderStyleLeft cRef = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  return . fromIntegral . styleBorderStyleLeft . styleBorderStyle $ attrs




ffiStyleAttrsSetBorderStyle :: CInt -> CInt -> IO ()
ffiStyleAttrsSetBorderStyle cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let new = styleAttrsSetBorderStyle old val
  globalStyleAttrsUpdate ref new
  return ()




ffiStyleAttrsReset :: CInt -> IO ()
ffiStyleAttrsReset cRef = do
  let ref = fromIntegral cRef
  old <- globalStyleAttrsGet ref
  let new = styleAttrsReset old
  globalStyleAttrsUpdate ref new
  return ()

