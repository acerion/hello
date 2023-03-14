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




module Hello.Ffi.Css.StyleEngineGlobal
  (
  )
where




import Prelude
import Foreign.C.Types

import Hello.Css.StyleEngine
import Hello.Css.StyleEngineGlobal




#include "../../hello.h"




foreign export ccall "ffiStyleEngineCtor" ffiStyleEngineCtor :: IO CInt
foreign export ccall "ffiStyleEngineStyleNodesStackSize" ffiStyleEngineStyleNodesStackSize :: CInt -> IO CInt
foreign export ccall "ffiStyleEngineStyleNodesStackPushEmptyNode" ffiStyleEngineStyleNodesStackPushEmptyNode :: CInt -> IO ()
foreign export ccall "ffiStyleEngineStyleNodesStackPop" ffiStyleEngineStyleNodesStackPop :: CInt -> IO ()
foreign export ccall "ffiStyleEngineStyleNodesClearNonCssHints" ffiStyleEngineStyleNodesClearNonCssHints :: CInt -> IO ()




ffiStyleEngineCtor :: IO CInt
ffiStyleEngineCtor = fmap fromIntegral globalStyleEngineCtor




ffiStyleEngineStyleNodesStackSize :: CInt -> IO CInt
ffiStyleEngineStyleNodesStackSize cRef = do
  engine <- globalStyleEngineGet . fromIntegral $ cRef
  return . fromIntegral . styleNodesStackSize $ engine




ffiStyleEngineStyleNodesStackPushEmptyNode :: CInt -> IO ()
ffiStyleEngineStyleNodesStackPushEmptyNode cRef = do
  let ref = fromIntegral cRef
  engine <- globalStyleEngineGet ref
  let engine' = styleEngineNodesStackPushEmptyNode engine
  globalStyleEngineUpdate ref engine'
  return ()




ffiStyleEngineStyleNodesStackPop :: CInt -> IO ()
ffiStyleEngineStyleNodesStackPop cRef = do
  let ref = fromIntegral cRef
  engine <- globalStyleEngineGet ref
  let engine' = styleEngineNodesStackPop engine
  globalStyleEngineUpdate ref engine'
  return ()





ffiStyleEngineStyleNodesClearNonCssHints :: CInt -> IO ()
ffiStyleEngineStyleNodesClearNonCssHints cRef = do
  let ref = fromIntegral cRef
  engine <- globalStyleEngineGet ref
  let engine' = styleEngineNodesStackClearNonCssHints engine
  globalStyleEngineUpdate ref engine'
  return ()

