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




module Hello.Ffi.Css.Context
  (
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

import Hello.Css.ContextGlobal

import Hello.Ffi.Css.Misc




#include "../../hello.h"




foreign export ccall "hll_cssContextCtor" hll_cssContextCtor :: IO CInt
foreign export ccall "hll_cssContextUpdate" hll_cssContextUpdate :: CInt -> Ptr FfiCssContext -> IO ()
foreign export ccall "hll_cssContextPut" hll_cssContextPut :: Ptr FfiCssContext -> IO CInt



hll_cssContextCtor :: IO CInt
hll_cssContextCtor = fmap fromIntegral globalContextCtor




hll_cssContextUpdate :: CInt -> Ptr FfiCssContext -> IO ()
hll_cssContextUpdate cRef ptrStructCssContext = do
  let ref  = fromIntegral cRef
  context <- peekCssContext ptrStructCssContext

  globalContextUpdate ref context





hll_cssContextPut :: Ptr FfiCssContext -> IO CInt
hll_cssContextPut ptrStructCssContext = do
  context <- peekCssContext ptrStructCssContext
  ref <- globalContextPut context

  return . fromIntegral $ ref

