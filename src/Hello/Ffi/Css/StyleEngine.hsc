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


module Hello.Ffi.Css.StyleEngine
  (
  )
where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector as V
import qualified Data.List as L
import Control.Monad -- when
import Debug.Trace

import Hello.Css.Parser
import Hello.Css.Tokenizer
import Hello.Css.StyleSheet
import Hello.Css.Selector
import Hello.Css.DoctreeNode
import Hello.Css.Match
import Hello.Css.MediaQuery

import Hello.Ffi.Css.Parser
import Hello.Ffi.Utils
import Hello.Ffi.Css.DoctreeNode




#include "../../hello.h"




foreign export ccall "hll_makeCssDeclaration" hll_makeCssDeclaration :: CInt -> Ptr FfiCssValue -> IO (Ptr FfiCssDeclaration)


hll_makeCssDeclaration :: CInt -> Ptr FfiCssValue -> IO (Ptr FfiCssDeclaration)
hll_makeCssDeclaration cProperty ptrFfiCssValue = do
  let property = fromIntegral cProperty

  ffiCssValue :: FfiCssValue <- peek ptrFfiCssValue
  cssValue <- peekCssValue ffiCssValue

  let declaration = CssDeclaration property cssValue False

  allocAndPokeCssDeclaration declaration

