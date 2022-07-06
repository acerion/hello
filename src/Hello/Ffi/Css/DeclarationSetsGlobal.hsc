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




module Hello.Ffi.Css.DeclarationSetsGlobal
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

import System.IO

import Debug.Trace




import Hello.Css.DeclarationSetsGlobal

import Hello.Utils



#include "../../hello.h"




foreign export ccall "hll_declarationSetCtor" hll_declarationSetCtor :: IO CInt




hll_declarationSetCtor :: IO CInt
hll_declarationSetCtor = fmap fromIntegral globalDeclarationSetCtor



