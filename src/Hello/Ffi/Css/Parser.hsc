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




module Hello.Ffi.Css.Parser
  (
  )
where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.Text.Encoding as T.E
import qualified Data.ByteString.Unsafe as BSU
-- import Debug.Trace

import Hello.Css.Parser.Rule
import Hello.Css.StyleEngine
import Hello.Css.StyleEngineGlobal
import Hello.Css.StyleNode




foreign export ccall "ffiCssParseElementStyleAttribute" ffiCssParseElementStyleAttribute :: CInt -> Ptr () -> CString -> CInt -> IO ()




ffiCssParseElementStyleAttribute :: CInt -> Ptr () -> CString -> CInt -> IO ()
ffiCssParseElementStyleAttribute cStyleEngineRef _ptrBaseUrl ptrStringCssStyleAttribute buflen = do

  -- FFI part.
  let refEngine = fromIntegral cStyleEngineRef
  engine <- globalStyleEngineGet refEngine
  cssStyleAttribute <- BSU.unsafePackCStringLen (ptrStringCssStyleAttribute, fromIntegral buflen)

  -- The main part.
  let styleNode  = styleNodesStackPeek engine
      (m, i)     = parseElementStyleAttribute "" (T.E.decodeLatin1 cssStyleAttribute) (mainDeclSet styleNode, importantDeclSet styleNode)
      styleNode' = styleNode { mainDeclSet = m, importantDeclSet = i }
      engine'    = styleNodesStackUpdateTop engine styleNode'

  -- FFI part.
  globalStyleEngineUpdate refEngine engine'
  return ()




