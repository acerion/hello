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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Debug.Trace
import qualified Data.Text.Encoding as T.E

import Hello.Css.Parser
import Hello.Ffi.Css.Parser
import Hello.Ffi.Utils




#include "../../hello.h"




foreign export ccall "hll_makeCssDeclaration" hll_makeCssDeclaration :: CInt -> Ptr FfiCssValue -> IO (Ptr FfiCssDeclaration)
foreign export ccall "hll_styleEngineSetNonCssHintOfCurrentNodeInt" hll_styleEngineSetNonCssHintOfCurrentNodeInt :: Ptr FfiCssDeclarationSet -> CInt -> CInt -> CInt -> IO (Ptr FfiCssDeclarationSet)
foreign export ccall "hll_styleEngineSetNonCssHintOfCurrentNodeString" hll_styleEngineSetNonCssHintOfCurrentNodeString :: Ptr FfiCssDeclarationSet -> CInt -> CInt -> CString -> IO (Ptr FfiCssDeclarationSet)




hll_makeCssDeclaration :: CInt -> Ptr FfiCssValue -> IO (Ptr FfiCssDeclaration)
hll_makeCssDeclaration cProperty ptrFfiCssValue = do
  let property = fromIntegral cProperty

  ffiCssValue :: FfiCssValue <- peek ptrFfiCssValue
  cssValue <- peekCssValue ffiCssValue

  let declaration = CssDeclaration property cssValue False

  allocAndPokeCssDeclaration declaration




makeValue valType intVal textVal | valType ==  0 = CssValueTypeInt intVal
                                 | valType ==  1 = CssValueTypeEnum intVal
                                 | valType ==  2 = CssValueTypeMultiEnum intVal
                                 | valType ==  3 = CssValueTypeLengthPercent $ cssLengthWordToDistance intVal
                                 | valType ==  4 = CssValueTypeLength $ cssLengthWordToDistance intVal
                                 | valType ==  5 = CssValueTypeSignedLength $ cssLengthWordToDistance intVal
                                 | valType ==  6 = CssValueTypeLengthPercentNumber $ cssLengthWordToDistance intVal
                                 | valType ==  7 = CssValueTypeAuto $ cssLengthWordToDistance intVal
                                 | valType ==  8 = CssValueTypeColor intVal
                                 | valType ==  9 = CssValueTypeFontWeight intVal
                                 | valType == 10 = CssValueTypeString textVal
                                 | valType == 11 = CssValueTypeStringList textVal
                                 | valType == 12 = CssValueTypeURI textVal
                                 | valType == 13 = CssValueTypeBgPosition
                                 | otherwise = CssValueTypeUnused




hll_styleEngineSetNonCssHintOfCurrentNodeInt :: Ptr FfiCssDeclarationSet -> CInt -> CInt -> CInt -> IO (Ptr FfiCssDeclarationSet)
hll_styleEngineSetNonCssHintOfCurrentNodeInt ptrFfiCssDeclarationSet cProperty cValueType cIntVal = do

  declSet :: CssDeclarationSet <- if nullPtr == ptrFfiCssDeclarationSet
                                  then return defaultCssDeclarationSet
                                  else peekCssDeclarationSet ptrFfiCssDeclarationSet

  let property = fromIntegral cProperty
  let valType  = fromIntegral cValueType
  let intVal   = fromIntegral cIntVal
  let textVal  = ""

  let cssValue :: CssValue = makeValue valType intVal textVal
  let decl :: CssDeclaration = CssDeclaration property cssValue False

  let newDeclSet = declarationsSetUpdateOrAdd declSet decl
  newPtrStructDeclarationSet :: Ptr FfiCssDeclarationSet  <- callocBytes #{size c_css_declaration_set_t}

  pokeCssDeclarationSet newPtrStructDeclarationSet newDeclSet

  return newPtrStructDeclarationSet




hll_styleEngineSetNonCssHintOfCurrentNodeString :: Ptr FfiCssDeclarationSet -> CInt -> CInt -> CString -> IO (Ptr FfiCssDeclarationSet)
hll_styleEngineSetNonCssHintOfCurrentNodeString ptrFfiCssDeclarationSet cProperty cValueType cStringVal = do

  declSet :: CssDeclarationSet <- if nullPtr == ptrFfiCssDeclarationSet
                                  then return defaultCssDeclarationSet
                                  else peekCssDeclarationSet ptrFfiCssDeclarationSet

  let property = fromIntegral cProperty
  let valType  = fromIntegral cValueType
  let intVal   = 0
  stringVal <- BSU.unsafePackCString $ cStringVal
  let textVal  = T.E.decodeLatin1 stringVal

  let cssValue :: CssValue = makeValue valType intVal textVal
  let decl :: CssDeclaration = CssDeclaration property cssValue False

  let newDeclSet = declarationsSetUpdateOrAdd declSet decl
  newPtrStructDeclarationSet :: Ptr FfiCssDeclarationSet  <- callocBytes #{size c_css_declaration_set_t}

  pokeCssDeclarationSet newPtrStructDeclarationSet newDeclSet

  return newPtrStructDeclarationSet




