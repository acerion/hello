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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E

import Hello.Css.ContextGlobal
import Hello.Css.Distance
import Hello.Css.Parser
import Hello.Css.StyleEngine
import Hello.Css.UserAgentStyle

import Hello.Dw.DwLength
import Hello.Dw.Style

import Hello.Ffi.Css.Context
import Hello.Ffi.Css.Parser
import Hello.Ffi.Css.Value

import Hello.Ffi.Dw.DwLength
import Hello.Ffi.Dw.Style

import Hello.Ffi.Preferences
import Hello.Ffi.Utils




#include "../../hello.h"




foreign export ccall "hll_makeCssDeclaration" hll_makeCssDeclaration :: CInt -> Ptr FfiCssValue -> IO (Ptr FfiCssDeclaration)
foreign export ccall "hll_styleEngineSetNonCssHintOfCurrentNodeInt" hll_styleEngineSetNonCssHintOfCurrentNodeInt :: Ptr FfiCssDeclarationSet -> CInt -> CInt -> CInt -> Float -> CInt -> IO (Ptr FfiCssDeclarationSet)
foreign export ccall "hll_styleEngineSetNonCssHintOfCurrentNodeString" hll_styleEngineSetNonCssHintOfCurrentNodeString :: Ptr FfiCssDeclarationSet -> CInt -> CInt -> CString -> IO (Ptr FfiCssDeclarationSet)
foreign export ccall "hll_styleEngineComputeAbsoluteLengthValue" hll_styleEngineComputeAbsoluteLengthValue :: Float -> CInt -> Ptr FfiFontAttrs -> CInt -> Float -> Float -> Ptr CInt -> IO CInt

foreign export ccall "hll_setFontFamily" hll_setFontFamily :: Ptr FfiCssValue -> Ptr FfiPreferences -> Ptr FfiFontAttrs -> IO ()
foreign export ccall "hll_setFontWeight" hll_setFontWeight :: Ptr FfiFontAttrs -> Ptr FfiCssValue -> IO ()
foreign export ccall "hll_setFontSize" hll_setFontSize :: Ptr FfiCssValue -> Ptr FfiPreferences -> Float -> Float -> Ptr FfiFontAttrs -> Ptr FfiFontAttrs -> IO ()
foreign export ccall "hll_setFontStyle" hll_setFontStyle :: Ptr FfiFontAttrs -> Ptr FfiCssValue -> IO ()
foreign export ccall "hll_setFontLetterSpacing" hll_setFontLetterSpacing :: Ptr FfiCssValue -> Float -> Float -> Ptr FfiFontAttrs -> Ptr FfiFontAttrs -> IO ()
foreign export ccall "hll_setFontVariant" hll_setFontVariant :: Ptr FfiFontAttrs -> Ptr FfiCssValue -> IO ()

foreign export ccall "hll_styleEngineApplyStyleToFont" hll_styleEngineApplyStyleToFont :: Ptr FfiCssDeclarationSet -> Ptr FfiPreferences -> Float -> Float -> Ptr FfiFontAttrs -> Ptr FfiFontAttrs -> IO ()

foreign export ccall "hll_styleEngineComputeBorderWidth" hll_styleEngineComputeBorderWidth :: Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> IO Int
foreign export ccall "hll_styleEngineSetBorderWidth" hll_styleEngineSetBorderWidth :: CInt -> Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> Ptr FfiStyleBorderWidth -> IO ()
foreign export ccall "hll_styleEngineSetBorderStyle" hll_styleEngineSetBorderStyle :: CInt -> Ptr FfiCssValue -> Ptr FfiStyleBorderStyle -> IO ()
foreign export ccall "hll_styleEngineSetMargin" hll_styleEngineSetMargin :: CInt -> Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> Ptr FfiStyleMargin -> IO ()
foreign export ccall "hll_styleEngineSetPadding" hll_styleEngineSetPadding :: CInt -> Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> Ptr FfiStylePadding -> IO ()

foreign export ccall "hll_computeDwLength" hll_computeDwLength :: Ptr FfiDwLength -> CDouble -> CInt -> Ptr FfiFontAttrs -> Float -> Float -> IO Int




hll_makeCssDeclaration :: CInt -> Ptr FfiCssValue -> IO (Ptr FfiCssDeclaration)
hll_makeCssDeclaration cProperty ptrFfiCssValue = do
  let property = fromIntegral cProperty

  ffiCssValue :: FfiCssValue <- peek ptrFfiCssValue
  cssValue <- peekCssValue ffiCssValue

  let declaration = CssDeclaration property cssValue False

  allocAndPokeCssDeclaration declaration




hll_styleEngineSetNonCssHintOfCurrentNodeInt :: Ptr FfiCssDeclarationSet -> CInt -> CInt -> CInt -> Float -> CInt -> IO (Ptr FfiCssDeclarationSet)
hll_styleEngineSetNonCssHintOfCurrentNodeInt ptrFfiCssDeclarationSet cProperty cValueType cIntVal cLengthValue cLengthType  = do

  declSet :: CssDeclarationSet <- if nullPtr == ptrFfiCssDeclarationSet
                                  then return defaultCssDeclarationSet
                                  else peekCssDeclarationSet ptrFfiCssDeclarationSet

  let property = fromIntegral cProperty
  let valType  = fromIntegral cValueType
  let intVal   = fromIntegral cIntVal
  let textVal  = ""
  let lengthValue = cLengthValue
  let lengthType  = fromIntegral cLengthType

  let cssValue :: CssValue = makeValue valType intVal textVal lengthValue lengthType
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
  let lengthValue = 0 -- cLengthValue
  let lengthType  = 0 -- fromIntegral cLengthType

  let cssValue :: CssValue = makeValue valType intVal textVal lengthValue lengthType
  let decl :: CssDeclaration = CssDeclaration property cssValue False

  let newDeclSet = declarationsSetUpdateOrAdd declSet decl
  newPtrStructDeclarationSet :: Ptr FfiCssDeclarationSet  <- callocBytes #{size c_css_declaration_set_t}

  pokeCssDeclarationSet newPtrStructDeclarationSet newDeclSet

  return newPtrStructDeclarationSet




{-
hll_styleEngineBuildUserAgentStyle :: CInt -> IO ()
hll_styleEngineBuildUserAgentStyle cRef = do

  let ref  = fromIntegral cRef
  context <- globalContextGet ref

  let context' = styleEngineBuildUserAgentStyle context

  globalContextUpdate ref context'

  return ()

-}




hll_styleEngineComputeAbsoluteLengthValue :: Float -> CInt -> Ptr FfiFontAttrs -> CInt -> Float -> Float -> Ptr CInt -> IO CInt
hll_styleEngineComputeAbsoluteLengthValue lengthValue cLengthType ptrStructFontAttrs cPercentageBase dpiX dpiY ptrOut = do
  let lengthType     = fromIntegral cLengthType
  fontAttrs         <- peekFontAttrs ptrStructFontAttrs
  let percentageBase = fromIntegral cPercentageBase
  let distance       = cssLengthToDistance lengthValue lengthType

  case styleEngineComputeAbsoluteLengthValue distance fontAttrs percentageBase dpiX dpiY of
    Just val -> do
      let out = round val -- TODO: a type of Float -> Int function to be verified here
      poke ptrOut (fromIntegral out)
      return 1 -- True
    Nothing -> return 0 -- False




data FfiFontAttrs = FfiFontAttrs
  {
    cFontSize          :: CInt
  , cFontWeight        :: CInt
  , cFontName          :: Ptr CChar
  , cFontVariant       :: CInt
  , cFontStyle         :: CInt

  , cFontXHeight       :: CInt
  , cFontLetterSpacing :: CInt
  } deriving (Show)




instance Storable FfiFontAttrs where
  sizeOf    _ = #{size c_font_attrs_t}
  alignment _ = #{alignment c_font_attrs_t}

  poke ptr (FfiFontAttrs argSize argWeight argName argFontVariant argStyle argXHeight argLetterSpacing) = do
    #{poke c_font_attrs_t, size}          ptr argSize
    #{poke c_font_attrs_t, weight}        ptr argWeight
    #{poke c_font_attrs_t, name}          ptr argName
    #{poke c_font_attrs_t, fontVariant}   ptr argFontVariant
    #{poke c_font_attrs_t, style}         ptr argStyle
    #{poke c_font_attrs_t, xHeight}       ptr argXHeight
    #{poke c_font_attrs_t, letterSpacing} ptr argLetterSpacing

  peek ptr = do
    a <- #{peek c_font_attrs_t, size} ptr
    b <- #{peek c_font_attrs_t, weight}  ptr
    c <- #{peek c_font_attrs_t, name} ptr
    d <- #{peek c_font_attrs_t, fontVariant} ptr
    e <- #{peek c_font_attrs_t, style} ptr
    f <- #{peek c_font_attrs_t, xHeight} ptr
    g <- #{peek c_font_attrs_t, letterSpacing} ptr
    return (FfiFontAttrs a b c d e f g)




peekFontAttrs :: Ptr FfiFontAttrs -> IO FontAttrs
peekFontAttrs ptrStructFontAttrs = do
  ffiAttrs <- peek ptrStructFontAttrs
  name     <- ptrCCharToText . cFontName $ ffiAttrs

  let fontAttrs = FontAttrs
        {
          fontSize          = fromIntegral . cFontSize $ ffiAttrs
        , fontWeight        = fromIntegral . cFontWeight $ ffiAttrs
        , fontName          = name
        , fontVariant       = fromIntegral . cFontVariant $ ffiAttrs
        , fontStyle         = fromIntegral . cFontStyle $ ffiAttrs
        , fontXHeight       = fromIntegral . cFontXHeight $ ffiAttrs
        , fontLetterSpacing = fromIntegral . cFontLetterSpacing $ ffiAttrs
        }

  return fontAttrs




pokeFontAttrs :: FontAttrs -> Ptr FfiFontAttrs -> IO ()
pokeFontAttrs fontAttrs ptrStructFontAttrs = do
  let size   = fromIntegral . fontSize $ fontAttrs
  let weight = fromIntegral . fontWeight $ fontAttrs
  name <- newCString . T.unpack . fontName $ fontAttrs
  let variant = fromIntegral . fontVariant $ fontAttrs
  let style   = fromIntegral . fontStyle $ fontAttrs
  let height  = fromIntegral . fontXHeight $ fontAttrs
  let spacing = fromIntegral . fontLetterSpacing $ fontAttrs

  poke ptrStructFontAttrs $ FfiFontAttrs size weight name variant style height spacing




hll_setFontFamily :: Ptr FfiCssValue -> Ptr FfiPreferences -> Ptr FfiFontAttrs -> IO ()
hll_setFontFamily ptrStructCssValue ptrStructPreferences ptrStructFontAttrs = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs
  prefs       <- peekPreferences ptrStructPreferences

  let fontAttrs' = styleEngineSetFontFamily value prefs fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs



hll_setFontWeight :: Ptr FfiFontAttrs -> Ptr FfiCssValue -> IO ()
hll_setFontWeight ptrStructFontAttrs ptrStructCssValue = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs

  let fontAttrs' = styleEngineSetFontWeight value fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs




hll_setFontSize :: Ptr FfiCssValue -> Ptr FfiPreferences -> Float -> Float -> Ptr FfiFontAttrs -> Ptr FfiFontAttrs -> IO ()
hll_setFontSize ptrStructCssValue ptrStructPreferences dpiX dpiY ptrStructParentFontAttrs ptrStructFontAttrs = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs
  parentFontAttrs   <- peekFontAttrs ptrStructParentFontAttrs
  prefs       <- peekPreferences ptrStructPreferences

  let fontAttrs' = styleEngineSetFontSize' value prefs dpiX dpiY parentFontAttrs fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs




hll_setFontStyle :: Ptr FfiFontAttrs -> Ptr FfiCssValue -> IO ()
hll_setFontStyle ptrStructFontAttrs ptrStructCssValue = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs

  let fontAttrs' = styleEngineSetFontStyle value fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs




hll_setFontLetterSpacing :: Ptr FfiCssValue -> Float -> Float -> Ptr FfiFontAttrs -> Ptr FfiFontAttrs -> IO ()
hll_setFontLetterSpacing ptrStructCssValue dpiX dpiY ptrStructParentFontAttrs ptrStructFontAttrs = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs
  parentFontAttrs   <- peekFontAttrs ptrStructParentFontAttrs

  let fontAttrs' = styleEngineSetLetterSpacing value dpiX dpiY parentFontAttrs fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs




hll_setFontVariant :: Ptr FfiFontAttrs -> Ptr FfiCssValue -> IO ()
hll_setFontVariant ptrStructFontAttrs ptrStructCssValue = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs

  let fontAttrs' = styleEngineSetFontVariant value fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs




hll_styleEngineApplyStyleToFont :: Ptr FfiCssDeclarationSet -> Ptr FfiPreferences -> Float -> Float -> Ptr FfiFontAttrs -> Ptr FfiFontAttrs -> IO ()
hll_styleEngineApplyStyleToFont ptrStructDeclSet ptrStructPrefs dpiX dpiY ptrStructParentFontAttrs ptrStructFontAttrs = do
  declSet         <- peekCssDeclarationSet ptrStructDeclSet
  prefs           <- peekPreferences ptrStructPrefs
  fontAttrs       <- peekFontAttrs ptrStructFontAttrs
  parentFontAttrs <- peekFontAttrs ptrStructParentFontAttrs

  let fontAttrs' = styleEngineApplyStyleToFont declSet prefs dpiX dpiY parentFontAttrs fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs





hll_styleEngineComputeBorderWidth :: Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> IO Int
hll_styleEngineComputeBorderWidth ptrStructCssValue ptrStructFontAttrs dpiX dpiY  = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs

  case styleEngineComputeBorderWidth value dpiX dpiY fontAttrs of
    Just x    -> return x
    otherwise -> return 0





hll_styleEngineSetBorderWidth :: CInt -> Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> Ptr FfiStyleBorderWidth -> IO ()
hll_styleEngineSetBorderWidth cProperty ptrStructCssValue ptrStructFontAttrs dpiX dpiY ptrStructBorderWidth = do
  let property = fromIntegral cProperty
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs
  borderWidth <- peekStyleBorderWidth ptrStructBorderWidth

  let borderWidth' = styleEngineSetBorderWidth property value dpiX dpiY fontAttrs borderWidth

  pokeStyleBorderWidth borderWidth' ptrStructBorderWidth




hll_styleEngineSetBorderStyle :: CInt -> Ptr FfiCssValue -> Ptr FfiStyleBorderStyle -> IO ()
hll_styleEngineSetBorderStyle cProperty ptrStructCssValue ptrStructBorderStyle = do
  let property = fromIntegral cProperty
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  borderStyle <- peekStyleBorderStyle ptrStructBorderStyle

  let borderStyle' = styleEngineSetBorderStyle property value borderStyle

  pokeStyleBorderStyle borderStyle' ptrStructBorderStyle




hll_styleEngineSetMargin :: CInt -> Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> Ptr FfiStyleMargin -> IO ()
hll_styleEngineSetMargin cProperty ptrStructCssValue ptrStructFontAttrs dpiX dpiY ptrStructStyleMargin = do
  let property = fromIntegral cProperty
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs
  margin      <- peekStyleMargin ptrStructStyleMargin

  let margin' = styleEngineSetMargin property value dpiX dpiY fontAttrs margin

  pokeStyleMargin margin' ptrStructStyleMargin




hll_styleEngineSetPadding :: CInt -> Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> Ptr FfiStylePadding -> IO ()
hll_styleEngineSetPadding cProperty ptrStructCssValue ptrStructFontAttrs dpiX dpiY ptrStructStylePadding = do
  let property  = fromIntegral cProperty
  ffiCssValue  <- peek ptrStructCssValue
  value        <- peekCssValue ffiCssValue
  fontAttrs    <- peekFontAttrs ptrStructFontAttrs
  padding      <- peekStylePadding ptrStructStylePadding

  let padding' = styleEngineSetPadding property value dpiX dpiY fontAttrs padding

  pokeStylePadding padding' ptrStructStylePadding




hll_computeDwLength :: Ptr FfiDwLength -> CDouble -> CInt -> Ptr FfiFontAttrs -> Float -> Float -> IO Int
hll_computeDwLength ptrStructDwLength cLenValue cLenType ptrStructFontAttrs dpiX dpiY = do
  let lenType  = fromIntegral cLenType
  let lenValue = cDoubleToDouble cLenValue
  let distance = cssLengthToDistance (realToFrac lenValue) lenType
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs

  case styleEngineCalculateDwLength distance fontAttrs dpiX dpiY of
    Just len -> do
      pokeDwLength len ptrStructDwLength
      return 1
    Nothing -> return 0




