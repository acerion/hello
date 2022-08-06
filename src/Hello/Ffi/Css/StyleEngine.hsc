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

-- Needed to "maxBound @Type" to work. See https://typeclasses.com/phrasebook/enum-ranges
{-# LANGUAGE TypeApplications #-}




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
import Hello.Css.Declaration
import Hello.Css.DeclarationSetsGlobal
import Hello.Css.Distance
import Hello.Css.Parser
import Hello.Css.StyleEngine
import Hello.Css.UserAgentStyle
import Hello.Css.Value

import Hello.Display

import Hello.Dw.DwLength
import Hello.Dw.FontAttrs
import Hello.Dw.Style

import Hello.Ffi.Css.Context
import Hello.Ffi.Css.Parser
import Hello.Ffi.Css.Value

import Hello.Ffi.Dw.DwLength
import Hello.Ffi.Dw.FontAttrs
import Hello.Ffi.Dw.Style

import Hello.Ffi.Preferences
import Hello.Ffi.Utils




#include "../../hello.h"




--foreign export ccall "hll_makeCssDeclaration" hll_makeCssDeclaration :: CInt -> Ptr FfiCssValue -> IO (Ptr FfiCssDeclaration)
foreign export ccall "hll_styleEngineSetNonCssHintOfNodeInt" hll_styleEngineSetNonCssHintOfNodeInt :: CInt -> CInt -> CInt -> CInt -> Float -> CInt -> IO CInt
foreign export ccall "hll_styleEngineSetNonCssHintOfNodeLength2" hll_styleEngineSetNonCssHintOfNodeLength2 :: CInt -> CInt -> CInt -> Float -> CInt -> IO CInt
foreign export ccall "hll_styleEngineSetNonCssHintOfNodeEnum" hll_styleEngineSetNonCssHintOfNodeEnum :: CInt -> CInt -> CInt -> IO CInt
foreign export ccall "hll_styleEngineSetNonCssHintOfNodeString" hll_styleEngineSetNonCssHintOfNodeString :: CInt -> CInt -> CInt -> CString -> IO CInt
foreign export ccall "hll_styleEngineSetNonCssHintOfNodeColor" hll_styleEngineSetNonCssHintOfNodeColor :: CInt -> CInt -> CInt -> IO CInt

--foreign export ccall "hll_styleEngineComputeAbsoluteLengthValue" hll_styleEngineComputeAbsoluteLengthValue :: Float -> CInt -> Ptr FfiFontAttrs -> CInt -> Float -> Float -> Ptr CInt -> IO CInt
--foreign export ccall "hll_setFontFamily" hll_setFontFamily :: Ptr FfiCssValue -> Ptr FfiPreferences -> Ptr FfiFontAttrs -> IO ()
--foreign export ccall "hll_setFontWeight" hll_setFontWeight :: Ptr FfiFontAttrs -> Ptr FfiCssValue -> IO ()
--foreign export ccall "hll_setFontSize" hll_setFontSize :: Ptr FfiCssValue -> Ptr FfiPreferences -> Float -> Float -> Ptr FfiFontAttrs -> Ptr FfiFontAttrs -> IO ()
--foreign export ccall "hll_setFontStyle" hll_setFontStyle :: Ptr FfiFontAttrs -> Ptr FfiCssValue -> IO ()
--foreign export ccall "hll_setFontLetterSpacing" hll_setFontLetterSpacing :: Ptr FfiCssValue -> Float -> Float -> Ptr FfiFontAttrs -> Ptr FfiFontAttrs -> IO ()
--foreign export ccall "hll_setFontVariant" hll_setFontVariant :: Ptr FfiFontAttrs -> Ptr FfiCssValue -> IO ()
--foreign export ccall "hll_styleEngineApplyStyleToFont" hll_styleEngineApplyStyleToFont :: Ptr FfiCssDeclarationSet -> Ptr FfiPreferences -> Float -> Float -> Ptr FfiFontAttrs -> Ptr FfiFontAttrs -> IO ()

--foreign export ccall "hll_styleEngineComputeBorderWidth" hll_styleEngineComputeBorderWidth :: Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> IO Int
--foreign export ccall "hll_styleEngineSetStyle" hll_styleEngineSetStyle :: CInt -> Ptr FfiCssValue -> Float -> Float -> Ptr FfiStyleAttrs -> IO ()
foreign export ccall "hll_styleEngineApplyStyleToGivenNode" hll_styleEngineApplyStyleToGivenNode :: CInt -> Ptr FfiPreferences -> Float -> Float -> Ptr FfiStyleAttrs -> Ptr FfiStyleAttrs -> IO ()

--foreign export ccall "hll_computeDwLength" hll_computeDwLength :: Ptr FfiDwLength -> CDouble -> CInt -> Ptr FfiFontAttrs -> Float -> Float -> IO Int

foreign export ccall "hll_inheritNonCssHints" hll_inheritNonCssHints :: CInt -> CInt -> IO CInt




{-
hll_makeCssDeclaration :: CInt -> Ptr FfiCssValue -> IO (Ptr FfiCssDeclaration)
hll_makeCssDeclaration cProperty ptrFfiCssValue = do
  let property = fromIntegral cProperty

  ffiCssValue :: FfiCssValue <- peek ptrFfiCssValue
  cssValue <- peekCssValue ffiCssValue

  let declaration = CssDeclaration property cssValue False

  allocAndPokeCssDeclaration declaration
-}




getSomeDeclSet3 nonCssDeclSetRef = if (-1) == nonCssDeclSetRef
                                   then
                                     do
                                       newRef <- globalDeclarationSetCtor
                                       newDeclSet <- globalDeclarationSetGet newRef
                                       return (newDeclSet, newRef)
                                   else
                                     do
                                       declSet <- globalDeclarationSetGet nonCssDeclSetRef
                                       return (declSet, nonCssDeclSetRef)




hll_styleEngineSetNonCssHintOfNodeInt :: CInt -> CInt -> CInt -> CInt -> Float -> CInt -> IO CInt
hll_styleEngineSetNonCssHintOfNodeInt cNonCssDeclSetRef cProperty cValueType cIntVal cLengthValue cLengthType  = do

  (declSet, ref) <- getSomeDeclSet3 $ fromIntegral cNonCssDeclSetRef

  let propMaker = fst (allDeclMakers !! (fromIntegral cProperty))
  let valType  = fromIntegral cValueType
  let intVal   = fromIntegral cIntVal
  let textVal  = ""
  let lengthValue = cLengthValue
  let lengthType  = fromIntegral cLengthType

  let cssValue :: CssValue = makeValue valType intVal textVal lengthValue lengthType
  let decl :: CssDeclWrapper = CssDeclWrapper (propMaker cssValue) False

  let newDeclSet = declarationsSetUpdateOrAdd declSet decl

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




hll_styleEngineSetNonCssHintOfNodeLength2 :: CInt -> CInt -> CInt -> Float -> CInt -> IO CInt
hll_styleEngineSetNonCssHintOfNodeLength2 cNonCssDeclSetRef cProperty cValueType cLengthValue cLengthType  = do

  (declSet, ref) <- getSomeDeclSet3 $ fromIntegral cNonCssDeclSetRef

  let propMaker = fst (allDeclMakers !! (fromIntegral cProperty))
  let valType  = fromIntegral cValueType
  let intVal   = 0
  let textVal  = ""
  let lengthValue = cLengthValue
  let lengthType  = fromIntegral cLengthType

  let property = fromIntegral cProperty
  let cssValue :: CssValue = makeValue valType intVal textVal lengthValue lengthType
  let distance = cssValueToDistance cssValue
  let decl | property ==  7 = CssDeclarationBorderBottomWidth $ CssValueBorderWidthDistance distance
           | property == 11 = CssDeclarationBorderLeftWidth   $ CssValueBorderWidthDistance distance
           | property == 14 = CssDeclarationBorderRightWidth  $ CssValueBorderWidthDistance distance
           | property == 15 = CssDeclarationBorderSpacing     $ CssValueBorderSpacingDistance distance
           | property == 18 = CssDeclarationBorderTopWidth    $ CssValueBorderWidthDistance distance
           | property == 60 = CssDeclarationPaddingBottom     $ CssValuePadding distance
           | property == 61 = CssDeclarationPaddingLeft       $ CssValuePadding distance
           | property == 62 = CssDeclarationPaddingRight      $ CssValuePadding distance
           | property == 63 = CssDeclarationPaddingTop        $ CssValuePadding distance
           | otherwise      = trace ("[EE] Unhandled length property " ++ (show property)) (undefined)

  let newDeclSet = declarationsSetUpdateOrAdd declSet (CssDeclWrapper decl False)

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




cssValueToDistance :: CssValue -> CssDistance
cssValueToDistance value = case value of
                             CssValueTypeLengthPercent d       -> d
                             CssValueTypeLength d              -> d
                             CssValueTypeSignedLength d        -> d
                             CssValueTypeLengthPercentNumber d -> d
                             CssValueTypeAuto d                -> d  -- TODO: 'auto' appears to be handled incorrectly this function
                             otherwise                         -> CssNumericAuto 0 -- TODO: I'm not sure if this is the best 'otherwise' value



-- TODO: Add Enum class to CssValueBorderStyle, rewrite this function using
-- toEnum (see implementation of getListStyleType below).
getBorderStyle :: Int -> CssValueBorderStyle
getBorderStyle i | i ==  0 = CssValueBorderStyleNone
                 | i ==  1 = CssValueBorderStyleHidden
                 | i ==  2 = CssValueBorderStyleDotted
                 | i ==  3 = CssValueBorderStyleDashed
                 | i ==  4 = CssValueBorderStyleSolid
                 | i ==  5 = CssValueBorderStyleDouble
                 | i ==  6 = CssValueBorderStyleGroove
                 | i ==  7 = CssValueBorderStyleRidge
                 | i ==  8 = CssValueBorderStyleInset
                 | i ==  9 = CssValueBorderStyleOutset
                 | i == 10 = CssValueBorderStyleInherit
                 | otherwise = CssValueBorderStyleNone -- Not going to happen




-- '@': see https://typeclasses.com/phrasebook/enum-ranges
getListStylePosition :: Int -> CssValueListStylePosition
getListStylePosition i | i > (fromEnum (maxBound @CssValueListStylePosition)) = CssValueListStylePositionOutside
                       | i < (fromEnum (minBound @CssValueListStylePosition)) = CssValueListStylePositionOutside
                       | otherwise    = toEnum i




-- '@': see https://typeclasses.com/phrasebook/enum-ranges
getListStyleType :: Int -> CssValueListStyleType
getListStyleType i | i > (fromEnum (maxBound @CssValueListStyleType)) = CssValueListStyleTypeCircle
                   | i < (fromEnum (minBound @CssValueListStyleType)) = CssValueListStyleTypeCircle
                   | otherwise    = toEnum i




-- '@': see https://typeclasses.com/phrasebook/enum-ranges
getWhitespace :: Int -> CssValueWhitespace
getWhitespace i | i > (fromEnum (maxBound @CssValueWhitespace)) = CssValueWhitespaceNormal
                | i < (fromEnum (minBound @CssValueWhitespace)) = CssValueWhitespaceNormal
                | otherwise    = toEnum i




-- '@': see https://typeclasses.com/phrasebook/enum-ranges
getVerticalAlign :: Int -> CssValueVerticalAlign
getVerticalAlign i | i > (fromEnum (maxBound @CssValueVerticalAlign)) = CssValueVerticalAlignBaseline
                   | i < (fromEnum (minBound @CssValueVerticalAlign)) = CssValueVerticalAlignBaseline
                   | otherwise    = toEnum i




-- '@': see https://typeclasses.com/phrasebook/enum-ranges
getTextAlign :: Int -> CssValueTextAlign
getTextAlign i | i > (fromEnum (maxBound @CssValueTextAlign)) = CssValueTextAlignLeft
               | i < (fromEnum (minBound @CssValueTextAlign)) = CssValueTextAlignLeft
               | otherwise                                    = toEnum i




hll_styleEngineSetNonCssHintOfNodeEnum :: CInt -> CInt -> CInt -> IO CInt
hll_styleEngineSetNonCssHintOfNodeEnum cNonCssDeclSetRef cProperty cEnumVal = do
  (declSet, ref) <- getSomeDeclSet3 $ fromIntegral cNonCssDeclSetRef

  let property = fromIntegral cProperty
  let enumVal   = fromIntegral cEnumVal
  let decl | property ==  6 = CssDeclarationBorderBottomStyle $ getBorderStyle enumVal
           | property == 10 = CssDeclarationBorderLeftStyle   $ getBorderStyle enumVal
           | property == 13 = CssDeclarationBorderRightStyle  $ getBorderStyle enumVal
           | property == 17 = CssDeclarationBorderTopStyle    $ getBorderStyle enumVal
           | property == 44 = CssDeclarationListStylePosition $ getListStylePosition enumVal
           | property == 45 = CssDeclarationListStyleType     $ getListStyleType enumVal
           | property == 67 = CssDeclarationTextAlign         $ getTextAlign enumVal
           | property == 74 = CssDeclarationVerticalAlign     $ getVerticalAlign enumVal
           | property == 76 = CssDeclarationWhitespace        $ getWhitespace enumVal
           | otherwise      = trace ("[EE] Unhandled enum property " ++ (show property)) (undefined)

  let newDeclSet = declarationsSetUpdateOrAdd declSet (CssDeclWrapper decl False)

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




hll_styleEngineSetNonCssHintOfNodeColor :: CInt -> CInt -> CInt -> IO CInt
hll_styleEngineSetNonCssHintOfNodeColor cNonCssDeclSetRef cProperty cColor  = do

  (declSet, ref) <- getSomeDeclSet3 $ fromIntegral cNonCssDeclSetRef

  let property = fromIntegral cProperty
  let color    = fromIntegral cColor
  let decl | property ==  1 = CssDeclarationBackgroundColor $ CssValueBackgroundColorColor color
           | property == 23 = CssDeclarationColor $ CssValueColor color
           | otherwise      = trace ("Unhandled color property " ++ (show property)) (undefined)

  let newDeclSet = declarationsSetUpdateOrAdd declSet (CssDeclWrapper decl False)

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




hll_styleEngineSetNonCssHintOfNodeString :: CInt -> CInt -> CInt -> CString -> IO CInt
hll_styleEngineSetNonCssHintOfNodeString cNonCssDeclSetRef cProperty cValueType cStringVal = do

  (declSet, ref) <- getSomeDeclSet3 $ fromIntegral cNonCssDeclSetRef

  let propMaker = fst (allDeclMakers !! (fromIntegral cProperty))
  let valType  = fromIntegral cValueType
  let intVal   = 0
  stringVal <- BSU.unsafePackCString $ cStringVal
  let textVal  = T.E.decodeLatin1 stringVal
  let lengthValue = 0 -- cLengthValue
  let lengthType  = 0 -- fromIntegral cLengthType

  let cssValue :: CssValue = makeValue valType intVal textVal lengthValue lengthType
  let decl :: CssDeclWrapper = CssDeclWrapper (propMaker cssValue) False

  let newDeclSet = declarationsSetUpdateOrAdd declSet decl

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




{-
hll_styleEngineBuildUserAgentStyle :: CInt -> IO ()
hll_styleEngineBuildUserAgentStyle cRef = do

  let ref  = fromIntegral cRef
  context <- globalContextGet ref

  let context' = styleEngineBuildUserAgentStyle context

  globalContextUpdate ref context'

  return ()




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




hll_styleEngineSetBorderWidth :: CInt -> Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> Ptr FfiStyleAttrs -> IO ()
hll_styleEngineSetBorderWidth cProperty ptrStructCssValue ptrStructFontAttrs dpiX dpiY ptrStructStyleAttrs = do
  let property = fromIntegral cProperty
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs
  styleAttrs  <- peekStyleAttrs ptrStructStyleAttrs
  let borderWidth = styleBorderWidth styleAttrs

  let borderWidth' = styleEngineSetBorderWidth property value dpiX dpiY fontAttrs borderWidth
  let styleAttrs' = styleAttrs { styleBorderWidth = borderWidth' }

  pokeStyleAttrs styleAttrs' ptrStructStyleAttrs




hll_styleEngineSetBorderStyle :: CInt -> Ptr FfiCssValue -> Ptr FfiStyleAttrs -> IO ()
hll_styleEngineSetBorderStyle cProperty ptrStructCssValue ptrStructStyleAttrs = do
  let property = fromIntegral cProperty
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue

  styleAttrs  <- peekStyleAttrs ptrStructStyleAttrs
  let borderStyle  = styleBorderStyle styleAttrs

  let borderStyle' = styleEngineSetBorderStyle property value borderStyle
  let styleAttrs' = styleAttrs { styleBorderStyle = borderStyle' }

  pokeStyleAttrs styleAttrs' ptrStructStyleAttrs




hll_styleEngineSetMargin :: CInt -> Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> Ptr FfiStyleAttrs -> IO ()
hll_styleEngineSetMargin cProperty ptrStructCssValue ptrStructFontAttrs dpiX dpiY ptrStructStyleAttrs = do
  let property = fromIntegral cProperty
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs

  styleAttrs  <- peekStyleAttrs ptrStructStyleAttrs
  let margin  = styleMargin styleAttrs

  let margin' = styleEngineSetMargin property value dpiX dpiY fontAttrs margin
  let styleAttrs' = styleAttrs { styleMargin = margin' }

  pokeStyleAttrs styleAttrs' ptrStructStyleAttrs





hll_styleEngineSetPadding :: CInt -> Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> Ptr FfiStyleAttrs -> IO ()
hll_styleEngineSetPadding cProperty ptrStructCssValue ptrStructFontAttrs dpiX dpiY ptrStructStyleAttrs = do
  let property  = fromIntegral cProperty
  ffiCssValue  <- peek ptrStructCssValue
  value        <- peekCssValue ffiCssValue
  fontAttrs    <- peekFontAttrs ptrStructFontAttrs

  styleAttrs  <- peekStyleAttrs ptrStructStyleAttrs
  let padding = stylePadding styleAttrs

  let padding' = styleEngineSetPadding property value dpiX dpiY fontAttrs padding
  let styleAttrs' = styleAttrs { stylePadding = padding' }

  pokeStyleAttrs styleAttrs' ptrStructStyleAttrs




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




hll_styleEngineSetStyle :: CInt -> Ptr FfiCssValue -> Float -> Float -> Ptr FfiStyleAttrs -> IO ()
hll_styleEngineSetStyle cProperty ptrStructCssValue dpiX dpiY ptrStructStyleAttrs = do
  let property  = fromIntegral cProperty
  ffiCssValue  <- peek ptrStructCssValue
  value        <- peekCssValue ffiCssValue
  styleAttrs   <- peekStyleAttrs ptrStructStyleAttrs

  let styleAttrs' = styleEngineSetStyle property value dpiX dpiY styleAttrs

  pokeStyleAttrs styleAttrs' ptrStructStyleAttrs

  return ()
-}




hll_styleEngineApplyStyleToGivenNode :: CInt -> Ptr FfiPreferences -> Float -> Float -> Ptr FfiStyleAttrs -> Ptr FfiStyleAttrs -> IO ()
hll_styleEngineApplyStyleToGivenNode cMergedDeclSetRef ptrStructPrefs dpiX dpiY ptrStructParentStyleAttrs ptrStructStyleAttrs = do
  prefs                        <- peekPreferences ptrStructPrefs
  styleAttrs :: StyleAttrs     <- peekStyleAttrs ptrStructStyleAttrs
  parentStyleAttrs :: StyleAttrs  <- peekStyleAttrs ptrStructParentStyleAttrs
  let display :: Display = defaultDisplay { dpiX = dpiX, dpiY = dpiY }

  let mergedDeclSetRef = fromIntegral cMergedDeclSetRef

  declSet :: CssDeclarationSet <- globalDeclarationSetGet mergedDeclSetRef

  let styleAttrs' = styleEngineApplyStyleToGivenNode declSet prefs display parentStyleAttrs styleAttrs

  pokeStyleAttrs styleAttrs' ptrStructStyleAttrs

  return ()






hll_inheritNonCssHints :: CInt -> CInt -> IO CInt
hll_inheritNonCssHints cParentNonCssDeclSetRef cNonCssDeclSetRef = do
  let parentNonCssDeclSetRef = fromIntegral cParentNonCssDeclSetRef
  let nonCssDeclSetRef       = fromIntegral cNonCssDeclSetRef
  parent  <- globalDeclarationSetGet parentNonCssDeclSetRef
  current <- globalDeclarationSetGet nonCssDeclSetRef

  if parentNonCssDeclSetRef /= -1
    then
    -- Parent has some non-CSS hints. Either use parent's hints entirely or
    -- (if current node has some hints) merge current's and parent's hints.
    do
      let mCurrent = if nonCssDeclSetRef /= -1
                     then Just current
                     else Nothing
      let inheritedAndCurrent = styleEngineInheritNonCssHints parent mCurrent
      fmap fromIntegral $ globalDeclarationSetPut inheritedAndCurrent
    else
    -- There are no hints that can be inherited from parent. Return unchanged
    -- current (possibly empty/NULL).
    do
      return . fromIntegral $ nonCssDeclSetRef

{-

   if (parentNode->declLists.non_css_decl_set_ref != -1) {

      int orig_non_css_decl_set_ref = currentNode->declLists.non_css_decl_set_ref;

      currentNode->declLists.non_css_decl_set_ref = declarationListNew(parentNode->declLists.non_css_decl_set_ref); // NOTICE: copy constructo

      if (orig_non_css_decl_set_ref != -1) {// original declListNonCss have precedence
         hll_declarationListAppend(currentNode->declLists.non_css_decl_set_ref, orig_non_css_decl_set_ref);
      }

      //delete origDeclListNonCss;
   }


-}
