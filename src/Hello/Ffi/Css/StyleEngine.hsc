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
import qualified Data.ByteString.Unsafe as BSU
import Debug.Trace
import qualified Data.Text.Encoding as T.E

--import Hello.Css.ContextGlobal
import Hello.Css.Declaration
import Hello.Css.DeclarationSetsGlobal
import Hello.Css.Distance
import Hello.Css.Parser.Declaration
import Hello.Css.StyleEngine
--import Hello.Css.UserAgentStyle

import Hello.Display

--import Hello.Dw.DwLength
--import Hello.Dw.FontAttrs
import Hello.Dw.Style

--import Hello.Ffi.Css.Context
--import Hello.Ffi.Css.Parser

--import Hello.Ffi.Dw.DwLength
--import Hello.Ffi.Dw.FontAttrs
import Hello.Ffi.Dw.Style

import Hello.Ffi.Preferences
--import Hello.Ffi.Utils




#include "../../hello.h"




foreign export ccall "hll_styleEngineSetNonCssHintOfNodeLength" hll_styleEngineSetNonCssHintOfNodeLength :: CInt -> CInt -> Float -> CInt -> IO CInt
foreign export ccall "hll_styleEngineSetNonCssHintOfNodeEnum" hll_styleEngineSetNonCssHintOfNodeEnum :: CInt -> CInt -> CInt -> IO CInt
foreign export ccall "hll_styleEngineSetNonCssHintOfNodeString" hll_styleEngineSetNonCssHintOfNodeString :: CInt -> CInt -> CString -> IO CInt
foreign export ccall "hll_styleEngineSetNonCssHintOfNodeColor" hll_styleEngineSetNonCssHintOfNodeColor :: CInt -> CInt -> CInt -> IO CInt

foreign export ccall "hll_styleEngineSetXImgOfNode" hll_styleEngineSetXImgOfNode :: CInt -> CInt -> IO CInt
foreign export ccall "hll_styleEngineSetXLangOfNode" hll_styleEngineSetXLangOfNode :: CInt -> CString -> IO CInt
foreign export ccall "hll_styleEngineSetXLinkOfNode" hll_styleEngineSetXLinkOfNode :: CInt -> CInt -> IO CInt
foreign export ccall "hll_styleEngineSetXTooltipOfNode" hll_styleEngineSetXTooltipOfNode :: CInt -> CString -> IO CInt

foreign export ccall "hll_styleEngineApplyStyleToGivenNode" hll_styleEngineApplyStyleToGivenNode :: CInt -> Ptr FfiPreferences -> Float -> Float -> Ptr FfiStyleAttrs -> Ptr FfiStyleAttrs -> IO ()
foreign export ccall "hll_inheritNonCssHints" hll_inheritNonCssHints :: CInt -> CInt -> IO CInt




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




hll_styleEngineSetXImgOfNode :: CInt -> CInt -> IO CInt
hll_styleEngineSetXImgOfNode cNonCssDeclSetRef cIntVal = do

  (declSet, ref) <- getSomeDeclSet3 $ fromIntegral cNonCssDeclSetRef

  let intVal     = fromIntegral cIntVal
  let newDeclSet = declarationsSetUpdateOrAdd declSet (CssDeclaration (CssPropertyXImg $ CssValueXImg intVal) False)

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




hll_styleEngineSetXLinkOfNode :: CInt -> CInt -> IO CInt
hll_styleEngineSetXLinkOfNode cNonCssDeclSetRef cIntVal = do

  (declSet, ref) <- getSomeDeclSet3 $ fromIntegral cNonCssDeclSetRef

  let intVal     = fromIntegral cIntVal
  let newDeclSet = declarationsSetUpdateOrAdd declSet (CssDeclaration (CssPropertyXLink $ CssValueXLink intVal) False)

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




hll_styleEngineSetNonCssHintOfNodeLength :: CInt -> CInt -> Float -> CInt -> IO CInt
hll_styleEngineSetNonCssHintOfNodeLength cNonCssDeclSetRef cProperty cLengthValue cLengthType  = do

  (declSet, ref) <- getSomeDeclSet3 $ fromIntegral cNonCssDeclSetRef

  let lengthValue = cLengthValue
  let lengthType  = fromIntegral cLengthType
  let propertyArg :: Int = fromIntegral cProperty
  let distance    = cssLengthToDistance lengthValue lengthType

  let decl | propertyArg ==  7 = CssPropertyBorderBottomWidth $ CssValueBorderWidthDistance distance
           | propertyArg == 11 = CssPropertyBorderLeftWidth   $ CssValueBorderWidthDistance distance
           | propertyArg == 14 = CssPropertyBorderRightWidth  $ CssValueBorderWidthDistance distance
           | propertyArg == 15 = CssPropertyBorderSpacing     $ CssValueBorderSpacingDistance distance
           | propertyArg == 18 = CssPropertyBorderTopWidth    $ CssValueBorderWidthDistance distance
           | propertyArg == 39 = CssPropertyHeight            $ CssValueHeightDistance distance

           | propertyArg == 49 = CssPropertyMarginTop         $ CssValueMarginXDistance distance
           | propertyArg == 48 = CssPropertyMarginRight       $ CssValueMarginXDistance distance
           | propertyArg == 46 = CssPropertyMarginBottom      $ CssValueMarginXDistance distance
           | propertyArg == 47 = CssPropertyMarginLeft        $ CssValueMarginXDistance distance

           | propertyArg == 60 = CssPropertyPaddingBottom     $ CssValuePaddingX distance
           | propertyArg == 61 = CssPropertyPaddingLeft       $ CssValuePaddingX distance
           | propertyArg == 62 = CssPropertyPaddingRight      $ CssValuePaddingX distance
           | propertyArg == 63 = CssPropertyPaddingTop        $ CssValuePaddingX distance
           | propertyArg == 77 = CssPropertyWidth             $ CssValueWidthDistance distance
           | otherwise         = trace ("[EE] Unhandled length propertyArg " ++ (show propertyArg)) (undefined)

  let newDeclSet = declarationsSetUpdateOrAdd declSet (CssDeclaration decl False)

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




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

  let propertyArg :: Int = fromIntegral cProperty
  let enumVal   = fromIntegral cEnumVal
  let decl | propertyArg ==  6 = CssPropertyBorderBottomStyle $ getBorderStyle enumVal
           | propertyArg == 10 = CssPropertyBorderLeftStyle   $ getBorderStyle enumVal
           | propertyArg == 13 = CssPropertyBorderRightStyle  $ getBorderStyle enumVal
           | propertyArg == 17 = CssPropertyBorderTopStyle    $ getBorderStyle enumVal
           | propertyArg == 44 = CssPropertyListStylePosition $ getListStylePosition enumVal
           | propertyArg == 45 = CssPropertyListStyleType     $ getListStyleType enumVal
           | propertyArg == 67 = CssPropertyTextAlign         $ getTextAlign enumVal
           | propertyArg == 74 = CssPropertyVerticalAlign     $ getVerticalAlign enumVal
           | propertyArg == 76 = CssPropertyWhitespace        $ getWhitespace enumVal
           | otherwise         = trace ("[EE] Unhandled enum propertyArg " ++ (show propertyArg)) (undefined)

  let newDeclSet = declarationsSetUpdateOrAdd declSet (CssDeclaration decl False)

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




hll_styleEngineSetNonCssHintOfNodeColor :: CInt -> CInt -> CInt -> IO CInt
hll_styleEngineSetNonCssHintOfNodeColor cNonCssDeclSetRef cProperty cColor  = do

  (declSet, ref) <- getSomeDeclSet3 $ fromIntegral cNonCssDeclSetRef

  let propertyArg :: Int = fromIntegral cProperty
  let color    = fromIntegral cColor
  let decl | propertyArg ==  1 = CssPropertyBackgroundColor $ CssValueBackgroundColorColor color
           | propertyArg == 23 = CssPropertyColor $ CssValueColor color
           | otherwise          = trace ("[EE] Unhandled color propertyArg " ++ (show propertyArg)) (undefined)

  let newDeclSet = declarationsSetUpdateOrAdd declSet (CssDeclaration decl False)

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




hll_styleEngineSetNonCssHintOfNodeString :: CInt -> CInt -> CString -> IO CInt
hll_styleEngineSetNonCssHintOfNodeString cNonCssDeclSetRef cProperty cStringVal = do

  (declSet, ref) <- getSomeDeclSet3 $ fromIntegral cNonCssDeclSetRef

  textVal     <- fmap T.E.decodeLatin1 (BSU.unsafePackCString cStringVal)
  let propertyArg :: Int = fromIntegral cProperty

  let decl | propertyArg == 32 = CssPropertyFontFamily $ CssValueFontFamilyList [textVal]
           | otherwise         = trace ("[EE] Unhandled string propertyArg " ++ (show propertyArg)) (undefined)

  let newDeclSet = declarationsSetUpdateOrAdd declSet (CssDeclaration decl False)

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




hll_styleEngineSetXLangOfNode :: CInt -> CString -> IO CInt
hll_styleEngineSetXLangOfNode cNonCssDeclSetRef cStringVal = do

  (declSet, ref) <- getSomeDeclSet3 $ fromIntegral cNonCssDeclSetRef

  textVal <- fmap T.E.decodeLatin1 (BSU.unsafePackCString cStringVal)
  let decl = CssPropertyXLang $ CssValueXLang textVal
  let newDeclSet = declarationsSetUpdateOrAdd declSet (CssDeclaration decl False)

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




hll_styleEngineSetXTooltipOfNode :: CInt -> CString -> IO CInt
hll_styleEngineSetXTooltipOfNode cNonCssDeclSetRef cStringVal = do

  (declSet, ref) <- getSomeDeclSet3 $ fromIntegral cNonCssDeclSetRef

  textVal <- fmap T.E.decodeLatin1 (BSU.unsafePackCString cStringVal)
  let decl = CssPropertyXTooltip $ CssValueXTooltip textVal
  let newDeclSet = declarationsSetUpdateOrAdd declSet (CssDeclaration decl False)

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref




hll_styleEngineApplyStyleToGivenNode :: CInt -> Ptr FfiPreferences -> Float -> Float -> Ptr FfiStyleAttrs -> Ptr FfiStyleAttrs -> IO ()
hll_styleEngineApplyStyleToGivenNode cMergedDeclSetRef ptrStructPrefs dpiXArg dpiYArg ptrStructParentStyleAttrs ptrStructStyleAttrs = do
  prefs                        <- peekPreferences ptrStructPrefs
  styleAttrs :: StyleAttrs     <- peekStyleAttrs ptrStructStyleAttrs
  parentStyleAttrs :: StyleAttrs  <- peekStyleAttrs ptrStructParentStyleAttrs
  let display :: Display = defaultDisplay { dpiX = dpiXArg, dpiY = dpiYArg }

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




{-
hll_makeCssDeclaration :: CInt -> Ptr FfiCssValue -> IO (Ptr FfiCssProperty)
hll_makeCssDeclaration cProperty ptrFfiCssValue = do
  let property = fromIntegral cProperty

  ffiCssValue :: FfiCssValue <- peek ptrFfiCssValue
  cssValue <- peekCssValue ffiCssValue

  let declaration = CssProperty property cssValue False

  allocAndPokeCssDeclaration declaration
-}




{-
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
  let decl :: CssDeclaration = CssDeclaration (propMaker cssValue) False

  let newDeclSet = declarationsSetUpdateOrAdd declSet decl

  globalDeclarationSetUpdate ref newDeclSet

  return . fromIntegral $ ref
-}


