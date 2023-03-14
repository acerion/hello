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
import qualified Data.Sequence as S
import qualified Data.Text.Encoding as T.E

import Hello.Css.Declaration
import Hello.Css.DeclarationSetsGlobal
import Hello.Css.Distance
import Hello.Css.Parser.Property
import Hello.Css.StyleEngine
import Hello.Css.StyleEngineGlobal
import Hello.Css.StyleNode

import Hello.Display
import Hello.Dw.Style
import Hello.Dw.StyleAttrsGlobal

import Hello.Ffi.Css.Distance
--import Hello.Ffi.Dw.Style
import Hello.Ffi.Preferences




#include "../../hello.h"




foreign export ccall "ffiStyleEngineSetNonCssHintOfNodeLength" ffiStyleEngineSetNonCssHintOfNodeLength :: CInt -> CInt -> Float -> CInt -> IO CInt
foreign export ccall "ffiStyleEngineSetNonCssHintOfNodeEnum" ffiStyleEngineSetNonCssHintOfNodeEnum :: CInt -> CInt -> CInt -> IO CInt
foreign export ccall "ffiStyleEngineSetNonCssHintOfNodeString" ffiStyleEngineSetNonCssHintOfNodeString :: CInt -> CInt -> CString -> IO CInt
foreign export ccall "ffiStyleEngineSetNonCssHintOfNodeColor" ffiStyleEngineSetNonCssHintOfNodeColor :: CInt -> CInt -> CInt -> IO CInt

foreign export ccall "ffiStyleEngineSetXImgOfNode" ffiStyleEngineSetXImgOfNode :: CInt -> CInt -> IO CInt
foreign export ccall "ffiStyleEngineSetXLangOfNode" ffiStyleEngineSetXLangOfNode :: CInt -> CString -> IO CInt
foreign export ccall "ffiStyleEngineSetXLinkOfNode" ffiStyleEngineSetXLinkOfNode :: CInt -> CInt -> IO CInt
foreign export ccall "ffiStyleEngineSetXTooltipOfNode" ffiStyleEngineSetXTooltipOfNode :: CInt -> CString -> IO CInt

foreign export ccall "ffiStyleEngineApplyStyleToGivenNode" ffiStyleEngineApplyStyleToGivenNode :: CInt -> Ptr FfiPreferences -> Float -> Float -> CInt -> CInt -> IO ()
foreign export ccall "ffiInheritNonCssHints" ffiInheritNonCssHints :: CInt -> IO ()

foreign export ccall "ffiStyleEnginePostprocessAttrs" ffiStyleEnginePostprocessAttrs :: CInt -> IO ()

foreign export ccall "ffiStyleEngineMakeWordStyle" ffiStyleEngineMakeWordStyle :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleEnginePreprocessAttrsInheritBackground" ffiStyleEnginePreprocessAttrsInheritBackground :: CInt -> CInt -> IO ()
foreign export ccall "ffiStyleEnginePreprocessAttrs" ffiStyleEnginePreprocessAttrs :: CInt -> IO ()
foreign export ccall "ffiStyleEngineMakeWordStyleInheritBackground" ffiStyleEngineMakeWordStyleInheritBackground :: CInt -> CInt -> IO ()




{-
getSomeDeclSet3 :: Int -> IO (CssDeclarationSet, Int)
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
-}




ffiStyleEngineSetXImgOfNode :: CInt -> CInt -> IO CInt
ffiStyleEngineSetXImgOfNode cEngineRef cIntVal = do

  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  let styleNode = styleEngineNodesStackPeek engine

  let intVal     = fromIntegral cIntVal
  let styleNode' = styleNode { nonCssDeclSet = declarationsSetUpdateOrAdd (nonCssDeclSet styleNode) (CssDeclaration (CssPropertyXImg $ CssValueXImg intVal) False) }

  let engine' = styleEngineNodesStackUpdateTop engine styleNode'
  globalStyleEngineUpdate engineRef engine'

  return 0




ffiStyleEngineSetXLinkOfNode :: CInt -> CInt -> IO CInt
ffiStyleEngineSetXLinkOfNode cEngineRef cIntVal = do

  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  let styleNode = styleEngineNodesStackPeek engine

  let intVal     = fromIntegral cIntVal
  let styleNode' = styleNode { nonCssDeclSet = declarationsSetUpdateOrAdd (nonCssDeclSet styleNode) (CssDeclaration (CssPropertyXLink $ CssValueXLink intVal) False) }

  let engine' = styleEngineNodesStackUpdateTop engine styleNode'
  globalStyleEngineUpdate engineRef engine'

  return 0




ffiStyleEngineSetNonCssHintOfNodeLength :: CInt -> CInt -> Float -> CInt -> IO CInt
ffiStyleEngineSetNonCssHintOfNodeLength cEngineRef cProperty cLengthValue cLengthType  = do

  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  let styleNode = styleEngineNodesStackPeek engine

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
           | otherwise         = trace ("[EE] Unhandled length propertyArg " ++ show propertyArg) undefined

  let styleNode' = styleNode { nonCssDeclSet = declarationsSetUpdateOrAdd (nonCssDeclSet styleNode) (CssDeclaration decl False) }

  let engine' = styleEngineNodesStackUpdateTop engine styleNode'
  globalStyleEngineUpdate engineRef engine'

  return 0




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
getListStylePosition i | i > fromEnum (maxBound @CssValueListStylePosition) = CssValueListStylePositionOutside
                       | i < fromEnum (minBound @CssValueListStylePosition) = CssValueListStylePositionOutside
                       | otherwise    = toEnum i




-- '@': see https://typeclasses.com/phrasebook/enum-ranges
getListStyleType :: Int -> CssValueListStyleType
getListStyleType i | i > fromEnum (maxBound @CssValueListStyleType) = CssValueListStyleTypeCircle
                   | i < fromEnum (minBound @CssValueListStyleType) = CssValueListStyleTypeCircle
                   | otherwise    = toEnum i




-- '@': see https://typeclasses.com/phrasebook/enum-ranges
getWhitespace :: Int -> CssValueWhitespace
getWhitespace i | i > fromEnum (maxBound @CssValueWhitespace) = CssValueWhitespaceNormal
                | i < fromEnum (minBound @CssValueWhitespace) = CssValueWhitespaceNormal
                | otherwise    = toEnum i




-- '@': see https://typeclasses.com/phrasebook/enum-ranges
getVerticalAlign :: Int -> CssValueVerticalAlign
getVerticalAlign i | i > fromEnum (maxBound @CssValueVerticalAlign) = CssValueVerticalAlignBaseline
                   | i < fromEnum (minBound @CssValueVerticalAlign) = CssValueVerticalAlignBaseline
                   | otherwise    = toEnum i




-- '@': see https://typeclasses.com/phrasebook/enum-ranges
getTextAlign :: Int -> CssValueTextAlign
getTextAlign i | i > fromEnum (maxBound @CssValueTextAlign) = CssValueTextAlignLeft
               | i < fromEnum (minBound @CssValueTextAlign) = CssValueTextAlignLeft
               | otherwise                                  = toEnum i




ffiStyleEngineSetNonCssHintOfNodeEnum :: CInt -> CInt -> CInt -> IO CInt
ffiStyleEngineSetNonCssHintOfNodeEnum cEngineRef cProperty cEnumVal = do

  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  let styleNode = styleEngineNodesStackPeek engine

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
           | otherwise         = trace ("[EE] Unhandled enum propertyArg " ++ show propertyArg) undefined

  let styleNode' = styleNode { nonCssDeclSet = declarationsSetUpdateOrAdd (nonCssDeclSet styleNode) (CssDeclaration decl False) }

  let engine' = styleEngineNodesStackUpdateTop engine styleNode'
  globalStyleEngineUpdate engineRef engine'

  return 0




ffiStyleEngineSetNonCssHintOfNodeColor :: CInt -> CInt -> CInt -> IO CInt
ffiStyleEngineSetNonCssHintOfNodeColor cEngineRef cProperty cColor  = do

  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  let styleNode = styleEngineNodesStackPeek engine

  let propertyArg :: Int = fromIntegral cProperty
  let color    = fromIntegral cColor
  let decl | propertyArg ==  1 = CssPropertyBackgroundColor $ CssValueBackgroundColorColor color
           | propertyArg == 23 = CssPropertyColor $ CssValueColor color
           | otherwise          = trace ("[EE] Unhandled color propertyArg " ++ show propertyArg) undefined

  let styleNode' = styleNode { nonCssDeclSet = declarationsSetUpdateOrAdd (nonCssDeclSet styleNode) (CssDeclaration decl False) }

  let engine' = styleEngineNodesStackUpdateTop engine styleNode'
  globalStyleEngineUpdate engineRef engine'

  return 0




ffiStyleEngineSetNonCssHintOfNodeString :: CInt -> CInt -> CString -> IO CInt
ffiStyleEngineSetNonCssHintOfNodeString cEngineRef cProperty cStringVal = do

  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  let styleNode = styleEngineNodesStackPeek engine

  textVal     <- fmap T.E.decodeLatin1 (BSU.unsafePackCString cStringVal)
  let propertyArg :: Int = fromIntegral cProperty

  let decl | propertyArg == 32 = CssPropertyFontFamily $ CssValueFontFamilyList [textVal]
           | otherwise         = trace ("[EE] Unhandled string propertyArg " ++ show propertyArg) undefined

  let styleNode' = styleNode { nonCssDeclSet = declarationsSetUpdateOrAdd (nonCssDeclSet styleNode) (CssDeclaration decl False) }

  let engine' = styleEngineNodesStackUpdateTop engine styleNode'
  globalStyleEngineUpdate engineRef engine'

  return 0




ffiStyleEngineSetXLangOfNode :: CInt -> CString -> IO CInt
ffiStyleEngineSetXLangOfNode cEngineRef cStringVal = do

  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  let styleNode = styleEngineNodesStackPeek engine

  textVal <- fmap T.E.decodeLatin1 (BSU.unsafePackCString cStringVal)
  let decl = CssPropertyXLang $ CssValueXLang textVal
  let styleNode' = styleNode { nonCssDeclSet = declarationsSetUpdateOrAdd (nonCssDeclSet styleNode) (CssDeclaration decl False) }

  let engine' = styleEngineNodesStackUpdateTop engine styleNode'
  globalStyleEngineUpdate engineRef engine'

  return 0




ffiStyleEngineSetXTooltipOfNode :: CInt -> CString -> IO CInt
ffiStyleEngineSetXTooltipOfNode cEngineRef cStringVal = do

  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  let styleNode = styleEngineNodesStackPeek engine

  textVal <- fmap T.E.decodeLatin1 (BSU.unsafePackCString cStringVal)
  let decl = CssPropertyXTooltip $ CssValueXTooltip textVal
  let styleNode' = styleNode { nonCssDeclSet = declarationsSetUpdateOrAdd (nonCssDeclSet styleNode) (CssDeclaration decl False) }

  let engine' = styleEngineNodesStackUpdateTop engine styleNode'
  globalStyleEngineUpdate engineRef engine'

  return 0




ffiStyleEngineApplyStyleToGivenNode :: CInt -> Ptr FfiPreferences -> Float -> Float -> CInt -> CInt -> IO ()
ffiStyleEngineApplyStyleToGivenNode cMergedDeclSetRef ptrStructPrefs dpiXArg dpiYArg cParentRef cRef = do

  let ref       = fromIntegral cRef
  let parentRef = fromIntegral cParentRef

  styleAttrs       <- globalStyleAttrsGet ref
  parentStyleAttrs <- globalStyleAttrsGet parentRef

  prefs                        <- peekPreferences ptrStructPrefs
  let display :: Display = defaultDisplay { dpiX = dpiXArg, dpiY = dpiYArg }

  let mergedDeclSetRef = fromIntegral cMergedDeclSetRef

  declSet :: CssDeclarationSet <- globalDeclarationSetGet mergedDeclSetRef

  let styleAttrs' = styleEngineApplyStyleToGivenNode declSet prefs display parentStyleAttrs styleAttrs

  -- We changed style attrs, let's update them in our global storage too.
  globalStyleAttrsUpdate ref styleAttrs'

  return ()




-- Instruct StyleEngine to use the nonCssHints from parent element This is
-- only used for tables where nonCssHints on the TABLE-element (e.g.
-- border=1) also affect child elements like TD.
ffiInheritNonCssHints :: CInt -> IO ()
ffiInheritNonCssHints cEngineRef = do
  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef

  let currentNode = styleEngineNodesStackPeek engine
      parent = nonCssDeclSet . styleEngineNodesStackPeekParent $ engine
      current = nonCssDeclSet currentNode

  if (S.length . items $ parent) > 0
    then
    -- Parent has some non-CSS hints. Either use parent's hints entirely or
    -- (if current node has some hints) merge current's and parent's hints.
    do
      let mCurrent = if (S.length . items $ current) > 0
                     then Just current
                     else Nothing
      let inheritedAndCurrent = styleEngineInheritNonCssHints parent mCurrent
      let engine' = styleEngineNodesStackUpdateTop engine currentNode { nonCssDeclSet = inheritedAndCurrent }
      globalStyleEngineUpdate engineRef engine'
      return ()
    else
    do
      return ()




cssLengthToDistance :: Float -> Int -> CssDistance
cssLengthToDistance lenValue lenType | lenType == cssLengthTypeNone       = CssNumericNone lenValue
                                     | lenType == cssLengthTypeMM         = CssDistanceAbsMm lenValue
                                     | lenType == cssLengthTypePX         = CssDistanceAbsPx lenValue
                                     | lenType == cssLengthTypeEM         = CssDistanceRelEm lenValue
                                     | lenType == cssLengthTypeEX         = CssDistanceRelEx lenValue
                                     | lenType == cssLengthTypePercentage = CssNumericPercentage lenValue
                                     | lenType == cssLengthTypeRelative   = CssNumericRelative lenValue
                                     | lenType == cssLengthTypeAuto       = CssDistanceAuto
                                     | otherwise                          = CssNumericNone 0.0




ffiStyleEnginePostprocessAttrs :: CInt -> IO ()
ffiStyleEnginePostprocessAttrs cRef = do
  let ref = fromIntegral cRef
  sa <- globalStyleAttrsGet ref
  let sa' = styleEnginePostprocessAttrs sa
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleEngineMakeWordStyle :: CInt -> CInt -> IO ()
ffiStyleEngineMakeWordStyle cRefAttrs cRefBwAttrs = do
  let refAttrs   = fromIntegral cRefAttrs
  let refBwAttrs = fromIntegral cRefBwAttrs
  attrs   <- globalStyleAttrsGet refAttrs
  bwAttrs <- globalStyleAttrsGet refBwAttrs
  let attrs' = attrs { styleVerticalAlign = styleVerticalAlign bwAttrs }
  globalStyleAttrsUpdate refAttrs attrs'
  return ()




ffiStyleEnginePreprocessAttrsInheritBackground :: CInt -> CInt -> IO ()
ffiStyleEnginePreprocessAttrsInheritBackground cRefTo cRefFrom = do
    let refTo   = fromIntegral cRefTo
    let refFrom = fromIntegral cRefFrom
    to   <- globalStyleAttrsGet refTo
    from <- globalStyleAttrsGet refFrom
    let to' = styleEnginePreprocessAttrsInheritBackground to from
    globalStyleAttrsUpdate refTo to'
    return ()




ffiStyleEnginePreprocessAttrs :: CInt -> IO ()
ffiStyleEnginePreprocessAttrs cRef = do
    let ref   = fromIntegral cRef
    sa   <- globalStyleAttrsGet ref
    let sa' = styleEnginePreprocessAttrs sa
    globalStyleAttrsUpdate ref sa'
    return ()




ffiStyleEngineMakeWordStyleInheritBackground :: CInt -> CInt -> IO ()
ffiStyleEngineMakeWordStyleInheritBackground cRefTo cRefFrom = do
    let refTo   = fromIntegral cRefTo
    let refFrom = fromIntegral cRefFrom
    to   <- globalStyleAttrsGet refTo
    from <- globalStyleAttrsGet refFrom
    let to' = styleEngineMakeWordStyleInheritBackground to from
    globalStyleAttrsUpdate refTo to'
    return ()




{-

   if (parentNode->declLists.non_css_decl_set_ref != -1) {

      int orig_non_css_decl_set_ref = currentNode->declLists.non_css_decl_set_ref;

      currentNode->declLists.non_css_decl_set_ref = declarationListNew(parentNode->declLists.non_css_decl_set_ref); // NOTICE: copy constructo

      if (orig_non_css_decl_set_ref != -1) {// original declListNonCss have precedence
         ffiDeclarationListAppend(currentNode->declLists.non_css_decl_set_ref, orig_non_css_decl_set_ref);
      }

      //delete origDeclListNonCss;
   }


-}




{-
ffiStyleEngineBuildUserAgentStyle :: CInt -> IO ()
ffiStyleEngineBuildUserAgentStyle cRef = do

  let ref  = fromIntegral cRef
  context <- globalContextGet ref

  let context' = styleEngineBuildUserAgentStyle context

  globalContextUpdate ref context'

  return ()




ffiStyleEngineComputeAbsoluteLengthValue :: Float -> CInt -> Ptr FfiFontAttrs -> CInt -> Float -> Float -> Ptr CInt -> IO CInt
ffiStyleEngineComputeAbsoluteLengthValue lengthValue cLengthType ptrStructFontAttrs cPercentageBase dpiX dpiY ptrOut = do
  let lengthType     = fromIntegral cLengthType
  fontAttrs         <- peekFontAttrs ptrStructFontAttrs
  let percentageBase = fromIntegral cPercentageBase
  let distance       = cssLengthToDistance lengthValue lengthType

  case computeAbsoluteLengthValue distance fontAttrs percentageBase dpiX dpiY of
    Just val -> do
      let out = round val -- TODO: a type of Float -> Int function to be verified here
      poke ptrOut (fromIntegral out)
      return 1 -- True
    Nothing -> return 0 -- False




ffiSetFontFamily :: Ptr FfiCssValue -> Ptr FfiPreferences -> Ptr FfiFontAttrs -> IO ()
ffiSetFontFamily ptrStructCssValue ptrStructPreferences ptrStructFontAttrs = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs
  prefs       <- peekPreferences ptrStructPreferences

  let fontAttrs' = styleEngineSetFontFamily value prefs fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs



ffiSetFontWeight :: Ptr FfiFontAttrs -> Ptr FfiCssValue -> IO ()
ffiSetFontWeight ptrStructFontAttrs ptrStructCssValue = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs

  let fontAttrs' = styleEngineSetFontWeight value fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs




ffiSetFontSize :: Ptr FfiCssValue -> Ptr FfiPreferences -> Float -> Float -> Ptr FfiFontAttrs -> Ptr FfiFontAttrs -> IO ()
ffiSetFontSize ptrStructCssValue ptrStructPreferences dpiX dpiY ptrStructParentFontAttrs ptrStructFontAttrs = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs
  parentFontAttrs   <- peekFontAttrs ptrStructParentFontAttrs
  prefs       <- peekPreferences ptrStructPreferences

  let fontAttrs' = styleEngineSetFontSize' value prefs dpiX dpiY parentFontAttrs fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs




ffiSetFontStyle :: Ptr FfiFontAttrs -> Ptr FfiCssValue -> IO ()
ffiSetFontStyle ptrStructFontAttrs ptrStructCssValue = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs

  let fontAttrs' = styleEngineSetFontStyle value fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs




ffiSetFontLetterSpacing :: Ptr FfiCssValue -> Float -> Float -> Ptr FfiFontAttrs -> Ptr FfiFontAttrs -> IO ()
ffiSetFontLetterSpacing ptrStructCssValue dpiX dpiY ptrStructParentFontAttrs ptrStructFontAttrs = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs
  parentFontAttrs   <- peekFontAttrs ptrStructParentFontAttrs

  let fontAttrs' = styleEngineSetLetterSpacing value dpiX dpiY parentFontAttrs fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs




ffiSetFontVariant :: Ptr FfiFontAttrs -> Ptr FfiCssValue -> IO ()
ffiSetFontVariant ptrStructFontAttrs ptrStructCssValue = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs

  let fontAttrs' = styleEngineSetFontVariant value fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs




ffiStyleEngineApplyStyleToFont :: Ptr FfiCssDeclarationSet -> Ptr FfiPreferences -> Float -> Float -> Ptr FfiFontAttrs -> Ptr FfiFontAttrs -> IO ()
ffiStyleEngineApplyStyleToFont ptrStructDeclSet ptrStructPrefs dpiX dpiY ptrStructParentFontAttrs ptrStructFontAttrs = do
  declSet         <- peekCssDeclarationSet ptrStructDeclSet
  prefs           <- peekPreferences ptrStructPrefs
  fontAttrs       <- peekFontAttrs ptrStructFontAttrs
  parentFontAttrs <- peekFontAttrs ptrStructParentFontAttrs

  let fontAttrs' = styleEngineApplyStyleToFont declSet prefs dpiX dpiY parentFontAttrs fontAttrs
  pokeFontAttrs fontAttrs' ptrStructFontAttrs





ffiStyleEngineComputeBorderWidth :: Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> IO Int
ffiStyleEngineComputeBorderWidth ptrStructCssValue ptrStructFontAttrs dpiX dpiY  = do
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs

  case styleEngineComputeBorderWidth value dpiX dpiY fontAttrs of
    Just x    -> return x
    otherwise -> return 0




ffiStyleEngineSetBorderWidth :: CInt -> Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> Ptr FfiStyleAttrs -> IO ()
ffiStyleEngineSetBorderWidth cProperty ptrStructCssValue ptrStructFontAttrs dpiX dpiY ptrStructStyleAttrs = do
  let property = fromIntegral cProperty
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs
  styleAttrs  <- peekStyleAttrs ptrStructStyleAttrs
  let borderWidth = styleBorderWidth styleAttrs

  let borderWidth' = styleEngineSetBorderWidth property value dpiX dpiY fontAttrs borderWidth
  let styleAttrs' = styleAttrs { styleBorderWidth = borderWidth' }

  pokeStyleAttrs styleAttrs' ptrStructStyleAttrs




ffiStyleEngineSetBorderStyle :: CInt -> Ptr FfiCssValue -> Ptr FfiStyleAttrs -> IO ()
ffiStyleEngineSetBorderStyle cProperty ptrStructCssValue ptrStructStyleAttrs = do
  let property = fromIntegral cProperty
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue

  styleAttrs  <- peekStyleAttrs ptrStructStyleAttrs
  let borderStyle  = styleBorderStyle styleAttrs

  let borderStyle' = styleEngineSetBorderStyle property value borderStyle
  let styleAttrs' = styleAttrs { styleBorderStyle = borderStyle' }

  pokeStyleAttrs styleAttrs' ptrStructStyleAttrs




ffiStyleEngineSetMargin :: CInt -> Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> Ptr FfiStyleAttrs -> IO ()
ffiStyleEngineSetMargin cProperty ptrStructCssValue ptrStructFontAttrs dpiX dpiY ptrStructStyleAttrs = do
  let property = fromIntegral cProperty
  ffiCssValue <- peek ptrStructCssValue
  value       <- peekCssValue ffiCssValue
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs

  styleAttrs  <- peekStyleAttrs ptrStructStyleAttrs
  let margin  = styleMargin styleAttrs

  let margin' = styleEngineSetMargin property value dpiX dpiY fontAttrs margin
  let styleAttrs' = styleAttrs { styleMargin = margin' }

  pokeStyleAttrs styleAttrs' ptrStructStyleAttrs





ffiStyleEngineSetPadding :: CInt -> Ptr FfiCssValue -> Ptr FfiFontAttrs -> Float -> Float -> Ptr FfiStyleAttrs -> IO ()
ffiStyleEngineSetPadding cProperty ptrStructCssValue ptrStructFontAttrs dpiX dpiY ptrStructStyleAttrs = do
  let property  = fromIntegral cProperty
  ffiCssValue  <- peek ptrStructCssValue
  value        <- peekCssValue ffiCssValue
  fontAttrs    <- peekFontAttrs ptrStructFontAttrs

  styleAttrs  <- peekStyleAttrs ptrStructStyleAttrs
  let padding = stylePadding styleAttrs

  let padding' = styleEngineSetPadding property value dpiX dpiY fontAttrs padding
  let styleAttrs' = styleAttrs { stylePadding = padding' }

  pokeStyleAttrs styleAttrs' ptrStructStyleAttrs




ffiComputeDwLength :: Ptr FfiDwLength -> CDouble -> CInt -> Ptr FfiFontAttrs -> Float -> Float -> IO Int
ffiComputeDwLength ptrStructDwLength cLenValue cLenType ptrStructFontAttrs dpiX dpiY = do
  let lenType  = fromIntegral cLenType
  let lenValue = cDoubleToDouble cLenValue
  let distance = cssLengthToDistance (realToFrac lenValue) lenType
  fontAttrs   <- peekFontAttrs ptrStructFontAttrs

  case styleEngineCalculateDwLength distance fontAttrs dpiX dpiY of
    Just len -> do
      pokeDwLength len ptrStructDwLength
      return 1
    Nothing -> return 0




ffiStyleEngineSetStyle :: CInt -> Ptr FfiCssValue -> Float -> Float -> Ptr FfiStyleAttrs -> IO ()
ffiStyleEngineSetStyle cProperty ptrStructCssValue dpiX dpiY ptrStructStyleAttrs = do
  let property  = fromIntegral cProperty
  ffiCssValue  <- peek ptrStructCssValue
  value        <- peekCssValue ffiCssValue
  styleAttrs   <- peekStyleAttrs ptrStructStyleAttrs

  let styleAttrs' = styleEngineSetStyle property value dpiX dpiY styleAttrs

  pokeStyleAttrs styleAttrs' ptrStructStyleAttrs

  return ()
-}




{-
ffiMakeCssDeclaration :: CInt -> Ptr FfiCssValue -> IO (Ptr FfiCssProperty)
ffiMakeCssDeclaration cProperty ptrFfiCssValue = do
  let property = fromIntegral cProperty

  ffiCssValue :: FfiCssValue <- peek ptrFfiCssValue
  cssValue <- peekCssValue ffiCssValue

  let declaration = CssProperty property cssValue False

  allocAndPokeCssDeclaration declaration
-}




{-
ffiStyleEngineSetNonCssHintOfNodeInt :: CInt -> CInt -> CInt -> CInt -> Float -> CInt -> IO CInt
ffiStyleEngineSetNonCssHintOfNodeInt cNonCssDeclSetRef cProperty cValueType cIntVal cLengthValue cLengthType  = do

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


