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
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E

import System.IO

import Debug.Trace

import Hello.Css.ContextGlobal
import Hello.Css.Distance
import Hello.Css.Parser.Property
import qualified Hello.Css.StyleEngine as SE
import Hello.Css.StyleEngineGlobal
import qualified Hello.Css.StyleNode as SN

import Hello.Display
import Hello.Dw.Style
import Hello.Dw.StyleAttrsGlobal

import Hello.Ffi.Css.Distance
import Hello.Ffi.Preferences

import Hello.Html.DoctreeNode




#include "../../hello.h"




foreign export ccall "ffiStyleEngineSetNonCssHintOfNodeLength" ffiStyleEngineSetNonCssHintOfNodeLength :: CInt -> CInt -> Float -> CInt -> IO ()
foreign export ccall "ffiStyleEngineSetNonCssHintOfNodeEnum" ffiStyleEngineSetNonCssHintOfNodeEnum :: CInt -> CInt -> CInt -> IO ()
foreign export ccall "ffiStyleEngineSetNonCssHintOfNodeString" ffiStyleEngineSetNonCssHintOfNodeString :: CInt -> CInt -> CString -> IO ()
foreign export ccall "ffiStyleEngineSetNonCssHintOfNodeColor" ffiStyleEngineSetNonCssHintOfNodeColor :: CInt -> CInt -> CInt -> IO ()

foreign export ccall "ffiStyleEngineSetXImgOfNode" ffiStyleEngineSetXImgOfNode :: CInt -> CInt -> IO ()
foreign export ccall "ffiStyleEngineSetXLangOfNode" ffiStyleEngineSetXLangOfNode :: CInt -> CString -> IO ()
foreign export ccall "ffiStyleEngineSetXLinkOfNode" ffiStyleEngineSetXLinkOfNode :: CInt -> CInt -> IO ()
foreign export ccall "ffiStyleEngineSetXTooltipOfNode" ffiStyleEngineSetXTooltipOfNode :: CInt -> CString -> IO ()

--foreign export ccall "ffiStyleEngineApplyStyleToGivenNode" ffiStyleEngineApplyStyleToGivenNode :: CInt -> Ptr FfiPreferences -> Float -> Float -> CInt -> CInt -> IO ()
foreign export ccall "ffiInheritNonCssHints" ffiInheritNonCssHints :: CInt -> IO ()

foreign export ccall "ffiStyleEngineMakeStyleAttrs" ffiStyleEngineMakeStyleAttrs :: CInt -> CInt -> CInt -> Ptr FfiPreferences -> Float -> Float -> CInt -> CInt -> IO ()

foreign export ccall "ffiStyleEnginePostprocessAttrs" ffiStyleEnginePostprocessAttrs :: CInt -> IO ()

foreign export ccall "ffiStyleEngineMakeWordStyle" ffiStyleEngineMakeWordStyle :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleEnginePreprocessAttrsInheritBackground" ffiStyleEnginePreprocessAttrsInheritBackground :: CInt -> CInt -> IO ()
foreign export ccall "ffiStyleEnginePreprocessAttrs" ffiStyleEnginePreprocessAttrs :: CInt -> IO ()
foreign export ccall "ffiStyleEngineMakeWordStyleInheritBackground" ffiStyleEngineMakeWordStyleInheritBackground :: CInt -> CInt -> IO ()

--foreign export ccall "ffiStyleEngineDoctreePushNode" ffiStyleEngineDoctreePushNode :: CInt -> CInt -> IO CInt
foreign export ccall "ffiStyleEngineDoctreePopNode" ffiStyleEngineDoctreePopNode :: CInt -> IO ()
foreign export ccall "ffiStyleEngineDoctreeGetTopNodeElementSelectorId" ffiStyleEngineDoctreeGetTopNodeElementSelectorId :: CInt -> IO CString
foreign export ccall "ffiStyleEngineDoctreeGetTopNode" ffiStyleEngineDoctreeGetTopNode :: CInt -> IO CInt
foreign export ccall "ffiStyleEngineDoctreeGetTopNodeHtmlElementIdx" ffiStyleEngineDoctreeGetTopNodeHtmlElementIdx :: CInt -> IO CInt

foreign export ccall "ffiStyleEngineDoctreeSetElementId" ffiStyleEngineDoctreeSetElementId :: CInt -> CString -> IO ()
foreign export ccall "ffiStyleEngineDoctreeSetElementClass" ffiStyleEngineDoctreeSetElementClass :: CInt -> CString -> IO ()
foreign export ccall "ffiStyleEngineDoctreeSetElementPseudoClass" ffiStyleEngineDoctreeSetElementPseudoClass :: CInt -> CString -> IO ()

foreign export ccall "ffiStyleEngineStartElement" ffiStyleEngineStartElement :: CInt -> CInt -> IO ()




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




ffiStyleEngineSetXImgOfNode :: CInt -> CInt -> IO ()
ffiStyleEngineSetXImgOfNode cEngineRef cIntVal = do

  let engineRef = fromIntegral cEngineRef
      val       = fromIntegral cIntVal

  engine <- globalStyleEngineGet engineRef
  let styleNode = SE.styleNodesStackPeek engine

  let styleNode' = SN.updateOrAddHint styleNode (CssPropertyXImg $ CssValueXImg val)
      engine'    = SE.styleNodesStackUpdateTop engine styleNode'

  globalStyleEngineUpdate engineRef engine'
  return ()




ffiStyleEngineSetXLinkOfNode :: CInt -> CInt -> IO ()
ffiStyleEngineSetXLinkOfNode cEngineRef cVal = do

  let engineRef = fromIntegral cEngineRef
      val       = fromIntegral cVal

  engine <- globalStyleEngineGet engineRef
  let styleNode = SE.styleNodesStackPeek engine

  let styleNode' = SN.updateOrAddHint styleNode (CssPropertyXLink $ CssValueXLink val)
  let engine'    = SE.styleNodesStackUpdateTop engine styleNode'

  globalStyleEngineUpdate engineRef engine'
  return ()




ffiStyleEngineSetNonCssHintOfNodeLength :: CInt -> CInt -> Float -> CInt -> IO ()
ffiStyleEngineSetNonCssHintOfNodeLength cEngineRef cProperty cLengthValue cLengthType  = do

  let engineRef = fromIntegral cEngineRef
      propertyArg :: Int = fromIntegral cProperty
      lengthValue = cLengthValue
      lengthType  = fromIntegral cLengthType

  engine <- globalStyleEngineGet engineRef
  let styleNode = SE.styleNodesStackPeek engine
  let distance  = cssLengthToDistance lengthValue lengthType

  let prop | propertyArg ==  7 = CssPropertyBorderBottomWidth $ CssValueBorderWidthDistance distance
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

  let styleNode' = SN.updateOrAddHint styleNode prop
  let engine'    = SE.styleNodesStackUpdateTop engine styleNode'
  globalStyleEngineUpdate engineRef engine'

  return ()




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




ffiStyleEngineSetNonCssHintOfNodeEnum :: CInt -> CInt -> CInt -> IO ()
ffiStyleEngineSetNonCssHintOfNodeEnum cEngineRef cProperty cEnumVal = do

  let engineRef   = fromIntegral cEngineRef
      propertyArg :: Int = fromIntegral cProperty
      enumVal     = fromIntegral cEnumVal
  engine <- globalStyleEngineGet engineRef
  let styleNode = SE.styleNodesStackPeek engine

  let prop | propertyArg ==  6 = CssPropertyBorderBottomStyle $ getBorderStyle enumVal
           | propertyArg == 10 = CssPropertyBorderLeftStyle   $ getBorderStyle enumVal
           | propertyArg == 13 = CssPropertyBorderRightStyle  $ getBorderStyle enumVal
           | propertyArg == 17 = CssPropertyBorderTopStyle    $ getBorderStyle enumVal
           | propertyArg == 44 = CssPropertyListStylePosition $ getListStylePosition enumVal
           | propertyArg == 45 = CssPropertyListStyleType     $ getListStyleType enumVal
           | propertyArg == 67 = CssPropertyTextAlign         $ getTextAlign enumVal
           | propertyArg == 74 = CssPropertyVerticalAlign     $ getVerticalAlign enumVal
           | propertyArg == 76 = CssPropertyWhitespace        $ getWhitespace enumVal
           | otherwise         = trace ("[EE] Unhandled enum propertyArg " ++ show propertyArg) undefined

  let styleNode' = SN.updateOrAddHint styleNode prop
  let engine'    = SE.styleNodesStackUpdateTop engine styleNode'
  globalStyleEngineUpdate engineRef engine'

  return ()




ffiStyleEngineSetNonCssHintOfNodeColor :: CInt -> CInt -> CInt -> IO ()
ffiStyleEngineSetNonCssHintOfNodeColor cEngineRef cProperty cColor = do

  let engineRef = fromIntegral cEngineRef
  let propertyArg :: Int = fromIntegral cProperty
  let color    = fromIntegral cColor

  engine <- globalStyleEngineGet engineRef
  let styleNode = SE.styleNodesStackPeek engine

  let prop | propertyArg ==  1 = CssPropertyBackgroundColor $ CssValueBackgroundColorColor color
           | propertyArg == 23 = CssPropertyColor $ CssValueColor color
           | otherwise          = trace ("[EE] Unhandled color propertyArg " ++ show propertyArg) undefined

  let styleNode' = SN.updateOrAddHint styleNode prop
  let engine'    = SE.styleNodesStackUpdateTop engine styleNode'
  globalStyleEngineUpdate engineRef engine'

  return ()




ffiStyleEngineSetNonCssHintOfNodeString :: CInt -> CInt -> CString -> IO ()
ffiStyleEngineSetNonCssHintOfNodeString cEngineRef cProperty cStringVal = do

  let engineRef   = fromIntegral cEngineRef
      propertyArg :: Int = fromIntegral cProperty
  textVal <- fmap T.E.decodeLatin1 (BSU.unsafePackCString cStringVal)

  engine <- globalStyleEngineGet engineRef
  let styleNode = SE.styleNodesStackPeek engine

  let prop | propertyArg == 32 = CssPropertyFontFamily $ CssValueFontFamilyList [textVal]
           | otherwise         = trace ("[EE] Unhandled string propertyArg " ++ show propertyArg) undefined

  let styleNode' = SN.updateOrAddHint styleNode prop
  let engine'    = SE.styleNodesStackUpdateTop engine styleNode'

  globalStyleEngineUpdate engineRef engine'
  return ()




ffiStyleEngineSetXLangOfNode :: CInt -> CString -> IO ()
ffiStyleEngineSetXLangOfNode cEngineRef cStringVal = do

  let engineRef = fromIntegral cEngineRef
  textVal <- fmap T.E.decodeLatin1 (BSU.unsafePackCString cStringVal)

  engine <- globalStyleEngineGet engineRef
  let styleNode = SE.styleNodesStackPeek engine

  let prop = CssPropertyXLang $ CssValueXLang textVal
  let styleNode' = SN.updateOrAddHint styleNode prop
  let engine'    = SE.styleNodesStackUpdateTop engine styleNode'

  globalStyleEngineUpdate engineRef engine'
  return ()




ffiStyleEngineSetXTooltipOfNode :: CInt -> CString -> IO ()
ffiStyleEngineSetXTooltipOfNode cEngineRef cStringVal = do

  let engineRef = fromIntegral cEngineRef
  textVal <- fmap T.E.decodeLatin1 (BSU.unsafePackCString cStringVal)

  engine <- globalStyleEngineGet engineRef
  let styleNode = SE.styleNodesStackPeek engine

  let prop = CssPropertyXTooltip $ CssValueXTooltip textVal
  let styleNode' = SN.updateOrAddHint styleNode prop
  let engine'    = SE.styleNodesStackUpdateTop engine styleNode'

  globalStyleEngineUpdate engineRef engine'

  return ()



{-
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

  let styleAttrs' = SE.styleEngineApplyStyleToGivenNode declSet prefs display parentStyleAttrs styleAttrs

  -- We changed style attrs, let's update them in our global storage too.
  globalStyleAttrsUpdate ref styleAttrs'

  return ()
-}



-- Instruct StyleEngine to use the nonCssHints from parent element This is
-- only used for tables where nonCssHints on the TABLE-element (e.g.
-- border=1) also affect child elements like TD.
ffiInheritNonCssHints :: CInt -> IO ()
ffiInheritNonCssHints cEngineRef = do
  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef

  let currentNode = SE.styleNodesStackPeek engine
      parentNode  = SE.styleNodesStackPeekParent $ engine

  if SN.hintsSize parentNode > 0
    then
    -- Parent has some non-CSS hints. Either use parent's hints entirely or
    -- (if current node has some hints) merge current's and parent's hints.
    do
      let engine' = SE.styleNodesStackUpdateTop engine (SN.inheritHints parentNode currentNode)
      globalStyleEngineUpdate engineRef engine'
      return ()
    else
    do
      return ()




ffiStyleEngineMakeStyleAttrs :: CInt -> CInt -> CInt -> Ptr FfiPreferences -> Float -> Float -> CInt -> CInt -> IO ()
ffiStyleEngineMakeStyleAttrs cEngineRef cContextRef cStyleNodeIndex ptrStructPrefs dpiXArg dpiYArg cParentStyleRef cStyleRef = do

  -- FFI, globals and debug
  engine  <- globalStyleEngineGet . fromIntegral $ cEngineRef
  context <- globalContextGet . fromIntegral $ cContextRef
  let styleNodeIndex = fromIntegral cStyleNodeIndex
  prefs <- peekPreferences ptrStructPrefs
  let display :: Display = defaultDisplay { dpiX = dpiXArg, dpiY = dpiYArg }

  let styleRef = fromIntegral cStyleRef
  styleAttrs       <- globalStyleAttrsGet styleRef
  parentStyleAttrs <- globalStyleAttrsGet . fromIntegral $ cParentStyleRef

  -- The main part. Generate style attributes for node of document specified
  -- by styleNodeIndex.
  let logs :: [String] = []
      (styleAttrs', logs') = SE.makeStyleAttrs engine context styleNodeIndex prefs display parentStyleAttrs styleAttrs logs

  -- We changed style attrs, let's update them in our global storage too.
  globalStyleAttrsUpdate styleRef styleAttrs'

  fDebugHandle <- openFile "/tmp/hello_browser_matching_rules_debug.txt" AppendMode
  _ <- sequence (fmap (hPutStr fDebugHandle) (reverse logs'))
  hClose fDebugHandle

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
  let sa' = SE.styleEnginePostprocessAttrs sa
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
    let to' = SE.styleEnginePreprocessAttrsInheritBackground to from
    globalStyleAttrsUpdate refTo to'
    return ()




ffiStyleEnginePreprocessAttrs :: CInt -> IO ()
ffiStyleEnginePreprocessAttrs cRef = do
    let ref   = fromIntegral cRef
    sa   <- globalStyleAttrsGet ref
    let sa' = SE.styleEnginePreprocessAttrs sa
    globalStyleAttrsUpdate ref sa'
    return ()




ffiStyleEngineMakeWordStyleInheritBackground :: CInt -> CInt -> IO ()
ffiStyleEngineMakeWordStyleInheritBackground cRefTo cRefFrom = do
    let refTo   = fromIntegral cRefTo
    let refFrom = fromIntegral cRefFrom
    to   <- globalStyleAttrsGet refTo
    from <- globalStyleAttrsGet refFrom
    let to' = SE.styleEngineMakeWordStyleInheritBackground to from
    globalStyleAttrsUpdate refTo to'
    return ()



{-
ffiStyleEngineDoctreePushNode :: CInt -> CInt -> IO CInt
ffiStyleEngineDoctreePushNode cEngineRef cElementIdx = do
  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  let elementIdx = fromIntegral cElementIdx

  let engine' = SE.doctreePushNode engine elementIdx
  globalStyleEngineUpdate engineRef engine'

  case SE.doctreePeekNode engine' of
    Just dtn -> return . fromIntegral . uniqueNum $ dtn
    Nothing  -> return (-1)
-}



ffiStyleEngineDoctreePopNode :: CInt -> IO ()
ffiStyleEngineDoctreePopNode cEngineRef = do
  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  let engine' = SE.doctreePopNode engine
  globalStyleEngineUpdate engineRef engine'




ffiStyleEngineDoctreeGetTopNodeHtmlElementIdx :: CInt -> IO CInt
ffiStyleEngineDoctreeGetTopNodeHtmlElementIdx cEngineRef = do
  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  case SE.doctreePeekNode engine of
    Just dtn -> return . fromIntegral . htmlElementIdx $ dtn
    Nothing  -> return 0




ffiStyleEngineDoctreeGetTopNodeElementSelectorId :: CInt -> IO CString
ffiStyleEngineDoctreeGetTopNodeElementSelectorId cEngineRef = do
  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  case SE.doctreePeekNode engine of
    Just dtn -> if T.null . selId $ dtn
                then return nullPtr -- Returning empty string ("") would confuse C++ code.
                else newCString . T.unpack . selId $ dtn
    Nothing  -> return nullPtr




ffiStyleEngineDoctreeGetTopNode :: CInt -> IO CInt
ffiStyleEngineDoctreeGetTopNode cEngineRef = do
  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  case SE.doctreePeekNode engine of
    Just dtn -> return . fromIntegral . uniqueNum $ dtn
    Nothing  -> return (-1)





ffiStyleEngineDoctreeSetElementId :: CInt -> CString -> IO ()
ffiStyleEngineDoctreeSetElementId cEngineRef cElementId = do
  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef

  stringVal     <- BSU.unsafePackCString cElementId
  let elementId  = T.E.decodeLatin1 stringVal

  let engine' = SE.doctreeSetElementIdOnTopNode engine elementId
  globalStyleEngineUpdate engineRef engine'




ffiStyleEngineDoctreeSetElementClass :: CInt -> CString -> IO ()
ffiStyleEngineDoctreeSetElementClass cEngineRef cElementClassTokens = do
  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef

  tokens        <- BSU.unsafePackCString cElementClassTokens
  -- With ' ' character as separator of selectors, we can use 'words' to
  -- get the list of selectors.
  let ws = words . Char8.unpack $ tokens
  let classSelectors = fmap T.pack ws

  let engine' = SE.doctreeSetClassOnTopNode engine classSelectors
  globalStyleEngineUpdate engineRef engine'




ffiStyleEngineDoctreeSetElementPseudoClass :: CInt -> CString -> IO ()
ffiStyleEngineDoctreeSetElementPseudoClass cEngineRef cElementPseudoClass = do
  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef

  stringVal             <- BSU.unsafePackCString cElementPseudoClass
  let elementPseudoClass = T.E.decodeLatin1 stringVal

  let engine' = SE.doctreeSetPseudoClassOnTopNode engine elementPseudoClass
  globalStyleEngineUpdate engineRef engine'




ffiStyleEngineStartElement :: CInt -> CInt -> IO ()
ffiStyleEngineStartElement cEngineRef cHtmlElementIdx = do
  let engineRef = fromIntegral cEngineRef
  engine <- globalStyleEngineGet engineRef
  let htmlElemIdx = fromIntegral cHtmlElementIdx
  let engine' = SE.startElement engine htmlElemIdx
  globalStyleEngineUpdate engineRef engine'

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


