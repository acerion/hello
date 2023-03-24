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




{-# LANGUAGE ScopedTypeVariables #-}




module Hello.Ffi.Dw.StyleAttrsGlobal
  (
  )
where




import Prelude
import Foreign.C.Types
import Foreign.C.String -- castCCharToChar
import Foreign

--import Debug.Trace

import qualified Data.Text as T

import Hello.Dw.Style
import Hello.Dw.StyleAttrsGlobal
import Hello.Ffi.Dw.DwLength
import Hello.Ffi.Dw.FontAttrs
import Hello.Ffi.Dw.Style
import Hello.Ffi.Utils




#include "../../hello.h"




foreign export ccall "ffiStyleAttrsCtor" ffiStyleAttrsCtor :: IO CInt
foreign export ccall "ffiStyleAttrsCopyCtor" ffiStyleAttrsCopyCtor :: CInt -> IO CInt


foreign export ccall "ffiStyleAttrsInitValues" ffiStyleAttrsInitValues :: CInt -> IO ()
foreign export ccall "ffiStyleAttrsEqual" ffiStyleAttrsEqual :: CInt -> CInt -> IO Bool
foreign export ccall "ffiStyleAttrsHashValue" ffiStyleAttrsHashValue :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsCopy" ffiStyleAttrsCopy :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsResetNonInheritedValues" ffiStyleAttrsResetNonInheritedValues :: CInt -> IO ()

foreign export ccall "ffiStyleAttrsTextAlign" ffiStyleAttrsTextAlign :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsTextDecoration" ffiStyleAttrsTextDecoration :: CInt -> IO Word32
foreign export ccall "ffiStyleAttrsSetTextDecoration" ffiStyleAttrsSetTextDecoration :: CInt -> Word32 -> IO ()

foreign export ccall "ffiStyleAttrsTextTransform" ffiStyleAttrsTextTransform :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsCursor" ffiStyleAttrsCursor :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsSetCursor" ffiStyleAttrsSetCursor :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsWhiteSpace" ffiStyleAttrsWhiteSpace :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsListStylePosition" ffiStyleAttrsListStylePosition :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsListStyleType" ffiStyleAttrsListStyleType :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsXLink" ffiStyleAttrsXLink :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsSetXLink" ffiStyleAttrsSetXLink :: CInt -> CInt -> IO ()
foreign export ccall "ffiStyleAttrsXImg" ffiStyleAttrsXImg :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsBorderCollapse" ffiStyleAttrsBorderCollapse :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsSetCollapseTableAttrs" ffiStyleAttrsSetCollapseTableAttrs :: CInt -> CInt -> CInt -> IO ()
foreign export ccall "ffiStyleAttrsSetCollapseCellAttrs" ffiStyleAttrsSetCollapseCellAttrs :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsBorderStyleTop" ffiStyleAttrsBorderStyleTop :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsBorderStyleRight" ffiStyleAttrsBorderStyleRight :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsBorderStyleBottom" ffiStyleAttrsBorderStyleBottom :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsBorderStyleLeft" ffiStyleAttrsBorderStyleLeft :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsSetBorderStyle" ffiStyleAttrsSetBorderStyle :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsGetWidth" ffiStyleAttrsGetWidth :: CInt -> Ptr FfiDwLength -> IO ()
foreign export ccall "ffiStyleAttrsGetHeight" ffiStyleAttrsGetHeight :: CInt -> Ptr FfiDwLength -> IO ()

foreign export ccall "ffiStyleAttrsSetWidth" ffiStyleAttrsSetWidth :: CInt -> Ptr FfiDwLength -> IO ()
foreign export ccall "ffiStyleAttrsSetHeight" ffiStyleAttrsSetHeight :: CInt -> Ptr FfiDwLength -> IO ()

foreign export ccall "ffiStyleAttrsGetTextIndent" ffiStyleAttrsGetTextIndent :: CInt -> Ptr FfiDwLength -> IO ()

foreign export ccall "ffiStyleAttrsGetLineHeight" ffiStyleAttrsGetLineHeight :: CInt -> Ptr FfiDwLength -> IO ()

foreign export ccall "ffiStyleAttrsVerticalAlign" ffiStyleAttrsVerticalAlign :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsBgPositionX" ffiStyleAttrsBgPositionX :: CInt -> Ptr FfiDwLength -> IO ()
foreign export ccall "ffiStyleAttrsBgPositionY" ffiStyleAttrsBgPositionY :: CInt -> Ptr FfiDwLength -> IO ()

foreign export ccall "ffiStyleAttrsSetBgPositionX" ffiStyleAttrsSetBgPositionX :: CInt -> Ptr FfiDwLength -> IO ()
foreign export ccall "ffiStyleAttrsSetBgPositionY" ffiStyleAttrsSetBgPositionY :: CInt -> Ptr FfiDwLength -> IO ()

foreign export ccall "ffiStyleAttrsHorizBorderSpacing" ffiStyleAttrsHorizBorderSpacing :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsVertBorderSpacing" ffiStyleAttrsVertBorderSpacing :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsSetHorizBorderSpacing" ffiStyleAttrsSetHorizBorderSpacing :: CInt -> CInt -> IO ()
foreign export ccall "ffiStyleAttrsSetVertBorderSpacing" ffiStyleAttrsSetVertBorderSpacing :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsDisplay" ffiStyleAttrsDisplay :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsWordSpacing" ffiStyleAttrsWordSpacing :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsXTooltip" ffiStyleAttrsXTooltip :: CInt -> IO (Ptr CChar)

foreign export ccall "ffiStyleAttrsXLang" ffiStyleAttrsXLang :: CInt -> Ptr CChar -> CInt -> IO ()
foreign export ccall "ffiStyleAttrsSetXLang" ffiStyleAttrsSetXLang :: CInt -> CChar -> CChar -> IO ()

foreign export ccall "ffiStyleAttrsBgRepeat" ffiStyleAttrsBgRepeat :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsSetBgRepeat" ffiStyleAttrsSetBgRepeat :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsBgAttachment" ffiStyleAttrsBgAttachment :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsMargin" ffiStyleAttrsMargin :: CInt -> Ptr FfiStyleMargin -> IO ()
foreign export ccall "ffiStyleAttrsSetMargin" ffiStyleAttrsSetMargin :: CInt -> CInt -> IO ()
foreign export ccall "ffiStyleAttrsSetMargin2" ffiStyleAttrsSetMargin2 :: CInt -> Ptr FfiStyleMargin -> IO ()

foreign export ccall "ffiStyleAttrsPadding" ffiStyleAttrsPadding :: CInt -> Ptr FfiStylePadding -> IO ()
foreign export ccall "ffiStyleAttrsSetPadding" ffiStyleAttrsSetPadding :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsBorderWidth" ffiStyleAttrsBorderWidth :: CInt -> Ptr FfiStyleBorderWidth -> IO ()
foreign export ccall "ffiStyleAttrsSetBorderWidth" ffiStyleAttrsSetBorderWidth :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsBoxOffsetX" ffiStyleAttrsBoxOffsetX :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsBoxOffsetY" ffiStyleAttrsBoxOffsetY :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsBoxRestWidth" ffiStyleAttrsBoxRestWidth :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsBoxRestHeight" ffiStyleAttrsBoxRestHeight :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsBoxDiffWidth" ffiStyleAttrsBoxDiffWidth :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsBoxDiffHeight" ffiStyleAttrsBoxDiffHeight :: CInt -> IO CInt

foreign export ccall "ffiStyleAttrsColor" ffiStyleAttrsColor :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsSetColor" ffiStyleAttrsSetColor :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsBackgroundColor" ffiStyleAttrsBackgroundColor :: CInt -> IO CInt
foreign export ccall "ffiStyleAttrsSetBackgroundColor" ffiStyleAttrsSetBackgroundColor :: CInt -> CInt -> IO ()

foreign export ccall "ffiStyleAttrsBorderColor" ffiStyleAttrsBorderColor :: CInt -> Ptr FfiStyleBorderColor -> IO ()
foreign export ccall "ffiStyleAttrsSetBorderColor" ffiStyleAttrsSetBorderColor :: CInt -> CInt -> IO ()
foreign export ccall "ffiStyleAttrsSetBorderColor2" ffiStyleAttrsSetBorderColor2 :: CInt -> Ptr FfiStyleBorderColor -> IO ()

foreign export ccall "ffiStyleAttrsFontAttrs" ffiStyleAttrsFontAttrs :: CInt -> Ptr FfiFontAttrs -> IO ()
foreign export ccall "ffiStyleAttrsSetFontAttrs" ffiStyleAttrsSetFontAttrs :: CInt -> Ptr FfiFontAttrs -> IO ()

foreign export ccall "ffiStyleAttrsBgImage" ffiStyleAttrsBgImage :: CInt -> IO (Ptr CChar)




ffiStyleAttrsCtor :: IO CInt
ffiStyleAttrsCtor = do
  ref :: Int <- fmap fromIntegral globalStyleAttrsCtor
  return . fromIntegral $ ref




-- A copy constructor.
-- Construct a new style attribute from given style attribute.
ffiStyleAttrsCopyCtor :: CInt -> IO CInt
ffiStyleAttrsCopyCtor cExistingRef = do
  fmap fromIntegral $ globalStyleAttrsCopyCtor . fromIntegral $ cExistingRef




ffiStyleAttrsInitValues :: CInt -> IO ()
ffiStyleAttrsInitValues cRef = do
  let ref = fromIntegral cRef
  old <- globalStyleAttrsGet ref
  let sa' = styleAttrsInitValues old
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsEqual :: CInt -> CInt -> IO Bool
ffiStyleAttrsEqual cRef1 cRef2 = do
  attrs1 <- globalStyleAttrsGet . fromIntegral $ cRef1
  attrs2 <- globalStyleAttrsGet . fromIntegral $ cRef2
  return $ attrs1 == attrs2




ffiStyleAttrsHashValue :: CInt -> IO CInt
ffiStyleAttrsHashValue cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleAttrsHashValue $ attrs




ffiStyleAttrsCopy :: CInt -> CInt -> IO ()
ffiStyleAttrsCopy cRefTo cRefFrom = do
  attrsFrom <- globalStyleAttrsGet . fromIntegral $ cRefFrom
  globalStyleAttrsUpdate (fromIntegral cRefTo) attrsFrom
  return ()




ffiStyleAttrsTextAlign :: CInt -> IO CInt
ffiStyleAttrsTextAlign cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleTextAlign $ attrs




ffiStyleAttrsTextDecoration :: CInt -> IO Word32
ffiStyleAttrsTextDecoration cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleTextDecoration $ attrs




ffiStyleAttrsSetTextDecoration :: CInt -> Word32 -> IO ()
ffiStyleAttrsSetTextDecoration cRef val = do
  let ref = fromIntegral cRef
  old <- globalStyleAttrsGet ref
  let sa' = old { styleTextDecoration = val }
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsTextTransform :: CInt -> IO CInt
ffiStyleAttrsTextTransform cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleTextTransform $ attrs




ffiStyleAttrsCursor :: CInt -> IO CInt
ffiStyleAttrsCursor cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleCursor $ attrs




ffiStyleAttrsSetCursor :: CInt -> CInt -> IO ()
ffiStyleAttrsSetCursor cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let sa' = old { styleCursor = val }
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsWhiteSpace :: CInt -> IO CInt
ffiStyleAttrsWhiteSpace cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleWhiteSpace $ attrs




ffiStyleAttrsListStylePosition :: CInt -> IO CInt
ffiStyleAttrsListStylePosition cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleListStylePosition $ attrs




ffiStyleAttrsListStyleType :: CInt -> IO CInt
ffiStyleAttrsListStyleType cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleListStyleType $ attrs




ffiStyleAttrsXLink :: CInt -> IO CInt
ffiStyleAttrsXLink cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleXLink $ attrs




ffiStyleAttrsSetXLink :: CInt -> CInt -> IO ()
ffiStyleAttrsSetXLink cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let sa' = old { styleXLink = val }
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsXImg :: CInt -> IO CInt
ffiStyleAttrsXImg cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleXImg $ attrs




ffiStyleAttrsBorderCollapse :: CInt -> IO CInt
ffiStyleAttrsBorderCollapse cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleBorderCollapse $ attrs




ffiStyleAttrsSetCollapseTableAttrs :: CInt -> CInt -> CInt -> IO ()
ffiStyleAttrsSetCollapseTableAttrs cRefTable cRefCell cBorderWidthTop = do
  let refTable = fromIntegral cRefTable
  let refCell  = fromIntegral cRefCell
  let borderWidthTop = fromIntegral cBorderWidthTop
  attrsTable <- globalStyleAttrsGet refTable
  attrsCell  <- globalStyleAttrsGet refCell
  let attrs = styleAttrsSetCollapseTableAttrs attrsTable attrsCell borderWidthTop
  globalStyleAttrsUpdate refTable attrs
  return ()




ffiStyleAttrsSetCollapseCellAttrs :: CInt -> CInt -> IO ()
ffiStyleAttrsSetCollapseCellAttrs cRef cBorderWidthTop = do
  let ref = fromIntegral cRef
  let borderWidthTop = fromIntegral cBorderWidthTop
  attrs <- globalStyleAttrsGet ref
  let attrs' = styleAttrsSetCollapseCellAttrs attrs borderWidthTop
  globalStyleAttrsUpdate ref attrs'
  return ()




ffiStyleAttrsBorderStyleTop :: CInt -> IO CInt
ffiStyleAttrsBorderStyleTop cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleBorderStyleTop . styleBorderStyle $ attrs




ffiStyleAttrsBorderStyleRight :: CInt -> IO CInt
ffiStyleAttrsBorderStyleRight cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleBorderStyleRight . styleBorderStyle $ attrs




ffiStyleAttrsBorderStyleBottom :: CInt -> IO CInt
ffiStyleAttrsBorderStyleBottom cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleBorderStyleBottom . styleBorderStyle $ attrs




ffiStyleAttrsBorderStyleLeft :: CInt -> IO CInt
ffiStyleAttrsBorderStyleLeft cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleBorderStyleLeft . styleBorderStyle $ attrs




ffiStyleAttrsSetBorderStyle :: CInt -> CInt -> IO ()
ffiStyleAttrsSetBorderStyle cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let sa' = styleAttrsSetBorderStyle old val
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsResetNonInheritedValues :: CInt -> IO ()
ffiStyleAttrsResetNonInheritedValues cRef = do
  let ref = fromIntegral cRef
  old <- globalStyleAttrsGet ref
  let sa' = styleAttrsResetNonInheritedValues old
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsGetWidth :: CInt -> Ptr FfiDwLength -> IO ()
ffiStyleAttrsGetWidth cRef ptrStructDwLength = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  pokeDwLength (styleWidth sa) ptrStructDwLength
  return ()




ffiStyleAttrsGetHeight :: CInt -> Ptr FfiDwLength -> IO ()
ffiStyleAttrsGetHeight cRef ptrStructDwLength = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  pokeDwLength (styleHeight sa) ptrStructDwLength
  return ()




ffiStyleAttrsSetWidth :: CInt -> Ptr FfiDwLength -> IO ()
ffiStyleAttrsSetWidth cRef ptrStructDwLength = do
  let ref = fromIntegral cRef
  sa <- globalStyleAttrsGet ref
  len <- peekDwLength ptrStructDwLength
  let sa' = sa { styleWidth = len }
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsSetHeight :: CInt -> Ptr FfiDwLength -> IO ()
ffiStyleAttrsSetHeight cRef ptrStructDwLength = do
  let ref = fromIntegral cRef
  sa <- globalStyleAttrsGet ref
  len <- peekDwLength ptrStructDwLength
  let sa' = sa { styleHeight = len }
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsGetTextIndent :: CInt -> Ptr FfiDwLength -> IO ()
ffiStyleAttrsGetTextIndent cRef ptrStructDwLength = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  pokeDwLength (styleTextIndent sa) ptrStructDwLength
  return ()




ffiStyleAttrsGetLineHeight :: CInt -> Ptr FfiDwLength -> IO ()
ffiStyleAttrsGetLineHeight cRef ptrStructDwLength = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  pokeDwLength (styleLineHeight sa) ptrStructDwLength
  return ()




ffiStyleAttrsVerticalAlign :: CInt -> IO CInt
ffiStyleAttrsVerticalAlign cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleVerticalAlign $ attrs




ffiStyleAttrsBgPositionX :: CInt -> Ptr FfiDwLength -> IO ()
ffiStyleAttrsBgPositionX cRef ptrStructDwLength = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  pokeDwLength (styleBgPositionX sa) ptrStructDwLength
  return ()




ffiStyleAttrsBgPositionY :: CInt -> Ptr FfiDwLength -> IO ()
ffiStyleAttrsBgPositionY cRef ptrStructDwLength = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  pokeDwLength (styleBgPositionY sa) ptrStructDwLength
  return ()




ffiStyleAttrsSetBgPositionX :: CInt -> Ptr FfiDwLength -> IO ()
ffiStyleAttrsSetBgPositionX cRef ptrStructDwLength = do
  let ref = fromIntegral cRef
  sa <- globalStyleAttrsGet ref
  len <- peekDwLength ptrStructDwLength
  let sa' = sa { styleBgPositionX = len }
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsSetBgPositionY :: CInt -> Ptr FfiDwLength -> IO ()
ffiStyleAttrsSetBgPositionY cRef ptrStructDwLength = do
  let ref = fromIntegral cRef
  sa <- globalStyleAttrsGet ref
  len <- peekDwLength ptrStructDwLength
  let sa' = sa { styleBgPositionY = len }
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsHorizBorderSpacing :: CInt -> IO CInt
ffiStyleAttrsHorizBorderSpacing cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleHorizBorderSpacing $ attrs




ffiStyleAttrsVertBorderSpacing :: CInt -> IO CInt
ffiStyleAttrsVertBorderSpacing cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleVertBorderSpacing $ attrs




ffiStyleAttrsSetHorizBorderSpacing :: CInt -> CInt -> IO ()
ffiStyleAttrsSetHorizBorderSpacing cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let sa' = old { styleHorizBorderSpacing = val }
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsSetVertBorderSpacing :: CInt -> CInt -> IO ()
ffiStyleAttrsSetVertBorderSpacing cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let sa' = old { styleVertBorderSpacing = val }
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsDisplay :: CInt -> IO CInt
ffiStyleAttrsDisplay cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleDisplay $ attrs




ffiStyleAttrsWordSpacing :: CInt -> IO CInt
ffiStyleAttrsWordSpacing cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleWordSpacing $ attrs



-- Returns a pointer to string allocated on heap.
--
-- TODO: the function probably should return NULL pointer if attribute is an
-- empty string.
--
-- FIXME: returned pointer is probably not deallocated anywhere.
ffiStyleAttrsXTooltip :: CInt -> IO (Ptr CChar)
ffiStyleAttrsXTooltip cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  allocAndPokeCString . styleXTooltip $ attrs




ffiStyleAttrsXLang :: CInt -> Ptr CChar -> CInt -> IO ()
ffiStyleAttrsXLang cRef ptrBufLang cBufSize = do
  let bufSize = fromIntegral cBufSize
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  pokeCharBuffer ptrBufLang bufSize (styleXLang attrs)




ffiStyleAttrsSetXLang :: CInt -> CChar -> CChar -> IO ()
ffiStyleAttrsSetXLang cRef cChar1 cChar2 = do
  let ref = fromIntegral cRef
  old <- globalStyleAttrsGet ref
  let sa' = old { styleXLang = T.pack [castCCharToChar cChar1, castCCharToChar cChar2] }
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsBgRepeat :: CInt -> IO CInt
ffiStyleAttrsBgRepeat cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleBgRepeat $ attrs




ffiStyleAttrsSetBgRepeat :: CInt -> CInt -> IO ()
ffiStyleAttrsSetBgRepeat cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let sa' = old { styleBgRepeat = val }
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsBgAttachment :: CInt -> IO CInt
ffiStyleAttrsBgAttachment cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleBgAttachment $ attrs




ffiStyleAttrsMargin :: CInt -> Ptr FfiStyleMargin -> IO ()
ffiStyleAttrsMargin cRef ptrStruct = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  pokeStyleMargin (styleMargin attrs) ptrStruct
  return ()




ffiStyleAttrsSetMargin :: CInt -> CInt -> IO ()
ffiStyleAttrsSetMargin cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  attrs <- globalStyleAttrsGet ref
  let attrs' = attrs { styleMargin = StyleMargin
                       {
                         styleMarginTop    = val
                       , styleMarginRight  = val
                       , styleMarginBottom = val
                       , styleMarginLeft   = val
                       }
                     }
  globalStyleAttrsUpdate ref attrs'
  return ()




ffiStyleAttrsSetMargin2 :: CInt -> Ptr FfiStyleMargin -> IO ()
ffiStyleAttrsSetMargin2 cRef ptrStruct = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  margin <- peekStyleMargin ptrStruct
  let attrs' = attrs { styleMargin = margin }
  globalStyleAttrsUpdate ref attrs'
  return ()




ffiStyleAttrsPadding :: CInt -> Ptr FfiStylePadding -> IO ()
ffiStyleAttrsPadding cRef ptrStruct = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  pokeStylePadding (stylePadding attrs) ptrStruct
  return ()




ffiStyleAttrsSetPadding :: CInt -> CInt -> IO ()
ffiStyleAttrsSetPadding cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  attrs <- globalStyleAttrsGet ref
  let attrs' = attrs { stylePadding = StylePadding
                       {
                         stylePaddingTop    = val
                       , stylePaddingRight  = val
                       , stylePaddingBottom = val
                       , stylePaddingLeft   = val
                       }
                     }
  globalStyleAttrsUpdate ref attrs'
  return ()




ffiStyleAttrsBorderWidth :: CInt -> Ptr FfiStyleBorderWidth -> IO ()
ffiStyleAttrsBorderWidth cRef ptrStruct = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  pokeStyleBorderWidth (styleBorderWidth attrs) ptrStruct
  return ()




ffiStyleAttrsSetBorderWidth :: CInt -> CInt -> IO ()
ffiStyleAttrsSetBorderWidth cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  attrs <- globalStyleAttrsGet ref
  let attrs' = attrs { styleBorderWidth = StyleBorderWidth
                       {
                         styleBorderWidthTop    = val
                       , styleBorderWidthRight  = val
                       , styleBorderWidthBottom = val
                       , styleBorderWidthLeft   = val
                       }
                     }
  globalStyleAttrsUpdate ref attrs'
  return ()





ffiStyleAttrsBoxOffsetX :: CInt -> IO CInt
ffiStyleAttrsBoxOffsetX cRef = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleAttrsBoxOffsetX $ sa




ffiStyleAttrsBoxOffsetY :: CInt -> IO CInt
ffiStyleAttrsBoxOffsetY cRef = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleAttrsBoxOffsetY $ sa




ffiStyleAttrsBoxRestWidth :: CInt -> IO CInt
ffiStyleAttrsBoxRestWidth cRef = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleAttrsBoxRestWidth $ sa




ffiStyleAttrsBoxRestHeight :: CInt -> IO CInt
ffiStyleAttrsBoxRestHeight cRef = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleAttrsBoxRestHeight $ sa




ffiStyleAttrsBoxDiffWidth :: CInt -> IO CInt
ffiStyleAttrsBoxDiffWidth cRef = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleAttrsBoxDiffWidth $ sa




ffiStyleAttrsBoxDiffHeight :: CInt -> IO CInt
ffiStyleAttrsBoxDiffHeight cRef = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleAttrsBoxDiffHeight $ sa




ffiStyleAttrsColor :: CInt -> IO CInt
ffiStyleAttrsColor cRef = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleColor $ sa




ffiStyleAttrsSetColor :: CInt -> CInt -> IO ()
ffiStyleAttrsSetColor cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let sa' = old { styleColor = val }
  globalStyleAttrsUpdate ref sa'
  return ()




ffiStyleAttrsBackgroundColor :: CInt -> IO CInt
ffiStyleAttrsBackgroundColor cRef = do
  sa <- globalStyleAttrsGet . fromIntegral $ cRef
  return . fromIntegral . styleBackgroundColor $ sa




ffiStyleAttrsSetBackgroundColor :: CInt -> CInt -> IO ()
ffiStyleAttrsSetBackgroundColor cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  old <- globalStyleAttrsGet ref
  let sa' = old { styleBackgroundColor = val }
  globalStyleAttrsUpdate ref sa'
  return ()





ffiStyleAttrsBorderColor :: CInt -> Ptr FfiStyleBorderColor -> IO ()
ffiStyleAttrsBorderColor cRef ptrStruct = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  pokeStyleBorderColor (styleBorderColor attrs) ptrStruct
  return ()




ffiStyleAttrsSetBorderColor :: CInt -> CInt -> IO ()
ffiStyleAttrsSetBorderColor cRef cVal = do
  let ref = fromIntegral cRef
  let val = fromIntegral cVal
  attrs <- globalStyleAttrsGet ref
  let attrs' = attrs { styleBorderColor = StyleBorderColor
                       {
                         styleBorderColorTop    = val
                       , styleBorderColorRight  = val
                       , styleBorderColorBottom = val
                       , styleBorderColorLeft   = val
                       }
                     }
  globalStyleAttrsUpdate ref attrs'
  return ()




ffiStyleAttrsSetBorderColor2 :: CInt -> Ptr FfiStyleBorderColor -> IO ()
ffiStyleAttrsSetBorderColor2 cRef ptrStruct = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  bcolor <- peekStyleBorderColor ptrStruct
  let attrs' = attrs { styleBorderColor = bcolor }
  globalStyleAttrsUpdate ref attrs'
  return ()




ffiStyleAttrsFontAttrs :: CInt -> Ptr FfiFontAttrs -> IO ()
ffiStyleAttrsFontAttrs cRef ptrStruct = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  pokeFontAttrs (styleFontAttrs attrs) ptrStruct
  return ()




ffiStyleAttrsSetFontAttrs :: CInt -> Ptr FfiFontAttrs -> IO ()
ffiStyleAttrsSetFontAttrs cRef ptrStruct = do
  let ref = fromIntegral cRef
  attrs <- globalStyleAttrsGet ref
  fa    <- peekFontAttrs ptrStruct
  let attrs' = attrs { styleFontAttrs = fa }
  globalStyleAttrsUpdate ref attrs'
  return ()




-- Returns a pointer to string allocated on heap.
-- Pointer is NULL if given attribute is an empty string.
--
-- FIXME: returned pointer is probably not deallocated anywhere.
ffiStyleAttrsBgImage :: CInt -> IO (Ptr CChar)
ffiStyleAttrsBgImage cRef = do
  attrs <- globalStyleAttrsGet . fromIntegral $ cRef
  allocAndPokeCStringIfNonEmpty . styleBgImage $ attrs

