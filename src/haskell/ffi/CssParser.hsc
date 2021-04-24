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


{-# LANGUAGE OverloadedStrings #-}


module CssParserFFI() where




import Prelude
import Foreign.C.String
import Foreign
import qualified Data.Text as T
import qualified Data.Text.Read as T.R
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.Encoding.Error as T.E.E
import qualified Data.Text.IO as T.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Control.Applicative
import Control.Monad -- when
import CssParser




foreign export ccall "hll_nextToken" hll_nextToken :: Ptr HelloCssParser -> CString -> IO CString
foreign export ccall "hll_declarationValueAsInt"   hll_declarationValueAsInt   :: Ptr HelloCssParser -> Int -> CString -> CString -> Int -> Int -> IO Int
foreign export ccall "hll_declarationValueAsString"   hll_declarationValueAsString :: Ptr HelloCssParser -> Int -> CString -> CString -> Int -> Int -> IO CString
foreign export ccall "hll_declarationValueAsMultiEnum" hll_declarationValueAsMultiEnum :: Ptr HelloCssParser -> Int -> CString -> CString -> Int -> IO Int
foreign export ccall "hll_tokenMatchesProperty" hll_tokenMatchesProperty :: Int -> CString -> Int -> IO Int
foreign export ccall "hll_ignoreBlock" hll_ignoreBlock :: Ptr HelloCssParser -> CString -> IO Int
foreign export ccall "hll_ignoreStatement" hll_ignoreStatement :: Ptr HelloCssParser -> CString -> IO Int

foreign export ccall "hll_cssLengthType" hll_cssLengthType :: Int -> IO Int
foreign export ccall "hll_cssLengthValue" hll_cssLengthValue :: Int -> IO Float
foreign export ccall "hll_cssCreateLength" hll_cssCreateLength :: Float -> Int -> IO Int

foreign export ccall "hll_cssParseWeight"  hll_cssParseWeight :: Ptr HelloCssParser -> Int -> CString -> CString -> IO Int
foreign export ccall "hll_cssParseSimpleSelector"  hll_cssParseSimpleSelector :: Ptr HelloCssParser -> Ptr HelloCssSimpleSelector -> Int -> CString -> CString -> IO Bool

foreign export ccall "hll_cssPropertyInfoIdxByName" hll_cssPropertyInfoIdxByName :: CString -> IO Int
foreign export ccall "hll_cssPropertyNameString" hll_cssPropertyNameString :: Int -> IO CString



#include "../hello.h"

data HelloCssParser = HelloCssParser {
    spaceSeparatedC :: Int
  , bufOffsetC      :: Int
  , tokenTypeC      :: Int
  , withinBlockC    :: Int
  , tokenValueC     :: CString
  } deriving (Show)


instance Storable HelloCssParser where
  sizeOf    _ = #{size hll_CssParser}
  alignment _ = alignment (undefined :: Int) -- #{alignment hll_CssParser} --


  poke ptr (HelloCssParser spaceSeparatedC bufOffsetC tokenTypeC withinBlockC tokenValueC) = do
    #{poke hll_CssParser, spaceSeparatedC} ptr spaceSeparatedC
    #{poke hll_CssParser, bufOffsetC}      ptr bufOffsetC
    #{poke hll_CssParser, tokenTypeC}      ptr tokenTypeC
    #{poke hll_CssParser, withinBlockC}    ptr withinBlockC
    #{poke hll_CssParser, tokenValueC}     ptr tokenValueC

{-
  poke ptr hll_CssParser = do
    #{poke hll_CssParser, spaceSeparatedC} ptr $ spaceSeparatedC hll_CssParser
    #{poke hll_CssParser, bufOffsetC} ptr      $ bufOffsetC hll_CssParser
    #{poke hll_CssParser, tokenTypeC} ptr      $ tokenTypeC hll_CssParser
    #{poke hll_CssParser, withinBlockC} ptr    $ withinBlockC hll_CssParser
-}
  peek ptr = do
    a <- #{peek hll_CssParser, spaceSeparatedC} ptr
    b <- #{peek hll_CssParser, bufOffsetC} ptr
    c <- #{peek hll_CssParser, tokenTypeC} ptr
    d <- #{peek hll_CssParser, withinBlockC} ptr
    e <- #{peek hll_CssParser, tokenValueC} ptr
    return (HelloCssParser a b c d e)
{-
  peek ptr = return HelloCssParser
    `ap` (#{peek hll_CssParser, spaceSeparatedC} ptr)
    `ap` (#{peek hll_CssParser, bufOffsetC} ptr)
    `ap` (#{peek hll_CssParser, tokenTypeC} ptr)
    `ap` (#{peek hll_CssParser, withinBlockC} ptr)
-}


hll_nextToken :: Ptr HelloCssParser -> CString -> IO CString
hll_nextToken hll_cssparser cBuf = do
  buf <- BSU.unsafePackCString cBuf
  oldParser <- peek hll_cssparser
  let inBlock = withinBlockC oldParser
  let (parser, token) = nextToken defaultParser{ remainder   = T.E.decodeLatin1 buf
                                               , withinBlock = inBlock > 0
                                               , bufOffset   = bufOffsetC oldParser
                                               , spaceSeparated = spaceSeparatedC oldParser > 0
                                               }
  {-
  putStr (show parser)
  putStr "  WWWW  "
  putStr (show token)
  putStr "\n"
-}
  manipulateOutPtr hll_cssparser parser token inBlock
  case token of
    (CssTokI i)   -> (newCString . show $ i)
    (CssTokF f)   -> (newCString . show $ f)
    (CssTokCol c) -> (newCString . T.unpack $ c)
    (CssTokSym s) -> (newCString . T.unpack $ s)
    (CssTokStr s) -> (newCString . T.unpack $ s)
    (CssTokCh c)  -> (newCString . T.unpack . T.singleton $ c)
    otherwise     -> return nullPtr --(newCString . T.unpack . cssTokenValue $ token)




-- Set fields in pointer to struct passed from C code.
manipulateOutPtr :: Ptr HelloCssParser -> CssParser -> CssToken -> Int -> IO ()
manipulateOutPtr hll_cssparser parser token inBlock = do
  s <- (cstr token)
  poke hll_cssparser $ HelloCssParser (if spaceSeparated parser then 1 else 0) (bufOffset parser) (getTokenType token) inBlock s


cstr :: CssToken -> IO CString
cstr token = case token of
    (CssTokI i)   -> (newCString . show $ i)
    (CssTokF f)   -> (newCString . show $ f)
    (CssTokCol c) -> (newCString . T.unpack $ c)
    (CssTokSym s) -> (newCString . T.unpack $ s)
    (CssTokStr s) -> (newCString . T.unpack $ s)
    (CssTokCh c)  -> (newCString . T.unpack . T.singleton $ c)
    otherwise     -> return nullPtr --(newCString . T.unpack . cssTokenValue $ token)




hll_declarationValueAsInt :: Ptr HelloCssParser -> Int -> CString -> CString -> Int -> Int -> IO Int
hll_declarationValueAsInt hll_cssparser tokType cTokValue cBuf valueType property = do
  buf      <- BSU.unsafePackCString $ cBuf
  tokValue <- BSU.unsafePackCString $ cTokValue
  hllParser <- peek hll_cssparser
  let inBlock = withinBlockC hllParser
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = bufOffsetC hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }

  let pair@(newParser, newToken) = declarationValueAsInt parser inputToken valueType property
  manipulateOutPtr hll_cssparser parser inputToken inBlock
  case pair of
    (_, Just i) -> return i
    (_, _)      -> return 999999999



hll_declarationValueAsString :: Ptr HelloCssParser -> Int -> CString -> CString -> Int -> Int -> IO CString
hll_declarationValueAsString hll_cssparser tokType cTokValue cBuf valueType property = do
  buf      <- BSU.unsafePackCString $ cBuf
  tokValue <- BSU.unsafePackCString $ cTokValue
  hllParser <- peek hll_cssparser
  let inBlock = withinBlockC hllParser
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = bufOffsetC hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }

  let pair@(newParser, newToken) = declarationValueAsString parser inputToken valueType property
  manipulateOutPtr hll_cssparser parser inputToken inBlock
  case pair of
    (_, Just s) -> newCString . T.unpack $ s
    (_, _)      -> return nullPtr




hll_declarationValueAsMultiEnum :: Ptr HelloCssParser -> Int -> CString -> CString -> Int -> IO Int
hll_declarationValueAsMultiEnum hll_cssparser tokType cTokValue cBuf property = do
  buf      <- BSU.unsafePackCString $ cBuf
  tokValue <- BSU.unsafePackCString $ cTokValue
  hllParser <- peek hll_cssparser
  let inBlock = withinBlockC hllParser
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = bufOffsetC hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }


  let pair@(newParser, newToken) = declarationValueAsMultiEnum parser inputToken property
  manipulateOutPtr hll_cssparser parser inputToken inBlock
  case pair of
    (_, Just i) -> return i
    (_, _)      -> return 999999999



hll_ignoreBlock :: Ptr HelloCssParser -> CString -> IO Int
hll_ignoreBlock hll_cssparser cBuf = do
  buf       <- BSU.unsafePackCString $ cBuf
  hllParser <- peek hll_cssparser
  let inBlock = withinBlockC hllParser
  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = bufOffsetC hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }

  let (newParser, newToken) = ignoreBlock parser
  manipulateOutPtr hll_cssparser parser newToken inBlock
  return 0




hll_ignoreStatement :: Ptr HelloCssParser -> CString -> IO Int
hll_ignoreStatement hll_cssparser cBuf = do
  buf       <- BSU.unsafePackCString $ cBuf
  hllParser <- peek hll_cssparser
  let inBlock = withinBlockC hllParser
  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = bufOffsetC hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }

  let (newParser, newToken) = ignoreBlock parser
  manipulateOutPtr hll_cssparser parser newToken inBlock
  return 0




getTokenType (CssTokI    _) = 0
getTokenType (CssTokF    _) = 1
getTokenType (CssTokCol  _) = 2
getTokenType (CssTokSym  _) = 3
getTokenType (CssTokStr  _) = 4
getTokenType (CssTokCh   _) = 5
getTokenType (CssTokEnd)    = 6
getTokenType _              = 7





getTokenADT tokType tokValue | tokType == 0 = case T.R.signed T.R.decimal tokValue of
                                                Right pair -> CssTokI (fst pair)
                                                Left  _    -> CssTokI 0
                             | tokType == 1 = case T.R.signed T.R.rational tokValue of
                                                Right pair -> CssTokF (fst pair)
                                                Left  _    -> CssTokF 0.0
                             | tokType == 2 = CssTokCol  tokValue
                             | tokType == 3 = CssTokSym  tokValue
                             | tokType == 4 = CssTokStr  tokValue
                             | tokType == 5 = CssTokCh   (T.head tokValue)
                             | tokType == 6 = CssTokEnd




hll_tokenMatchesProperty :: Int -> CString -> Int -> IO Int
hll_tokenMatchesProperty tokType cTokValue property = do
  tokValue <- BSU.unsafePackCString $ cTokValue
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)
  putStr (show inputToken)
  case tokenMatchesProperty inputToken property of
    Just i -> do
      putStr " results in just"
      putStr (show i)
      putStr "\n"
      return i
    _      -> do
      putStr " results innothing\n"
      return (-1)




hll_cssLengthType :: Int -> IO Int
hll_cssLengthType len = do
  return (cssLengthType len)




hll_cssLengthValue :: Int -> IO Float
hll_cssLengthValue len = do
  return (cssLengthValue len)




hll_cssCreateLength :: Float -> Int -> IO Int
hll_cssCreateLength val t = do
  return (cssCreateLength val t)




hll_cssPropertyInfoIdxByName :: CString -> IO Int
hll_cssPropertyInfoIdxByName cPropertyName = do
  propertyName       <- BSU.unsafePackCString $ cPropertyName
  let idx = cssPropertyInfoIdxByName . T.E.decodeLatin1 $ propertyName
  return idx




hll_cssPropertyNameString :: Int -> IO CString
hll_cssPropertyNameString property = do
  let name = cssPropertyNameString property
  newCString . T.unpack $ name






hll_cssParseWeight :: Ptr HelloCssParser -> Int -> CString -> CString -> IO Int
hll_cssParseWeight hll_cssparser tokType cTokValue cBuf = do
  buf      <- BSU.unsafePackCString $ cBuf
  tokValue <- BSU.unsafePackCString $ cTokValue
  hllParser <- peek hll_cssparser
  let inBlock = withinBlockC hllParser
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = bufOffsetC hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }

  let ((newParser, newToken), isImportant) = cssParseWeight (parser, inputToken)
  manipulateOutPtr hll_cssparser newParser newToken inBlock
  if isImportant
  then return 1
  else return 0




hll_cssParseSimpleSelector :: Ptr HelloCssParser -> Ptr HelloCssSimpleSelector -> Int -> CString -> CString -> IO Bool
hll_cssParseSimpleSelector hll_cssparser hll_simpleSelector tokType cTokValue cBuf = do
  buf      <- BSU.unsafePackCString $ cBuf
  tokValue <- BSU.unsafePackCString $ cTokValue
  hllParser <- peek hll_cssparser
  simSel    <- peek hll_simpleSelector
  let inBlock = withinBlockC hllParser
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = bufOffsetC hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }

  a <- if nullPtr == selectorPseudoClassC simSel then return "" else (peekCString . selectorPseudoClassC $ simSel)
  b <- if nullPtr == selectorIdC simSel then return "" else (peekCString . selectorIdC $ simSel)
  let c = [] -- selectorClassC simSel -- TODO: create the array from simSel
  let simSel2 = defaultSimpleSelector { selectorPseudoClass = []
                                      , selectorId = T.pack b
                                      , selectorClass = c
                                      , selectorElement = selectorElementC simSel
                                     }


  let ((newParser, newToken), simpleSelector, valid) =parseSimpleSelector ((parser, inputToken), simSel2)
  manipulateOutPtr hll_cssparser newParser newToken inBlock
  setSimpleSelector hll_simpleSelector simpleSelector
{-
  putStr "ZAAA"
  putStr (show inputToken)
  putStr " "
  putStr (show ret)
  putStr "\n"
-}
  return valid




data HelloCssSimpleSelector = HelloCssSimpleSelector {
    selectorClassC           :: CString
  , selectorClassSizeC       :: Int

  , selectorPseudoClassC     :: CString
  , selectorPseudoClassSizeC :: Int

  , selectorIdC              :: CString
  , selectorElementC         :: Int
  , allocedC                 :: Int
  } deriving (Show)




instance Storable HelloCssSimpleSelector where
  sizeOf    _ = #{size hll_CssSimpleSelector}
  alignment _ = #{alignment hll_CssSimpleSelector}

  peek ptr = do
    a <- #{peek hll_CssSimpleSelector, selector_class} ptr
    b <- #{peek hll_CssSimpleSelector, selector_class_size} ptr
    c <- #{peek hll_CssSimpleSelector, selector_pseudo_class} ptr
    d <- #{peek hll_CssSimpleSelector, selector_pseudo_class_size} ptr
    e <- #{peek hll_CssSimpleSelector, selector_id} ptr
    f <- #{peek hll_CssSimpleSelector, selector_element} ptr
    g <- #{peek hll_CssSimpleSelector, alloced} ptr
    return (HelloCssSimpleSelector a b c d e f g)


  poke ptr (HelloCssSimpleSelector selectorClassI selector_class_size_I selector_pseudo_class_I selector_pseudo_class_size_I selector_id_I selector_element_I alloced_I) = do
    #{poke hll_CssSimpleSelector, selector_class}             ptr selectorClassI
    #{poke hll_CssSimpleSelector, selector_class_size}        ptr selector_class_size_I
    #{poke hll_CssSimpleSelector, selector_pseudo_class}      ptr selector_pseudo_class_I
    #{poke hll_CssSimpleSelector, selector_pseudo_class_size} ptr selector_pseudo_class_size_I
    #{poke hll_CssSimpleSelector, selector_id}                ptr selector_id_I
    #{poke hll_CssSimpleSelector, selector_element}           ptr selector_element_I
    #{poke hll_CssSimpleSelector, alloced}                    ptr alloced_I




-- Save given Haskell simple selector to C simple selector.
setSimpleSelector :: Ptr HelloCssSimpleSelector -> CssSimpleSelector -> IO ()
setSimpleSelector hll_simpleSelector simpleSelector = do
  cStringPtrSelId <- if T.null . selectorId $ simpleSelector
                     then return nullPtr
                     else newCString . T.unpack . selectorId $ simpleSelector

  setArrayOfStringPointers (selectorClass simpleSelector) hll_simpleSelector 0             -- Bytes 0-9
  pokeByteOff hll_simpleSelector (8 * 10) (length . selectorClass $ simpleSelector)        -- Byte 10

  setArrayOfStringPointers (selectorPseudoClass simpleSelector) hll_simpleSelector 11      -- Bytes 11-20
  pokeByteOff hll_simpleSelector (8 * 21) (length . selectorPseudoClass $ simpleSelector)  -- Byte 21

  pokeByteOff hll_simpleSelector (8 * 22) cStringPtrSelId                                  -- Byte 22
  pokeByteOff hll_simpleSelector (8 * 23) (selectorElement simpleSelector)                 -- Byte 23




-- Save given array of texts as array pointer to C strings.
-- The pointers are allocated by this function.
-- The pointers are stored in a structure given by second arg.
-- Offset to beginning of the array of pointers (to first cell) is given by third arg.
setArrayOfStringPointers :: [T.Text] -> Ptr HelloCssSimpleSelector -> Int ->  IO Int
setArrayOfStringPointers [] hll_simpleSelector arrayPosition = do
  return arrayPosition
setArrayOfStringPointers (x:xs) hll_simpleSelector arrayPosition = do
  let pointerSize = 8
  str  <- newCString . T.unpack $ x
  pokeByteOff hll_simpleSelector (arrayPosition * pointerSize) str
  setArrayOfStringPointers xs hll_simpleSelector (arrayPosition + 1)

