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
import Foreign
import Foreign.C.String
import Foreign.C.Types
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

foreign export ccall "hll_cssParseSelector"  hll_cssParseSelector :: Ptr HelloCssParser -> Ptr HelloCssSelector -> Int -> CString -> CString -> IO Bool

foreign export ccall "hll_cssPropertyInfoIdxByName" hll_cssPropertyInfoIdxByName :: CString -> IO Int
foreign export ccall "hll_cssPropertyNameString" hll_cssPropertyNameString :: Int -> IO CString



#include "../hello.h"

data HelloCssParser = HelloCssParser {
    spaceSeparatedC :: CInt
  , bufOffsetC      :: CInt
  , tokenTypeC      :: CInt
  , withinBlockC    :: CInt
  , tokenValueC     :: CString
  } deriving (Show)


instance Storable HelloCssParser where
  sizeOf    _ = #{size c_css_parser_t}
  alignment _ = #{alignment c_css_parser_t} -- alignment (undefined :: Int)

  poke ptr (HelloCssParser argSpaceSeparated argBufOffset argTokenType argWithinBlock argTokenValue) = do
    #{poke c_css_parser_t, c_space_separated} ptr argSpaceSeparated
    #{poke c_css_parser_t, c_buf_offset}      ptr argBufOffset
    #{poke c_css_parser_t, c_token_type}      ptr argTokenType
    #{poke c_css_parser_t, c_within_block}    ptr argWithinBlock
    #{poke c_css_parser_t, c_token_value}     ptr argTokenValue

{-
  poke ptr c_css_parser_t = do
    #{poke c_css_parser_t, spaceSeparatedC} ptr $ spaceSeparatedC c_css_parser_t
    #{poke c_css_parser_t, bufOffsetC} ptr      $ bufOffsetC c_css_parser_t
    #{poke c_css_parser_t, tokenTypeC} ptr      $ tokenTypeC c_css_parser_t
    #{poke c_css_parser_t, withinBlockC} ptr    $ withinBlockC c_css_parser_t
-}
  peek ptr = do
    a <- #{peek c_css_parser_t, c_space_separated} ptr
    b <- #{peek c_css_parser_t, c_buf_offset}      ptr
    c <- #{peek c_css_parser_t, c_token_type}      ptr
    d <- #{peek c_css_parser_t, c_within_block}    ptr
    e <- #{peek c_css_parser_t, c_token_value}     ptr
    return (HelloCssParser a b c d e)
{-
  peek ptr = return HelloCssParser
    `ap` (#{peek c_css_parser_t, c_space_separated} ptr)
    `ap` (#{peek c_css_parser_t, c_buf_offset} ptr)
    `ap` (#{peek c_css_parser_t, c_token_type} ptr)
    `ap` (#{peek c_css_parser_t, c_within_block} ptr)
    `ap` (#{peek c_css_parser_t, c_token_value} ptr)
-}


hll_nextToken :: Ptr HelloCssParser -> CString -> IO CString
hll_nextToken ptrStructCssParser cBuf = do
  buf <- BSU.unsafePackCString cBuf
  hllParser <- peek ptrStructCssParser
  let inBlock = withinBlockC hllParser :: CInt
  let (parser, token) = nextToken defaultParser{ remainder   = T.E.decodeLatin1 buf
                                               , withinBlock = inBlock > 0
                                               , bufOffset   = fromIntegral . bufOffsetC $ hllParser
                                               , spaceSeparated = spaceSeparatedC hllParser > 0
                                               }
  manipulateOutPtr ptrStructCssParser parser token inBlock
  case token of -- TODO: use "cstr" defined below
    (CssTokI i)   -> (newCString . show $ i)
    (CssTokF f)   -> (newCString . show $ f)
    (CssTokCol c) -> (newCString . T.unpack $ c)
    (CssTokSym s) -> (newCString . T.unpack $ s)
    (CssTokStr s) -> (newCString . T.unpack $ s)
    (CssTokCh c)  -> (newCString . T.unpack . T.singleton $ c)
    CssTokWS      -> (newCString " ")
    otherwise     -> return nullPtr




-- Set fields in pointer to struct passed from C code.
manipulateOutPtr :: Ptr HelloCssParser -> CssParser -> CssToken -> CInt -> IO ()
manipulateOutPtr ptrStructCssParser parser token inBlock = do
  s <- (cstr token)
  poke ptrStructCssParser $ HelloCssParser (if spaceSeparated parser then 1 else 0) (fromIntegral . bufOffset $ parser) (getTokenType token) inBlock s


cstr :: CssToken -> IO CString
cstr token = case token of
    (CssTokI i)   -> (newCString . show $ i)
    (CssTokF f)   -> (newCString . show $ f)
    (CssTokCol c) -> (newCString . T.unpack $ c)
    (CssTokSym s) -> (newCString . T.unpack $ s)
    (CssTokStr s) -> (newCString . T.unpack $ s)
    (CssTokCh c)  -> (newCString . T.unpack . T.singleton $ c)
    CssTokWS      -> (newCString " ")
    otherwise     -> return nullPtr --(newCString . T.unpack . cssTokenValue $ token)




hll_declarationValueAsInt :: Ptr HelloCssParser -> Int -> CString -> CString -> Int -> Int -> IO Int
hll_declarationValueAsInt ptrStructCssParser tokType cTokValue cBuf valueType property = do
  buf      <- BSU.unsafePackCString $ cBuf
  tokValue <- BSU.unsafePackCString $ cTokValue
  hllParser <- peek ptrStructCssParser
  let inBlock = withinBlockC hllParser
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = fromIntegral . bufOffsetC $ hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }

  let pair@(newParser, intVal) = declarationValueAsInt (parser, inputToken) valueType property
  manipulateOutPtr ptrStructCssParser parser inputToken inBlock
  case intVal of
    Just i -> return i
    _      -> return invalidIntResult




hll_declarationValueAsString :: Ptr HelloCssParser -> Int -> CString -> CString -> Int -> Int -> IO CString
hll_declarationValueAsString ptrStructCssParser tokType cTokValue cBuf valueType property = do
  buf      <- BSU.unsafePackCString $ cBuf
  tokValue <- BSU.unsafePackCString $ cTokValue
  hllParser <- peek ptrStructCssParser
  let inBlock = withinBlockC hllParser
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = fromIntegral . bufOffsetC $ hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }

  let pair@((newParser, newToken), textVal) = declarationValueAsString (parser, inputToken) valueType property
  manipulateOutPtr ptrStructCssParser parser newToken inBlock
  case textVal of
    Just t -> newCString . T.unpack $ t
    _      -> return nullPtr




hll_declarationValueAsMultiEnum :: Ptr HelloCssParser -> Int -> CString -> CString -> Int -> IO Int
hll_declarationValueAsMultiEnum ptrStructCssParser tokType cTokValue cBuf property = do
  buf      <- BSU.unsafePackCString $ cBuf
  tokValue <- BSU.unsafePackCString $ cTokValue
  hllParser <- peek ptrStructCssParser
  let inBlock = withinBlockC hllParser
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = fromIntegral . bufOffsetC $ hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }


  let pair@(newParser, intVal) = declarationValueAsMultiEnum (parser, inputToken) property
  manipulateOutPtr ptrStructCssParser parser inputToken inBlock
  case intVal of
    Just i -> return i
    _      -> return invalidIntResult




hll_ignoreBlock :: Ptr HelloCssParser -> CString -> IO Int
hll_ignoreBlock ptrStructCssParser cBuf = do
  buf       <- BSU.unsafePackCString $ cBuf
  hllParser <- peek ptrStructCssParser
  let inBlock = withinBlockC hllParser
  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = fromIntegral . bufOffsetC $ hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }

  let (newParser, newToken) = ignoreBlock parser
  manipulateOutPtr ptrStructCssParser parser newToken inBlock
  return 0




hll_ignoreStatement :: Ptr HelloCssParser -> CString -> IO Int
hll_ignoreStatement ptrStructCssParser cBuf = do
  buf       <- BSU.unsafePackCString $ cBuf
  hllParser <- peek ptrStructCssParser
  let inBlock = withinBlockC hllParser
  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = fromIntegral . bufOffsetC $ hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }

  let (newParser, newToken) = ignoreBlock parser
  manipulateOutPtr ptrStructCssParser parser newToken inBlock
  return 0




getTokenType (CssTokI    _) = 0
getTokenType (CssTokF    _) = 1
getTokenType (CssTokCol  _) = 2
getTokenType (CssTokSym  _) = 3
getTokenType (CssTokStr  _) = 4
getTokenType (CssTokCh   _) = 5
getTokenType (CssTokWS)     = 6
getTokenType (CssTokEnd)    = 7
getTokenType _              = 8





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
                             | tokType == 6 = CssTokWS
                             | tokType == 7 = CssTokEnd




hll_tokenMatchesProperty :: Int -> CString -> Int -> IO Int
hll_tokenMatchesProperty tokType cTokValue property = do
  tokValue <- BSU.unsafePackCString $ cTokValue
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)
  putStr (show inputToken)
  case tokenMatchesProperty inputToken property of
    Just i -> do
      putStr " results in just "
      putStr (show i)
      putStr "\n"
      return i
    _      -> do
      putStr " results in nothing\n"
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
hll_cssParseWeight ptrStructCssParser tokType cTokValue cBuf = do
  buf      <- BSU.unsafePackCString $ cBuf
  tokValue <- BSU.unsafePackCString $ cTokValue
  hllParser <- peek ptrStructCssParser
  let inBlock = withinBlockC hllParser
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = fromIntegral . bufOffsetC $ hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }

  let ((newParser, newToken), isImportant) = cssParseWeight (parser, inputToken)
  manipulateOutPtr ptrStructCssParser newParser newToken inBlock
  if isImportant
  then return 1
  else return 0




hll_cssParseSimpleSelector :: Ptr HelloCssParser -> Ptr HelloCssSimpleSelector -> Int -> CString -> CString -> IO Bool
hll_cssParseSimpleSelector ptrStructCssParser ptrStructSimpleSelector tokType cTokValue cBuf = do
  buf      <- BSU.unsafePackCString $ cBuf
  tokValue <- BSU.unsafePackCString $ cTokValue
  hllParser <- peek ptrStructCssParser
  simSel    <- peek ptrStructSimpleSelector
  let inBlock = withinBlockC hllParser
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = fromIntegral . bufOffsetC $ hllParser
                            , spaceSeparated = spaceSeparatedC hllParser > 0
                            }

  a <- if nullPtr == selectorPseudoClassC simSel then return "" else (peekCString . selectorPseudoClassC $ simSel)
  b <- if nullPtr == selectorIdC simSel then return "" else (peekCString . selectorIdC $ simSel)
  let c = [] -- selectorClassC simSel -- TODO: create the array from simSel
  let simSel2 = defaultSimpleSelector { selectorPseudoClass = []
                                      , selectorId = T.pack b
                                      , selectorClass = c
                                      , selectorElement = fromIntegral . selectorElementC $ simSel
                                      , combinator = 1 --fromIntegral . combinatorC $ simSel
                                      }

  let ((newParser, newToken), simpleSelector, valid) = parseSimpleSelector (parser, inputToken)

  putStr "ZAAA - input simSel: "
  putStr (show simSel)
  putStr "\n"

  putStr "ZAAA - parsed simpleSelector: "
  putStr (show simpleSelector)
  putStr "\n"

  manipulateOutPtr ptrStructCssParser newParser newToken inBlock
  setSimpleSelector ptrStructSimpleSelector simpleSelector

  putStr "ZAAA - simpleSelector after setting pointer "
  putStr (show simpleSelector)
  putStr "\n"
  p <- peek ptrStructSimpleSelector
  putStr (show p)
  putStr "\n"

  return valid




data HelloCssSimpleSelector = HelloCssSimpleSelector {
    selectorClassC           :: CString
  , selectorClassSizeC       :: CInt

  , selectorPseudoClassC     :: CString
  , selectorPseudoClassSizeC :: CInt

  , selectorIdC              :: CString
  , selectorElementC         :: CInt

  , combinatorC              :: CInt
  } deriving (Show)




instance Storable HelloCssSimpleSelector where
  sizeOf    _ = #{size c_css_simple_selector_t}
  alignment _ = #{alignment c_css_simple_selector_t}

  peek ptr = do
    a <- #{peek c_css_simple_selector_t, c_selector_class} ptr
    b <- #{peek c_css_simple_selector_t, c_selector_class_size} ptr
    c <- #{peek c_css_simple_selector_t, c_selector_pseudo_class} ptr
    d <- #{peek c_css_simple_selector_t, c_selector_pseudo_class_size} ptr
    e <- #{peek c_css_simple_selector_t, c_selector_id} ptr
    f <- #{peek c_css_simple_selector_t, c_selector_element} ptr
    g <- #{peek c_css_simple_selector_t, c_combinator} ptr
    return (HelloCssSimpleSelector a b c d e f g)


  poke ptr (HelloCssSimpleSelector selectorClassI selector_class_size_I selector_pseudo_class_I selector_pseudo_class_size_I selector_id_I selector_element_I combinator_I) = do
    #{poke c_css_simple_selector_t, c_selector_class}             ptr selectorClassI
    #{poke c_css_simple_selector_t, c_selector_class_size}        ptr selector_class_size_I
    #{poke c_css_simple_selector_t, c_selector_pseudo_class}      ptr selector_pseudo_class_I
    #{poke c_css_simple_selector_t, c_selector_pseudo_class_size} ptr selector_pseudo_class_size_I
    #{poke c_css_simple_selector_t, c_selector_id}                ptr selector_id_I
    #{poke c_css_simple_selector_t, c_selector_element}           ptr selector_element_I
    #{poke c_css_simple_selector_t, c_combinator}                 ptr combinator_I




data HelloCssSelector = HelloCssSelector {
    matchCaseOffsetC         :: CInt
  , simpleSelectorListC      :: Ptr HelloCssSimpleSelector
  , simpleSelectorListSizeC  :: CInt
  } deriving (Show)




instance Storable HelloCssSelector where
  sizeOf    _ = #{size c_css_selector_t}
  alignment _ = #{alignment c_css_selector_t}

  peek ptr = do
    a <- #{peek c_css_selector_t, c_match_case_offset} ptr
    b <- #{peek c_css_selector_t, c_simple_selector_list} ptr
    c <- #{peek c_css_selector_t, c_simple_selector_list_size} ptr
    return (HelloCssSelector a b c)


  poke ptr (HelloCssSelector inMatchCaseOffset inSimpleSelectorList inSimpleSelectorListSize) = do
    #{poke c_css_selector_t, c_match_case_offset}          ptr inMatchCaseOffset
    #{poke c_css_selector_t, c_simple_selector_list}       ptr inSimpleSelectorList
    #{poke c_css_selector_t, c_simple_selector_list_size}  ptr inSimpleSelectorListSize




-- Save given Haskell simple selector to C simple selector.
-- https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/hsc2hs.html
setSimpleSelector :: Ptr HelloCssSimpleSelector -> CssSimpleSelector -> IO ()
setSimpleSelector ptrStructSimpleSelector simpleSelector = do
  cStringPtrSelId <- if T.null . selectorId $ simpleSelector
                     then return nullPtr
                     else newCString . T.unpack . selectorId $ simpleSelector

  setArrayOfPointers (selectorClass simpleSelector) textToPtrString ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_class)
  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_class_size) (length . selectorClass $ simpleSelector)        -- Byte 10

  setArrayOfPointers (selectorPseudoClass simpleSelector) textToPtrString ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_pseudo_class)
  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_pseudo_class_size) (length . selectorPseudoClass $ simpleSelector)  -- Byte 21

  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_id) cStringPtrSelId                                  -- Byte 22
  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_element) (selectorElement simpleSelector)                 -- Byte 23

  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_combinator) (combinator simpleSelector)                      -- Byte 24




-- Save given array of texts as array pointer to C strings.
-- The pointers are allocated by this function.
-- The pointers are stored in a structure given by second arg.
-- Offset to beginning of the array of pointers (to first cell) is given by third arg.
setArrayOfStringPointers :: [T.Text] -> Ptr HelloCssSimpleSelector -> Int ->  IO Int
setArrayOfStringPointers [] ptrStructSimpleSelector arrayPosition = do
  return arrayPosition
setArrayOfStringPointers (x:xs) ptrStructSimpleSelector arrayPosition = do
  let pointerSize = 8
  str  <- newCString . T.unpack $ x
  pokeByteOff ptrStructSimpleSelector (arrayPosition * pointerSize) str
  setArrayOfStringPointers xs ptrStructSimpleSelector (arrayPosition + 1)




-- Save given array of texts as array pointer to C strings.
-- The pointers are allocated by this function.
-- The pointers are stored in a structure given by second arg.
-- Offset to beginning of the array of pointers (to first cell) is given by third arg.
setArrayOfStructPointers :: [CssSimpleSelector] -> Ptr HelloCssSimpleSelector -> Int ->  IO Int
setArrayOfStructPointers [] ptrStructSimpleSelector arrayPosition = do
  return arrayPosition
setArrayOfStructPointers (x:xs) ptrStructSimpleSelector arrayPosition = do
  let pointerSize = 8
  setSimpleSelector (advancePtr ptrStructSimpleSelector arrayPosition) x
  setArrayOfStructPointers xs ptrStructSimpleSelector (arrayPosition + 1)


-- Save given array of texts as array pointer to C strings.
-- The pointers are allocated by this function.
-- The pointers are stored in a structure given by second arg.
-- Offset to beginning of the array of pointers (to first cell) is given by third arg.
setArrayOfStructPointers2 :: [CssSimpleSelector] -> Ptr HelloCssSimpleSelector -> IO Int
setArrayOfStructPointers2 [] ptrStructSimpleSelector = do
  return 0
setArrayOfStructPointers2 (x:xs) ptrStructSimpleSelector = do
  setSimpleSelector ptrStructSimpleSelector x --  x
  setArrayOfStructPointers2 xs  (advancePtr ptrStructSimpleSelector 1)




-- Save given Haskell simple selector to C simple selector.
-- https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/hsc2hs.html
setSelector :: Ptr HelloCssSimpleSelector -> CssSelector -> IO ()
setSelector ptrStructSimpleSelector selector = do
  setSelectorElem ptrStructSimpleSelector (simpleSelectorList selector)
    where
      setSelectorElem :: Ptr HelloCssSimpleSelector -> [CssSimpleSelector] -> IO ()
      setSelectorElem ptrStructSimpleSelector []     = return ()
      setSelectorElem ptrStructSimpleSelector (x:xs) = do
        putStr "++++++++++++++\n"
        putStr (show x)
        putStr "---------------\n"
        --ptrStructSimpleSelector <- callocBytes 192
        setSimpleSelector ptrStructSimpleSelector x

        p <- peek ptrStructSimpleSelector
        putStr ("configured simple selector 2: ")
        putStr (show p)
        putStr "\n"

        setSelectorElem (advancePtr ptrStructSimpleSelector 1) xs





hll_cssParseSelector :: Ptr HelloCssParser -> Ptr HelloCssSelector -> Int -> CString -> CString -> IO Bool
hll_cssParseSelector ptrStructCssParser ptrStructSelector tokType cTokValue cBuf = do
  buf      <- BSU.unsafePackCString $ cBuf
  tokValue <- BSU.unsafePackCString $ cTokValue
  hllParser <- peek ptrStructCssParser
  cStructPtrSelector  <- peek ptrStructSelector
  let inBlock = withinBlockC hllParser
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = fromIntegral . bufOffsetC $ hllParser
                            , spaceSeparated = (fromIntegral . bufOffsetC $ hllParser) > 0
                            }

  let ((newParser, newToken), newSelector, valid) = parseSelector (parser, inputToken)

{-
  putStr "ZAAA - input buffer: "
  putStrLn (show buf)
  putStr "ZAAA - input token: "
  putStrLn (show tokValue)
  putStr "ZAAA - output token: "
  putStrLn (show newToken)
  putStr "ZAAA - input parser: "
  putStrLn (show parser)
  putStr "ZAAA - parsed selector: "
  putStrLn (show newSelector)
  putStr "ZAA valid = "
  putStrLn (show valid)
  putStr "\n"
-}
  manipulateOutPtr ptrStructCssParser newParser newToken (if withinBlock newParser then 1 else 0)

  when (valid == 0) $ (setArrayOfPointers (simpleSelectorList newSelector) fun ptrStructSelector (#offset c_css_selector_t, c_simple_selector_list))
  when (valid == 0) $ pokeByteOff ptrStructSelector (#offset c_css_selector_t, c_simple_selector_list_size) (length . simpleSelectorList $ newSelector)

{-
  putStr "ZAAA - selector after setting pointer "
  putStr (show (length (simpleSelectorList newSelector)))
  putStr (show newSelector)
  putStr "\n"
  p <- peek ptrStructSelector
  putStr (show p)
  putStr "\n"
  --printSelector . simpleSelectorList $ newSelector
  putStr "\n\n\n"
-}
  return (valid == 0)




fun :: CssSimpleSelector -> IO (Ptr HelloCssSimpleSelector)
fun ss = do
  ptrStructSimpleSelector <- callocBytes 192
  setSimpleSelector ptrStructSimpleSelector ss
  return ptrStructSimpleSelector




printSelector :: Ptr HelloCssSimpleSelector -> CssSelector -> IO ()
printSelector ptrStructSimpleSelector selector = do
  setSelectorElem ptrStructSimpleSelector (simpleSelectorList selector)
    where
      setSelectorElem :: Ptr HelloCssSimpleSelector -> [CssSimpleSelector] -> IO ()
      setSelectorElem ptrStructSimpleSelector []     = return ()
      setSelectorElem ptrStructSimpleSelector (x:xs) = do
        putStr "++++++++++++++\n"
        putStr (show x)
        putStr "---------------\n"

        p <- peek ptrStructSimpleSelector
        putStr ("configured simple selector 3: ")
        putStr (show p)
        putStr "\n"

        setSelectorElem (advancePtr ptrStructSimpleSelector 1) xs




-- Save given array of items 'a' as array of pointers to objects b.
-- The pointers are allocated by this function.
-- The pointers are stored in a structure given by second arg.
-- Byte Offset to beginning of the array of pointers (to first cell) is given by fourth arg.
setArrayOfPointers :: (Storable b) => [a] -> (a -> IO b) -> Ptr c -> Int ->  IO ()
setArrayOfPointers [] f ptrStructSimpleSelector byteOffset = do
  return ()
setArrayOfPointers (x:xs) f ptrStructSimpleSelector byteOffset = do
  let pointerSize = 8
  ptr <- f x
  pokeByteOff ptrStructSimpleSelector byteOffset ptr
  setArrayOfPointers xs f ptrStructSimpleSelector (byteOffset + pointerSize)



textToPtrString :: T.Text -> IO CString
textToPtrString = newCString . T.unpack


