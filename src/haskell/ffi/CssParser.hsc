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

foreign export ccall "hll_cssParseSelector"  hll_cssParseSelector :: Ptr HelloCssParser -> Int -> CString -> CString -> IO (Ptr HelloCssSelector)

foreign export ccall "hll_cssPropertyInfoIdxByName" hll_cssPropertyInfoIdxByName :: CString -> IO Int
foreign export ccall "hll_cssPropertyNameString" hll_cssPropertyNameString :: Int -> IO CString

foreign export ccall "hll_parseDeclarationNormal" hll_parseDeclarationNormal :: Ptr HelloCssParser -> Int -> CString -> CString -> Ptr HelloCssDeclarationValue -> IO Int


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
  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_class_size) (length . selectorClass $ simpleSelector)

  setArrayOfPointers (selectorPseudoClass simpleSelector) textToPtrString ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_pseudo_class)
  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_pseudo_class_size) (length . selectorPseudoClass $ simpleSelector)

  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_id) cStringPtrSelId
  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_element) (selectorElement simpleSelector)

  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_combinator) (combinator simpleSelector)




hll_cssParseSelector :: Ptr HelloCssParser -> Int -> CString -> CString -> IO (Ptr HelloCssSelector)
hll_cssParseSelector ptrStructCssParser tokType cTokValue cBuf = do
  buf      <- BSU.unsafePackCString $ cBuf
  tokValue <- BSU.unsafePackCString $ cTokValue
  hllParser <- peek ptrStructCssParser
  let inBlock = withinBlockC hllParser
  let inputToken = getTokenADT tokType (T.E.decodeLatin1 tokValue)

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , withinBlock = inBlock > 0
                            , bufOffset   = fromIntegral . bufOffsetC $ hllParser
                            , spaceSeparated = (fromIntegral . bufOffsetC $ hllParser) > 0
                            }

  let ((newParser, newToken), newSelector) = parseSelector (parser, inputToken)

  manipulateOutPtr ptrStructCssParser newParser newToken (if withinBlock newParser then 1 else 0)
  case newSelector of
    Just sel -> do
      ptrStructSelector <- callocBytes #{size c_css_selector_t}
      (setArrayOfPointers (simpleSelectorList sel) simpleSelectorToPtrStruct ptrStructSelector (#offset c_css_selector_t, c_simple_selector_list))
      pokeByteOff ptrStructSelector (#offset c_css_selector_t, c_simple_selector_list_size) (length . simpleSelectorList $ sel)
      return ptrStructSelector
    Nothing ->
      return nullPtr




-- Get pointer to newly allocated pointer to C structure representing given
-- simple selector.
--
-- This function allocates memory, but since the goal of this project is to
-- replace C/C++ code with Haskell code, the allocation will be eventually
-- removed. So I don't care about deallocating the memory.
simpleSelectorToPtrStruct :: CssSimpleSelector -> IO (Ptr HelloCssSimpleSelector)
simpleSelectorToPtrStruct simSel = do
  ptrStructSimpleSelector <- callocBytes #{size c_css_simple_selector_t}
  setSimpleSelector ptrStructSimpleSelector simSel
  return ptrStructSimpleSelector




-- Get pointer to newly allocated pointer to C string representing given
-- text.
--
-- This function allocates memory, but since the goal of this project is to
-- replace C/C++ code with Haskell code, the allocation will be eventually
-- removed. So I don't care about deallocating the memory.
textToPtrString :: T.Text -> IO CString
textToPtrString = newCString . T.unpack




-- Save given array of items 'a' as array of pointers to objects 'b'.
-- The pointers are allocated by this function.
-- The pointers are stored in a structure given by second arg.
-- Byte Offset to beginning of the array of pointers (to first cell) is given by fourth arg.
--
-- 'c' is a parent object, of which the array is a member.
--
-- 'f' converts input items into pointers to allocated memory. The pointers
-- will be read and interpreted by C code. This can be a function that e.g.
-- converts Data.Text into char* strings.
setArrayOfPointers :: (Storable b) => [a] -> (a -> IO b) -> Ptr c -> Int -> IO ()
setArrayOfPointers [] f ptrParent byteOffset = do
  return ()
setArrayOfPointers (x:xs) f ptrParent byteOffset = do
  let pointerSize = 8 -- TODO: hardcoded value.
  ptr <- f x
  pokeByteOff ptrParent byteOffset ptr
  setArrayOfPointers xs f ptrParent (byteOffset + pointerSize)





data HelloCssDeclarationValue = HelloCssDeclarationValue {
    typeTagC   :: CInt
  , intValC    :: CInt
  , textValC   :: CString
  , importantC :: CInt
  } deriving (Show)




instance Storable HelloCssDeclarationValue where
  sizeOf    _ = #{size c_css_declaration_value_t}
  alignment _ = #{alignment c_css_declaration_value_t}

  poke ptr (HelloCssDeclarationValue argTypeTag argIntVal argTextVal argImportant) = do
    #{poke c_css_declaration_value_t, c_type_tag}  ptr argTypeTag
    #{poke c_css_declaration_value_t, c_int_val}   ptr argIntVal
    #{poke c_css_declaration_value_t, c_text_val}  ptr argTextVal
    #{poke c_css_declaration_value_t, c_important} ptr argImportant

  peek ptr = do
    a <- #{peek c_css_declaration_value_t, c_type_tag}  ptr
    b <- #{peek c_css_declaration_value_t, c_int_val}   ptr
    c <- #{peek c_css_declaration_value_t, c_text_val}  ptr
    d <- #{peek c_css_declaration_value_t, c_important} ptr
    return (HelloCssDeclarationValue a b c d)



hll_parseDeclarationNormal :: Ptr HelloCssParser -> Int -> CString -> CString -> Ptr HelloCssDeclarationValue -> IO Int
-- (CssParser, CssToken) -> ((CssParser, CssToken), Bool, Int, CssDeclarationValue)
hll_parseDeclarationNormal ptrStructCssParser tokType cTokValue cBuf ptrStructCssDeclarationValue = do
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

  let ((newParser, newToken), value, ret) = parseDeclarationNormal (parser, inputToken)
  manipulateOutPtr ptrStructCssParser parser newToken inBlock
  if ret >= 0
  then case value of
         Just v -> do
           ptrStr <- newCString . T.unpack . textVal $ v
           poke ptrStructCssDeclarationValue $ HelloCssDeclarationValue (fromIntegral . typeTag $ v) (fromIntegral . intVal $ v) ptrStr (if important v then 1 else 0)
           return ret
         Nothing ->
           return (-1)
  else return ret

