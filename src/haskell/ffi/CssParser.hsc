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
import qualified Data.Vector as V
import Control.Applicative
import Control.Monad -- when
import CssParser




foreign export ccall "hll_nextToken" hll_nextToken :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> IO CString
foreign export ccall "hll_declarationValueAsString" hll_declarationValueAsString :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> Int -> Int -> IO CString
foreign export ccall "hll_ignoreBlock" hll_ignoreBlock :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> IO Int
foreign export ccall "hll_ignoreStatement" hll_ignoreStatement :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> IO Int

foreign export ccall "hll_cssLengthType" hll_cssLengthType :: Int -> IO Int
foreign export ccall "hll_cssLengthValue" hll_cssLengthValue :: Int -> IO Float
foreign export ccall "hll_cssCreateLength" hll_cssCreateLength :: Float -> Int -> IO Int

foreign export ccall "hll_cssParseSelectors" hll_cssParseSelectors :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> Ptr HelloCssSelector -> IO Int

foreign export ccall "hll_cssShorthandInfoIdxByName" hll_cssShorthandInfoIdxByName :: CString -> IO Int
foreign export ccall "hll_cssPropertyInfoIdxByName" hll_cssPropertyInfoIdxByName :: CString -> IO Int
foreign export ccall "hll_cssPropertyNameString" hll_cssPropertyNameString :: Int -> IO CString

foreign export ccall "hll_parseDeclaration" hll_parseDeclaration :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> Ptr FfiCssDeclaration -> IO Int




#include "../hello.h"

data HelloCssParser = HelloCssParser {
    spaceSeparatedC :: CInt
  , bufOffsetC      :: CInt
  , inBlockC        :: CInt
  } deriving (Show)




data HelloCssToken = HelloCssToken {
    typeC  :: CInt
  , valueC :: CString
  } deriving (Show)




instance Storable HelloCssParser where
  sizeOf    _ = #{size c_css_parser_t}
  alignment _ = #{alignment c_css_parser_t}

  poke ptr (HelloCssParser argSpaceSeparated argBufOffset argInBlock) = do
    #{poke c_css_parser_t, c_space_separated} ptr argSpaceSeparated
    #{poke c_css_parser_t, c_buf_offset}      ptr argBufOffset
    #{poke c_css_parser_t, c_in_block}        ptr argInBlock

  peek ptr = do
    a <- #{peek c_css_parser_t, c_space_separated} ptr
    b <- #{peek c_css_parser_t, c_buf_offset}      ptr
    c <- #{peek c_css_parser_t, c_in_block}        ptr
    return (HelloCssParser a b c)




instance Storable HelloCssToken where
  sizeOf    _ = #{size c_css_token_t}
  alignment _ = #{alignment c_css_token_t}

  poke ptr (HelloCssToken argType argValue) = do
    #{poke c_css_token_t, c_type}  ptr argType
    #{poke c_css_token_t, c_value} ptr argValue

  peek ptr = do
    a <- #{peek c_css_token_t, c_type}  ptr
    b <- #{peek c_css_token_t, c_value} ptr
    return (HelloCssToken a b)




hll_nextToken :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> IO CString
hll_nextToken ptrStructCssParser ptrStructCssToken cBuf = do
  buf     <- BSU.unsafePackCString cBuf
  cParser <- peek ptrStructCssParser

  let (newParser, newToken) = nextToken defaultParser{ remainder = T.E.decodeLatin1 buf
                                                     , inBlock   = (fromIntegral . inBlockC $ cParser) /= 0
                                                     , bufOffset = fromIntegral . bufOffsetC $ cParser
                                                     , spaceSeparated = (fromIntegral . spaceSeparatedC $ cParser) /= 0
                                                     }

  updateParserStruct ptrStructCssParser newParser
  updateTokenStruct ptrStructCssToken newToken

  cstr newToken



-- Set fields in pointer to struct passed from C code.
updateParserStruct :: Ptr HelloCssParser -> CssParser -> IO ()
updateParserStruct ptrStructCssParser parser = do
  poke ptrStructCssParser $ HelloCssParser (if spaceSeparated parser then 1 else 0) (fromIntegral . bufOffset $ parser) (if inBlock parser then 1 else 0)




-- Set fields in pointer to struct passed from C code.
updateTokenStruct :: Ptr HelloCssToken-> CssToken -> IO ()
updateTokenStruct ptrStructCssToken token = do
  s <- cstr token
  poke ptrStructCssToken $ HelloCssToken (getTokenType token) s




cstr :: CssToken -> IO CString
cstr token = case token of
    (CssTokNum (CssNumI i)) -> (newCString . show $ i)
    (CssTokNum (CssNumF f)) -> (newCString . show $ f)
    (CssTokHash c) -> (newCString . T.unpack $ c)
    (CssTokIdent s) -> (newCString . T.unpack $ s)
    (CssTokStr s) -> (newCString . T.unpack $ s)
    (CssTokCh c)  -> (newCString . T.unpack . T.singleton $ c)
    CssTokWS      -> (newCString " ")
    otherwise     -> return nullPtr




hll_declarationValueAsString :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> Int -> Int -> IO CString
hll_declarationValueAsString ptrStructCssParser ptrStructCssToken cBuf valueType property = do
  buf     <- BSU.unsafePackCString $ cBuf
  cParser <- peek ptrStructCssParser

  let parser = defaultParser{ remainder   = T.E.decodeLatin1 buf
                            , inBlock     = (fromIntegral . inBlockC $ cParser) /= 0
                            , bufOffset   = fromIntegral . bufOffsetC $ cParser
                            , spaceSeparated = (fromIntegral . spaceSeparatedC $ cParser) /= 0
                            }

  cToken   <- peek ptrStructCssToken
  tokValue <- BSU.unsafePackCString . valueC $ cToken
  let token = getTokenADT (typeC cToken) (T.E.decodeLatin1 tokValue)

  let pair@((newParser, newToken), textVal) = declValueAsString (parser, token) (cssPropertyInfo V.! property) (toCssValueType valueType)

  updateParserStruct ptrStructCssParser newParser
  updateTokenStruct ptrStructCssToken newToken

  case textVal of
    Just t -> newCString . T.unpack $ t
    _      -> return nullPtr




hll_ignoreBlock :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> IO Int
hll_ignoreBlock ptrStructCssParser ptrStructCssToken cBuf = do
  buf     <- BSU.unsafePackCString $ cBuf
  cParser <- peek ptrStructCssParser
  let parser = defaultParser{ remainder = T.E.decodeLatin1 buf
                            , inBlock   = (fromIntegral . inBlockC $ cParser) /= 0
                            , bufOffset = fromIntegral . bufOffsetC $ cParser
                            , spaceSeparated = (fromIntegral . spaceSeparatedC $ cParser) /= 0
                            }

  let (newParser, newToken) = ignoreBlock parser -- TODO: shouldn't we pass current token to the function?
  updateParserStruct ptrStructCssParser newParser
  updateTokenStruct ptrStructCssToken newToken
  return 0




hll_ignoreStatement :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> IO Int
hll_ignoreStatement ptrStructCssParser ptrStructCssToken cBuf = do
  buf     <- BSU.unsafePackCString $ cBuf
  cParser <- peek ptrStructCssParser
  let parser = defaultParser{ remainder = T.E.decodeLatin1 buf
                            , inBlock   = (fromIntegral . inBlockC $ cParser) /= 0
                            , bufOffset = fromIntegral . bufOffsetC $ cParser
                            , spaceSeparated = (fromIntegral . spaceSeparatedC $ cParser) /= 0
                            }

  let (newParser, newToken) = ignoreBlock parser -- TODO: shouldn't we pass current token to the function?
  updateParserStruct ptrStructCssParser newParser
  updateTokenStruct ptrStructCssToken newToken
  return 0




getTokenType (CssTokIdent  _) = 0
getTokenType (CssTokStr  _) = 1
getTokenType (CssTokCh   _) = 2
getTokenType (CssTokEnd)    = 3
getTokenType _              = 4




getTokenADT tokType tokValue | tokType == 0 = CssTokIdent tokValue
                             | tokType == 1 = CssTokStr tokValue
                             | tokType == 2 = CssTokCh  (T.head tokValue)
                             | tokType == 3 = CssTokEnd





hll_cssLengthType :: Int -> IO Int
hll_cssLengthType len = do
  return (cssLengthType len)




hll_cssLengthValue :: Int -> IO Float
hll_cssLengthValue len = do
  return (cssLengthValue len)




hll_cssCreateLength :: Float -> Int -> IO Int
hll_cssCreateLength val t = do
  return (cssCreateLength val t)




hll_cssShorthandInfoIdxByName :: CString -> IO Int
hll_cssShorthandInfoIdxByName cShorthandName = do
  shorthandName <- BSU.unsafePackCString $ cShorthandName
  case cssShorthandInfoIdxByName . T.E.decodeLatin1 $ shorthandName of
    Just idx -> return idx
    Nothing  -> return (-1)




hll_cssPropertyInfoIdxByName :: CString -> IO Int
hll_cssPropertyInfoIdxByName cPropertyName = do
  propertyName <- BSU.unsafePackCString $ cPropertyName
  case cssPropertyInfoIdxByName . T.E.decodeLatin1 $ propertyName of
    Just idx -> return idx
    Nothing  -> return (-1)




hll_cssPropertyNameString :: Int -> IO CString
hll_cssPropertyNameString property = do
  let name = cssPropertyNameString property
  newCString . T.unpack $ name




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




hll_cssParseSelector :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> IO (Ptr HelloCssSelector)
hll_cssParseSelector ptrStructCssParser ptrStructCssToken cBuf = do
  buf     <- BSU.unsafePackCString $ cBuf
  cParser <- peek ptrStructCssParser

  let parser = defaultParser{ remainder = T.E.decodeLatin1 buf
                            , inBlock   = (fromIntegral . inBlockC $ cParser) /= 0
                            , bufOffset = fromIntegral . bufOffsetC $ cParser
                            , spaceSeparated = (fromIntegral . spaceSeparatedC $ cParser) /= 0
                            }

  cToken   <- peek ptrStructCssToken
  tokValue <- BSU.unsafePackCString . valueC $ cToken
  let token = getTokenADT (typeC cToken) (T.E.decodeLatin1 tokValue)

  let ((newParser, newToken), newSelector) = parseSelector (parser, token)

  updateParserStruct ptrStructCssParser newParser
  updateTokenStruct ptrStructCssToken newToken

  case newSelector of
    Just sel -> do
      ptrStructSelector <- callocBytes #{size c_css_selector_t}
      setArrayOfPointers (simpleSelectorList sel) simpleSelectorToPtrStruct ptrStructSelector (#offset c_css_selector_t, c_simple_selector_list)
      pokeByteOff ptrStructSelector (#offset c_css_selector_t, c_simple_selector_list_size) (length . simpleSelectorList $ sel)
      return ptrStructSelector
    Nothing ->
      return nullPtr



hll_cssParseSelectors :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> Ptr HelloCssSelector -> IO Int
hll_cssParseSelectors ptrStructCssParser ptrStructCssToken cBuf ptrStructCssSelector = do
  buf     <- BSU.unsafePackCString $ cBuf
  cParser <- peek ptrStructCssParser

  let parser = defaultParser{ remainder = T.E.decodeLatin1 buf
                            , inBlock   = (fromIntegral . inBlockC $ cParser) /= 0
                            , bufOffset = fromIntegral . bufOffsetC $ cParser
                            , spaceSeparated = (fromIntegral . spaceSeparatedC $ cParser) /= 0
                            }

  cToken   <- peek ptrStructCssToken
  tokValue <- BSU.unsafePackCString . valueC $ cToken
  let token = getTokenADT (typeC cToken) (T.E.decodeLatin1 tokValue)

  let ((newParser, newToken), selectors) = parseSelectors (parser, token)

  putStrLn ("Selectors are " ++ (show selectors))

  updateParserStruct ptrStructCssParser newParser
  updateTokenStruct ptrStructCssToken newToken
  updateSelectors ptrStructCssSelector selectors

  return (length selectors)




updateSelectors :: Ptr HelloCssSelector -> [CssSelector] -> IO ()
updateSelectors ptr (s:ss) = do

  setArrayOfPointers (simpleSelectorList s) simpleSelectorToPtrStruct ptr (#offset c_css_selector_t, c_simple_selector_list)
  pokeByteOff ptr (#offset c_css_selector_t, c_simple_selector_list_size) (length . simpleSelectorList $ s)

  -- ptrStr <- newCString . T.unpack . textVal . value $ s
  -- poke ptr $ HelloCssDecl (fromIntegral . typeTag . value $ s) (fromIntegral . intVal . value $ s) ptrStr (if important s then 1 else 0) (fromIntegral . property $ d)
  updateSelectors (advancePtr ptr 1) ss
updateSelectors ptr [] = return ()





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




data FfiCssDeclaration = FfiCssDeclaration {
    ptrValueC  :: Ptr FfiCssValue
  , importantC :: CInt
  , propertyC  :: CInt
  } deriving (Show)




data FfiCssValue = FfiCssValue {
    typeTagC   :: CInt
  , intValC    :: CInt
  , textValC   :: CString
  } deriving (Show)



instance Storable FfiCssDeclaration where
  sizeOf    _ = #{size c_css_declaration_t}
  alignment _ = #{alignment c_css_declaration_t}

  poke ptr (FfiCssDeclaration argPtrStructValue argImportant argProperty ) = do
    #{poke c_css_declaration_t, c_value}     ptr argPtrStructValue
    #{poke c_css_declaration_t, c_important} ptr argImportant
    #{poke c_css_declaration_t, c_property}  ptr argProperty

  peek ptr = do
    a <- #{peek c_css_declaration_t, c_value}     ptr
    b <- #{peek c_css_declaration_t, c_important} ptr
    c <- #{peek c_css_declaration_t, c_property}  ptr
    return (FfiCssDeclaration a b c)



instance Storable FfiCssValue where
  sizeOf    _ = #{size c_css_value_t}
  alignment _ = #{alignment c_css_value_t}

  poke ptr (FfiCssValue argTypeTag argIntVal argTextVal) = do
    #{poke c_css_value_t, c_type_tag} ptr argTypeTag
    #{poke c_css_value_t, c_int_val}  ptr argIntVal
    #{poke c_css_value_t, c_text_val} ptr argTextVal

  peek ptr = do
    a <- #{peek c_css_value_t, c_type_tag} ptr
    b <- #{peek c_css_value_t, c_int_val}  ptr
    c <- #{peek c_css_value_t, c_text_val} ptr
    return (FfiCssValue a b c)




hll_parseDeclaration :: Ptr HelloCssParser -> Ptr HelloCssToken -> CString -> Ptr FfiCssDeclaration -> IO Int
hll_parseDeclaration ptrStructCssParser ptrStructCssToken cBuf ptrStructCssValue = do
  buf     <- BSU.unsafePackCString $ cBuf
  cParser <- peek ptrStructCssParser

  let parser = defaultParser{ remainder = T.E.decodeLatin1 buf
                            , inBlock   = (fromIntegral . inBlockC $ cParser) /= 0
                            , bufOffset = fromIntegral . bufOffsetC $ cParser
                            , spaceSeparated = (fromIntegral . spaceSeparatedC $ cParser) /= 0
                            }

  cToken   <- peek ptrStructCssToken
  tokValue <- BSU.unsafePackCString . valueC $ cToken
  let token = getTokenADT (typeC cToken) (T.E.decodeLatin1 tokValue)

  let ((newParser, newToken), declarations) = parseDeclaration (parser, token)

  putStrLn ("Declarations are " ++ (show declarations))

  updateParserStruct ptrStructCssParser newParser
  updateTokenStruct ptrStructCssToken newToken
  updateDeclarations ptrStructCssValue declarations

  return (length declarations)




updateDeclarations :: Ptr FfiCssDeclaration -> [CssDeclaration] -> IO ()
updateDeclarations ptrStructDeclaration (d:ds) = do
  ptrString <- newCString . T.unpack . textVal . value $ d
  let t = cssValueTypeToInt . typeTag . value $ d
  ptrStructCssValue <- callocBytes #{size c_css_value_t}
  poke ptrStructCssValue $ FfiCssValue (fromIntegral t) (fromIntegral . intVal . value $ d) ptrString

  poke ptrStructDeclaration $ FfiCssDeclaration ptrStructCssValue (if important d then 1 else 0) (fromIntegral . property $ d)

  updateDeclarations (advancePtr ptrStructDeclaration 1) ds
updateDeclarations ptrStructDeclaration [] = return ()




cssValueTypeToInt valueType = case valueType of
                                CssValueTypeInt                 ->  0
                                CssValueTypeEnum                ->  1
                                CssValueTypeMultiEnum           ->  2
                                CssValueTypeLengthPercent       ->  3
                                CssValueTypeLength              ->  4
                                CssValueTypeSignedLength        ->  5
                                CssValueTypeLengthPercentNumber ->  6
                                CssValueTypeAuto                ->  7
                                CssValueTypeColor               ->  8
                                CssValueTypeFontWeight          ->  9
                                CssValueTypeString              -> 10
                                CssValueTypeStringList          -> 11
                                CssValueTypeURI                 -> 12
                                CssValueTypeBgPosition          -> 13
                                CssValueTypeUnused              -> 14




toCssValueType :: Int -> CssValueType
toCssValueType i | i ==  0 = CssValueTypeInt
                 | i ==  1 = CssValueTypeEnum
                 | i ==  2 = CssValueTypeMultiEnum
                 | i ==  3 = CssValueTypeLengthPercent
                 | i ==  4 = CssValueTypeLength
                 | i ==  5 = CssValueTypeSignedLength
                 | i ==  6 = CssValueTypeLengthPercentNumber
                 | i ==  7 = CssValueTypeAuto
                 | i ==  8 = CssValueTypeColor
                 | i ==  9 = CssValueTypeFontWeight
                 | i == 10 = CssValueTypeString
                 | i == 11 = CssValueTypeStringList
                 | i == 12 = CssValueTypeURI
                 | i == 13 = CssValueTypeBgPosition
                 | i == 14 = CssValueTypeUnused



