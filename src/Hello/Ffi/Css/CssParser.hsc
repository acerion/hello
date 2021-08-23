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




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Ffi.Css.Parser( FfiCssSimpleSelector (..)
                           , peekCssSimpleSelector

                           , FfiCssSelector (..)
                           , peekCssSelector
                           , pokeCssSelector

                           , FfiCssDeclarationSet (..)
                           , peekCssDeclarationSet
                           , pokeCssDeclarationSet

                           , FfiCssParser (..)
                           , peekCssParser
                           , pokeCssParser

                           , FfiCssToken (..)
                           , peekCssToken
                           , pokeCssToken

                           , getCssOrigin
                           )
  where




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
import qualified Data.Sequence as S
import qualified Data.Foldable as Foldable
import Control.Applicative
import Control.Monad -- when
import Debug.Trace

import Hello.Css.Tokenizer
import Hello.Css.Parser
import Hello.Ffi.Utils




foreign export ccall "hll_nextToken" hll_nextToken :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO CString
foreign export ccall "hll_declarationValueAsString" hll_declarationValueAsString :: Ptr FfiCssParser -> Ptr FfiCssToken -> Int -> Int -> IO CString
foreign export ccall "hll_ignoreBlock" hll_ignoreBlock :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO Int
foreign export ccall "hll_ignoreStatement" hll_ignoreStatement :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO Int

foreign export ccall "hll_cssLengthType" hll_cssLengthType :: Word32 -> IO Int
foreign export ccall "hll_cssLengthValue" hll_cssLengthValue :: Word32 -> IO Float
foreign export ccall "hll_cssCreateLength" hll_cssCreateLength :: Float -> Int -> IO Word32

foreign export ccall "hll_cssShorthandInfoIdxByName" hll_cssShorthandInfoIdxByName :: CString -> IO Int
foreign export ccall "hll_cssPropertyInfoIdxByName" hll_cssPropertyInfoIdxByName :: CString -> IO Int
foreign export ccall "hll_cssPropertyNameString" hll_cssPropertyNameString :: Int -> IO CString

foreign export ccall "hll_declarationListAddOrUpdateDeclaration" hll_declarationListAddOrUpdateDeclaration :: Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclaration -> IO Int

foreign export ccall "hll_declarationListAppend" hll_declarationListAppend :: Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> IO ()
foreign export ccall "hll_cssParseElementStyleAttribute" hll_cssParseElementStyleAttribute :: Ptr () -> CString -> CInt -> Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> IO ()

foreign export ccall "hll_isTokenComma" hll_isTokenComma :: Ptr FfiCssToken -> IO Int
foreign export ccall "hll_isTokenSemicolon" hll_isTokenSemicolon :: Ptr FfiCssToken -> IO Int
foreign export ccall "hll_isTokenBraceCurlyOpen" hll_isTokenBraceCurlyOpen :: Ptr FfiCssToken -> IO Int
foreign export ccall "hll_isTokenBraceCurlyClose" hll_isTokenBraceCurlyClose :: Ptr FfiCssToken -> IO Int



#include "../../hello.h"




data FfiCssParser = FfiCssParser {
    spaceSeparatedC :: CInt
  , bufOffsetC      :: CInt
  , inBlockC        :: CInt

  , cParserBuf      :: Ptr CChar
  , cParserBuflen   :: CInt
  , cCssOrigin      :: CInt
  } deriving (Show)




instance Storable FfiCssParser where
  sizeOf    _ = #{size c_css_parser_t}
  alignment _ = #{alignment c_css_parser_t}

  peek ptr = do
    a <- #{peek c_css_parser_t, c_space_separated} ptr
    b <- #{peek c_css_parser_t, c_buf_offset}      ptr
    c <- #{peek c_css_parser_t, c_in_block}        ptr
    d <- #{peek c_css_parser_t, c_parser_buf}      ptr
    e <- #{peek c_css_parser_t, c_parser_buflen}   ptr
    f <- #{peek c_css_parser_t, c_origin}          ptr
    return (FfiCssParser a b c d e f)

  poke ptr (FfiCssParser argSpaceSeparated argBufOffset argInBlock argBuf argBuflen argOrigin) = do
    #{poke c_css_parser_t, c_space_separated} ptr argSpaceSeparated
    #{poke c_css_parser_t, c_buf_offset}      ptr argBufOffset
    #{poke c_css_parser_t, c_in_block}        ptr argInBlock
    -- #{poke c_css_parser_t, c_parser_buf}      ptr argBuf
    -- #{poke c_css_parser_t, c_parser_buflen}   ptr argBuflen
    #{poke c_css_parser_t, c_origin}          ptr argOrigin




peekCssParser :: Ptr FfiCssParser -> IO CssParser
peekCssParser ptrStructCssParser = do
  ffiParser <- peek ptrStructCssParser

  let buf           = cParserBuf ffiParser
  let offset        = fromIntegral . bufOffsetC $ ffiParser
  let bufWithOffset = plusPtr buf offset
  rem <- ptrCCharToText bufWithOffset

  let parser = defaultParser{ remainder      = rem
                            , inBlock        = (fromIntegral . inBlockC $ ffiParser) /= 0
                            , bufOffset      = offset
                            , spaceSeparated = (fromIntegral . spaceSeparatedC $ ffiParser) /= 0
                            , cssOrigin      = getCssOrigin . fromIntegral . cCssOrigin $ ffiParser
                            }
  return parser



pokeCssParser :: Ptr FfiCssParser -> CssParser -> IO ()
pokeCssParser ptrStructCssParser parser = do
  let sep = if spaceSeparated parser then 1 else 0
  let off = fromIntegral . bufOffset $ parser
  let blk = if inBlock parser then 1 else 0

  let doesntMatter = 0
  let len = doesntMatter
  let origin = fromIntegral . getIntOrigin . cssOrigin $ parser

  poke ptrStructCssParser $ FfiCssParser sep off blk nullPtr len origin




data FfiCssToken = FfiCssToken {
    typeC  :: CInt
  , valueC :: CString
  } deriving (Show)




instance Storable FfiCssToken where
  sizeOf    _ = #{size c_css_token_t}
  alignment _ = #{alignment c_css_token_t}

  poke ptr (FfiCssToken argType argValue) = do
    #{poke c_css_token_t, c_type}  ptr argType
    #{poke c_css_token_t, c_value} ptr argValue

  peek ptr = do
    a <- #{peek c_css_token_t, c_type}  ptr
    b <- #{peek c_css_token_t, c_value} ptr
    return (FfiCssToken a b)




peekCssToken :: Ptr FfiCssToken -> IO CssToken
peekCssToken ptrStructCssToken = do

  ffiToken <- peek ptrStructCssToken

  let e = valueC ffiToken == nullPtr
  emptyString <- newCString ""

  tokValue <- BSU.unsafePackCString (if e then emptyString else valueC ffiToken)
  let token = getTokenADT (typeC ffiToken) (T.E.decodeLatin1 tokValue)
  return token




-- Set fields in pointer to struct passed from C code.
pokeCssToken :: Ptr FfiCssToken-> CssToken -> IO ()
pokeCssToken ptrStructCssToken token = do
  s <- cstr token
  poke ptrStructCssToken $ FfiCssToken (getTokenType token) s




getTokenType (CssTokIdent  _) = 0
getTokenType (CssTokStr  _)   = 1
getTokenType (CssTokCh   _)   = 2
getTokenType (CssTokEnd)      = 3
getTokenType (CssTokBraceCurlyClose)  = 4
getTokenType (CssTokColon)            = 5
getTokenType (CssTokBraceSquareOpen)  = 6
getTokenType (CssTokBraceSquareClose) = 7
getTokenType (CssTokHash CssHashUn _) = 8
getTokenType (CssTokHash CssHashId _) = 9
getTokenType _                = 10




getTokenADT tokType tokValue | tokType == 0 = CssTokIdent tokValue
                             | tokType == 1 = CssTokStr tokValue
                             | tokType == 2 = CssTokCh  (T.head tokValue)
                             | tokType == 3 = CssTokEnd
                             | tokType == 4 = CssTokBraceCurlyClose
                             | tokType == 5 = CssTokColon
                             | tokType == 6 = CssTokBraceSquareOpen
                             | tokType == 7 = CssTokBraceSquareClose
                             | tokType == 8 = CssTokHash CssHashUn tokValue
                             | tokType == 9 = CssTokHash CssHashId tokValue
                             | otherwise = trace ("tok type ============= = " ++ (show tokType)) (CssTokEnd)




hll_nextToken :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO CString
hll_nextToken ptrStructCssParser ptrStructCssToken = do
  parser <- peekCssParser ptrStructCssParser

  let (newParser, newToken) = nextToken1 parser

  pokeCssParser ptrStructCssParser newParser
  pokeCssToken ptrStructCssToken newToken

  cstr newToken




cstr :: CssToken -> IO CString
cstr token = case token of
    (CssTokNum (CssNumI i)) -> (newCString . show $ i)
    (CssTokNum (CssNumF f)) -> (newCString . show $ f)
    (CssTokHash _ s) -> (newCString . T.unpack $ s)
    (CssTokIdent s)  -> (newCString . T.unpack $ s)
    (CssTokStr s) -> (newCString . T.unpack $ s)
    (CssTokCh c)  -> (newCString . T.unpack . T.singleton $ c)
    CssTokWS      -> (newCString " ")
    otherwise     -> return nullPtr




hll_declarationValueAsString :: Ptr FfiCssParser -> Ptr FfiCssToken -> Int -> Int -> IO CString
hll_declarationValueAsString ptrStructCssParser ptrStructCssToken valueType property = do
  parser <- peekCssParser ptrStructCssParser
  token  <- peekCssToken ptrStructCssToken

  let pair@((newParser, newToken), textVal) = declValueAsString valueType (parser, token) (cssPropertyInfo V.! property)

  pokeCssParser ptrStructCssParser newParser
  pokeCssToken ptrStructCssToken newToken

  case textVal of
    Just t -> newCString . T.unpack $ t
    _      -> return nullPtr




hll_ignoreBlock :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO Int
hll_ignoreBlock ptrStructCssParser ptrStructCssToken = do
  parser <- peekCssParser ptrStructCssParser

  let (newParser, newToken) = ignoreBlock parser -- TODO: shouldn't we pass current token to the function?
  pokeCssParser ptrStructCssParser newParser
  pokeCssToken ptrStructCssToken newToken
  return 0




hll_ignoreStatement :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO Int
hll_ignoreStatement ptrStructCssParser ptrStructCssToken = do
  parser <- peekCssParser ptrStructCssParser

  let (newParser, newToken) = ignoreBlock parser -- TODO: shouldn't we pass current token to the function?
  pokeCssParser ptrStructCssParser newParser
  pokeCssToken ptrStructCssToken newToken
  return 0




hll_cssLengthType :: Word32 -> IO Int
hll_cssLengthType word = do
  return (cssLengthWordToType word)




hll_cssLengthValue :: Word32 -> IO Float
hll_cssLengthValue word = do
  let intVal  = fromIntegral word
  let lenType = cssLengthWordToType word
  return (cssLengthValue $ CssLength intVal lenType)




hll_cssCreateLength :: Float -> Int -> IO Word32
hll_cssCreateLength f lenType = do
  case cssCreateLength f lenType of
    CssLength word _ -> return (fromIntegral word)




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




data FfiCssSimpleSelector = FfiCssSimpleSelector {
  -- equals to <char * c_selector_class[10]>,
  -- which equals to <char ** c_selector_class>
    selectorClassC           :: Ptr (Ptr CChar)
  , selectorClassSizeC       :: CInt

  -- equals to <char * c_pseudo_selector_class[10]>,
  -- which equals to <char ** c_pseudo_selector_class>
  , selectorPseudoClassC     :: Ptr (Ptr CChar)
  , selectorPseudoClassSizeC :: CInt

  , selectorIdC              :: CString
  , selectorTypeC            :: CInt

  , combinatorC              :: CInt
  } deriving (Show)




instance Storable FfiCssSimpleSelector where
  sizeOf    _ = #{size c_css_simple_selector_t}
  alignment _ = #{alignment c_css_simple_selector_t}

  peek ptr = do
    let a = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_simple_selector_t, c_selector_class}) ptr
    b <- #{peek c_css_simple_selector_t, c_selector_class_size} ptr
    let c = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_simple_selector_t, c_selector_pseudo_class}) ptr
    d <- #{peek c_css_simple_selector_t, c_selector_pseudo_class_size} ptr
    e <- #{peek c_css_simple_selector_t, c_selector_id} ptr
    f <- #{peek c_css_simple_selector_t, c_selector_type} ptr
    g <- #{peek c_css_simple_selector_t, c_combinator} ptr
    return (FfiCssSimpleSelector a b c d e f g)


  poke ptr (FfiCssSimpleSelector selectorClassI selector_class_size_I selector_pseudo_class_I selector_pseudo_class_size_I selector_id_I selector_type_I combinator_I) = do
    #{poke c_css_simple_selector_t, c_selector_class}             ptr selectorClassI
    #{poke c_css_simple_selector_t, c_selector_class_size}        ptr selector_class_size_I
    #{poke c_css_simple_selector_t, c_selector_pseudo_class}      ptr selector_pseudo_class_I
    #{poke c_css_simple_selector_t, c_selector_pseudo_class_size} ptr selector_pseudo_class_size_I
    #{poke c_css_simple_selector_t, c_selector_id}                ptr selector_id_I
    #{poke c_css_simple_selector_t, c_selector_type}              ptr selector_type_I
    #{poke c_css_simple_selector_t, c_combinator}                 ptr combinator_I




peekCssSimpleSelector :: Ptr FfiCssSimpleSelector -> IO CssSimpleSelector
peekCssSimpleSelector ptrStructSimpleSelector = do

  ffiSimSel <- peek ptrStructSimpleSelector

  let pcStringArray :: Ptr CString = (selectorPseudoClassC ffiSimSel)
  pc <- peekArrayOfPointers pcStringArray (fromIntegral . selectorPseudoClassSizeC $ ffiSimSel) ptrCCharToText

  selId <- ptrCCharToText . selectorIdC $ ffiSimSel

  let cStringArray :: Ptr CString = (selectorClassC ffiSimSel)
  c <- peekArrayOfPointers cStringArray (fromIntegral . selectorClassSizeC $ ffiSimSel) ptrCCharToText

  return CssSimpleSelector{ selectorPseudoClass = pc
                          , selectorId    = selId
                          , selectorClass = c
                          , selectorType  = fromIntegral . selectorTypeC $ ffiSimSel
                          , combinator    = cssCombinatorIntToData . fromIntegral . combinatorC $ ffiSimSel
                          }




-- Save given Haskell simple selector to C simple selector.
-- https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/hsc2hs.html
pokeCssSimpleSelector :: Ptr FfiCssSimpleSelector -> CssSimpleSelector -> IO ()
pokeCssSimpleSelector ptrStructSimpleSelector simpleSelector = do
  cStringPtrSelId <- if T.null . selectorId $ simpleSelector
                     then return nullPtr
                     else newCString . T.unpack . selectorId $ simpleSelector

  ffiSimSel <- peek ptrStructSimpleSelector

  pokeArrayOfPointersWithAlloc (selectorClass simpleSelector) allocAndPokeCString (selectorClassC ffiSimSel)
  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_class_size) (length . selectorClass $ simpleSelector)

  pokeArrayOfPointersWithAlloc (selectorPseudoClass simpleSelector) allocAndPokeCString (selectorPseudoClassC ffiSimSel)
  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_pseudo_class_size) (length . selectorPseudoClass $ simpleSelector)

  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_id) cStringPtrSelId
  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_selector_type) (selectorType simpleSelector)

  let comb :: CInt = cssCombinatorDataToInt . combinator $ simpleSelector
  pokeByteOff ptrStructSimpleSelector (#offset c_css_simple_selector_t, c_combinator) comb




-- Get pointer to newly allocated pointer to C structure representing given
-- simple selector.
--
-- This function allocates memory, but since the goal of this project is to
-- replace C/C++ code with Haskell code, the allocation will be eventually
-- removed. So I don't care about deallocating the memory.
allocAndPokeCssSimpleSelector :: CssSimpleSelector -> IO (Ptr FfiCssSimpleSelector)
allocAndPokeCssSimpleSelector simSel = do
  ptrStructSimpleSelector <- callocBytes #{size c_css_simple_selector_t}
  pokeCssSimpleSelector ptrStructSimpleSelector simSel
  return ptrStructSimpleSelector




data FfiCssSelector = FfiCssSelector {
    cMatchCacheOffset    :: CInt

    --    <c_css_simple_selector_t * c_simple_selector_list[10]>
    -- == <c_css_simple_selector_t ** c_simple_selector_list>
    -- == pointer to pointer(s) to simple selector struct
  , cSimpleSelectors     :: Ptr (Ptr FfiCssSimpleSelector)
  , cSimpleSelectorsSize :: CInt
  } deriving (Show)




instance Storable FfiCssSelector where
  sizeOf    _ = #{size c_css_selector_t}
  alignment _ = #{alignment c_css_selector_t}

  peek ptr = do
    a <- #{peek c_css_selector_t, c_match_cache_offset} ptr
    let b = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_selector_t, c_simple_selectors}) ptr
    c <- #{peek c_css_selector_t, c_simple_selectors_size} ptr
    return (FfiCssSelector a b c)

  poke ptr (FfiCssSelector inMatchCaseOffset inSimpleSelectors inSimpleSelectorsSize) = do
    #{poke c_css_selector_t, c_match_cache_offset}    ptr inMatchCaseOffset
    #{poke c_css_selector_t, c_simple_selectors}      ptr inSimpleSelectors
    #{poke c_css_selector_t, c_simple_selectors_size} ptr inSimpleSelectorsSize




peekCssSelector :: Ptr FfiCssSelector -> IO CssSelector
peekCssSelector ptrStructCssSelector = do

  ffiSel <- peek ptrStructCssSelector

  {- This would also work:
  let offset = #{offset c_css_selector_t, c_simple_selector_list}
  let ptrSimSelArray :: Ptr (Ptr FfiCssSimpleSelector) = plusPtr ptrStructCssSelector offset
  -}
  let ptrSimSelArray :: Ptr (Ptr FfiCssSimpleSelector) = cSimpleSelectors ffiSel

  let simSelCount = fromIntegral . cSimpleSelectorsSize $ ffiSel
  simSels <- peekArrayOfPointers ptrSimSelArray simSelCount peekCssSimpleSelector

  return CssSelector{ matchCacheOffset = fromIntegral . cMatchCacheOffset $ ffiSel
                    , simpleSelectors  = simSels
                    }




pokeCssSelector :: Ptr FfiCssSelector -> CssSelector -> IO ()
pokeCssSelector ptrStructCssSelector selector = do

  ffiSel <- peek ptrStructCssSelector

  pokeArrayOfPointersWithAlloc (simpleSelectors selector) allocAndPokeCssSimpleSelector (cSimpleSelectors ffiSel)
  pokeByteOff ptrStructCssSelector (#offset c_css_selector_t, c_simple_selectors_size) (length . simpleSelectors $ selector)




data FfiCssDeclaration = FfiCssDeclaration {
    importantC :: CInt
  , propertyC  :: CInt
  , ptrValueC  :: Ptr FfiCssValue
  } deriving (Show)




instance Storable FfiCssDeclaration where
  sizeOf    _ = #{size c_css_declaration_t}
  alignment _ = #{alignment c_css_declaration_t}

  poke ptr (FfiCssDeclaration argImportant argProperty argPtrStructValue) = do
    #{poke c_css_declaration_t, c_important} ptr argImportant
    #{poke c_css_declaration_t, c_property}  ptr argProperty
    #{poke c_css_declaration_t, c_value}     ptr argPtrStructValue

  peek ptr = do
    a <- #{peek c_css_declaration_t, c_important} ptr
    b <- #{peek c_css_declaration_t, c_property}  ptr
    c <- #{peek c_css_declaration_t, c_value}     ptr
    return (FfiCssDeclaration a b c)




peekCssDeclaration :: Ptr FfiCssDeclaration -> IO CssDeclaration
peekCssDeclaration ptr = do

  ffiDecl <- peek ptr

  when (ptrValueC ffiDecl == nullPtr) (trace ("Error: null pointer inside of declaration") putStr (""))

  ffiCssValue :: FfiCssValue <- peek . ptrValueC $ ffiDecl
  cssValue <- peekCssValue ffiCssValue

  return defaultDeclaration{ property = fromIntegral . propertyC $ ffiDecl
                           , declValue = cssValue
                           , important = importantC ffiDecl > 0}




distanceToWord :: CssDistance -> Int
distanceToWord distance = case cssCreateLength2 distance of
                            CssLength word _ -> word




allocAndPokeCssDeclaration :: CssDeclaration -> IO (Ptr FfiCssDeclaration)
allocAndPokeCssDeclaration declaration = do
  let textVal = case declValue declaration of
                  CssValueTypeString t     -> t
                  CssValueTypeStringList t -> t
                  CssValueTypeURI t        -> t
                  otherwise                -> ""

  let intVal = case declValue declaration of
                 CssValueTypeInt i                                  -> i
                 CssValueTypeEnum i                                 -> i
                 CssValueTypeMultiEnum i                            -> i
                 CssValueTypeLengthPercent distance       -> distanceToWord distance
                 CssValueTypeLength distance              -> distanceToWord distance
                 CssValueTypeSignedLength distance        -> distanceToWord distance
                 CssValueTypeLengthPercentNumber distance -> distanceToWord distance
                 CssValueTypeAuto distance                -> distanceToWord distance
                 CssValueTypeColor i                                -> i
                 CssValueTypeFontWeight i                           -> i
                 otherwise                                          -> 0

  ptrString <- newCString . T.unpack $ textVal
  let t :: CInt = fromIntegral . cssValueToTypeTag . declValue $ declaration
  let i :: CInt = fromIntegral $ intVal
  ptrStructCssValue <- callocBytes #{size c_css_value_t}
  poke ptrStructCssValue $ FfiCssValue t i 0 0 ptrString

  let imp :: CInt = if important declaration then 1 else 0
  let prop :: CInt = fromIntegral . property $ declaration
  ptrStructDeclaration :: Ptr FfiCssDeclaration <- callocBytes #{size c_css_declaration_t}
  poke ptrStructDeclaration $ FfiCssDeclaration imp prop ptrStructCssValue

  return ptrStructDeclaration




data FfiCssValue = FfiCssValue {
    typeTagC    :: CInt
  , intValC     :: CInt
  , posXC       :: CInt
  , posYC       :: CInt
  , ptrTextValC :: CString
  } deriving (Show)




instance Storable FfiCssValue where
  sizeOf    _ = #{size c_css_value_t}
  alignment _ = #{alignment c_css_value_t}

  poke ptr (FfiCssValue argTypeTag argIntVal argX argY argTextVal) = do
    #{poke c_css_value_t, c_type_tag} ptr argTypeTag
    #{poke c_css_value_t, c_int_val}  ptr argIntVal
    #{poke c_css_value_t, c_bg_pos_x} ptr argX
    #{poke c_css_value_t, c_bg_pos_y} ptr argY
    #{poke c_css_value_t, c_text_val} ptr argTextVal

  peek ptr = do
    a <- #{peek c_css_value_t, c_type_tag} ptr
    b <- #{peek c_css_value_t, c_int_val}  ptr
    c <- #{peek c_css_value_t, c_bg_pos_x} ptr
    d <- #{peek c_css_value_t, c_bg_pos_y} ptr
    e <- #{peek c_css_value_t, c_text_val} ptr
    return (FfiCssValue a b c d e)




data FfiCssDeclarationSet = FfiCssDeclarationSet {
    isSafeC           :: CInt
  , ptrDeclarationsC  :: Ptr (Ptr FfiCssDeclaration)
  , declarationsSizeC :: CInt
  } deriving (Show)




instance Storable FfiCssDeclarationSet where
  sizeOf    _ = #{size c_css_declaration_set_t}
  alignment _ = #{alignment c_css_declaration_set_t}

  poke ptr (FfiCssDeclarationSet argIsSafe argDeclarations argDeclarationsSize) = do
    #{poke c_css_declaration_set_t, c_is_safe}           ptr argIsSafe
    #{poke c_css_declaration_set_t, c_declarations}      ptr argDeclarations
    #{poke c_css_declaration_set_t, c_declarations_size} ptr argDeclarationsSize

  peek ptr = do
    a <- #{peek c_css_declaration_set_t, c_is_safe}           ptr
    let b = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_declaration_set_t, c_declarations}) ptr
    c <- #{peek c_css_declaration_set_t, c_declarations_size} ptr
    return (FfiCssDeclarationSet a b c)




peekCssDeclarationSet :: Ptr FfiCssDeclarationSet -> IO CssDeclarationSet
peekCssDeclarationSet ptrStructDeclarationSet = do

  ffiDeclSet <- peek ptrStructDeclarationSet

  when (ptrDeclarationsC ffiDeclSet == nullPtr) (trace ("Error: null pointer inside of declaration set") putStr (""))

  let len = (fromIntegral . declarationsSizeC $ ffiDeclSet)
  let array :: Ptr (Ptr FfiCssDeclaration) = ptrDeclarationsC ffiDeclSet
  decls <- peekArrayOfPointers array len peekCssDeclaration
  return CssDeclarationSet{ isSafe = isSafeC ffiDeclSet > 0
                          , items = S.fromList decls
                          }




pokeCssDeclarationSet :: Ptr FfiCssDeclarationSet -> CssDeclarationSet -> IO ()
pokeCssDeclarationSet ptrStructDeclarationSet newDeclSet = do

  ffiDeclSet <- peek ptrStructDeclarationSet

  let cIsSafe :: CInt = if isSafe newDeclSet then 1 else 0
  let cCount  :: CInt = fromIntegral . length . items $ newDeclSet

  pokeByteOff ptrStructDeclarationSet (#offset c_css_declaration_set_t, c_is_safe) cIsSafe
  pokeArrayOfPointersWithAlloc (Foldable.toList . items $ newDeclSet) allocAndPokeCssDeclaration (ptrDeclarationsC ffiDeclSet)
  pokeByteOff ptrStructDeclarationSet (#offset c_css_declaration_set_t, c_declarations_size) cCount
  --poke ptrStructDeclarationSet $ FfiCssDeclarationSet cIsSafe (ptrDeclarationsC ffiDeclSet) cCount -- TODO: why setting "array of pointers" field doesn't work?

  return ()




hll_parseDeclarationWrapper :: Ptr FfiCssParser -> Ptr FfiCssToken -> Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> IO ()
hll_parseDeclarationWrapper ptrStructCssParser ptrStructCssToken ptrStructCssDeclarationSet ptrStructCssDeclarationSetImp = do
  parser <- peekCssParser ptrStructCssParser
  token  <- peekCssToken ptrStructCssToken

  declSet    :: CssDeclarationSet <- peekCssDeclarationSet ptrStructCssDeclarationSet
  declSetImp :: CssDeclarationSet <- peekCssDeclarationSet ptrStructCssDeclarationSetImp

  let ((newParser, newToken), (newDeclSet, newDeclSetImp)) = parseDeclarationWrapper2 (parser, token) (declSet, declSetImp)

  pokeCssParser ptrStructCssParser newParser
  pokeCssToken ptrStructCssToken newToken

  pokeCssDeclarationSet ptrStructCssDeclarationSet newDeclSet
  pokeCssDeclarationSet ptrStructCssDeclarationSetImp newDeclSetImp

  when ((length . items $ newDeclSetImp) > 0) (putStrLn ("important decl set = " ++ (show newDeclSetImp)))

  return ()




hll_declarationListAddOrUpdateDeclaration :: Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclaration -> IO Int
hll_declarationListAddOrUpdateDeclaration ptrStructDeclarationSet ptrStructDeclaration = do

  when (ptrStructDeclarationSet == nullPtr) (trace ("Error: first arg to declarationListAddOrUpdateDeclaration is null pointer") putStr (""))

  declSet :: CssDeclarationSet <- peekCssDeclarationSet ptrStructDeclarationSet
  decl    :: CssDeclaration    <- peekCssDeclaration ptrStructDeclaration

  let newDeclSet = declarationsSetUpdateOrAdd declSet decl

  pokeCssDeclarationSet ptrStructDeclarationSet newDeclSet

  return 0



hll_declarationListAppend :: Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> IO ()
hll_declarationListAppend ptrStructTarget ptrStructSource = do

  source :: CssDeclarationSet <- peekCssDeclarationSet ptrStructSource
  target :: CssDeclarationSet <- peekCssDeclarationSet ptrStructTarget

  let merged = declarationsSetAppend target source
  pokeCssDeclarationSet ptrStructTarget merged

  return ()




peekCssValue :: FfiCssValue -> IO CssValue
peekCssValue ffiCssValue = do
  when (ptrTextValC ffiCssValue == nullPtr) (trace ("Error: peekCssValue: null pointer inside of css value") putStr (""))

  let e = ptrTextValC ffiCssValue == nullPtr
  emptyString <- newCString ""

  buf :: BS.ByteString <- BSU.unsafePackCString (if e then emptyString else ptrTextValC ffiCssValue)
  let valType = fromIntegral . typeTagC $ ffiCssValue
  let intVal = fromIntegral . intValC $ ffiCssValue
  let textVal = T.E.decodeLatin1 $ buf
  let v | valType ==  0 = CssValueTypeInt intVal
        | valType ==  1 = CssValueTypeEnum intVal
        | valType ==  2 = CssValueTypeMultiEnum intVal
        | valType ==  3 = CssValueTypeLengthPercent $ cssLengthWordToDistance intVal
        | valType ==  4 = CssValueTypeLength $ cssLengthWordToDistance intVal
        | valType ==  5 = CssValueTypeSignedLength $ cssLengthWordToDistance intVal
        | valType ==  6 = CssValueTypeLengthPercentNumber $ cssLengthWordToDistance intVal
        | valType ==  7 = CssValueTypeAuto $ cssLengthWordToDistance intVal
        | valType ==  8 = CssValueTypeColor intVal
        | valType ==  9 = CssValueTypeFontWeight intVal
        | valType == 10 = CssValueTypeString textVal
        | valType == 11 = CssValueTypeStringList textVal
        | valType == 12 = CssValueTypeURI textVal
        | valType == 13 = CssValueTypeBgPosition
        | otherwise = CssValueTypeUnused

  return v




hll_cssParseElementStyleAttribute :: Ptr () -> CString -> CInt -> Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> IO ()
hll_cssParseElementStyleAttribute ptrBaseUrl ptrStringCssStyleAttribute buflen ptrStructDeclSet ptrStructDeclSetImp = do

  cssStyleAttribute <- BSU.unsafePackCStringLen (ptrStringCssStyleAttribute, fromIntegral buflen)

  declSet    :: CssDeclarationSet <- peekCssDeclarationSet ptrStructDeclSet
  declSetImp :: CssDeclarationSet <- peekCssDeclarationSet ptrStructDeclSetImp

  let (newDeclSet, newDeclSetImp) = parseElementStyleAttribute "" (T.E.decodeLatin1 cssStyleAttribute) (declSet, declSetImp)

  pokeCssDeclarationSet ptrStructDeclSet newDeclSet
  pokeCssDeclarationSet ptrStructDeclSetImp newDeclSetImp

  return ()




cssValueToTypeTag value = case value of
                            CssValueTypeInt _                 ->  0
                            CssValueTypeEnum _                ->  1
                            CssValueTypeMultiEnum _           ->  2
                            CssValueTypeLengthPercent _       ->  3
                            CssValueTypeLength _              ->  4
                            CssValueTypeSignedLength _        ->  5
                            CssValueTypeLengthPercentNumber _ ->  6
                            CssValueTypeAuto _                ->  7
                            CssValueTypeColor _               ->  8
                            CssValueTypeFontWeight _          ->  9
                            CssValueTypeString _              -> 10
                            CssValueTypeStringList _          -> 11
                            CssValueTypeURI _                 -> 12
                            CssValueTypeBgPosition            -> 13
                            CssValueTypeUnused                -> 14



cssCombinatorIntToData i = case i of
                             0 -> CssCombinatorNone
                             1 -> CssCombinatorDescendant
                             2 -> CssCombinatorChild
                             3 -> CssCombinatorAdjacentSibling
                             _ -> CssCombinatorNone

cssCombinatorDataToInt d = case d of
                             CssCombinatorNone            -> 0
                             CssCombinatorDescendant      -> 1
                             CssCombinatorChild           -> 2
                             CssCombinatorAdjacentSibling -> 3




getCssOrigin :: Int -> CssOrigin
getCssOrigin o = case o of
                   0 -> CssOriginUserAgent
                   1 -> CssOriginUser
                   2 -> CssOriginAuthor




getIntOrigin :: CssOrigin -> Int
getIntOrigin origin = case origin of
                        CssOriginUserAgent -> 0
                        CssOriginUser      -> 1
                        CssOriginAuthor    -> 2




{-
  Lengths are represented as int in the following way:

     | <------   integer value   ------> |

     +---+ - - - +---+---+- - - - - -+---+---+---+---+
     |          integer part             |   type    |
     +---+ - - - +---+---+- - - - - -+---+---+---+---+
     | integer part  | decimal fraction  |   type    |
     +---+ - - - +---+---+- - - - - -+---+---+---+---+
      n-1          15  14              3   2  1   0

     | <------ fixed point value ------> |

  where type is one of the CSS_LENGTH_TYPE_* values.
  CSS_LENGTH_TYPE_PX values are stored as
  29 bit signed integer, all other types as fixed point values.

What you see below is some wild attempt to make Haskell code correctly
interpret floats encoded in upper bits of integers. Not the best approach to
take.
-}




data CssLength = CssLength Int Int -- word (with LSB bits indicating type) + lenType
  deriving (Show, Eq)




cssLengthWordToType :: Word32 -> Int
cssLengthWordToType word = fromIntegral (word .&. 0x07)




cssLengthWordToDistance :: Int -> CssDistance
cssLengthWordToDistance word | lenType == cssLengthTypeNone       = CssNumericNone f
                             | lenType == cssLengthTypeMM         = CssDistanceAbsMm f
                             | lenType == cssLengthTypePX         = CssDistanceAbsPx f
                             | lenType == cssLengthTypeEM         = CssDistanceRelEm f
                             | lenType == cssLengthTypeEX         = CssDistanceRelEx f
                             | lenType == cssLengthTypePercentage = CssNumericPercentage f
                             | lenType == cssLengthTypeRelative   = CssNumericRelative f
                             | lenType == cssLengthTypeAuto       = CssNumericAuto (round f)
                             | otherwise                          = CssNumericNone 0.0

  where
    len = CssLength word lenType
    f = cssLengthValue len
    lenType = cssLengthWordToType (fromIntegral word)




cssLengthValue :: CssLength -> Float
cssLengthValue (CssLength word lenType) | lenType == cssLengthTypePX = let
                                            z = (word `shiftR` 3)
                                          in
                                            if (0xf0000000 .&. word) == 0xf0000000
                                            then fromIntegral ((-1) * ((4294967295 - word) `shiftR` 3) - 1)
                                            else fromIntegral z
                                        | lenType == cssLengthTypeNone
                                          || lenType == cssLengthTypeMM
                                          || lenType == cssLengthTypeEM
                                          || lenType == cssLengthTypeEX
                                          || lenType == cssLengthTypePercentage
                                          || lenType == cssLengthTypeRelative =
                                          (fromIntegral (up2 word)) / (fromIntegral down2)
                                        | lenType == cssLengthTypeAuto = 0.0
                                        | otherwise = 0.0
  where
    up2 lenA = let
      z = lenA .&. (complement 0x00000007) :: Int
      in
        if (0xf0000000 .&. z) == 0xf0000000
        then (-1) * (4294967295 - z - 1)
        else z
    down2 = 1 `shiftL` 15 :: Int




css_LENGTH_FRAC_MAX = (1 `shiftL` (32 - 15 - 1)) - 1 :: Int
css_LENGTH_INT_MAX  = (1 `shiftL` (32 - 4)) - 1 :: Int




cssCreateLength2 :: CssDistance -> CssLength
cssCreateLength2 distance = cssCreateLength f lenType
  where
    (f, lenType) = case distance of
                     CssDistanceRelEm x     -> (x, cssLengthTypeEM)
                     CssDistanceRelEx x     -> (x, cssLengthTypeEX)
                     CssDistanceAbsMm x     -> (x, cssLengthTypeMM)
                     CssDistanceAbsPx x     -> (x, cssLengthTypePX)
                     CssNumericPercentage x -> (x, cssLengthTypePercentage)
                     CssNumericNone     x   -> (x, cssLengthTypeNone)
                     CssNumericRelative x   -> (x, cssLengthTypeRelative)
                     CssNumericAuto     x   -> (0.0, cssLengthTypeAuto)




cssCreateLength :: Float -> Int -> CssLength
cssCreateLength f lenType | lenType == cssLengthTypePX = CssLength word1 lenType
                          | lenType == cssLengthTypeNone
                            || lenType == cssLengthTypeMM
                            || lenType == cssLengthTypeEM
                            || lenType == cssLengthTypeEX
                            || lenType == cssLengthTypePercentage
                            || lenType == cssLengthTypeRelative = CssLength word2 lenType
                          | lenType == cssLengthTypeAuto = CssLength lenType lenType
                          | otherwise = CssLength cssLengthTypeAuto cssLengthTypeAuto

  where
    word1 = (((asInt1 (round f)) `shiftL` 3) .|. lenType)
    word2 = (((round ((asInt2 f) * (fromIntegral shift15L))) .&. (complement 7)) .|. lenType)

    shift15L = (1 `shiftL` 15) :: Int

    asInt1 :: Int -> Int
    asInt1 f = if f > css_LENGTH_INT_MAX
               then css_LENGTH_INT_MAX
               else if f < (-css_LENGTH_INT_MAX)
                    then (-css_LENGTH_INT_MAX)
                    else f

    asInt2 :: Float -> Float
    asInt2 f = if f > fromIntegral css_LENGTH_FRAC_MAX
               then fromIntegral css_LENGTH_FRAC_MAX
               else if f < fromIntegral (-css_LENGTH_FRAC_MAX)
                    then fromIntegral (-css_LENGTH_FRAC_MAX)
                    else f

    autoAsWord = cssLengthTypeAuto




hll_isTokenComma :: Ptr FfiCssToken -> IO Int
hll_isTokenComma ptrStructCssToken = do
  token <- peekCssToken ptrStructCssToken
  case token of
    CssTokComma -> return 1
    otherwise   -> return 0

hll_isTokenSemicolon :: Ptr FfiCssToken -> IO Int
hll_isTokenSemicolon ptrStructCssToken = do
  token <- peekCssToken ptrStructCssToken
  case token of
    CssTokSemicolon -> return 1
    otherwise    -> return 0

hll_isTokenBraceCurlyOpen :: Ptr FfiCssToken -> IO Int
hll_isTokenBraceCurlyOpen ptrStructCssToken = do
  token <- peekCssToken ptrStructCssToken
  case token of
    CssTokBraceCurlyOpen -> return 1
    otherwise            -> return 0

hll_isTokenBraceCurlyClose :: Ptr FfiCssToken -> IO Int
hll_isTokenBraceCurlyClose ptrStructCssToken = do
  token <- peekCssToken ptrStructCssToken
  case token of
    CssTokBraceCurlyClose -> return 1
    otherwise             -> return 0
