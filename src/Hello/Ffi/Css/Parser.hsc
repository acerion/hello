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




module Hello.Ffi.Css.Parser
  (
    FfiCssParser (..)
  , peekCssParser
  , pokeCssParser

  , FfiCssToken (..)
  , peekCssToken
  , pokeCssToken

  , getCssOrigin

  , allDeclMakers
  )
where




import Prelude
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Data.List as L
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

import Hello.Css.Declaration
import Hello.Css.DeclarationSetsGlobal
import Hello.Css.Distance
import Hello.Css.Parser
import Hello.Css.Selector
import Hello.Css.Tokenizer
import Hello.Css.Value

import Hello.Ffi.Css.Distance
import Hello.Ffi.Css.SelectorLink
import Hello.Ffi.Css.Value
import Hello.Ffi.Utils




foreign export ccall "hll_nextToken" hll_nextToken :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO CString
foreign export ccall "hll_declarationValueAsString" hll_declarationValueAsString :: Ptr FfiCssParser -> Ptr FfiCssToken -> Int -> Int -> IO CString
foreign export ccall "hll_ignoreBlock" hll_ignoreBlock :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO Int
foreign export ccall "hll_ignoreStatement" hll_ignoreStatement :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO Int


--foreign export ccall "hll_declarationListAppend" hll_declarationListAppend :: Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> IO ()
foreign export ccall "hll_cssParseElementStyleAttribute" hll_cssParseElementStyleAttribute :: Ptr () -> CString -> CInt -> CInt -> CInt -> IO ()

foreign export ccall "hll_isTokenComma" hll_isTokenComma :: Ptr FfiCssToken -> IO Int
foreign export ccall "hll_isTokenSemicolon" hll_isTokenSemicolon :: Ptr FfiCssToken -> IO Int



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
getTokenType (CssTokDelim _)  = 2
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
                             | tokType == 2 = CssTokDelim  (T.head tokValue)
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
    (CssTokStr s)    -> (newCString . T.unpack $ s)
    (CssTokDelim c)  -> (newCString . T.unpack . T.singleton $ c)
    CssTokWS         -> (newCString " ")
    otherwise        -> return nullPtr




hll_declarationValueAsString :: Ptr FfiCssParser -> Ptr FfiCssToken -> Int -> Int -> IO CString
hll_declarationValueAsString ptrStructCssParser ptrStructCssToken valueType property = do
  parser <- peekCssParser ptrStructCssParser
  token  <- peekCssToken ptrStructCssToken

  let pair@((newParser, newToken), textVal) = declValueAsString valueType (parser, token)

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




{-
typedef struct c_css_complex_selector_link_t {
   /* It's possible that more than one of these is set in a single
      CssComplexSelectorLink struct. */
   char * c_selector_class[10];
   int c_selector_class_size;

   /* In CSS there can be more pseudo-classes and Haskell can read them, but
      for now C/C++ code will only use first one. */
   char * c_selector_pseudo_class[10];
   int c_selector_pseudo_class_size;

   char * c_selector_id;
   int c_selector_type; /* Index corresponding to html.cc::Tags[]. */

   int c_combinator;
} c_css_complex_selector_link_t;


typedef struct c_css_compound_selector_t {
   /* It's possible that more than one of these is set in a single
      CssComplexSelectorLink struct. */
   char * c_selector_class[10];
   int c_selector_class_size;

   /* In CSS there can be more pseudo-classes and Haskell can read them, but
      for now C/C++ code will only use first one. */
   char * c_selector_pseudo_class[10];
   int c_selector_pseudo_class_size;

   char * c_selector_id;
   int c_selector_type; /* Index corresponding to html.cc::Tags[]. */
} c_css_compound_selector_t;



typedef struct c_css_cached_complex_selector_t {
   int c_match_cache_offset;
   c_css_complex_selector_link_t * c_links[10];
   int c_links_size;
} c_css_cached_complex_selector_t;





data FfiCssComplexSelectorLink = FfiCssComplexSelectorLink {
  -- equals to <char * c_selector_class[10]>,
  -- which equals to <char ** c_selector_class>
    selectorClassC           :: Ptr (Ptr CChar)
  , selectorClassSizeC       :: CInt

  -- equals to <char * c_pseudo_selector_class[10]>,
  -- which equals to <char ** c_pseudo_selector_class>
  , selectorPseudoClassC     :: Ptr (Ptr CChar)
  , selectorPseudoClassSizeC :: CInt

  , selectorIdC              :: CString
  , selectorTagNameC         :: CInt

  , combinatorC              :: CInt
  } deriving (Show)




instance Storable FfiCssComplexSelectorLink where
  sizeOf    _ = #{size c_css_complex_selector_link_t}
  alignment _ = #{alignment c_css_complex_selector_link_t}

  peek ptr = do
    let a = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_complex_selector_link_t, c_selector_class}) ptr
    b <- #{peek c_css_complex_selector_link_t, c_selector_class_size} ptr
    let c = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_complex_selector_link_t, c_selector_pseudo_class}) ptr
    d <- #{peek c_css_complex_selector_link_t, c_selector_pseudo_class_size} ptr
    e <- #{peek c_css_complex_selector_link_t, c_selector_id} ptr
    f <- #{peek c_css_complex_selector_link_t, c_selector_type} ptr
    g <- #{peek c_css_complex_selector_link_t, c_combinator} ptr
    return (FfiCssComplexSelectorLink a b c d e f g)


  poke ptr (FfiCssComplexSelectorLink selectorClassI selector_class_size_I selector_pseudo_class_I selector_pseudo_class_size_I selector_id_I selector_type_I combinator_I) = do
    #{poke c_css_complex_selector_link_t, c_selector_class}             ptr selectorClassI
    #{poke c_css_complex_selector_link_t, c_selector_class_size}        ptr selector_class_size_I
    #{poke c_css_complex_selector_link_t, c_selector_pseudo_class}      ptr selector_pseudo_class_I
    #{poke c_css_complex_selector_link_t, c_selector_pseudo_class_size} ptr selector_pseudo_class_size_I
    #{poke c_css_complex_selector_link_t, c_selector_id}                ptr selector_id_I
    #{poke c_css_complex_selector_link_t, c_selector_type}              ptr selector_type_I
    #{poke c_css_complex_selector_link_t, c_combinator}                 ptr combinator_I




peekCssComplexSelectorLink :: Ptr FfiCssComplexSelectorLink -> IO CssComplexSelectorLink
peekCssComplexSelectorLink ptrStructLink = do

  ffiLink <- peek ptrStructLink

  let pcStringArray :: Ptr CString = (selectorPseudoClassC ffiLink)
  pc <- peekArrayOfPointers pcStringArray (fromIntegral . selectorPseudoClassSizeC $ ffiLink) ptrCCharToText

  selId <- ptrCCharToText . selectorIdC $ ffiLink

  let cStringArray :: Ptr CString = (selectorClassC ffiLink)
  c <- peekArrayOfPointers cStringArray (fromIntegral . selectorClassSizeC $ ffiLink) ptrCCharToText

  let cpd = CssCompoundSelector
            { selectorPseudoClass = pc
            , selectorId          = selId
            , selectorClass       = c
            , selectorTagName     = mkCssTypeSelector . fromIntegral . selectorTagNameC $ ffiLink
            }

  return CssComplexSelectorLink
    { compound   = cpd
    , combinator = cssCombinatorIntToData . fromIntegral . combinatorC $ ffiLink
    }




-- Save given Haskell compound selector to C compound selector.
-- https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/hsc2hs.html
pokeCssComplexSelectorLink :: Ptr FfiCssComplexSelectorLink -> CssComplexSelectorLink -> IO ()
pokeCssComplexSelectorLink ptrStructLink link = do
  cStringPtrSelId <- if T.null . selectorId . compound $ link
                     then return nullPtr
                     else newCString . T.unpack . selectorId . compound $ link

  ffiLink <- peek ptrStructLink

  pokeArrayOfPointersWithAlloc (selectorClass . compound $ link) allocAndPokeCString (selectorClassC ffiLink)
  pokeByteOff ptrStructLink (#offset c_css_complex_selector_link_t, c_selector_class_size) (length . selectorClass . compound $ link)

  pokeArrayOfPointersWithAlloc (selectorPseudoClass . compound $ link) allocAndPokeCString (selectorPseudoClassC ffiLink)
  pokeByteOff ptrStructLink (#offset c_css_complex_selector_link_t, c_selector_pseudo_class_size) (length . selectorPseudoClass . compound $ link)

  pokeByteOff ptrStructLink (#offset c_css_complex_selector_link_t, c_selector_id) cStringPtrSelId
  pokeByteOff ptrStructLink (#offset c_css_complex_selector_link_t, c_selector_type) (unCssTypeSelector . selectorTagName . compound $ link)

  let comb :: CInt = cssCombinatorDataToInt . combinator $ link
  pokeByteOff ptrStructLink (#offset c_css_complex_selector_link_t, c_combinator) comb




-- Get pointer to newly allocated pointer to C structure representing given
-- compound selector.
--
-- This function allocates memory, but since the goal of this project is to
-- replace C/C++ code with Haskell code, the allocation will be eventually
-- removed. So I don't care about deallocating the memory.
allocAndPokeCssComplexSelectorLink :: CssComplexSelectorLink -> IO (Ptr FfiCssComplexSelectorLink)
allocAndPokeCssComplexSelectorLink link = do
  ptrStructLink <- callocBytes #{size c_css_complex_selector_link_t}
  pokeCssComplexSelectorLink ptrStructLink link
  return ptrStructLink




data FfiCssCompoundSelector = FfiCssCompoundSelector {
  -- equals to <char * c_selector_class[10]>,
  -- which equals to <char ** c_selector_class>
    selectorClassC2           :: Ptr (Ptr CChar)
  , selectorClassSizeC2       :: CInt

  -- equals to <char * c_pseudo_selector_class[10]>,
  -- which equals to <char ** c_pseudo_selector_class>
  , selectorPseudoClassC2     :: Ptr (Ptr CChar)
  , selectorPseudoClassSizeC2 :: CInt

  , selectorIdC2              :: CString
  , selectorTagNameC2         :: CInt
  } deriving (Show)




instance Storable FfiCssCompoundSelector where
  sizeOf    _ = #{size c_css_compound_selector_t}
  alignment _ = #{alignment c_css_compound_selector_t}

  peek ptr = do
    let a = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_compound_selector_t, c_selector_class}) ptr
    b <- #{peek c_css_compound_selector_t, c_selector_class_size} ptr
    let c = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_compound_selector_t, c_selector_pseudo_class}) ptr
    d <- #{peek c_css_compound_selector_t, c_selector_pseudo_class_size} ptr
    e <- #{peek c_css_compound_selector_t, c_selector_id} ptr
    f <- #{peek c_css_compound_selector_t, c_selector_type} ptr
    return (FfiCssCompoundSelector a b c d e f)


  poke ptr (FfiCssCompoundSelector selector_class_I selector_class_size_I selector_pseudo_class_I selector_pseudo_class_size_I selector_id_I selector_type_I) = do
    #{poke c_css_compound_selector_t, c_selector_class}             ptr selector_class_I
    #{poke c_css_compound_selector_t, c_selector_class_size}        ptr selector_class_size_I
    #{poke c_css_compound_selector_t, c_selector_pseudo_class}      ptr selector_pseudo_class_I
    #{poke c_css_compound_selector_t, c_selector_pseudo_class_size} ptr selector_pseudo_class_size_I
    #{poke c_css_compound_selector_t, c_selector_id}                ptr selector_id_I
    #{poke c_css_compound_selector_t, c_selector_type}              ptr selector_type_I




peekCssCompoundSelector :: Ptr FfiCssCompoundSelector -> IO CssCompoundSelector
peekCssCompoundSelector ptrStructCompoundSelector = do

  ffiCpdSel <- peek ptrStructCompoundSelector

  let pcStringArray :: Ptr CString = (selectorPseudoClassC2 ffiCpdSel)
  pseudoClassIdents <- peekArrayOfPointers pcStringArray (fromIntegral . selectorPseudoClassSizeC2 $ ffiCpdSel) ptrCCharToText

  idIdent <- ptrCCharToText . selectorIdC2 $ ffiCpdSel

  let cStringArray :: Ptr CString = (selectorClassC2 ffiCpdSel)
  classIdents <- peekArrayOfPointers cStringArray (fromIntegral . selectorClassSizeC2 $ ffiCpdSel) ptrCCharToText

  return CssCompoundSelector{ selectorPseudoClass = pseudoClassIdents
                            , selectorId          = idIdent
                            , selectorClass       = classIdents
                            , selectorTagName     = mkCssTypeSelector . fromIntegral . selectorTagNameC2 $ ffiCpdSel
                            }




data FfiCssComplexSelector = FfiCssComplexSelector {
    cMatchCacheOffset    :: CInt

    --    <c_css_complex_selector_link_t * c_links[10]>
    -- == <c_css_complex_selector_link_t ** c_links>
    -- == pointer to pointer(s) to link struct
  , cLinks     :: Ptr (Ptr FfiCssComplexSelectorLink)
  , cLinksSize :: CInt
  } deriving (Show)




instance Storable FfiCssComplexSelector where
  sizeOf    _ = #{size c_css_cached_complex_selector_t}
  alignment _ = #{alignment c_css_cached_complex_selector_t}

  peek ptr = do
    a <- #{peek c_css_cached_complex_selector_t, c_match_cache_offset} ptr
    let b = (\hsc_ptr -> plusPtr hsc_ptr #{offset c_css_cached_complex_selector_t, c_links}) ptr
    c <- #{peek c_css_cached_complex_selector_t, c_links_size} ptr
    return (FfiCssComplexSelector a b c)

  poke ptr (FfiCssComplexSelector inMatchCaseOffset inLinks inLinksSize) = do
    #{poke c_css_cached_complex_selector_t, c_match_cache_offset}    ptr inMatchCaseOffset
    #{poke c_css_cached_complex_selector_t, c_links}      ptr inLinks
    #{poke c_css_cached_complex_selector_t, c_links_size} ptr inLinksSize




peekCssComplexSelector :: Ptr FfiCssComplexSelector -> IO CssCachedComplexSelector
peekCssComplexSelector ptrStructCssComplexSelector = do

  ffiSel <- peek ptrStructCssComplexSelector

  {- This would also work:
  let offset = #{offset c_css_cached_complex_selector_t, c_links}
  let ptrLinkArray :: Ptr (Ptr FfiCssComplexSelectorLink) = plusPtr ptrStructCssComplexSelector offset
  -}
  let ptrLinkArray :: Ptr (Ptr FfiCssComplexSelectorLink) = cLinks ffiSel

  let linksCount = fromIntegral . cLinksSize $ ffiSel
  linksData <- peekArrayOfPointers ptrLinkArray linksCount peekCssComplexSelectorLink

  return CssCachedComplexSelector{ matchCacheOffset = fromIntegral . cMatchCacheOffset $ ffiSel
                                 , chain            = linksToChain linksData
                                 }




pokeCssComplexSelector :: Ptr FfiCssComplexSelector -> CssCachedComplexSelector -> IO ()
pokeCssComplexSelector ptrStructCssComplexSelector selector = do

  ffiSel <- peek ptrStructCssComplexSelector
  let len = chainLength . chain $ selector :: Int

  pokeArrayOfPointersWithAlloc (chainToLinks (chain selector) []) allocAndPokeCssComplexSelectorLink (cLinks ffiSel)
  pokeByteOff ptrStructCssComplexSelector (#offset c_css_cached_complex_selector_t, c_links_size) len

  pokeByteOff ptrStructCssComplexSelector (#offset c_css_cached_complex_selector_t, c_match_cache_offset) (matchCacheOffset selector)




data FfiCssDeclWrapper = FfiCssDeclWrapper {
    importantC :: CInt
  , propertyC  :: CInt
  , ptrValueC  :: Ptr FfiCssValue
  } deriving (Show)




/**
 * \brief This class holds a CSS declaration: a pair of property and value.
 */
typedef struct c_css_declaration_t {
   int c_important;
   int c_property;
   c_css_value_t * c_value;
} c_css_declaration_t;




instance Storable FfiCssDeclWrapper where
  sizeOf    _ = #{size c_css_declaration_t}
  alignment _ = #{alignment c_css_declaration_t}

  poke ptr (FfiCssDeclWrapper argImportant argProperty argPtrStructValue) = do
    #{poke c_css_declaration_t, c_important} ptr argImportant
    #{poke c_css_declaration_t, c_property}  ptr argProperty
    #{poke c_css_declaration_t, c_value}     ptr argPtrStructValue

  peek ptr = do
    a <- #{peek c_css_declaration_t, c_important} ptr
    b <- #{peek c_css_declaration_t, c_property}  ptr
    c <- #{peek c_css_declaration_t, c_value}     ptr
    return (FfiCssDeclWrapper a b c)




peekCssDeclWrapper :: Ptr FfiCssDeclWrapper -> IO CssDeclWrapper
peekCssDeclWrapper ptr = do

  ffiDecl <- peek ptr

  when (ptrValueC ffiDecl == nullPtr) (trace ("Error: null pointer inside of declaration") putStr (""))

  ffiCssValue :: FfiCssValue <- peek . ptrValueC $ ffiDecl
  cssValue <- peekCssValue ffiCssValue

  let propMaker :: (CssValue -> CssDeclaration) = fst (allDeclMakers !! (fromIntegral . propertyC $ ffiDecl))

  return defaultDeclaration{ property  = propMaker cssValue
                           , important = importantC ffiDecl > 0}




allocAndPokeCssDeclWrapper :: CssDeclWrapper -> IO (Ptr FfiCssDeclWrapper)
allocAndPokeCssDeclWrapper declaration = do
  let textVal = case getDeclValue . property $ declaration of
                  CssValueTypeString t     -> t
                  CssValueTypeStringList (x:xs) -> x -- Unfortunately we drop here the tail of the list
                  CssValueTypeStringList [] -> ""
                  CssValueTypeURI t        -> t
                  otherwise                -> ""

  let intVal = case getDeclValue . property $ declaration of
                 CssValueTypeInt i                                  -> i
                 CssValueTypeEnum i                                 -> i
                 CssValueTypeMultiEnum i                            -> i
                 CssValueTypeColor i                                -> i
                 CssValueTypeFontWeight i                           -> i
                 otherwise                                          -> 0

  let (lenVal, lenType) = case distanceFromValue . getDeclValue . property $ declaration of
                            Just distance -> distanceToCssLength distance
                            otherwise     -> (0.0, 0)

  ptrString <- newCString . T.unpack $ textVal
  let t :: CInt = fromIntegral . cssValueToTypeTag . getDeclValue . property $ declaration
  let i :: CInt = fromIntegral $ intVal
  ptrStructCssValue <- callocBytes #{size c_css_value_t}
  poke ptrStructCssValue $ FfiCssValue t i 0 0 ptrString lenVal (fromIntegral lenType)

  let imp :: CInt = if important declaration then 1 else 0
  let prop :: CInt = fromIntegral . findPropertyIndex . property $ declaration
  ptrStructDeclaration :: Ptr FfiCssDeclWrapper <- callocBytes #{size c_css_declaration_t}
  poke ptrStructDeclaration $ FfiCssDeclWrapper imp prop ptrStructCssValue

  return ptrStructDeclaration




data FfiCssValue = FfiCssValue {
    typeTagC    :: CInt
  , intValC     :: CInt
  , posXC       :: CInt
  , posYC       :: CInt
  , ptrTextValC :: CString

  , lengthValC   :: Float
  , lengthTypeC  :: CInt
  } deriving (Show)



typedef struct c_css_value_t {
   int c_type_tag;
   int c_int_val;
   int32_t c_bg_pos_x;
   int32_t c_bg_pos_y;
   char * c_text_val;

   float c_length_val;
   int c_length_type;
} c_css_value_t;




instance Storable FfiCssValue where
  sizeOf    _ = #{size c_css_value_t}
  alignment _ = #{alignment c_css_value_t}

  poke ptr (FfiCssValue argTypeTag argIntVal argX argY argTextVal argLengthVal argLengthType) = do
    #{poke c_css_value_t, c_type_tag} ptr argTypeTag
    #{poke c_css_value_t, c_int_val}  ptr argIntVal
    #{poke c_css_value_t, c_bg_pos_x} ptr argX
    #{poke c_css_value_t, c_bg_pos_y} ptr argY
    #{poke c_css_value_t, c_text_val} ptr argTextVal
    #{poke c_css_value_t, c_length_val} ptr argLengthVal
    #{poke c_css_value_t, c_length_type} ptr argLengthType

  peek ptr = do
    a <- #{peek c_css_value_t, c_type_tag} ptr
    b <- #{peek c_css_value_t, c_int_val}  ptr
    c <- #{peek c_css_value_t, c_bg_pos_x} ptr
    d <- #{peek c_css_value_t, c_bg_pos_y} ptr
    e <- #{peek c_css_value_t, c_text_val} ptr
    f <- #{peek c_css_value_t, c_length_val} ptr
    g <- #{peek c_css_value_t, c_length_type} ptr
    return (FfiCssValue a b c d e f g)





data FfiCssDeclarationSet = FfiCssDeclarationSet {
    isSafeC           :: CInt
  , ptrDeclarationsC  :: Ptr (Ptr FfiCssDeclWrapper)
  , declarationsSizeC :: CInt
  } deriving (Show)



/**
 * \brief A list of c_css_declaration_t objects.
 */
#define DECLARATIONS_COUNT_IN_SET 100
typedef struct c_css_declaration_set_t {
   int c_is_safe; // TODO: this should be true by default
   c_css_declaration_t * c_declarations[DECLARATIONS_COUNT_IN_SET];
   int c_declarations_size;
} c_css_declaration_set_t;





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
  let array :: Ptr (Ptr FfiCssDeclWrapper) = ptrDeclarationsC ffiDeclSet
  decls <- peekArrayOfPointers array len peekCssDeclWrapper
  return CssDeclarationSet{ isSafe = isSafeC ffiDeclSet > 0
                          , items = S.fromList decls
                          }




pokeCssDeclarationSet :: Ptr FfiCssDeclarationSet -> CssDeclarationSet -> IO ()
pokeCssDeclarationSet ptrStructDeclarationSet newDeclSet = do

  ffiDeclSet <- peek ptrStructDeclarationSet

  let cIsSafe :: CInt = if isSafe newDeclSet then 1 else 0
  let cCount  :: CInt = fromIntegral . length . items $ newDeclSet

  pokeByteOff ptrStructDeclarationSet (#offset c_css_declaration_set_t, c_is_safe) cIsSafe
  pokeArrayOfPointersWithAlloc (Foldable.toList . items $ newDeclSet) allocAndPokeCssDeclWrapper (ptrDeclarationsC ffiDeclSet)
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




hll_declarationListAddOrUpdateDeclaration :: Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclWrapper -> IO (Ptr FfiCssDeclarationSet)
hll_declarationListAddOrUpdateDeclaration ptrStructDeclarationSet ptrStructDeclaration = do

  declSet :: CssDeclarationSet <- if ptrStructDeclarationSet == nullPtr
                                  then return defaultCssDeclarationSet
                                  else peekCssDeclarationSet ptrStructDeclarationSet
  decl    :: CssDeclWrapper    <- peekCssDeclWrapper ptrStructDeclaration

  let newDeclSet = declarationsSetUpdateOrAdd declSet decl

  newPtrStructDeclarationSet :: Ptr FfiCssDeclarationSet  <- callocBytes #{size c_css_declaration_set_t}

  pokeCssDeclarationSet newPtrStructDeclarationSet newDeclSet

  return newPtrStructDeclarationSet




hll_declarationListAppend :: Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> IO ()
hll_declarationListAppend ptrStructTarget ptrStructSource = do

  source :: CssDeclarationSet <- peekCssDeclarationSet ptrStructSource
  target :: CssDeclarationSet <- peekCssDeclarationSet ptrStructTarget

  let merged = declarationsSetAppend target source
  pokeCssDeclarationSet ptrStructTarget merged

  return ()
-}



{-
hll_declarationListAppend2 :: Ptr FfiCssDeclarationSet -> CssDeclarationSet -> IO ()
hll_declarationListAppend2 ptrStructTarget source = do

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

  let lengthVal = lengthValC ffiCssValue
  let lengthType = fromIntegral . lengthTypeC $ ffiCssValue

  let v = makeValue valType intVal textVal lengthVal lengthType
  return v
-}




hll_cssParseElementStyleAttribute :: Ptr () -> CString -> CInt -> CInt -> CInt -> IO ()
hll_cssParseElementStyleAttribute ptrBaseUrl ptrStringCssStyleAttribute buflen cMainDeclSetRef cImportantDeclSetRef = do

  cssStyleAttribute <- BSU.unsafePackCStringLen (ptrStringCssStyleAttribute, fromIntegral buflen)

  let mainDeclSetRef = fromIntegral cMainDeclSetRef
  mainDeclSet :: CssDeclarationSet <- globalDeclarationSetGet mainDeclSetRef

  let importantDeclSetRef = fromIntegral cImportantDeclSetRef
  importantDeclSet :: CssDeclarationSet <- globalDeclarationSetGet importantDeclSetRef

  let (mainDeclSet', importantDeclSet') = parseElementStyleAttribute "" (T.E.decodeLatin1 cssStyleAttribute) (mainDeclSet, importantDeclSet)

  globalDeclarationSetUpdate mainDeclSetRef mainDeclSet'
  globalDeclarationSetUpdate importantDeclSetRef importantDeclSet'

  return ()




{-
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
                             1 -> Just CssCombinatorDescendant
                             2 -> Just CssCombinatorChild
                             3 -> Just CssCombinatorAdjacentSibling
                             _ -> Nothing


cssCombinatorDataToInt d = case d of
                             Just CssCombinatorDescendant      -> 1
                             Just CssCombinatorChild           -> 2
                             Just CssCombinatorAdjacentSibling -> 3
                             otherwise                         -> 0
-}



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



{-
findPropertyIndex :: CssDeclaration -> Int
findPropertyIndex decl = case decl of
                           CssDeclarationBackgroundAttachment v -> 0
                           CssDeclarationBackgroundColor v -> 1
                           CssDeclarationBackgroundImage v -> 2
                           CssDeclarationBackgroundPosition v -> 3
                           CssDeclarationBackgroundRepeat v -> 4
                           CssDeclarationBorderBottomColor v -> 5
                           CssDeclarationBorderBottomStyle v -> 6
                           CssDeclarationBorderBottomWidth v -> 7
                           CssDeclarationBorderCollapse v -> 8
                           CssDeclarationBorderLeftColor v -> 9
                           CssDeclarationBorderLeftStyle v -> 10
                           CssDeclarationBorderLeftWidth v -> 11
                           CssDeclarationBorderRightColor v -> 12
                           CssDeclarationBorderRightStyle v -> 13
                           CssDeclarationBorderRightWidth v -> 14
                           CssDeclarationBorderSpacing v -> 15
                           CssDeclarationBorderTopColor v -> 16
                           CssDeclarationBorderTopStyle v -> 17
                           CssDeclarationBorderTopWidth v -> 18
                           CssDeclarationBottom v -> 19
                           CssDeclarationCaptionSide v -> 20
                           CssDeclarationClear v -> 21
                           CssDeclarationClip v -> 22
                           CssDeclarationColor v -> 23
                           CssDeclarationContent v -> 24
                           CssDeclarationCounterIncrement v -> 25
                           CssDeclarationCounterReset v -> 26
                           CssDeclarationCursor v -> 27
                           CssDeclarationDirection v -> 28
                           CssDeclarationDisplay v -> 29
                           CssDeclarationEmptyCells v -> 30
                           CssDeclarationFloat v -> 31
                           CssDeclarationFontFamily v -> 32
                           CssDeclarationFontSize v -> 33
                           CssDeclarationFontSizeAdjust v -> 34
                           CssDeclarationFontStretch v -> 35
                           CssDeclarationFontStyle v -> 36
                           CssDeclarationFontVariant v -> 37
                           CssDeclarationFontWeight v -> 38
                           CssDeclarationHeight v -> 39
                           CssDeclarationLeft v -> 40
                           CssDeclarationLetterSpacing v -> 41
                           CssDeclarationLineHeight v -> 42
                           CssDeclarationListStyleImage v -> 43
                           CssDeclarationListStylePosition v -> 44
                           CssDeclarationListStyleType v -> 45
                           CssDeclarationMarginBottom v -> 46
                           CssDeclarationMarginLeft v -> 47
                           CssDeclarationMarginRight v -> 48
                           CssDeclarationMarginTop v -> 49
                           CssDeclarationMarkerOffset v -> 50
                           CssDeclarationMarks v -> 51
                           CssDeclarationMaxHeight v -> 52
                           CssDeclarationMaxWidth v -> 53
                           CssDeclarationMinHeight v -> 54
                           CssDeclarationMinWidth v -> 55
                           CssDeclarationOutlineColor v -> 56
                           CssDeclarationOutlineStyle v -> 57
                           CssDeclarationOutlineWidth v ->  58
                           CssDeclarationOverflow v -> 59
                           CssDeclarationPaddingBottom v -> 60
                           CssDeclarationPaddingLeft v -> 61
                           CssDeclarationPaddingRight v -> 62
                           CssDeclarationPaddingTop v -> 63
                           CssDeclarationPosition v -> 64
                           CssDeclarationQuotes v -> 65
                           CssDeclarationRight v -> 66
                           CssDeclarationTextAlign v -> 67
                           CssDeclarationTextDecoration v -> 68
                           CssDeclarationTextIndent v -> 69
                           CssDeclarationTextShadow v -> 70
                           CssDeclarationTextTransform v -> 71
                           CssDeclarationTop v -> 72
                           CssDeclarationUnicodeBiDi v -> 73
                           CssDeclarationVerticalAlign v -> 74
                           CssDeclarationVisibility v -> 75
                           CssDeclarationWhitespace v -> 76
                           CssDeclarationWidth v -> 77
                           CssDeclarationWordSpacing v -> 78
                           CssDeclarationZIndex v -> 79
                           CssDeclarationXLink v -> 80
                           CssDeclarationXColSpan v -> 81
                           CssDeclarationXRowSpan v -> 82
                           CssDeclarationXLang v -> 83
                           CssDeclarationXImg v -> 84
                           CssDeclarationXTooltip v -> 85
                           CssDeclaration_LAST -> 86
-}




allDeclMakers :: [(CssValue -> CssDeclaration, Int)]
allDeclMakers =
  [ ( (\v -> CssDeclaration_LAST), 0 )                   -- CssDeclarationBackgroundAttachment; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 1 )                   -- CssDeclarationBackgroundColor; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeColor
  , ( (\v -> CssDeclarationBackgroundImage v), 2 )
  , ( (\v -> CssDeclarationBackgroundPosition v), 3 )
  , ( (\v -> CssDeclarationBackgroundRepeat v), 4 )
  , ( (\v -> CssDeclaration_LAST), 5 )                   -- CssDeclarationBorderBottomColor; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 6 )                   -- CssDeclarationBorderBottomStyle; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 7 )                   -- CssDeclarationBorderBottomWidth; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeLength2
  , ( (\v -> CssDeclaration_LAST), 8 )                   -- CssDeclarationBorderCollapse; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 9 )                   -- CssDeclarationBorderLeftColor; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 10 )                  -- CssDeclarationBorderLeftStyle; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 11 )                  -- CssDeclarationBorderLeftWidth; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeLength2
  , ( (\v -> CssDeclaration_LAST), 12 )                  -- CssDeclarationBorderRightColor; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 13 )                  -- CssDeclarationBorderRightStyle; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 14 )                  -- CssDeclarationBorderRightWidth; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeLength2
  , ( (\v -> CssDeclarationBorderSpacing v), 15 )
  , ( (\v -> CssDeclaration_LAST), 16 )                  -- CssDeclarationBorderTopColor; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 17 )                  -- CssDeclarationBorderTopStyle; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 18 )                  -- CssDeclarationBorderTopWidth; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeLength2
  , ( (\v -> CssDeclarationBottom v), 19 )
  , ( (\v -> CssDeclarationCaptionSide v), 20 )
  , ( (\v -> CssDeclarationClear v), 21 )
  , ( (\v -> CssDeclarationClip v), 22 )
  , ( (\v -> CssDeclaration_LAST), 23 )                  -- CssDeclarationColor; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeColor
  , ( (\v -> CssDeclarationContent v), 24 )
  , ( (\v -> CssDeclarationCounterIncrement v), 25 )
  , ( (\v -> CssDeclarationCounterReset v), 26 )
  , ( (\v -> CssDeclaration_LAST), 27 )                  -- CssDeclarationCursor; C++ code will never ask for this property
  , ( (\v -> CssDeclarationDirection v), 28 )
  , ( (\v -> CssDeclaration_LAST), 29 )                  -- CssDeclarationDisplay; C++ code will never ask for this property
  , ( (\v -> CssDeclarationEmptyCells v), 30 )
  , ( (\v -> CssDeclarationFloat v), 31 )
  , ( (\v -> CssDeclarationFontFamily v), 32 )
  , ( (\v -> CssDeclaration_LAST), 33 )                  -- CssDeclarationFontSize; C++ code will never ask for this property
  , ( (\v -> CssDeclarationFontSizeAdjust v), 34 )
  , ( (\v -> CssDeclarationFontStretch v), 35 )
  , ( (\v -> CssDeclaration_LAST), 36 )                  -- CssDeclarationFontStyle; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 37 )                  -- CssDeclarationFontVariant; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 38 )                  -- CssDeclarationFontWeight; C++ code will never ask for this property
  , ( (\v -> CssDeclarationHeight v), 39 )
  , ( (\v -> CssDeclarationLeft v), 40 )
  , ( (\v -> CssDeclaration_LAST), 41 )                  -- CssDeclarationLetterSpacing; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 42 )                  -- CssDeclarationLineHeight; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 43 )                  -- CssDeclarationListStyleImage; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 44 )                  -- CssDeclarationListStylePosition; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 45 )                  -- CssDeclarationListStyleType; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclarationMarginBottom v), 46 )
  , ( (\v -> CssDeclarationMarginLeft v), 47 )
  , ( (\v -> CssDeclarationMarginRight v), 48 )
  , ( (\v -> CssDeclarationMarginTop v), 49 )
  , ( (\v -> CssDeclarationMarkerOffset v), 50 )
  , ( (\v -> CssDeclarationMarks v), 51 )
  , ( (\v -> CssDeclarationMaxHeight v), 52 )
  , ( (\v -> CssDeclarationMaxWidth v), 53 )
  , ( (\v -> CssDeclarationMinHeight v), 54 )
  , ( (\v -> CssDeclarationMinWidth v), 55 )
  , ( (\v -> CssDeclarationOutlineColor v), 56 )
  , ( (\v -> CssDeclarationOutlineStyle v), 57 )
  , ( (\v -> CssDeclarationOutlineWidth v), 58 )
  , ( (\v -> CssDeclarationOverflow v), 59 )
  , ( (\v -> CssDeclaration_LAST), 60 )                  -- CssDeclarationPaddingBottom; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeLength2
  , ( (\v -> CssDeclaration_LAST), 61 )                  -- CssDeclarationPaddingLeft; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeLength2
  , ( (\v -> CssDeclaration_LAST), 62 )                  -- CssDeclarationPaddingRight; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeLength2
  , ( (\v -> CssDeclaration_LAST), 63 )                  -- CssDeclarationPaddingTop; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeLength2
  , ( (\v -> CssDeclarationPosition v), 64 )
  , ( (\v -> CssDeclarationQuotes v), 65 )
  , ( (\v -> CssDeclarationRight v), 66 )
  , ( (\v -> CssDeclaration_LAST), 67 )                  -- CssDeclarationTextAlign; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 68 )                  -- CssDeclarationTextDecoration; C++ code will never ask for this property
  , ( (\v -> CssDeclarationTextIndent v), 69 )
  , ( (\v -> CssDeclarationTextShadow v), 70 )
  , ( (\v -> CssDeclaration_LAST), 71 )                  -- CssDeclarationTextTransform; C++ code will never ask for this property
  , ( (\v -> CssDeclarationTop v), 72 )
  , ( (\v -> CssDeclarationUnicodeBiDi v), 73 )
  , ( (\v -> CssDeclaration_LAST), 74 )                  -- CssDeclarationVerticalAlign; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclarationVisibility v), 75 )
  , ( (\v -> CssDeclaration_LAST), 76 )                  -- CssDeclarationWhitespace; Handling of a request from C++ to add this declaration is done in hll_styleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclarationWidth v), 77 )
  , ( (\v -> CssDeclaration_LAST), 78 )                  -- CssDeclarationWordSpacing; C++ code will never ask for this property
  , ( (\v -> CssDeclarationZIndex v), 79 )
  , ( (\v -> CssDeclarationXLink v), 80 )
  , ( (\v -> CssDeclarationXColSpan v), 81 )
  , ( (\v -> CssDeclarationXRowSpan v), 82 )
  , ( (\v -> CssDeclarationXLang v), 83 )
  , ( (\v -> CssDeclarationXImg v), 84 )
  , ( (\v -> CssDeclarationXTooltip v), 85 )
  , ( (\v -> CssDeclaration_LAST), 86 )
  ]





{-
getDeclValue :: CssDeclaration -> CssValue
getDeclValue decl = case decl of
                      CssDeclarationBackgroundAttachment v -> v
                      CssDeclarationBackgroundColor v -> v
                      CssDeclarationBackgroundImage v -> v
                      CssDeclarationBackgroundPosition v -> v
                      CssDeclarationBackgroundRepeat v -> v
                      CssDeclarationBorderBottomColor v -> v
                      CssDeclarationBorderBottomStyle v -> v
                      CssDeclarationBorderBottomWidth v -> v
                      CssDeclarationBorderCollapse v -> v
                      CssDeclarationBorderLeftColor v -> v
                      CssDeclarationBorderLeftStyle v -> v
                      CssDeclarationBorderLeftWidth v -> v
                      CssDeclarationBorderRightColor v -> v
                      CssDeclarationBorderRightStyle v -> v
                      CssDeclarationBorderRightWidth v -> v
                      CssDeclarationBorderSpacing v -> v
                      CssDeclarationBorderTopColor v -> v
                      CssDeclarationBorderTopStyle v -> v
                      CssDeclarationBorderTopWidth v -> v
                      CssDeclarationBottom v -> v
                      CssDeclarationCaptionSide v -> v
                      CssDeclarationClear v -> v
                      CssDeclarationClip v -> v
                      CssDeclarationColor v -> v
                      CssDeclarationContent v -> v
                      CssDeclarationCounterIncrement v -> v
                      CssDeclarationCounterReset v -> v
                      CssDeclarationCursor v -> v
                      CssDeclarationDirection v -> v
                      CssDeclarationDisplay v -> v
                      CssDeclarationEmptyCells v -> v
                      CssDeclarationFloat v -> v
                      CssDeclarationFontFamily v -> v
                      CssDeclarationFontSize v -> v
                      CssDeclarationFontSizeAdjust v -> v
                      CssDeclarationFontStretch v -> v
                      CssDeclarationFontStyle v -> v
                      CssDeclarationFontVariant v -> v
                      CssDeclarationFontWeight v -> v
                      CssDeclarationHeight v -> v
                      CssDeclarationLeft v -> v
                      CssDeclarationLetterSpacing v -> v
                      CssDeclarationLineHeight v -> v
                      CssDeclarationListStyleImage v -> v
                      CssDeclarationListStylePosition v -> v
                      CssDeclarationListStyleType v -> v
                      CssDeclarationMarginBottom v -> v
                      CssDeclarationMarginLeft v -> v
                      CssDeclarationMarginRight v -> v
                      CssDeclarationMarginTop v -> v
                      CssDeclarationMarkerOffset v -> v
                      CssDeclarationMarks v -> v
                      CssDeclarationMaxHeight v -> v
                      CssDeclarationMaxWidth v -> v
                      CssDeclarationMinHeight v -> v
                      CssDeclarationMinWidth v -> v
                      CssDeclarationOutlineColor v -> v
                      CssDeclarationOutlineStyle v -> v
                      CssDeclarationOutlineWidth v -> v
                      CssDeclarationOverflow v -> v
                      CssDeclarationPaddingBottom v -> v
                      CssDeclarationPaddingLeft v -> v
                      CssDeclarationPaddingRight v -> v
                      CssDeclarationPaddingTop v -> v
                      CssDeclarationPosition v -> v
                      CssDeclarationQuotes v -> v
                      CssDeclarationRight v -> v
                      CssDeclarationTextAlign v -> v
                      CssDeclarationTextDecoration v -> v
                      CssDeclarationTextIndent v -> v
                      CssDeclarationTextShadow v -> v
                      CssDeclarationTextTransform v -> v
                      CssDeclarationTop v -> v
                      CssDeclarationUnicodeBiDi v -> v
                      CssDeclarationVerticalAlign v -> v
                      CssDeclarationVisibility v -> v
                      CssDeclarationWhitespace v -> v
                      CssDeclarationWidth v -> v
                      CssDeclarationWordSpacing v -> v
                      CssDeclarationZIndex v -> v
                      CssDeclarationXLink v -> v
                      CssDeclarationXColSpan v -> v
                      CssDeclarationXRowSpan v -> v
                      CssDeclarationXLang v -> v
                      CssDeclarationXImg v -> v
                      CssDeclarationXTooltip v -> v
                      CssDeclaration_LAST -> CssValueTypeUnused
-}

