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




module Hello.Ffi.Css.Parser
  (
    FfiCssParser (..)
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
import qualified Data.Text.Encoding as T.E
import qualified Data.ByteString.Unsafe as BSU
import Debug.Trace

import Hello.Css.Parser.Rule
import Hello.Css.StyleEngine
import Hello.Css.StyleEngineGlobal
import Hello.Css.StyleNode
import Hello.Css.Tokenizer

import Hello.Ffi.Utils




foreign export ccall "ffiNextToken" ffiNextToken :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO CString
--foreign export ccall "ffiDeclarationValueAsString" ffiDeclarationValueAsString :: Ptr FfiCssParser -> Ptr FfiCssToken -> Int -> IO CString
foreign export ccall "ffiIgnoreBlock" ffiIgnoreBlock :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO Int
foreign export ccall "ffiIgnoreStatement" ffiIgnoreStatement :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO Int


--foreign export ccall "ffiDeclarationListAppend" ffiDeclarationListAppend :: Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> IO ()
foreign export ccall "ffiCssParseElementStyleAttribute" ffiCssParseElementStyleAttribute :: CInt -> Ptr () -> CString -> CInt -> IO ()

foreign export ccall "ffiIsTokenComma" ffiIsTokenComma :: Ptr FfiCssToken -> IO Int
foreign export ccall "ffiIsTokenSemicolon" ffiIsTokenSemicolon :: Ptr FfiCssToken -> IO Int



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

  poke ptr (FfiCssParser argSpaceSeparated argBufOffset argInBlock _argBuf _argBuflen argOrigin) = do
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
  remd <- ptrCCharToText bufWithOffset
  let inBlockInt :: Int = fromIntegral . inBlockC $ ffiParser
  let spaceSeparatedInt :: Int = fromIntegral . spaceSeparatedC $ ffiParser

  let parser = defaultParserEmpty { remainder      = remd
                                  , inBlock        = inBlockInt /= 0
                                  , bufOffset      = offset
                                  , spaceSeparated = spaceSeparatedInt /= 0
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
pokeCssToken :: Ptr FfiCssToken -> CssToken -> IO ()
pokeCssToken ptrStructCssToken token = do
  s <- cstr token
  poke ptrStructCssToken $ FfiCssToken (getTokenType token) s




-- This function corresponds with cssparser.hh::CssTokenType
getTokenType :: CssToken -> CInt
getTokenType (CssTokIdent  _)         =  0
getTokenType (CssTokStr  _)           =  1
getTokenType (CssTokDelim _)          =  2
getTokenType CssTokEnd                =  3
getTokenType CssTokBraceCurlyClose    =  4
getTokenType CssTokColon              =  5
getTokenType CssTokBraceSquareOpen    =  6
getTokenType CssTokBraceSquareClose   =  7
getTokenType (CssTokHash CssHashUn _) =  8
getTokenType (CssTokHash CssHashId _) =  9
getTokenType (CssTokAtKeyword _)      = 10
getTokenType CssTokWS                 = 11
getTokenType _                        = 12




-- Combine integer and text value into a properly typed token.
getTokenADT :: CInt -> T.Text -> CssToken
getTokenADT tokType tokValue | tokType ==  0 = CssTokIdent tokValue
                             | tokType ==  1 = CssTokStr tokValue
                             | tokType ==  2 = CssTokDelim (T.head tokValue)
                             | tokType ==  3 = CssTokEnd
                             | tokType ==  4 = CssTokBraceCurlyClose
                             | tokType ==  5 = CssTokColon
                             | tokType ==  6 = CssTokBraceSquareOpen
                             | tokType ==  7 = CssTokBraceSquareClose
                             | tokType ==  8 = CssTokHash CssHashUn tokValue
                             | tokType ==  9 = CssTokHash CssHashId tokValue
                             | tokType == 10 = CssTokAtKeyword tokValue
                             | tokType == 11 = CssTokWS
                             | otherwise     = trace ("[EE] Unhandled token type " ++ show tokType) CssTokNone




ffiNextToken :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO CString
ffiNextToken ptrStructCssParser ptrStructCssToken = do
  parser <- peekCssParser ptrStructCssParser

  let (newParser, newToken) = nextToken parser

  pokeCssParser ptrStructCssParser newParser
  pokeCssToken ptrStructCssToken newToken

  cstr newToken




cstr :: CssToken -> IO CString
cstr token = case token of
               CssTokNum (CssNumI i) -> newCString . show $ i
               CssTokNum (CssNumF f) -> newCString . show $ f
               CssTokHash _ s        -> newCString . T.unpack $ s
               CssTokIdent s         -> newCString . T.unpack $ s
               CssTokStr s           -> newCString . T.unpack $ s
               CssTokDelim c         -> newCString . T.unpack . T.singleton $ c
               CssTokWS              -> newCString " "
               CssTokAtKeyword s     -> newCString . T.unpack $ s
               _                     -> return nullPtr



{-
ffiDeclarationValueAsString :: Ptr FfiCssParser -> Ptr FfiCssToken -> Int -> IO CString
ffiDeclarationValueAsString ptrStructCssParser ptrStructCssToken valueType = do
  parser <- peekCssParser ptrStructCssParser
  token  <- peekCssToken ptrStructCssToken

  let ((newParser, newToken), textVal) = declValueAsString valueType (parser, token)

  pokeCssParser ptrStructCssParser newParser
  pokeCssToken ptrStructCssToken newToken

  case textVal of
    Just t -> newCString . T.unpack $ t
    _      -> return nullPtr
-}



ffiIgnoreBlock :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO Int
ffiIgnoreBlock ptrStructCssParser ptrStructCssToken = do
  parser <- peekCssParser ptrStructCssParser

  let (newParser, newToken) = ignoreBlock parser -- TODO: shouldn't we pass current token to the function?
  pokeCssParser ptrStructCssParser newParser
  pokeCssToken ptrStructCssToken newToken
  return 0




ffiIgnoreStatement :: Ptr FfiCssParser -> Ptr FfiCssToken -> IO Int
ffiIgnoreStatement ptrStructCssParser ptrStructCssToken = do
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
  let len = chainDatumLength . chain $ selector :: Int

  pokeArrayOfPointersWithAlloc (chainToLinks (chain selector) []) allocAndPokeCssComplexSelectorLink (cLinks ffiSel)
  pokeByteOff ptrStructCssComplexSelector (#offset c_css_cached_complex_selector_t, c_links_size) len

  pokeByteOff ptrStructCssComplexSelector (#offset c_css_cached_complex_selector_t, c_match_cache_offset) (matchCacheOffset selector)




data FfiCssDeclaration = FfiCssDeclaration {
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

  let propMaker :: (CssValue -> CssDeclaration) = fst (allDeclMakers !! (fromIntegral . propertyC $ ffiDecl))

  return defaultDeclaration{ property  = propMaker cssValue
                           , important = importantC ffiDecl > 0}




allocAndPokeCssDeclaration :: CssDeclaration -> IO (Ptr FfiCssDeclaration)
allocAndPokeCssDeclaration declaration = do
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
  ptrStructDeclaration :: Ptr FfiCssDeclaration <- callocBytes #{size c_css_declaration_t}
  poke ptrStructDeclaration $ FfiCssDeclaration imp prop ptrStructCssValue

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
  , ptrDeclarationsC  :: Ptr (Ptr FfiCssDeclaration)
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




ffiParseDeclarationWrapper :: Ptr FfiCssParser -> Ptr FfiCssToken -> Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> IO ()
ffiParseDeclarationWrapper ptrStructCssParser ptrStructCssToken ptrStructCssDeclarationSet ptrStructCssDeclarationSetImp = do
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




ffiDeclarationListAddOrUpdateDeclaration :: Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclaration -> IO (Ptr FfiCssDeclarationSet)
ffiDeclarationListAddOrUpdateDeclaration ptrStructDeclarationSet ptrStructDeclaration = do

  declSet :: CssDeclarationSet <- if ptrStructDeclarationSet == nullPtr
                                  then return defaultCssDeclarationSet
                                  else peekCssDeclarationSet ptrStructDeclarationSet
  decl    :: CssDeclaration    <- peekCssDeclaration ptrStructDeclaration

  let newDeclSet = declarationsSetUpdateOrAdd declSet decl

  newPtrStructDeclarationSet :: Ptr FfiCssDeclarationSet  <- callocBytes #{size c_css_declaration_set_t}

  pokeCssDeclarationSet newPtrStructDeclarationSet newDeclSet

  return newPtrStructDeclarationSet




ffiDeclarationListAppend :: Ptr FfiCssDeclarationSet -> Ptr FfiCssDeclarationSet -> IO ()
ffiDeclarationListAppend ptrStructTarget ptrStructSource = do

  source :: CssDeclarationSet <- peekCssDeclarationSet ptrStructSource
  target :: CssDeclarationSet <- peekCssDeclarationSet ptrStructTarget

  let merged = declarationsSetAppend target source
  pokeCssDeclarationSet ptrStructTarget merged

  return ()
-}



{-
ffiDeclarationListAppend2 :: Ptr FfiCssDeclarationSet -> CssDeclarationSet -> IO ()
ffiDeclarationListAppend2 ptrStructTarget source = do

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




ffiCssParseElementStyleAttribute :: CInt -> Ptr () -> CString -> CInt -> IO ()
ffiCssParseElementStyleAttribute cStyleEngineRef _ptrBaseUrl ptrStringCssStyleAttribute buflen = do

  -- FFI part.
  let refEngine = fromIntegral cStyleEngineRef
  engine <- globalStyleEngineGet refEngine
  cssStyleAttribute <- BSU.unsafePackCStringLen (ptrStringCssStyleAttribute, fromIntegral buflen)

  -- The main part.
  let styleNode  = styleNodesStackPeek engine
      (m, i)     = parseElementStyleAttribute "" (T.E.decodeLatin1 cssStyleAttribute) (mainDeclSet styleNode, importantDeclSet styleNode)
      styleNode' = styleNode { mainDeclSet = m, importantDeclSet = i }
      engine'    = styleNodesStackUpdateTop engine styleNode'

  -- FFI part.
  globalStyleEngineUpdate refEngine engine'
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
                   _ -> CssOriginAuthor




getIntOrigin :: CssOrigin -> Int
getIntOrigin origin = case origin of
                        CssOriginUserAgent -> 0
                        CssOriginUser      -> 1
                        CssOriginAuthor    -> 2




ffiIsTokenComma :: Ptr FfiCssToken -> IO Int
ffiIsTokenComma ptrStructCssToken = do
  token <- peekCssToken ptrStructCssToken
  case token of
    CssTokComma -> return 1
    _           -> return 0

ffiIsTokenSemicolon :: Ptr FfiCssToken -> IO Int
ffiIsTokenSemicolon ptrStructCssToken = do
  token <- peekCssToken ptrStructCssToken
  case token of
    CssTokSemicolon -> return 1
    _               -> return 0



{-
allDeclMakers :: [(CssValue -> CssDeclaration, Int)]
allDeclMakers =
  [ ( (\v -> CssDeclaration_LAST), 0 )                   -- CssDeclarationBackgroundAttachment; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 1 )                   -- CssDeclarationBackgroundColor; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeColor
  , ( (\v -> CssDeclaration_LAST), 2 )                   -- CssDeclarationBackgroundImage; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 3 )                   -- CssDeclarationBackgroundPosition; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 4 )                   -- CssDeclarationBackgroundRepeat; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 5 )                   -- CssDeclarationBorderBottomColor; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 6 )                   -- CssDeclarationBorderBottomStyle; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 7 )                   -- CssDeclarationBorderBottomWidth; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclaration_LAST), 8 )                   -- CssDeclarationBorderCollapse; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 9 )                   -- CssDeclarationBorderLeftColor; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 10 )                  -- CssDeclarationBorderLeftStyle; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 11 )                  -- CssDeclarationBorderLeftWidth; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclaration_LAST), 12 )                  -- CssDeclarationBorderRightColor; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 13 )                  -- CssDeclarationBorderRightStyle; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 14 )                  -- CssDeclarationBorderRightWidth; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclaration_LAST), 15 )                  -- CssDeclarationBorderSpacing; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclaration_LAST), 16 )                  -- CssDeclarationBorderTopColor; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 17 )                  -- CssDeclarationBorderTopStyle; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 18 )                  -- CssDeclarationBorderTopWidth; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclarationBottom v), 19 )
  , ( (\v -> CssDeclarationCaptionSide v), 20 )
  , ( (\v -> CssDeclarationClear v), 21 )
  , ( (\v -> CssDeclarationClip v), 22 )
  , ( (\v -> CssDeclaration_LAST), 23 )                  -- CssDeclarationColor; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeColor
  , ( (\v -> CssDeclaration_LAST), 24 )                  -- CssDeclarationContent; C++ code will never ask for this property
  , ( (\v -> CssDeclarationCounterIncrement v), 25 )
  , ( (\v -> CssDeclarationCounterReset v), 26 )
  , ( (\v -> CssDeclaration_LAST), 27 )                  -- CssDeclarationCursor; C++ code will never ask for this property
  , ( (\v -> CssDeclarationDirection v), 28 )
  , ( (\v -> CssDeclaration_LAST), 29 )                  -- CssDeclarationDisplay; C++ code will never ask for this property
  , ( (\v -> CssDeclarationEmptyCells v), 30 )
  , ( (\v -> CssDeclarationFloat v), 31 )
  , ( (\v -> CssDeclaration_LAST), 32 )                  -- CssDeclarationFontFamily; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeString
  , ( (\v -> CssDeclaration_LAST), 33 )                  -- CssDeclarationFontSize; C++ code will never ask for this property
  , ( (\v -> CssDeclarationFontSizeAdjust v), 34 )
  , ( (\v -> CssDeclarationFontStretch v), 35 )
  , ( (\v -> CssDeclaration_LAST), 36 )                  -- CssDeclarationFontStyle; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 37 )                  -- CssDeclarationFontVariant; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 38 )                  -- CssDeclarationFontWeight; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 39 )                  -- CssDeclarationHeight; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclarationLeft v), 40 )
  , ( (\v -> CssDeclaration_LAST), 41 )                  -- CssDeclarationLetterSpacing; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 42 )                  -- CssDeclarationLineHeight; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 43 )                  -- CssDeclarationListStyleImage; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 44 )                  -- CssDeclarationListStylePosition; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 45 )                  -- CssDeclarationListStyleType; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 46 )                  -- CssDeclarationMarginBottom; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclaration_LAST), 47 )                  -- CssDeclarationMarginLeft;   Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclaration_LAST), 48 )                  -- CssDeclarationMarginRight;  Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclaration_LAST), 49 )                  -- CssDeclarationMarginTop;    Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
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
  , ( (\v -> CssDeclaration_LAST), 60 )                  -- CssDeclarationPaddingBottom; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclaration_LAST), 61 )                  -- CssDeclarationPaddingLeft; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclaration_LAST), 62 )                  -- CssDeclarationPaddingRight; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclaration_LAST), 63 )                  -- CssDeclarationPaddingTop; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclarationPosition v), 64 )
  , ( (\v -> CssDeclarationQuotes v), 65 )
  , ( (\v -> CssDeclarationRight v), 66 )
  , ( (\v -> CssDeclaration_LAST), 67 )                  -- CssDeclarationTextAlign; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 68 )                  -- CssDeclarationTextDecoration; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 69 )                  -- CssDeclarationTextIndent; C++ code will never ask for this property
  , ( (\v -> CssDeclarationTextShadow v), 70 )
  , ( (\v -> CssDeclaration_LAST), 71 )                  -- CssDeclarationTextTransform; C++ code will never ask for this property
  , ( (\v -> CssDeclarationTop v), 72 )
  , ( (\v -> CssDeclarationUnicodeBiDi v), 73 )
  , ( (\v -> CssDeclaration_LAST), 74 )                  -- CssDeclarationVerticalAlign; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclarationVisibility v), 75 )
  , ( (\v -> CssDeclaration_LAST), 76 )                  -- CssDeclarationWhitespace; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeEnum
  , ( (\v -> CssDeclaration_LAST), 77 )                  -- CssDeclarationWidth; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetNonCssHintOfNodeLength
  , ( (\v -> CssDeclaration_LAST), 78 )                  -- CssDeclarationWordSpacing; C++ code will never ask for this property
  , ( (\v -> CssDeclarationZIndex v), 79 )
  , ( (\v -> CssDeclaration_LAST), 80 )                  -- CssDeclarationXLink; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetXLinkOfNode
  , ( (\v -> CssDeclaration_LAST), 81 )                  -- CssDeclarationXColSpan; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 82 )                  -- CssDeclarationXRowSpan; C++ code will never ask for this property
  , ( (\v -> CssDeclaration_LAST), 83 )                  -- CssDeclarationXLang; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetXLangOfNode
  , ( (\v -> CssDeclaration_LAST), 84 )                  -- CssDeclarationXImg; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetXImgOfNode
  , ( (\v -> CssDeclaration_LAST), 85 )                  -- CssDeclarationXTooltip; Handling of a request from C++ to add this declaration is done in ffiStyleEngineSetXTooltipOfNode
  , ( (\v -> CssDeclaration_LAST), 86 )                  -- "last" item
  ]
-}

