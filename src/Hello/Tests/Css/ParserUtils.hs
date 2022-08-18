{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Css.Parser.Utils
  (
    buildSuccessRow
  , makeDistanceCtorsList
  )
where




import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Sequence as S

import Test.HUnit
import Debug.Trace

import Hello.Css.Declaration
import Hello.Css.Distance
import Hello.Css.Parser
import Hello.Css.Tokenizer
import Hello.Css.Value

import Hello.Utils




-- Build a snippet of CSS that will be passed to CSS parser and parsed by it.
-- The parser should then return a CssDeclaration.
--
-- This function constructs valid CSS, so it can't be used to test handing of
-- invalid CSS.
--
-- Example result looks like this: "margin: 10px 20px 15em 0ex"
--
-- TODO: randomize count of spaces in the result string: spaces around colon
-- and between distances.
buildValidInput :: String -> [Float] -> [String] -> T.Text
buildValidInput propName values units = T.pack $ propName ++ ": " ++ (L.intercalate " " (zipWith f values units))
  where f value unit = (show value) ++ unit




-- | Build expected value of test in success case
--
-- | Can be used for shortcut properties such as "margin", where 4, 3, 2 or one property value can be provided.
--   Output of the function is always four declarations (for top, right, bottom, left properties).
buildSuccessOut :: [declValue -> CssDeclaration] -- ^ Constructors of declarations, e.g. CssDeclarationMarginTop.
                -> (a -> declValue)              -- ^ Constructor of declarations' value, common for all declarations.
                                                 --   E.g. CssValueMargin.
                -> [v -> a]                      -- ^ Converter between raw value and type accepted by
                                                 --   declaration value constructor. E.g. CssDistanceRelPx constructor.
                -> [v]                           -- ^ Raw value of property, e.g. 11.5.
                -> [CssDeclWrapper]
buildSuccessOut declCtors declValueCtor (ct:cl:cb:cr:[]) (vt:vl:vb:vr:[]) = [ CssDeclWrapper { property = (declCtors !! 0) . declValueCtor . ct $ vt, important = False }
                                                                            , CssDeclWrapper { property = (declCtors !! 1) . declValueCtor . cl $ vl, important = False }
                                                                            , CssDeclWrapper { property = (declCtors !! 2) . declValueCtor . cb $ vb, important = False }
                                                                            , CssDeclWrapper { property = (declCtors !! 3) . declValueCtor . cr $ vr, important = False }
                                                                            ]
buildSuccessOut declCtors declValueCtor (ct:clr:cb:[])   (vt:vlr:vb:[])   = [ CssDeclWrapper { property = (declCtors !! 0) . declValueCtor . ct $ vt,   important = False }
                                                                            , CssDeclWrapper { property = (declCtors !! 1) . declValueCtor . clr $ vlr, important = False }
                                                                            , CssDeclWrapper { property = (declCtors !! 2) . declValueCtor . cb $ vb,   important = False }
                                                                            , CssDeclWrapper { property = (declCtors !! 3) . declValueCtor . clr $ vlr, important = False }
                                                                            ]
buildSuccessOut declCtors declValueCtor (ctb:clr:[])     (vtb:vlr:[])     = [ CssDeclWrapper { property = (declCtors !! 0) . declValueCtor . ctb $ vtb, important = False }
                                                                            , CssDeclWrapper { property = (declCtors !! 1) . declValueCtor . clr $ vlr, important = False }
                                                                            , CssDeclWrapper { property = (declCtors !! 2) . declValueCtor . ctb $ vtb, important = False }
                                                                            , CssDeclWrapper { property = (declCtors !! 3) . declValueCtor . clr $ vlr, important = False }
                                                                            ]
buildSuccessOut declCtors declValueCtor (ctlbr:[])       (vtlbr:[])       = [ CssDeclWrapper { property = (declCtors !! 0) . declValueCtor . ctlbr $ vtlbr, important = False }
                                                                            , CssDeclWrapper { property = (declCtors !! 1) . declValueCtor . ctlbr $ vtlbr, important = False }
                                                                            , CssDeclWrapper { property = (declCtors !! 2) . declValueCtor . ctlbr $ vtlbr, important = False }
                                                                            , CssDeclWrapper { property = (declCtors !! 3) . declValueCtor . ctlbr $ vtlbr, important = False }
                                                                            ]




-- Build CSS snippet as an input to parser, and an expected CSS declaration
-- that matches that CSS snippet.
--
-- The CSS snippet will be passed to CSS parser that should produce a CSS
-- declaration that matches the CSS declaration returned by this function.
--
-- If CSS parser works correctly, then there will be a match between its
-- result and a second element of tuple returned by this function.
buildSuccessRow :: String ->  [(declValue -> CssDeclaration)] -> (CssDistance -> declValue) -> [String] -> [Float] ->  (T.Text, [CssDeclWrapper])
buildSuccessRow name declCtors declValueCtor units values = ( buildValidInput name values units
                                                            , buildSuccessOut declCtors declValueCtor distanceCtors values)
    where
      distanceCtors = makeDistanceCtorsList units




makeDistanceCtorsList :: [String] -> [(Float -> CssDistance)]
makeDistanceCtorsList unitStrings = fmap f unitStrings
  where
    f u | u == "px" = CssDistanceAbsPx
        | u == "mm" = CssDistanceAbsMm
        | u == "em" = CssDistanceRelEm
        | u == "ex" = CssDistanceRelEx
        | otherwise = error ("[EE] unhandled unit " ++ (show u))



