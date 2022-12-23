{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}




module Hello.Tests.Css.Parser.Utils
  (
    buildSuccessRow
  , makeDistanceCtorsList
  )
where




import qualified Data.List as L
import qualified Data.Text as T

--import Debug.Trace

import Hello.Css.Declaration
import Hello.Css.Distance




-- Build a snippet of CSS that will be passed to CSS parser and parsed by it.
-- The parser should then return a CssProperty.
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
{-
buildSuccessOut :: [v -> a]                      -- ^ Converter between raw value and type accepted by
                                                 --   declaration value constructor. E.g. CssDistanceRelPx constructor.
                -> [v]                           -- ^ Raw value of property, e.g. 11.5.
                -> Maybe CssDeclaration
-}
buildSuccessOut (ct:cr:cb:cl:[]) (vt:vr:vb:vl:[]) = Just CssDeclaration { property = CssPropertyMargin (CssValueMargin
                                                                                                          (CssValueMarginXDistance . ct $ vt)
                                                                                                          (CssValueMarginXDistance . cr $ vr)
                                                                                                          (CssValueMarginXDistance . cb $ vb)
                                                                                                          (CssValueMarginXDistance . cl $ vl)
                                                                                                         )
                                                                          , important = False }

buildSuccessOut (ct:clr:cb:[])   (vt:vlr:vb:[])   = Just CssDeclaration { property = CssPropertyMargin (CssValueMargin
                                                                                                    (CssValueMarginXDistance . ct  $ vt)
                                                                                                    (CssValueMarginXDistance . clr $ vlr)
                                                                                                    (CssValueMarginXDistance . cb  $ vb)
                                                                                                    (CssValueMarginXDistance . clr $ vlr)
                                                                                                  )
                                                                     , important = False }

buildSuccessOut (ctb:clr:[])     (vtb:vlr:[])     = Just CssDeclaration { property = CssPropertyMargin (CssValueMargin
                                                                                                      (CssValueMarginXDistance . ctb $ vtb)
                                                                                                      (CssValueMarginXDistance . clr $ vlr)
                                                                                                      (CssValueMarginXDistance . ctb $ vtb)
                                                                                                      (CssValueMarginXDistance . clr $ vlr)
                                                                                                    )
                                                                     , important = False }

buildSuccessOut (ctrlb:[])       (vtrbl:[])       = Just CssDeclaration { property = CssPropertyMargin (CssValueMargin
                                                                                                      (CssValueMarginXDistance . ctrlb $ vtrbl)
                                                                                                      (CssValueMarginXDistance . ctrlb $ vtrbl)
                                                                                                      (CssValueMarginXDistance . ctrlb $ vtrbl)
                                                                                                      (CssValueMarginXDistance . ctrlb $ vtrbl)
                                                                                                    )
                                                                     , important = False }





-- Build CSS snippet as an input to parser, and an expected CSS declaration
-- that matches that CSS snippet.
--
-- The CSS snippet will be passed to CSS parser that should produce a CSS
-- declaration that matches the CSS declaration returned by this function.
--
-- If CSS parser works correctly, then there will be a match between its
-- result and a second element of tuple returned by this function.
buildSuccessRow :: String ->  [String] -> [Float] ->  (T.Text, Maybe CssDeclaration)
-- buildSuccessRow name  units values = trace ("generated input string = " ++ show input ++ ", generated output = " ++ show output) (input, output)
buildSuccessRow name units values = (input, output)
    where
      distanceCtors = makeDistanceCtorsList units
      input  = buildValidInput name values units
      output = buildSuccessOut distanceCtors values




makeDistanceCtorsList :: [String] -> [(Float -> CssDistance)]
makeDistanceCtorsList unitStrings = fmap f unitStrings
  where
    f u | u == "px" = CssDistanceAbsPx
        | u == "mm" = CssDistanceAbsMm
        | u == "em" = CssDistanceRelEm
        | u == "ex" = CssDistanceRelEx
        | otherwise = error ("[EE] unhandled unit " ++ (show u))



