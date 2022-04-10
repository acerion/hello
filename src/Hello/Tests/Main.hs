{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




module Main
  (
    main
  )
where




import System.Exit

import Hello.Tests.Colors
import Hello.Tests.Cookies

import Hello.Tests.Gif
import Hello.Tests.Gif2

import Hello.Tests.Css.Css
import Hello.Tests.Css.Rule
import Hello.Tests.Css.Selector
import Hello.Tests.Css.Tokenizer
import Hello.Tests.Css.Value

import Hello.Tests.Html.Entity
import Hello.Tests.Html.Tag
import Hello.Tests.Html.Attribute
import Hello.Tests.Html.Doctype




testFunctions = [testsColors, testsCookies,
                 testsGif, testsGif2,
                 testsCssCss, testsCssRule, testsCssComplexSelector, testsCssTokenizer, testsCssValue,
                 testsHtmlEntity, testsHtmlTag, testsHtmlAttribute, testsHtmlDoctype]




main :: IO ()
main = do
  results <- sequence testFunctions
  let failures = filter (\c -> c /= "") results
  if length failures > 0
    then
    do
      sequence $ map putStrLn failures
      exitFailure
    else exitSuccess
