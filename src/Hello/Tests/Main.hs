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

import Hello.Tests.Chain
import Hello.Tests.Colors
import Hello.Tests.Cookies

import Hello.Tests.Gif

import Hello.Tests.Css.Cascade
import Hello.Tests.Css.Css
import Hello.Tests.Css.DeclarationSet
import Hello.Tests.Css.Parser
import Hello.Tests.Css.Parser.Combinators
import Hello.Tests.Css.Parser.Declaration
import Hello.Tests.Css.Parser.ImportRule
import Hello.Tests.Css.Parser.QuickCheck
import Hello.Tests.Css.Parser.Selector
import Hello.Tests.Css.Parser.Rule
import Hello.Tests.Css.PropertyValue
import Hello.Tests.Css.Rule
import Hello.Tests.Css.Selector
import Hello.Tests.Css.StyleEngine
import Hello.Tests.Css.Tokenizer

import Hello.Tests.Html.Attribute
import Hello.Tests.Html.Doctree
import Hello.Tests.Html.Doctype
import Hello.Tests.Html.Entity
import Hello.Tests.Html.Tag

import Hello.Tests.Utils



testFunctions :: [IO String]
testFunctions = [ testsChain
                , testsColors
                , testsCookies
                , testsGif
                -- , testsGif2 -- Disabled because of failure described in top-level comment in TestsGif2.hs

                , testsCssCascade
                , testsCssCss
                , testsCssComplexSelector
                , testsCssDeclarationSet
                , testsCssParser
                , testsCssParserCombinators
                , testsCssParserDeclaration
                , testsCssParserImportRule
                , testsCssParserQuickCheck
                , testsCssParserRule
                , testsCssParserSelector
                , testsCssPropertyValue
                , testsCssRule
                , testsCssStyleEngine
                , testsCssTokenizer

                , testsHtmlAttribute
                , testsHtmlDoctree
                , testsHtmlDoctype
                , testsHtmlEntity
                , testsHtmlTag

                , testsUtils
                ]




main :: IO ()
main = do
  results <- sequence testFunctions
  let failures = filter (\c -> c /= "") results
  if length failures > 0
    then
    do
      _ <- sequence $ map putStrLn failures
      exitFailure
    else exitSuccess
