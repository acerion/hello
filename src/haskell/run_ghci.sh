ghci -fobject-code \
     -XOverloadedStrings \
     ./CssParser.hs           ../Hello/Tests/Css/Tokenizer.hs ../Hello/Tests/Css/Selector.hs ../Hello/Tests/Css/Rule.hs \
     ./Css.hs                 ../Hello/Tests/Css/Css.hs       ../Hello/Tests/Css/CssTestData.hs \
     ./cookies.hs             ../Hello/Tests/TestsCookies.hs \
     ./Colors.hs              ../Hello/Tests/TestsColors.hs \
     ./Gif.hs                 ../Hello/Tests/TestsGif.hs ../Hello/Tests/TestsGif2.hs \
     ./HtmlEntity.hs          ../Hello/Tests/Html/TestsHtmlEntity.hs \
     ./HtmlTag.hs             ../Hello/Tests/Html/TestsHtmlTag.hs \
     ./Utils.hs \
     ../Hello/Tests/TestUtils/Gifted.hs
