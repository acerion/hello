ghci -fobject-code \
     -XOverloadedStrings \
     ./Css/CssParser.hs           ./Tests/Css/Tokenizer.hs ./Tests/Css/Selector.hs ./Tests/Css/Rule.hs \
     ./Css/Css.hs                 ./Tests/Css/Css.hs       ./Tests/Css/CssTestData.hs \
     ./cookies.hs             ./Tests/TestsCookies.hs \
     ./Colors.hs              ./Tests/TestsColors.hs \
     ./Gif.hs                 ./Tests/TestsGif.hs ./Tests/TestsGif2.hs \
     ./Html/HtmlEntity.hs          ./Tests/Html/TestsHtmlEntity.hs \
     ./Html/HtmlTag.hs             ./Tests/Html/TestsHtmlTag.hs \
     ./Utils.hs \
     ./Tests/TestUtils/Gifted.hs
