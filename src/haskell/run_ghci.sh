ghci -fobject-code \
     -XOverloadedStrings \
     ./CssParser.hs           ./tests/Css/Tokenizer.hs ../Hello/Tests/Css/Selector.hs \
     ./Css.hs                 ./tests/Css/Css.hs       ./tests/Css/CssTestData.hs \
     ./cookies.hs             ./tests/TestsCookies.hs \
     ./Colors.hs              ./tests/TestsColors.hs \
     ./Gif.hs                 ./tests/TestsGif.hs ./tests/TestsGif2.hs \
     ./HtmlEntity.hs          ./tests/TestsHtmlEntity.hs \
     ./HtmlTag.hs             ./tests/TestsHtmlTag.hs \
     ./Utils.hs \
     ./tests_tools/Gifted.hs
