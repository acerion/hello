ghci -fobject-code \
     -XOverloadedStrings \
     ./cookies.hs             ./tests/TestsCookies.hs \
     ./Colors.hs              ./tests/TestsColors.hs \
     ./HtmlEntity.hs          ./tests/TestsHtmlEntity.hs \
     ./HtmlTag.hs             ./tests/TestsHtmlTag.hs \
     ./gif.hs                 ./tests/TestsGif.hs ./tests/TestsGif2.hs \
     ./tests_tools/Gifted.hs
