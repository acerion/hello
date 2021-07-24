# Calls to ghc: https://www.vex.net/~trebla/haskell/so.xhtml


all:
	hsc2hs ./Ffi/Gif.hsc
	hsc2hs ./Ffi/Html/HtmlTag.hsc
	hsc2hs ./Ffi/Css/CssParser.hsc
	hsc2hs ./Ffi/Css/Css.hsc

	ghc -g -O0 -fPIC -c ./wrapper.c
	ghc -g -O0 -dynamic -shared -fPIC -o libhello.so \
		./Css/StyleSheet.hs \
		./URL.hs ./cookies.hs \
		./Gif.hs              ./Ffi/Gif.hs \
		./Css/Css.hs \
		./Css/CssParser.hs        ./Ffi/Css/CssParser.hs \
		                    ./Ffi/Css/Css.hs \
		                    ./Ffi/Utils.hs \
		./Colors.hs           ./Ffi/Colors.hs \
		./Html/HtmlEntity.hs \
		./Html/HtmlTag.hs          ./Ffi/Html/HtmlTag.hs \
		./Utils.hs \
		./wrapper.o -lHSrts-ghc8.4.4

distclean: clean

clean:
	find ./ -name "*.hi"      | xargs rm -f
	find ./ -name "*.a"       | xargs rm -f
	find ./ -name "*.o"       | xargs rm -f
	find ./ -name "*.so"      | xargs rm -f
	find ./ -name "*.out"     | xargs rm -f
	find ./ -name "*_stub.h"  | xargs rm -f

	find ./ -name "*.hi"     | xargs rm -f
	find ./ -name "*.o"      | xargs rm -f
	find ./Ffi/ -name "*.h"  | xargs rm -f

	rm -f ./Ffi/Gif.hs
	rm -f ./Ffi/Html/HtmlTag.hs
	rm -f ./Ffi/Css/CssParser.hs
	rm -f ./Ffi/Css/Css.hs

