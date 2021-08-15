# Calls to ghc: https://www.vex.net/~trebla/haskell/so.xhtml


all:
	hsc2hs ./Ffi/Css/Css.hsc
	hsc2hs ./Ffi/Css/CssParser.hsc
	hsc2hs ./Ffi/Gif.hsc
	hsc2hs ./Ffi/Html/HtmlTag.hsc
	hsc2hs ./Ffi/Url.hsc

	ghc -g -O0 -fPIC -c ./wrapper.c
	ghc -g -O0 -dynamic -shared -fPIC -o libhello.so \
		./*.hs \
		./Css/*.hs \
		./Ffi/*.hs \
		./Ffi/Css/*.hs \
		./Ffi/Html/*.hs \
		./Html/*.hs \
		./wrapper.o -lHSrts-ghc8.8.4

distclean: clean

clean:
	find ./ -name "*.hi"      | xargs rm -f
	find ./ -name "*.a"       | xargs rm -f
	find ./ -name "*.o"       | xargs rm -f
	find ./ -name "*.so"      | xargs rm -f
	find ./ -name "*.out"     | xargs rm -f
	find ./ -name "*_stub.h"  | xargs rm -f
	find ./Ffi/ -name "*.h"   | xargs rm -f

	rm -f ./Ffi/Css/Css.hs
	rm -f ./Ffi/Css/CssParser.hs
	rm -f ./Ffi/Gif.hs
	rm -f ./Ffi/Html/HtmlTag.hs
	rm -f ./Ffi/Url.hs

