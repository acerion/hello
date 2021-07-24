# Calls to ghc: https://www.vex.net/~trebla/haskell/so.xhtml


all:
	hsc2hs ../Hello/Ffi/Gif.hsc
	hsc2hs ../Hello/Ffi/Html/HtmlTag.hsc
	hsc2hs ../Hello/Ffi/Css/CssParser.hsc
	hsc2hs ../Hello/Ffi/Css/Css.hsc

	ghc -g -O0 -fPIC -c wrapper.c
	ghc -g -O0 -dynamic -shared -fPIC -o libhello.so \
		../Hello/Css/StyleSheet.hs \
		URL.hs cookies.hs \
		Gif.hs              ../Hello/Ffi/Gif.hs \
		Css.hs \
		CssParser.hs        ../Hello/Ffi/Css/CssParser.hs \
		                    ../Hello/Ffi/Css/Css.hs \
		                    ../Hello/Ffi/Utils.hs \
		Colors.hs           ../Hello/Ffi/Colors.hs \
		HtmlEntity.hs \
		HtmlTag.hs          ../Hello/Ffi/Html/HtmlTag.hs \
		Utils.hs \
		wrapper.o -lHSrts-ghc8.4.4

distclean: clean

clean:
	find ./ -name "*.hi"      | xargs rm -f
	find ./ -name "*.a"       | xargs rm -f
	find ./ -name "*.o"       | xargs rm -f
	find ./ -name "*.so"      | xargs rm -f
	find ./ -name "*.out"     | xargs rm -f
	find ./ -name "*_stub.h"  | xargs rm -f

	find ../Hello -name "*.hi"     | xargs rm -f
	find ../Hello -name "*.o"      | xargs rm -f
	find ../Hello/Ffi/ -name "*.h" | xargs rm -f

	rm -f ../Hello/Ffi/Gif.hs
	rm -f ../Hello/Ffi/Html/HtmlTag.hs
	rm -f ../Hello/Ffi/Css/CssParser.hs
	rm -f ../Hello/Ffi/Css/Css.hs

