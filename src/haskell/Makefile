# Calls to ghc: https://www.vex.net/~trebla/haskell/so.xhtml


all:
	hsc2hs ffi/Gif.hsc
	hsc2hs ffi/HtmlTag.hsc
	hsc2hs ffi/CssParser.hsc
	hsc2hs ffi/Css.hsc

	ghc -g -O0 -fPIC -c wrapper.c
	ghc -g -O0 -dynamic -shared -fPIC -o libhello.so \
		URL.hs cookies.hs \
		Gif.hs              ffi/Gif.hs \
		Css.hs \
		CssParser.hs        ffi/CssParser.hs \
		                    ffi/Css.hs \
		                    ../Hello/Ffi/Utils.hs \
		Colors.hs           ffi/Colors.hs \
		HtmlEntity.hs \
		HtmlTag.hs          ffi/HtmlTag.hs \
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

	find ../Hello -name "*.hi" | xargs rm -f
	find ../Hello -name "*.o"  | xargs rm -f

	rm -f ffi/*.h
	rm -f ffi/Gif.hs
	rm -f ffi/HtmlTag.hs
	rm -f ffi/CssParser.hs
	rm -f ffi/Css.hs

