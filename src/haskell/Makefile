# Calls to ghc: https://www.vex.net/~trebla/haskell/so.xhtml


all:
	hsc2hs ffi/Gif.hsc
	hsc2hs ffi/HtmlTag.hsc
	hsc2hs ffi/CssParser.hsc

	ghc -O2 -fPIC -c wrapper.c
	ghc -O2 -dynamic -shared -fPIC -o libhello.so \
		URL.hs cookies.hs \
		Gif.hs              ffi/Gif.hs \
		CssParser.hs        ffi/CssParser.hs \
		Colors.hs           ffi/Colors.hs \
		HtmlEntity.hs \
		HtmlTag.hs          ffi/HtmlTag.hs \
		Utils.hs \
		wrapper.o -lHSrts-ghc8.4.4

distclean: clean

clean:
	rm -rf *.o *.a *.so *.hi *_stub.h *.out
	rm -rf tests/*.o tests/*.hi
	rm -rf tests_tools/*.o tests_tools/*.hi
	rm -rf ffi/*.o ffi/*.hi ffi/*.h
	rm ffi/Gif.hs
	rm ffi/HtmlTag.hs
