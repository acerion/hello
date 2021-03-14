# Calls to ghc: https://www.vex.net/~trebla/haskell/so.xhtml


all:
	hsc2hs gif.hsc
	hsc2hs ffi/HtmlTag.hsc

	ghc -O2 -fPIC -c wrapper.c
	ghc -O2 -dynamic -shared -fPIC -o libhello.so \
		URL.hs cookies.hs gif.hs Colors.hs \
		HtmlEntity.hs \
		HtmlTag.hs ffi/HtmlTag.hs \
		wrapper.o -lHSrts-ghc8.4.4

distclean: clean

clean:
	rm -rf *.o *.a *.so *.hi *_stub.h *.out
	rm -rf tests/*.o tests/*.hi
	rm -rf tests_tools/*.o tests_tools/*.hi
	rm -rf ffi/*.o ffi/*.hs ffi/*.hi ffi/*.h
