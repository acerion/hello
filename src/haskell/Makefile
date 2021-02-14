# https://www.vex.net/~trebla/haskell/so.xhtml


all:
	hsc2hs gif.hsc
	ghc -O2 -fPIC -c wrapper.c
	ghc -O2 -dynamic -shared -fPIC -o libhello.so URL.hs cookies.hs gif.hs wrapper.o -lHSrts-ghc8.0.1

distclean: clean

clean:
	rm -rf *.o *.a *.so *.hi *_stub.h *.out

