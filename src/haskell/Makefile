# https://www.vex.net/~trebla/haskell/so.xhtml


all:
	ghc -O2 -fPIC -c wrapper.c
	ghc -O2 -dynamic -shared -fPIC -o libhello.so URL.hs cookies.hs wrapper.o -lHSrts-ghc8.0.1

clean:
	rm -rf *.o *.a *.so

