# Calls to ghc: https://www.vex.net/~trebla/haskell/so.xhtml


all:
	hsc2hs ./Ffi/Css/Css.hsc
	hsc2hs ./Ffi/Css/Context.hsc
	hsc2hs ./Ffi/Css/DeclarationSetsGlobal.hsc
	hsc2hs ./Ffi/Css/Distance.hsc
	hsc2hs ./Ffi/Css/DoctreeNode.hsc
	hsc2hs ./Ffi/Css/Doctree.hsc
	hsc2hs ./Ffi/Css/MatchCache.hsc
	hsc2hs ./Ffi/Css/Parser.hsc
	hsc2hs ./Ffi/Css/StyleEngine.hsc
	hsc2hs ./Ffi/Css/StyleSheet.hsc
	hsc2hs ./Ffi/Dw/DwLength.hsc
	hsc2hs ./Ffi/Dw/FontAttrs.hsc
	hsc2hs ./Ffi/Dw/Style.hsc
	hsc2hs ./Ffi/Gif.hsc
	hsc2hs ./Ffi/Html/Attribute.hsc
	hsc2hs ./Ffi/Html/Doctype.hsc
	hsc2hs ./Ffi/Html/HtmlTag.hsc
	hsc2hs ./Ffi/Preferences.hsc
	hsc2hs ./Ffi/Url.hsc

	ghc -g -O0 -fPIC -c ./wrapper.c
	ghc -j4 -g -O0 -dynamic -shared -fPIC -o libhello.so \
		-fwarn-incomplete-patterns \
		./*.hs \
		./Css/*.hs \
		./Dw/*.hs \
		./Ffi/*.hs \
		./Ffi/Css/*.hs \
		./Ffi/Dw/*.hs \
		./Ffi/Html/*.hs \
		./Html/*.hs \
		./wrapper.o -lHSrts-ghc8.8.4

distclean: clean
	make -C Tests distclean

clean:
	find ./ -name "*.hi"      | xargs rm -f
	find ./ -name "*.a"       | xargs rm -f
	find ./ -name "*.o"       | xargs rm -f
	find ./ -name "*.so"      | xargs rm -f
	find ./ -name "*.out"     | xargs rm -f
	find ./ -name "*_stub.h"  | xargs rm -f
	find ./Ffi/ -name "*.h"   | xargs rm -f

	rm -f ./Ffi/Css/Css.hs
	rm -f ./Ffi/Css/Context.hs
	rm -f ./Ffi/Css/DeclarationSetsGlobal.hs
	rm -f ./Ffi/Css/Distance.hs
	rm -f ./Ffi/Css/DoctreeNode.hs
	rm -f ./Ffi/Css/Doctree.hs
	rm -f ./Ffi/Css/MatchCache.hs
	rm -f ./Ffi/Css/Parser.hs
	rm -f ./Ffi/Css/StyleEngine.hs
	rm -f ./Ffi/Css/StyleSheet.hs
	rm -f ./Ffi/Dw/DwLength.hs
	rm -f ./Ffi/Dw/FontAttrs.hs
	rm -f ./Ffi/Dw/Style.hs
	rm -f ./Ffi/Gif.hs
	rm -f ./Ffi/Html/Attribute.hs
	rm -f ./Ffi/Html/Doctype.hs
	rm -f ./Ffi/Html/HtmlTag.hs
	rm -f ./Ffi/Preferences.hs
	rm -f ./Ffi/Url.hs

