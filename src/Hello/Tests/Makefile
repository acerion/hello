TEST_BIN=tests




all: clean
	ghc -j4 -g -O0 -o $(TEST_BIN) \
		-Wname-shadowing \
		-Wunused-imports \
		-Wno-missing-signatures \
		../*.hs \
		../Css/*.hs \
		../Css/Parser/*.hs \
		../Dw/*.hs \
		../Html/*.hs \
		../Ffi/Css/*.hs \
		../Ffi/Dw/*.hs \
		../Ffi/*.hs \
		./*.hs \
		./Css/*.hs \
		./Css/Parser/*.hs \
		./Html/*.hs \
		./TestUtils/*.hs




distclean: clean
	find ./ -name "*.o"  | xargs rm -f
	find ./ -name "*.hi" | xargs rm -f



clean:
	rm -rf $(TEST_BIN)
