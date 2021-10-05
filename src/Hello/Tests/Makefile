TEST_BIN=tests




all: clean
	ghc -j4 -g -O0 -o $(TEST_BIN) \
		../*.hs \
		../Css/*.hs \
		../Html/*.hs \
		../Ffi/Css/*.hs \
		../Ffi/*.hs \
		./*.hs \
		./Css/*.hs \
		./Html/*.hs \
		./TestUtils/*.hs




distclean: clean




clean:
	rm -rf $(TEST_BIN)
