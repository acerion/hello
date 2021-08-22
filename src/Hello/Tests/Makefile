TEST_BIN=tests




all: clean
	ghc -g -O0 -o $(TEST_BIN) \
		../*.hs \
		../Css/*.hs \
		../Html/*.hs \
		./*.hs \
		./Css/*.hs \
		./Html/*.hs \
		./TestUtils/*.hs




distclean: clean




clean:
	rm -rf $(TEST_BIN)
