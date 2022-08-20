TEST_BIN=tests




all: clean
	ghc -j4 -g -O0 -o $(TEST_BIN) \
		../*.hs \
		../Css/*.hs \
		../Dw/*.hs \
		../Html/*.hs \
		../Ffi/Css/*.hs \
		../Ffi/Dw/*.hs \
		../Ffi/*.hs \
		./*.hs \
		./Css/*.hs \
		./Html/*.hs \
		./TestUtils/*.hs




distclean: clean
	find ./ -name "*.o"  | xargs rm -f
	find ./ -name "*.hi" | xargs rm -f



clean:
	rm -rf $(TEST_BIN)
