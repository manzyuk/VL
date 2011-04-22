.PHONY: test clean
test:
	ghc --make -O2 -Wall  \
		Test/Test.hs  \
		Language/*.hs \
		Alacarte/*.hs \
		Abstract/*.hs \
		Concrete/*.hs \
	-threaded -o test && ./test

clean:
	rm -f test
	find . -name "*.o"  -delete
	find . -name "*.hi" -delete
