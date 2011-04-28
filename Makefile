.PHONY: test-evaluator test-compiler clean vl2c
test-evaluator:
	ghc --make -O2 -Wall  \
		Test/Test.hs  \
		Language/*.hs \
		Alacarte/*.hs \
		Abstract/*.hs \
		Concrete/*.hs \
	-threaded -o test && ./test
test-compiler:
	Test/./run-tests.sh
vl2c:
	ghc --make -O2 -Wall  \
		Compiler/*.hs \
		Language/*.hs \
		Alacarte/*.hs \
		Abstract/*.hs \
		Concrete/*.hs \
	-o vl2c
clean:
	rm -f test vl2c
	find . -name "*.o"   -delete
	find . -name "*.hi"  -delete
	find . -name "*.c"   -delete
	find . -name "*.out" -delete
