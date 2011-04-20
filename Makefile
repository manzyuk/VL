.PHONY: test clean
test: Test.hs
	ghc --make -O2 \
		Test.hs \
		AbstractEvaluator.hs \
		AbstractAnalysis.hs \
		Environment.hs \
		Scalar.hs \
		Pretty.hs \
		Parser.hs \
		AbstractValue.hs \
		Expression.hs \
		Common.hs \
		ConcreteValue.hs \
		Token.hs \
		ConcreteEvaluator.hs \
		Coproduct.hs \
		Desugar.hs \
		Prepare.hs \
		Uniquify.hs \
		FixedPoint.hs \
	-threaded -o test && ./test

clean:
	rm -f test *.o *.hi
