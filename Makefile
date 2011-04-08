.PHONY: test
test: Test.o
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
	-threaded -o test && ./test
