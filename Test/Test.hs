import VL.Language.Parser (parse)
import VL.Language.Pretty (pprint)

import qualified VL.Concrete.Evaluator as Concrete (interpret)
import qualified VL.Abstract.Evaluator as Abstract (interpret)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

concreteInterpreter :: String -> String
concreteInterpreter = Concrete.interpret

abstractInterpreter :: String -> String
abstractInterpreter = Abstract.interpret

main :: IO ()
main = defaultMain tests

testProgramWith :: (String -> String) -> String -> String -> Assertion
testProgramWith interpret program expected
    = assertEqual message expected computed
    where
      message  = unlines [ "The program"
			 , program
			 , "with the expansion"
			 , pprint . fst . parse $ program
			 , "produced the wrong answer"
			 ]
      computed = interpret program

mkTestCase name program expected interpreter = testCase name assertion
    where
      assertion = testProgramWith interpreter program expected

mkTestGroup name interpreter test_defns = testGroup name tests
    where
      tests = [ mkTestCase name program expected interpreter
		    | (name, program, expected) <- test_defns ]

tests = [ testGroup "Concrete interpreter"
	  (testInterpreter concreteInterpreter)
	, testGroup "Abstract interpreter"
	  (testInterpreter abstractInterpreter)
	, mkTestGroup "Interaction between LETREC and REAL" abstractInterpreter
	  [ ("prog44", "(letrec ((fact (n)                        \
		       \           (if (== n 0)                   \
		       \               1                          \
		       \               (* n (fact (- n 1))))))    \
		       \ (fact (real 4)))                         ", "R"                         )
	  , ("prog45", "(letrec ((even? (n)                       \
		       \           (if (== n 0)                   \
		       \               #t                         \
		       \               (odd? (- n 1))))           \
		       \         (odd? (n)                        \
		       \           (if (== n 0)                   \
		       \               #f                         \
		       \               (even? (- n 1)))))         \
		       \  (even? (real 10)))                      ", "B"                         )
	  , ("prog46", "(letrec ((gcd (a b)                       \
		       \           (if (== a 0)                   \
		       \               b                          \
		       \               (if (< a b)                \
		       \                   (gcd b a)              \
		       \                   (gcd (- a b) b)))))    \
		       \  (gcd (real 18) (real 15)))              ", "R"                         )
	  , ("prog47", "(letrec ((map (f l)                       \
		       \           (if (pair? l)                  \
		       \               (cons (f (car l))          \
		       \                     (map f (cdr l)))     \
		       \               ())))                      \
		       \  (map (lambda (x) (* 2 x))               \
		       \       (list (real 1) (real 2) (real 3))))", "(R . (R . (R . ())))"      )
	  ]
	]

testInterpreter interpreter
    = [ testGroup "Primitives"
	[ mkTestGroup "CAR and CDR" interpreter
	  [ ("prog01", "(car (cons 2 #f))                         ", "2.0"                       )
	  , ("prog02", "(cdr (cons 2 #f))                         ", "#f"                        )
	  , ("prog03", "(cons (car (cons 2 #f))                   \
		       \      (cdr (cons 2 #f)))                  ", "(2.0 . #f)"                )
	  ]
	, mkTestGroup "Arithmetics" interpreter
	  [ ("prog04", "(+  3 4)                                  ", "7.0"                       )
	  , ("prog05", "(-  3 4)                                  ", "-1.0"                      )
	  , ("prog06", "(*  3 4)                                  ", "12.0"                      )
	  , ("prog07", "(/  3 4)                                  ", "0.75"                      )
	  , ("prog08", "(** 3 4)                                  ", "81.0"                      )
	  ]
	, mkTestGroup "Comparisons" interpreter
	  [ ("prog09", "(== 3 4)                                  ", "#f"                        )
	  , ("prog10", "(/= 3 4)                                  ", "#t"                        )
	  , ("prog11", "(<  3 4)                                  ", "#t"                        )
	  , ("prog12", "(<= 3 4)                                  ", "#t"                        )
	  , ("prog13", "(>  3 4)                                  ", "#f"                        )
	  , ("prog14", "(>= 3 4)                                  ", "#f"                        )
	  ]
	, mkTestGroup "Unary functions" interpreter
	  [ ("prog15", "(exp 0)                                   ", "1.0"                       )
	  , ("prog16", "(log 1)                                   ", "0.0"                       )
	  , ("prog17", "(< (- 3 (log (exp 3)))                    \
		       \   0.001)                                 ", "#t"                        )
	  , ("prog18", "(+ (* (sin 3) (sin 3))                    \
		       \   (* (cos 3) (cos 3)))                   ", "1.0"                       )
	  , ("prog19", "(sqrt 4)                                  ", "2.0"                       )
	  , ("prog20", "(negate 3)                                ", "-3.0"                      )
	  , ("prog21", "(- (cosh 3)                               \
		       \   (/ (+ (exp 3)                          \
		       \         (exp (negate 3)))                \
		       \      2))                                 ", "0.0"                       )
	  , ("prog22", "(- (sinh 3)                               \
		       \   (/ (- (exp 3)                          \
		       \         (exp (negate 3)))                \
		       \      2))                                 ", "0.0"                       )
	  , ("prog23", "(< (- 1                                   \
		       \      (- (* (cosh 2) (cosh 2))            \
		       \         (* (sinh 2) (sinh 2))))          \
		       \   0.001)                                 ", "#t"                        )
	  ]
	, mkTestGroup "IF expressions" interpreter
	  [ ("prog24", "(if #t 1 (cons #f 3))                     ", "1.0"                       )
	  , ("prog25", "(if #f 1 (cons #f 3))                     ", "(#f . 3.0)"                )
	  , ("prog26", "((lambda (x)                              \
		       \   (if (> x 0)                            \
		       \       x                                  \
		       \       (negate x)))                       \
		       \ -3)                                      ", "3.0"                       )
	  , ("prog27", "((lambda (a b c)                          \
		       \   (if (< a b)                            \
		       \       (if (< b c)                        \
		       \           a                              \
		       \           (if (< a c)                    \
		       \               a                          \
		       \               c))                        \
		       \       (if (< a c)                        \
		       \           b                              \
		       \           (if (< b c)                    \
		       \               b                          \
		       \               c))))                      \
		       \ 3 -1 7)                                  ", "-1.0"                      )
	  ]
	, mkTestGroup "Predicates" interpreter
	  [ ("prog28", "(null? 3)                                 ", "#f"                        )
	  , ("prog29", "(null? ())                                ", "#t"                        )
	  , ("prog30", "(pair? 3)                                 ", "#f"                        )
	  , ("prog31", "(pair? (cons 1 2))                        ", "#t"                        )
	  , ("prog32", "(real? 3)                                 ", "#t"                        )
	  , ("prog33", "(real? #t)                                ", "#f"                        )
	  , ("prog34", "(boolean? #f)                             ", "#t"                        )
	  , ("prog35", "(boolean? 3)                              ", "#f"                        )
	  , ("prog48", "(real? (real 3))                          ", "#t"                        )
	  , ("prog49", "((lambda (x)                              \
		       \   (boolean? (> x 0)))                    \
		       \ (real 3))                                ", "#t"                        )
	  ]
	]
      , testGroup "LET and LETREC"
	[ mkTestGroup "LET" interpreter
	  [ ("prog36", "(let ((x 1) (y 2))                        \
		       \  (+ x y))                                ", "3.0"                       )
	  , ("prog37", "(let ((abs (lambda (x)                    \
		       \             (if (> x 0)                  \
		       \                 x                        \
		       \                 (negate x)))))           \
		       \  (abs (- 3 4)))                          ", "1.0"                       )
	  , ("prog38", "(let ((s (sin 3))                         \
		       \      (c (cos 3)))                        \
		       \  (+ (* s s) (* c c)))                    ", "1.0"                       )
	  -- Z-combinator approach to computing factorial
	  , ("prog39", "(let ((z (lambda (f)                      \
		       \           ((lambda (x)                   \
		       \              (f (lambda (y)              \
		       \                   ((x x) y))))           \
		       \            (lambda (x)                   \
		       \              (f (lambda (y)              \
		       \                   ((x x) y)))))))        \
		       \      (fact (lambda (f)                   \
		       \              (lambda (x)                 \
		       \                (if (== x 0)              \
		       \                    1                     \
		       \                    (* x (f (- x 1))))))))\
		       \  ((z fact) 5))                           ", "120.0"                     )
	  ]
	, mkTestGroup "LETREC" interpreter
	  [ ("prog40", "(letrec ((fact (n)                        \
		       \           (if (== n 0)                   \
		       \               1                          \
		       \               (* n (fact (- n 1))))))    \
		       \  (fact 4))                               ", "24.0"                      )
	  , ("prog41", "(letrec ((even? (n)                       \
		       \           (if (== n 0)                   \
		       \               #t                         \
		       \               (odd? (- n 1))))           \
		       \         (odd? (n)                        \
		       \           (if (== n 0)                   \
		       \               #f                         \
		       \               (even? (- n 1)))))         \
		       \  (even? 10))                             ", "#t"                        )
	  , ("prog42", "(letrec ((gcd (a b)                       \
		       \           (if (== a 0)                   \
		       \               b                          \
		       \               (if (< a b)                \
		       \                   (gcd b a)              \
		       \                   (gcd (- a b) b)))))    \
		       \  (gcd 18 15))                            ", "3.0"                       )
	  , ("prog43", "(letrec ((map (f l)                       \
		       \           (if (pair? l)                  \
		       \               (cons (f (car l))          \
		       \                     (map f (cdr l)))     \
		       \               ())))                      \
		       \  (map (lambda (x) (* 2 x))               \
		       \       (list 1 2 3)))                     ", "(2.0 . (4.0 . (6.0 . ())))")
	  ]
	]
      ]
