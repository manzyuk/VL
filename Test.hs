import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import VL.Parser (parse)
import VL.Pretty (pprint)

import qualified VL.ConcreteEvaluator as Concrete (interpret)
import qualified VL.AbstractEvaluator as Abstract (interpret)

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

tests = [ testGroup "Concrete interpreter"
          (testInterpreter concreteInterpreter)
        , testGroup "Abstract interpreter"
          (testInterpreter abstractInterpreter)
        , testGroup "Interaction between LETREC and REAL"
          [ testCase "prog44" (test_prog44 abstractInterpreter)
          , testCase "prog45" (test_prog45 abstractInterpreter)
          , testCase "prog46" (test_prog46 abstractInterpreter)
          , testCase "prog47" (test_prog47 abstractInterpreter)
          ]
        ]

testInterpreter interpreter
    = [ testGroup "Primitives"
        [ testGroup "CAR and CDR"
          [ testCase "prog01" (test_prog01 interpreter)
          , testCase "prog02" (test_prog02 interpreter)
          , testCase "prog03" (test_prog03 interpreter)
          ]
        , testGroup "Arithmetics"
          [ testCase "prog04" (test_prog04 interpreter)
          , testCase "prog05" (test_prog05 interpreter)
          , testCase "prog06" (test_prog06 interpreter)
          , testCase "prog07" (test_prog07 interpreter)
          , testCase "prog08" (test_prog08 interpreter)
          ]
        , testGroup "Comparisons"
          [ testCase "prog09" (test_prog09 interpreter)
          , testCase "prog10" (test_prog10 interpreter)
          , testCase "prog11" (test_prog11 interpreter)
          , testCase "prog12" (test_prog12 interpreter)
          , testCase "prog13" (test_prog13 interpreter)
          , testCase "prog14" (test_prog14 interpreter)
          ]
        , testGroup "Unary functions"
          [ testCase "prog15" (test_prog15 interpreter)
          , testCase "prog16" (test_prog16 interpreter)
          , testCase "prog17" (test_prog17 interpreter)
          , testCase "prog18" (test_prog18 interpreter)
          , testCase "prog19" (test_prog19 interpreter)
          , testCase "prog20" (test_prog20 interpreter)
          , testCase "prog21" (test_prog21 interpreter)
          , testCase "prog22" (test_prog22 interpreter)
          , testCase "prog23" (test_prog23 interpreter)
          ]
        , testGroup "IF expressions"
          [ testCase "prog24" (test_prog24 interpreter)
          , testCase "prog25" (test_prog25 interpreter)
          , testCase "prog26" (test_prog26 interpreter)
          , testCase "prog27" (test_prog27 interpreter)
          ]
        , testGroup "Predicates"
          [ testCase "prog28" (test_prog28 interpreter)
          , testCase "prog29" (test_prog29 interpreter)
          , testCase "prog30" (test_prog30 interpreter)
          , testCase "prog31" (test_prog31 interpreter)
          , testCase "prog32" (test_prog32 interpreter)
          , testCase "prog33" (test_prog33 interpreter)
          , testCase "prog34" (test_prog34 interpreter)
          , testCase "prog35" (test_prog35 interpreter)
          ]
        ]
      , testGroup "LET and LETREC"
        [ testGroup "LET"
          [ testCase "prog36" (test_prog36 interpreter)
          , testCase "prog37" (test_prog37 interpreter)
          , testCase "prog38" (test_prog38 interpreter)
          , testCase "prog39" (test_prog39 interpreter)
          ]
        , testGroup "LETREC"
          [ testCase "prog40" (test_prog40 interpreter)
          , testCase "prog41" (test_prog41 interpreter)
          , testCase "prog42" (test_prog42 interpreter)
          , testCase "prog43" (test_prog43 interpreter)
          ]
        ]
      ]

test_prog01 interpreter = testProgramWith interpreter "(car (cons 2 #f))" "2.0"
test_prog02 interpreter = testProgramWith interpreter "(cdr (cons 2 #f))" "#f"
test_prog03 interpreter = testProgramWith interpreter "(cons (car (cons 2 #f)) (cdr (cons 2 #f)))" "(2.0 . #f)"

test_prog04 interpreter = testProgramWith interpreter "(+ 3 4)" "7.0"
test_prog05 interpreter = testProgramWith interpreter "(- 3 4)" "-1.0"
test_prog06 interpreter = testProgramWith interpreter "(* 3 4)" "12.0"
test_prog07 interpreter = testProgramWith interpreter "(/ 3 4)" "0.75"
test_prog08 interpreter = testProgramWith interpreter "(** 3 4)" "81.0"

test_prog09 interpreter = testProgramWith interpreter "(== 3 4)" "#f"
test_prog10 interpreter = testProgramWith interpreter "(/= 3 4)" "#t"
test_prog11 interpreter = testProgramWith interpreter "(<  3 4)" "#t"
test_prog12 interpreter = testProgramWith interpreter "(<= 3 4)" "#t"
test_prog13 interpreter = testProgramWith interpreter "(>  3 4)" "#f"
test_prog14 interpreter = testProgramWith interpreter "(>= 3 4)" "#f"

test_prog15 interpreter = testProgramWith interpreter "(exp 0)" "1.0"
test_prog16 interpreter = testProgramWith interpreter "(log 1)" "0.0"
test_prog17 interpreter = testProgramWith interpreter "(< (- 3 (log (exp 3))) 0.001)" "#t"
test_prog18 interpreter = testProgramWith interpreter "(+ (* (sin 3) (sin 3)) (* (cos 3) (cos 3)))" "1.0"
test_prog19 interpreter = testProgramWith interpreter "(sqrt 4)" "2.0"
test_prog20 interpreter = testProgramWith interpreter "(negate 3)" "-3.0"
test_prog21 interpreter = testProgramWith interpreter "(- (cosh 3) (/ (+ (exp 3) (exp (negate 3))) 2))" "0.0"
test_prog22 interpreter = testProgramWith interpreter "(- (sinh 3) (/ (- (exp 3) (exp (negate 3))) 2))" "0.0"
test_prog23 interpreter = testProgramWith interpreter "(< (- 1 (- (* (cosh 2) (cosh 2)) (* (sinh 2) (sinh 2)))) 0.001)" "#t"

test_prog24 interpreter = testProgramWith interpreter "(if #t 1 (cons #f 3))" "1.0"
test_prog25 interpreter = testProgramWith interpreter "(if #f 1 (cons #f 3))" "(#f . 3.0)"
test_prog26 interpreter = testProgramWith interpreter "((lambda (x) (if (> x 0) x (negate x))) -3)" "3.0"
test_prog27 interpreter = testProgramWith interpreter "((lambda (a b c) (if (< a b) (if (< b c) a (if (< a c) a c)) (if (< a c) b (if (< b c) b c)))) 3 -1 7)" "-1.0"

test_prog28 interpreter = testProgramWith interpreter "(null? 3)" "#f"
test_prog29 interpreter = testProgramWith interpreter "(null? ())" "#t"
test_prog30 interpreter = testProgramWith interpreter "(pair? 3)" "#f"
test_prog31 interpreter = testProgramWith interpreter "(pair? (cons 1 2))" "#t"
test_prog32 interpreter = testProgramWith interpreter "(real? 3)" "#t"
test_prog33 interpreter = testProgramWith interpreter "(real? #t)" "#f"
test_prog34 interpreter = testProgramWith interpreter "(boolean? #f)" "#t"
test_prog35 interpreter = testProgramWith interpreter "(boolean? 3)" "#f"

test_prog36 interpreter = testProgramWith interpreter "(let ((x 1) (y 2)) (+ x y))" "3.0"
test_prog37 interpreter = testProgramWith interpreter "(let ((abs (lambda (x) (if (> x 0) x (negate x))))) (abs (- 3 4)))" "1.0"
test_prog38 interpreter = testProgramWith interpreter "(let ((s (sin 3)) (c (cos 3))) (+ (* s s) (* c c)))" "1.0"
test_prog39 interpreter = testProgramWith interpreter "(let ((z (lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y))))))) (fact (lambda (f) (lambda (x) (if (== x 0) 1 (* x (f (- x 1)))))))) ((z fact) 5))" "120.0" -- Z combinator approach to computing factorial

test_prog40 interpreter = testProgramWith interpreter "(letrec ((fact (n) (if (== n 0) 1 (* n (fact (- n 1)))))) (fact 4))" "24.0"
test_prog41 interpreter = testProgramWith interpreter "(letrec ((even? (n) (if (== n 0) #t (odd? (- n 1)))) (odd? (n) (if (== n 0) #f (even? (- n 1))))) (even? 10))" "#t"
test_prog42 interpreter = testProgramWith interpreter "(letrec ((gcd (a b) (if (== a 0) b (if (< a b) (gcd b a) (gcd (- a b) b))))) (gcd 18 15))" "3.0"
test_prog43 interpreter = testProgramWith interpreter "(letrec ((map (f l) (if (pair? l) (cons (f (car l)) (map f (cdr l))) ()))) (map (lambda (x) (* 2 x)) (list 1 2 3)))" "(2.0 . (4.0 . (6.0 . ())))"

test_prog44 interpreter = testProgramWith interpreter "(letrec ((fact (n) (if (== n 0) 1 (* n (fact (- n 1)))))) (fact (real 4)))" "R"
test_prog45 interpreter = testProgramWith interpreter "(letrec ((even? (n) (if (== n 0) #t (odd? (- n 1)))) (odd? (n) (if (== n 0) #f (even? (- n 1))))) (even? (real 10)))" "B"
test_prog46 interpreter = testProgramWith interpreter "(letrec ((gcd (a b) (if (== a 0) b (if (< a b) (gcd b a) (gcd (- a b) b))))) (gcd (real 18) (real 15)))" "R"
test_prog47 interpreter = testProgramWith interpreter "(letrec ((map (f l) (if (pair? l) (cons (f (car l)) (map f (cdr l))) ()))) (map (lambda (x) (* 2 x)) (list (real 1) (real 2) (real 3))))" "(R . (R . (R . ())))"