import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import VL.Scalar
import qualified VL.Environment as Environment

import VL.Parser (parse)
import VL.Pretty (pp, render)

import VL.Macroexpand (macroexpand)

import qualified VL.ConcreteEvaluator as Concrete (interpret)

import qualified VL.AbstractAnalysis as Analysis
import VL.AbstractValue (AbstractValue (..))
import VL.AbstractEvaluator (analyze)

concreteInterpret :: String -> String
concreteInterpret = Concrete.interpret

abstractInterpret :: String -> String
abstractInterpret input = render (pp output)
    where
      (surface, constants) = parse input
      environment = Environment.map AbstractScalar
                  $ primitives `Environment.union` constants
      analysis    = analyze (core, constants)
      output      = Analysis.lookup core environment analysis
      core        = macroexpand surface

main :: IO ()
main = defaultMain tests

testProgramWith :: (String -> String) -> String -> String -> Assertion
testProgramWith interpret program expected
    = assertEqual message expected computed
    where
      message  = unlines [ "The program"
                         , program
                         , "with the expansion"
                         , render . pp . fst . parse $ program
                         , "produced the wrong answer"
                         ]
      computed = interpret program

tests = [ testGroup "Concrete interpreter"
          (testInterpreter concreteInterpret)
        , testGroup "Abstract interpreter"
          (testInterpreter abstractInterpret)
        ]

testInterpreter interpret
    = [ testGroup "Primitives"
        [ testGroup "car and cdr"
          [ testCase "prog01" (test_prog01 interpret)
          , testCase "prog02" (test_prog02 interpret)
          , testCase "prog03" (test_prog03 interpret)
          ]
        , testGroup "Arithmetics"
          [ testCase "prog04" (test_prog04 interpret)
          , testCase "prog05" (test_prog05 interpret)
          , testCase "prog06" (test_prog06 interpret)
          , testCase "prog07" (test_prog07 interpret)
          , testCase "prog08" (test_prog08 interpret)
          ]
        , testGroup "Comparisons"
          [ testCase "prog09" (test_prog09 interpret)
          , testCase "prog10" (test_prog10 interpret)
          , testCase "prog11" (test_prog11 interpret)
          , testCase "prog12" (test_prog12 interpret)
          , testCase "prog13" (test_prog13 interpret)
          , testCase "prog14" (test_prog14 interpret)
          ]
        , testGroup "Unary functions"
          [ testCase "prog15" (test_prog15 interpret)
          , testCase "prog16" (test_prog16 interpret)
          , testCase "prog17" (test_prog17 interpret)
          , testCase "prog18" (test_prog18 interpret)
          , testCase "prog19" (test_prog19 interpret)
          , testCase "prog20" (test_prog20 interpret)
          , testCase "prog21" (test_prog21 interpret)
          , testCase "prog22" (test_prog22 interpret)
          , testCase "prog23" (test_prog23 interpret)
          ]
        , testGroup "IF expressions"
          [ testCase "prog24" (test_prog24 interpret)
          , testCase "prog25" (test_prog25 interpret)
          , testCase "prog26" (test_prog26 interpret)
          , testCase "prog27" (test_prog27 interpret)
          ]
        , testGroup "Predicates"
          [ testCase "prog28" (test_prog28 interpret)
          , testCase "prog29" (test_prog29 interpret)
          , testCase "prog30" (test_prog30 interpret)
          , testCase "prog31" (test_prog31 interpret)
          , testCase "prog32" (test_prog32 interpret)
          , testCase "prog33" (test_prog33 interpret)
          , testCase "prog34" (test_prog34 interpret)
          , testCase "prog35" (test_prog35 interpret)
          ]
        ]
      , testGroup "let and letrec"
        [ testGroup "let"
          [ testCase "prog36" (test_prog36 interpret)
          , testCase "prog37" (test_prog37 interpret)
          , testCase "prog38" (test_prog38 interpret)
          , testCase "prog39" (test_prog39 interpret)
          ]
        , testGroup "letrec"
          [ testCase "prog40" (test_prog40 interpret)
          , testCase "prog41" (test_prog41 interpret)
          , testCase "prog42" (test_prog42 interpret)
          , testCase "prog43" (test_prog43 interpret)
          ]
        ]
      ]

test_prog01 interpret = testProgramWith interpret "(car (cons 2 #f))" "2.0"
test_prog02 interpret = testProgramWith interpret "(cdr (cons 2 #f))" "#f"
test_prog03 interpret = testProgramWith interpret "(cons (car (cons 2 #f)) (cdr (cons 2 #f)))" "(2.0 . #f)"

test_prog04 interpret = testProgramWith interpret "(+ 3 4)" "7.0"
test_prog05 interpret = testProgramWith interpret "(- 3 4)" "-1.0"
test_prog06 interpret = testProgramWith interpret "(* 3 4)" "12.0"
test_prog07 interpret = testProgramWith interpret "(/ 3 4)" "0.75"
test_prog08 interpret = testProgramWith interpret "(** 3 4)" "81.0"

test_prog09 interpret = testProgramWith interpret "(== 3 4)" "#f"
test_prog10 interpret = testProgramWith interpret "(/= 3 4)" "#t"
test_prog11 interpret = testProgramWith interpret "(<  3 4)" "#t"
test_prog12 interpret = testProgramWith interpret "(<= 3 4)" "#t"
test_prog13 interpret = testProgramWith interpret "(>  3 4)" "#f"
test_prog14 interpret = testProgramWith interpret "(>= 3 4)" "#f"

test_prog15 interpret = testProgramWith interpret "(exp 0)" "1.0"
test_prog16 interpret = testProgramWith interpret "(log 1)" "0.0"
test_prog17 interpret = testProgramWith interpret "(< (- 3 (log (exp 3))) 0.001)" "#t"
test_prog18 interpret = testProgramWith interpret "(+ (* (sin 3) (sin 3)) (* (cos 3) (cos 3)))" "1.0"
test_prog19 interpret = testProgramWith interpret "(sqrt 4)" "2.0"
test_prog20 interpret = testProgramWith interpret "(negate 3)" "-3.0"
test_prog21 interpret = testProgramWith interpret "(- (cosh 3) (/ (+ (exp 3) (exp (negate 3))) 2))" "0.0"
test_prog22 interpret = testProgramWith interpret "(- (sinh 3) (/ (- (exp 3) (exp (negate 3))) 2))" "0.0"
test_prog23 interpret = testProgramWith interpret "(< (- 1 (- (* (cosh 2) (cosh 2)) (* (sinh 2) (sinh 2)))) 0.001)" "#t"

test_prog24 interpret = testProgramWith interpret "(if #t 1 (cons #f 3))" "1.0"
test_prog25 interpret = testProgramWith interpret "(if #f 1 (cons #f 3))" "(#f . 3.0)"
test_prog26 interpret = testProgramWith interpret "((lambda (x) (if (> x 0) x (negate x))) -3)" "3.0"
test_prog27 interpret = testProgramWith interpret "((lambda (a b c) (if (< a b) (if (< b c) a (if (< a c) a c)) (if (< a c) b (if (< b c) b c)))) 3 -1 7)" "-1.0"

test_prog28 interpret = testProgramWith interpret "(null? 3)" "#f"
test_prog29 interpret = testProgramWith interpret "(null? ())" "#t"
test_prog30 interpret = testProgramWith interpret "(pair? 3)" "#f"
test_prog31 interpret = testProgramWith interpret "(pair? (cons 1 2))" "#t"
test_prog32 interpret = testProgramWith interpret "(real? 3)" "#t"
test_prog33 interpret = testProgramWith interpret "(real? #t)" "#f"
test_prog34 interpret = testProgramWith interpret "(boolean? #f)" "#t"
test_prog35 interpret = testProgramWith interpret "(boolean? 3)" "#f"

test_prog36 interpret = testProgramWith interpret "(let ((x 1) (y 2)) (+ x y))" "3.0"
test_prog37 interpret = testProgramWith interpret "(let ((abs (lambda (x) (if (> x 0) x (negate x))))) (abs (- 3 4)))" "1.0"
test_prog38 interpret = testProgramWith interpret "(let ((s (sin 3)) (c (cos 3))) (+ (* s s) (* c c)))" "1.0"
test_prog39 interpret = testProgramWith interpret "(let ((z (lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y))))))) (fact (lambda (f) (lambda (x) (if (== x 0) 1 (* x (f (- x 1)))))))) ((z fact) 5))" "120.0" -- Z combinator approach to computing factorial

test_prog40 interpret = testProgramWith interpret "(letrec ((fact (n) (if (== n 0) 1 (* n (fact (- n 1)))))) (fact 4))" "24.0"
test_prog41 interpret = testProgramWith interpret "(letrec ((even? (n) (if (== n 0) #t (odd? (- n 1)))) (odd? (n) (if (== n 0) #f (even? (- n 1))))) (even? 10))" "#t"
test_prog42 interpret = testProgramWith interpret "(letrec ((gcd (a b) (if (== a 0) b (if (< a b) (gcd b a) (gcd (- a b) b))))) (gcd 18 15))" "3.0"
test_prog43 interpret = testProgramWith interpret "(letrec ((map (f l) (if (pair? l) (cons (f (car l)) (map f (cdr l))) ()))) (map (lambda (x) (* 2 x)) (list 1 2 3)))" "(2.0 . (4.0 . (6.0 . ())))"