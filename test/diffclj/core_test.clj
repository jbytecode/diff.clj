(ns diffclj.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [diffclj.core :refer [deriv simplify] :as diff]))

(def ^:dynamic x 1)
(def ^:dynamic sqrt diff/sqrt)


(deftest is-product
  (testing "is that a product?"
    (is
     (diff/product? '(* 5 2)))

    (is
     (diff/product? '(* 60 70)))

    (is
     (not (diff/product? '(/ 60 70))))

    (is
     (not (diff/product? 50)))))



(deftest is-division-of-products
  (testing "is that a division of two products ?"
    (is
     (diff/division-of-products? '(/ (* 5 2) (* 10 10))))

    (is
     (not (diff/division-of-products? '(/ (+ 5 2) (* 6 7)))))

    (is
     (not (diff/division-of-products? '(/ 60 70))))

    (is
     (not (diff/division-of-products? 50)))))



(deftest division-simplification
  (testing "Testing AB/BC = AC type of divisions"
    (is
     (=
      (simplify '(/ (* (+ x 2) (+ x 5)) (* (+ x 5) (+ x 7))))
      '(/ (+ x 2) (+ x 7))))))


(deftest derivatives
  (testing "Derivative of (+ 5 5)"
    (is
     (= 0 (eval (deriv '(+ 5 5))))))

  (testing "Derivative of (- 5 5)"
    (is
     (= 0 (eval (deriv '(- 5 5))))))

  (testing "Derivative of (* 5 x) when x = 10"
    (binding [x 10]
      (is
       (=  5
           (eval
            (deriv '(* 5 diffclj.core-test/x)))))))

  (testing "Derivative of (* x x) when x = 10"
    (binding [x 10]
      (is
       (=  20
           (eval
            (deriv '(* diffclj.core-test/x
                       diffclj.core-test/x)))))))

  (testing "Derivative of (sqrt 5)"
    (is
     (=  0.0
         (binding [*ns* (find-ns 'diffclj.core)] (eval
                                                  (deriv '(sqrt 5)))))))

  (testing "Derivative of log(x + 5)"
    (is
     (=  (/ 1.0 15.0)
         (binding [*ns*          (find-ns 'diffclj.core)
                   x             10.0] (eval
                                        (deriv '(log (+ diffclj.core-test/x 5))))))))


  (testing "Derivative of exp(x + 5)"
    (is
     (=  (Math/exp 15.0)
         (binding [*ns*          (find-ns 'diffclj.core)
                   x             10.0] (eval
                                        (deriv '(exp (+ diffclj.core-test/x 5)))))))))





(deftest test-simplify
  (testing "Simplify functions"
    (is
     (=
      10
      (simplify '(+ 5 5))))


    (is
     (=
      0
      (simplify '(- 5 5))))

    (is
     (=
      0
      (simplify '(* 0 5))))

    (is
     (=
      0
      (simplify '(* 5 0))))

    (is
     (=
      5
      (simplify '(+ 5 0))))

    (is
     (=
      5
      (simplify '(+ 0 5))))

    (is
     (=
      0
      (simplify '(* 5 (- 10 10))))))

  (is
   (=
    1
    (simplify '(/ (+ 10 10) (+ 10 10)))))

  (is
   (=
    0
    (simplify '(/ (- 10 10) (+ 10 10)))))


  (is
   (=
    1
    (simplify '(exp (* 10 0)))))

  (is
   (=
    1
    (simplify '(pow 20 (* 10 0)))))

  (is
   (=
    0
    (simplify '(log (exp 0)))))

  (binding [*ns*          (find-ns 'diffclj.core)]
    (is
     (=
      (Math/cos (* 2.0 Math/PI))
      (eval (simplify '(cos (* 2.0 Math/PI)))))))

  (binding [*ns*          (find-ns 'diffclj.core)]
    (is
     (=
      (Math/sin (* 2.0 Math/PI))
      (eval (simplify '(sin (* 2.0 Math/PI)))))))

  (binding [*ns*          (find-ns 'diffclj.core)]
    (is
     (=
      (Math/tan (* 2.0 Math/PI))
      (eval (simplify '(tan (* 2.0 Math/PI)))))))

  (binding [*ns*          (find-ns 'diffclj.core)]
    (is
     (=
      (simplify (deriv '(cot x)))
      '(/ -1 (pow (sin x) 2.0)))))

  (binding [*ns*          (find-ns 'diffclj.core)]
    (is
     (=
      (simplify (deriv '(sec x)))
      '(/ (- 0 (* -1 (sin x))) (pow (cos x) 2.0)))))


  (binding [*ns*          (find-ns 'diffclj.core)]
    (is
     (=
      (simplify (deriv '(cosec x)))
      '(/ (- 0 (cos x)) (pow (sin x) 2.0))))))







(deftest test-hyperbolic-trigonmetrics
  (testing "Testing hyperbolic triginometric functions"

    (binding [*ns*          (find-ns 'diffclj.core)]
      (is
       (=
        (simplify (deriv '(sinh x)))
        '(* 0.5 (- (exp x) (* -1 (exp (* -1 x))))))))

    (binding [*ns*          (find-ns 'diffclj.core)]
      (is
       (=
        (simplify (deriv '(cosh x)))
        '(* 0.5 (+ (exp x) (* -1 (exp (* -1 x))))))))

    (binding [*ns*          (find-ns 'diffclj.core)]
      (is
       (=
        (simplify (deriv '(tanh x)))
        '(/ (- (* (* 0.5 (- (exp x) (* -1 (exp (* -1 x))))) (cosh x)) (* (* 0.5 (+ (exp x) (* -1 (exp (* -1 x))))) (sinh x))) (pow (cosh x) 2.0)))))

    (binding [*ns*          (find-ns 'diffclj.core)]
      (is
       (=
        (simplify (deriv '(coth x))))))
    '(/ (- (* (* 0.5 (+ (exp x) (* -1 (exp (* -1 x))))) (sinh x)) (* (* 0.5 (- (exp x) (* -1 (exp (* -1 x))))) (cosh x))) (pow (sinh x) 2.0))

    (binding [*ns*          (find-ns 'diffclj.core)]
      (is
       (=
        (simplify (deriv '(sech x)))
        '(/ (- 0 (* 0.5 (+ (exp x) (* -1 (exp (* -1 x)))))) (pow (cosh x) 2.0)))))


    (binding [*ns*          (find-ns 'diffclj.core)]
      (is
       (=
        (simplify (deriv '(csch x)))
        '(/ (- 0 (* 0.5 (- (exp x) (* -1 (exp (* -1 x)))))) (pow (sinh x) 2.0)))))))





(deftest test-inverse-trigonometric
  (testing "Inverse trigonometric functions"


    (binding [*ns*          (find-ns 'diffclj.core)]
      (is
       (=
        (simplify (deriv '(asin x)))
        '(/ 1 (sqrt (- 1 (pow x 2)))))))


    (binding [*ns*          (find-ns 'diffclj.core)]
      (is
       (=
        (simplify (deriv '(acos x)))
        '(/ -1 (sqrt (- 1 (pow x 2)))))))


    (binding [*ns*          (find-ns 'diffclj.core)]
      (is
       (=
        (simplify (deriv '(atan x)))
        '(/ 1 (+ 1 (pow x 2))))))


    (binding [*ns*          (find-ns 'diffclj.core)]
      (is
       (=
        (simplify (deriv '(acot x)))
        '(/ -1 (+ 1 (pow x 2))))))))

