(ns diffclj.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [diffclj.core :refer [deriv simplify] :as diff]))

(def ^:dynamic x 1)
(def ^:dynamic sqrt diff/sqrt)


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
    (simplify '(log (exp 0))))))
