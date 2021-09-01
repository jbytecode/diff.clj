(ns diffclj.gamma-test
  (:require
    [clojure.test :refer [deftest testing is]]
    [diffclj.gamma :refer [gamma]]
    [diffclj.utility :refer [approx?]]))


(deftest gamma-test-integers
  (testing "Gamma function for integers"
    (is (approx?
          120.0
          (gamma 6.0)
          0.001))
    (is (approx?
          24.0
          (gamma 5.0)
          0.001))
    (is (approx?
          6.0
          (gamma 4.0)
          0.001))))
