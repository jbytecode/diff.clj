(ns diffclj.erf-test
  (:require [clojure.test :refer [deftest testing is]]
            [diffclj.erf :refer [erf inv-erf]]
            [diffclj.utility :refer [approx?]]))

(deftest erf-test
  (testing "erf function"
    (is (approx? 0.8427 (erf 1.0) 0.0001))
    (is (approx? 0.5205 (erf 0.5) 0.0001))))

(deftest inv-erf-test
  (testing "inverse erf function"
    (is (approx? 0.088855 (inv-erf 0.1) 0.0001))
    (is (approx? 0.179143 (inv-erf 0.2) 0.0001))
    (is (approx? 0.272462 (inv-erf 0.3) 0.0001))
    (is (approx? 0.476936 (inv-erf 0.5) 0.0001))))
