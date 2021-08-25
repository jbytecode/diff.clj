(ns diffclj.utility)

(defn approx? [a b atol]
  (< (Math/abs (- a b)) atol))
