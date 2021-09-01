(ns diffclj.erf
  (:require
    [diffclj.integrate :as integration]))


(def c)


(defn erf
  [x]
  (let
    [part1 (/ 2 (Math/sqrt Math/PI))
     f (fn [t] (Math/exp (* -1 (* t t))))
     part2 (integration/integrate f 0 x 0.00001)
     result (* part1 part2)] result))


(def c-memo (atom {}))


(defn- memoize-c
  [n val]
  (let
    [vals (deref c-memo)
     cnt (count vals)
     kwd (keyword (str n))
     result   (if (> (inc n) cnt)
                (do (reset! c-memo (conj vals {kwd val})) val)
                (kwd vals))]
    result))


(defn- get-mem-c
  [n]
  (let [kwd (keyword (str n))
        vals (deref c-memo)
        result (kwd vals)]
    (if (nil? result) (memoize-c n (c n)) result)))


(defn- c
  [n]
  (if (zero? n)
    (memoize-c (double n) 1.0)
    (memoize-c (double n) (apply +
                                 (for [k (range n)]
                                   (/ (* (get-mem-c (double k)) (get-mem-c (- n 1.0 (double k)))) (* (inc k) (+ 1.0 (* 2.0 k)))))))))


(defn- a
  [n]
  (/ (c n) (+ 1.0 (* n 2.0))))


(defn inv-erf
  ([x nterms]
   (let
     [cx  (/ (* x (Math/sqrt Math/PI)) 2)
      an (map a (range nterms))
      pows (map #(inc (* %1 2)) (range nterms))
      terms (map #(* %1 (Math/pow cx %2)) an pows)] (apply + terms)))

  ([x]
   (inv-erf x 50)))
