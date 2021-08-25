(ns diffclj.integrate)

(defn integrate [f a b eps]
  (loop [start a x 0]
    (if (> start b)
      x
      (recur (+ start eps) (+ x (* eps (f start)))))))


