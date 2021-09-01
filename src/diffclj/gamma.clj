(ns diffclj.gamma
  (:require
    [diffclj.integrate :refer [integrate]]))


(defn gamma
  [z]
  (let [func (fn [x] (* (Math/pow x (- z 1.0)) (Math/exp (* -1.0 x))))
        eps 0.01
        result (integrate func 0 250 eps)] result))
