# diff.clj
Derivative of single variable functions and expression simplifier


# Examples

```clojure
diffclj.core=> (deriv '(* x x))
;; (+ (* 1 x) (* 1 x))

diffclj.core=> (simplify (deriv '(* x x)))
;; (* 2 x)
```

```clojure
diffclj.core=> (deriv '(* 2 x))
;; (+ (* 0 x) (* 1 2))

diffclj.core=> (simplify (deriv '(* 2 x)))
;; 2
```

```clojure
diffclj.core=> (def equation '(- 5 (log (* 2 x))))
;; #'diffclj.core/equation

diffclj.core=> (deriv equation)
;; (- 0 (/ (+ (* 0 x) (* 1 2)) (* 2 x)))

diffclj.core=> (simplify (deriv equation))
;; (- 0 (/ 2 (* 2 x)))

diffclj.core=> (def x 10)
;; #'diffclj.core/x

diffclj.core=> (eval (simplify (deriv equation)))
;; -1/10
```

```clojure
diffclj.core=> (deriv '(+ (* 3 (pow x 3)) (* 5 (pow x 2))))
;; (+ (+ (* 0 (pow x 3)) (* (* (pow x 3) (+ (* 0 (log x)) (* (/ 1 x) 3))) 3)) (+ (* 0 (pow x 2)) (* (* (pow x 2) (+ (* 0 (log x)) (* (/ 1 x) 2))) 5)))

diffclj.core=> (simplify (deriv '(+ (* 3 (pow x 3)) (* 5 (pow x 2)))))
;; (+ (* (* (pow x 3) (* (/ 1 x) 3)) 3) (* (* (pow x 2) (* (/ 1 x) 2)) 5))
```

## Higher order derivatives

```clojure
diffclj.core=> (deriv (deriv '(log x)))
;; (/ (- (* 0 x) (* 1 1)) (* x x))

diffclj.core=> (simplify (deriv (deriv '(log x))))
;; (/ -1 (pow x 2.0))

diffclj.core=> (def x 10)
;; #'diffclj.core/x

diffclj.core=> (eval (simplify (deriv (deriv '(log x)))))
;; -0.01
```