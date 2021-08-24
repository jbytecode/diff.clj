[![Clojars Project](https://img.shields.io/clojars/v/com.github.jbytecode/diffclj.svg)](https://clojars.org/com.github.jbytecode/diffclj)


# diff.clj
Derivative of single variable functions and expression simplifier

# Installation

## Leiningen/Boot

```clojure
[com.github.jbytecode/diffclj "0.1.0"]
```

## Clojure CLI/deps.edn

```clojure
com.github.jbytecode/diffclj {:mvn/version "0.1.0"}
```

## Gradle 

```gradle
implementation("com.github.jbytecode:diffclj:0.1.0")
```

## Maven

```XML
<dependency>
  <groupId>com.github.jbytecode</groupId>
  <artifactId>diffclj</artifactId>
  <version>0.1.0</version>
</dependency>
```

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

## Logarithmic rule

```clojure
diffclj.core=> (deriv '(pow x x))
;; (* (pow x x) (+ (* 1 (log x)) (* (/ 1 x) x)))

diffclj.core=> (simplify (deriv '(pow x x)))
;; (* (pow x x) (+ (log x) (* (/ 1 x) x)))
```

## Defined functions

```clojure
(declare deriv-cosec         ;; cosecant
         deriv-sec           ;; secant
         deriv-cot           ;; cotangent
         deriv-tan           ;; tangent
         deriv-cos           ;; cosine
         deriv-sin           ;; sine
         deriv-exp           ;; exponential
         deriv-plus          ;; +
         deriv-minus         ;; -
         deriv-product       ;; *
         deriv-divide        ;; /
         deriv-power         ;; ^
         deriv-log10         ;; Logarithm with base 10
         deriv-log2          ;; Logarithm with base 2
         deriv-log           ;; Natural logarithm
         deriv-sqrt          ;; Square root
         deriv-list        
         deriv)
```


## Basic symbolic simplification

```clojure
diffclj.core=> (simplify '(+ 2 5))
;; 7

diffclj.core=> (simplify '(* (+ x 5) (+ x 5)))
;; (pow (+ x 5) 2.0)

diffclj.core=> (simplify '(* 1 (sin (/ Math/PI 2))))
;; (sin (/ Math/PI 2))

diffclj.core=> (simplify '(/ 2 (/ x x)))
;; 2

diffclj.core=> (simplify '(exp (* x 0)))
;; 1
```
