(ns diffclj.core
  (:gen-class))


;; declare derivative functions
;; on top of the file, because
;; the call order may differs
;; than the order of definitions
(declare
 deriv-acot          ; Inverse cotangent
 deriv-atan          ; Inverse tangent
 deriv-acos          ; Inverse cosine
 deriv-asin          ; Inverse sine
 deriv-csch          ; hyperbolic cosecant
 deriv-sech          ; hyperbolic secant
 deriv-coth          ; hyperbolic cotangent
 deriv-tanh          ; hyperbolic tangent
 deriv-cosh          ; hyperbolic cosine
 deriv-sinh          ; hyperbolic sine
 deriv-cosec         ; cosecant
 deriv-sec           ; secant
 deriv-cot           ; cotangent
 deriv-tan           ; tangent
 deriv-cos           ; cosine
 deriv-sin           ; sine
 deriv-exp           ; exponential
 deriv-plus          ; +
 deriv-minus         ; -
 deriv-product       ; *
 deriv-divide        ; /
 deriv-power         ; ^
 deriv-log10         ; Logarithm with base 10
 deriv-log2          ; Logarithm with base 2
 deriv-log           ; Natural logarithm
 deriv-sqrt          ; Square root
 deriv-list
 deriv)


(declare
 simplify-acot
 simplify-atan
 simplify-acos
 simplify-asin
 simplify-csch
 simplify-sech
 simplify-coth
 simplify-tanh
 simplify-cosh
 simplify-sinh
 simplify-cosec
 simplify-cot
 simplify-tan
 simplify-sec
 simplify-cos
 simplify-sin
 simplify-log2
 simplify-log10
 simplify-log
 simplify-power
 simplify-exp
 simplify-division-of-products
 simplify-divide
 simplify-product
 simplify-plus
 simplify-minus
 simplify-list
 simplify)


;; Namespace aliases for Math fns
;; sqrt, log, pow, etc
(def sqrt  #(Math/sqrt %))
(def log   #(Math/log %))
(def log10 #(Math/log10 %))
(def pow   #(Math/pow %1 %2))
(def exp   #(Math/exp %))
(def sin   #(Math/sin %))
(def cos   #(Math/cos %))
(def tan   #(Math/tan %))
(def sinh  #(Math/sinh %))
(def cosh  #(Math/cosh %))
(def tanh  #(Math/tanh %))
(def asin  #(Math/asin %))
(def acos  #(Math/acos %))
(def atan  #(Math/atan %))


(defn log2
  [x]
  (/ (log x) (log 2)))


(defn cot
  [x]
  (/ 1.0 (tan x)))


(defn sec
  [x]
  (/ 1.0 (cos x)))


(defn cosec
  [x]
  (/ 1.0 (sin x)))


(defn coth
  [x]
  (/ 1.0 (tanh x)))


(defn sech
  [x]
  (/ 1.0 (cosh x)))


(defn csch
  [x]
  (/ 1.0 (sinh x)))


(defn acot
  [x]
  (/ 1.0 (atan x)))


;; %%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%

;; First test if x is number
;; and then test if it is zero
(defn number-and-zero?
  [x]
  (and
   (number? x)
   (zero? x)))


;; First test if x is number
;; and then test if it is one
(defn number-and-one?
  [x]
  (and
   (number? x)
   (= (double x) 1.0)))


(defn product?
  [expr]
  (and
   (list? expr)
   (= (count expr) 3)
   (= (first expr) '*)))


(defn division-of-products?
  [expr]
  (and
   (list? expr)
   (= (count expr) 3)
   (= (first expr) '/)
   (product? (second expr))
   (product? (last expr))))


;; %%%%%%%%%%% Derivation functions %%%%%%%%%%%%%%%%%%%



;; Derivative of sum of two terms
(defn deriv-plus
  [expr]
  (list
   '+
   (deriv (second expr))
   (deriv (last expr))))


;; Derivative of subtraction of two terms
(defn deriv-minus
  [expr]
  (list
   '-
   (deriv (second expr))
   (deriv (last expr))))


;; Derivative of production of two terms
(defn deriv-product
  [expr]
  (list
   '+
   (list
    '*
    (deriv (second expr))
    (last expr))
   (list
    '*
    (deriv (last expr))
    (second expr))))


;; Derivative of division of two terms
(defn deriv-divide
  [expr]
  (list
   '/
   (list
    '-
    (list
     '*
     (deriv (second expr))
     (last expr))
    (list
     '*
     (deriv (last expr))
     (second expr)))

   (list
    '*
    (last expr)
    (last expr))))


;; Derivative of x^a style expression where
;; a is a constant or a function
(defn deriv-power
  [expr]
  (list
   '*
   expr
   (deriv
    (list
     '*
     (last expr)
     (list 'log (second expr))))))


;; Derivative of ln a style expression where
;; a is either a constant or a function
(defn deriv-log
  [expr]
  (list
   '/
   (deriv (second expr))
   (second expr)))


(defn deriv-log10
  [expr]
  (list
   '/
   (deriv (second expr))
   (list
    '*
    (second expr)
    (log 10))))


(defn deriv-log2
  [expr]
  (list
   '/
   (deriv (second expr))
   (list
    '*
    (second expr)
    (log 2))))


;; Derivate of sqrt(a) where a is either
;; a constant or a function
(defn deriv-sqrt
  [expr]
  (list
   '*
   (deriv (second expr))
   (list
    '/
    1
    (list '* 2 (list 'sqrt (second expr))))))


;; Derivative of exp(a) where a is either
;; a constant or a function
(defn deriv-exp
  [expr]
  (list
   '*
   (deriv (second expr))
   (list
    'exp
    (second expr))))


;; Derivative of sin(a) where a is either
;; a constant or a function
(defn deriv-sin
  [expr]
  (list
   '*
   (deriv (second expr))
   (list
    'cos
    (second expr))))


;; Derivative of cos(a) where a is either
;; a constant or a function
(defn deriv-cos
  [expr]
  (list
   '*
   -1
   (list
    '*
    (deriv (second expr))
    (list
     'sin
     (second expr)))))


;; Derivative of tan(a) where a is either
;; a costant of a function.
;; tanf(x) = sinf(x) / cosf(x)
;; d(tanf(x))/dx = f'(x) / [cosf(x) * cosf(x)]
(defn deriv-tan
  [expr]
  (list
   '/
   (deriv (second expr))
   (list
    '*
    (list
     'cos
     (second expr))
    (list
     'cos
     (second expr)))))


;; Derivative of cot(a) where a is either
;; a costant of a function.
;; cotf(x) = cosf(x) / sinf(x)
;; d(cotf(x))/dx = -f'(x) / [sinf(x) * sinf(x)]
(defn deriv-cot
  [expr]
  (list
   '/
   (list
    '* -1 (deriv (second expr)))
   (list
    '*
    (list
     'sin
     (second expr))
    (list
     'sin
     (second expr)))))


;; sec(x) = 1 / cos(x)
(defn deriv-sec
  [expr]
  (deriv
   (list
    '/
    1.0
    (list
     'cos
     (second expr)))))


;; sec(x) = 1 / sin(x)
(defn deriv-cosec
  [expr]
  (deriv
   (list
    '/
    1.0
    (list
     'sin
     (second expr)))))


(defn deriv-sinh
  [expr]
  (list
   '*
   (list '/ 1 2)
   (list
    '-
    (list '* (deriv (second expr)) (list 'exp (second expr)))
    (list
     '*
     (deriv (list '* -1 (second expr)))
     (list 'exp (list '* -1 (second expr)))))))


(defn deriv-cosh
  [expr]
  (list
   '*
   (list '/ 1 2)
   (list
    '+
    (list '* (deriv (second expr)) (list 'exp (second expr)))
    (list
     '*
     (deriv (list '* -1 (second expr)))
     (list 'exp (list '* -1 (second expr)))))))


(defn deriv-tanh
  [expr]
  (deriv
   (list
    '/
    (list
     'sinh
     (second expr))
    (list
     'cosh
     (second expr)))))


(defn deriv-coth
  [expr]
  (deriv
   (list
    '/
    (list
     'cosh
     (second expr))
    (list
     'sinh
     (second expr)))))


(defn deriv-sech
  [expr]
  (deriv
   (list
    '/
    1
    (list
     'cosh
     (second expr)))))


(defn deriv-csch
  [expr]
  (deriv
   (list
    '/
    1
    (list
     'sinh
     (second expr)))))


(defn deriv-asin
  [expr]
  (list
   '/
   (deriv (second expr))
   (list
    'sqrt
    (list '- 1
          (list 'pow (second expr) 2)))))


(defn deriv-acos
  [expr]
  (list
   '/
   (list
    '* -1 (deriv (second expr)))
   (list
    'sqrt
    (list '- 1
          (list 'pow (second expr) 2)))))


(defn deriv-atan
  [expr]
  (list
   '/
   (deriv (second expr))
   (list
    '+
    1
    (list
     'pow (second expr) 2))))


(defn deriv-acot
  [expr]
  (list
   '/
   (list
    '*
    -1
    (deriv (second expr)))
   (list
    '+
    1
    (list
     'pow (second expr) 2))))


;; Derivation of (binaryop first-operand second operand) type
;; expression like (* 'x 2) or (/ 'x (* x 2))
(defn deriv-list
  [expr]
  (let [op (first expr)]
    (condp = op
      '+     (deriv-plus expr)
      '-     (deriv-minus expr)
      '*     (deriv-product expr)
      '/     (deriv-divide expr)
      'pow   (deriv-power expr)
      'log   (deriv-log  expr)
      'log10 (deriv-log10 expr)
      'log2  (deriv-log2 expr)
      'sqrt  (deriv-sqrt expr)
      'exp   (deriv-exp  expr)
      'sin   (deriv-sin expr)
      'cos   (deriv-cos expr)
      'tan   (deriv-tan expr)
      'cot   (deriv-cot expr)
      'sec   (deriv-sec expr)
      'cosec (deriv-cosec expr)
      'sinh  (deriv-sinh expr)
      'cosh  (deriv-cosh expr)
      'tanh  (deriv-tanh expr)
      'coth  (deriv-coth expr)
      'sech  (deriv-sech expr)
      'csch  (deriv-csch expr)
      'asin  (deriv-asin expr)
      'acos  (deriv-acos expr)
      'atan  (deriv-atan expr)
      'acot  (deriv-acot expr)
      (throw
       (Exception.
        (str "[ERROR] Deriv function not defined: " op))))))


;; Main dispatcher function for
;; derivatives of a single variable
;; function
(defn deriv
  [expr]
  (cond
    (number? expr)       0
    (list? expr)         (deriv-list expr)
    (symbol? expr)       1))


;; %%%%%%%%%%%%%%%%%%%%%%%
;; Functions for simplify
;; %%%%%%%%%%%%%%%%%%%%%%%

(defn simplify-plus
  [expr]
  (let
      [par1     (simplify (second expr))
       par2     (simplify (last expr))]
    (cond
      (number-and-zero? par1)               par2
      (number-and-zero? par2)               par1
      (and (number? par1) (number? par2))   (+ par1 par2)
      (= par1 par2)                         (list '* 2 par1)
      true                                  (list '+ par1 par2))))


(defn simplify-minus
  [expr]
  (let
      [par1     (simplify (second expr))
       par2     (simplify (last expr))]
    (cond
      (number-and-zero? par2)              par1
      (= par1 par2)                        0
      (and (number? par1) (number? par2))  (- par1 par2)
      true                                 (list '- par1 par2))))


(defn simplify-product
  [expr]
  (let
      [par1     (simplify (second expr))
       par2     (simplify (last expr))]
    (cond
      (number-and-zero? par1)               0
      (number-and-zero? par2)               0
      (number-and-one?  par1)              par2
      (number-and-one?  par2)              par1
      (and (number? par1) (number? par2))  (* par1 par2)
      (= par1 par2)                        (list 'pow par1 2.0)
      true                                 (list '* par1 par2))))


(defn simplify-division-of-products
  [expr]
  (let
      [nom                                (simplify (second expr))
       denom                              (simplify (last expr))
       nom-part1                          (if (list? nom) (simplify (second nom)) nom)
       nom-part2                          (if (list? nom) (simplify (last nom)) nom)
       denom-part1                        (if (list? denom) (simplify (second denom)) denom)
       denom-part2                        (if (list? denom) (simplify (last denom)) denom)]
    (cond
      (= nom-part1 denom-part1)         (simplify (list '/ nom-part2 denom-part2))  ; AB/AC = B/C
      (= nom-part1 denom-part2)         (simplify (list '/ nom-part2 denom-part1))  ; AB/CA = B/C
      (= nom-part2 denom-part1)         (simplify (list '/ nom-part1 denom-part2))  ; AB/BC = A/C
      (= nom-part2 denom-part2)         (simplify (list '/ nom-part1 denom-part1))  ; AB/CB = A/C
      true                              (list '/ nom denom))))           ; AB/CD = AB/CD



(defn simplify-divide
  [expr]
  (let
      [par1     (simplify (second expr))
       par2     (simplify (last expr))]
    (cond
      (number-and-zero? par1)            0

      (and (number? par2)
           (= par2 1))                   par1

      (= par1 par2)                      1

      (and (number? par1)
           (number? par2))              (double (/ par1 par2))

      (division-of-products? expr)      (simplify-division-of-products expr)

      true                              (list '/ par1 par2))))


(defn simplify-exp
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number-and-zero? par1)       1
      true                          (list 'exp par1))))


(defn simplify-log
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (and (number? par1)
           (= par1 1))                0
      true                            (list 'log par1))))


(defn simplify-log10
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (and (number? par1)
           (= par1 1))                0
      true                            (list 'log10 par1))))


(defn simplify-log2
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (and (number? par1)
           (= par1 1))                0
      true                            (list 'log2 par1))))


(defn simplify-sqrt
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (sqrt par1)
      true                           (list 'sqrt par1))))


(defn simplify-power
  [expr]
  (let
      [par1     (simplify (second expr))
       par2      (simplify (last expr))]
    (cond
      (number-and-zero? par2)       1
      (number-and-zero? par1)       0
      true                          (list 'pow par1 par2))))


(defn simplify-sin
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (sin par1)
      true                           (list 'sin par1))))


(defn simplify-cos
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (cos par1)
      true                           (list 'cos par1))))


(defn simplify-tan
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (tan par1)
      true                           (list 'tan par1))))


(defn simplify-cot
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (cot par1)
      true                           (list 'cot par1))))


(defn simplify-sec
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (sec par1)
      true                           (list 'sec par1))))


(defn simplify-cosec
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (cosec par1)
      true                           (list 'cosec par1))))


(defn simplify-sinh
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (sinh par1)
      true                           (list 'sinh par1))))


(defn simplify-cosh
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (cosh par1)
      true                           (list 'cosh par1))))


(defn simplify-tanh
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (tanh par1)
      true                           (list 'tanh par1))))


(defn simplify-coth
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (coth par1)
      true                           (list 'coth par1))))


(defn simplify-sech
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (sech par1)
      true                           (list 'sech par1))))


(defn simplify-csch
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (csch par1)
      true                           (list 'csch par1))))


(defn simplify-asin
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (asin par1)
      true                           (list 'asin par1))))


(defn simplify-acos
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (acos par1)
      true                           (list 'acos par1))))


(defn simplify-atan
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (atan par1)
      true                           (list 'atan par1))))


(defn simplify-acot
  [expr]
  (let
      [par1     (simplify (second expr))]
    (cond
      (number? par1)                 (acot par1)
      true                           (list 'acot par1))))


(defn simplify-list
  [expr]
  (let [op (first expr)]
    (condp = op
      '+     (simplify-plus expr)
      '-     (simplify-minus expr)
      '*     (simplify-product expr)
      '/     (simplify-divide expr)
      'exp   (simplify-exp expr)
      'pow   (simplify-power expr)
      'sqrt  (simplify-sqrt expr)
      'log   (simplify-log expr)
      'log10 (simplify-log10 expr)
      'log2  (simplify-log2 expr)
      'sin   (simplify-sin expr)
      'cos   (simplify-cos expr)
      'tan   (simplify-tan expr)
      'cot   (simplify-cot expr)
      'sec   (simplify-sec expr)
      'cosec (simplify-cosec expr)
      'sinh  (simplify-sinh expr)
      'cosh  (simplify-cosh expr)
      'tanh  (simplify-tanh expr)
      'coth  (simplify-coth expr)
      'sech  (simplify-sech expr)
      'csch  (simplify-csch expr)
      'asin  (simplify-asin expr)
      'acos  (simplify-acos expr)
      'atan  (simplify-atan expr)
      'acot  (simplify-acot expr)
      (throw
       (Exception.
        (str "[ERROR] Simplify function not defined: " op))))))


;; Main dispatcher function for
;; simplifying expressions
(defn simplify
  [expr]
  (cond
    (number? expr)       expr
    (list? expr)         (simplify-list expr)
    (symbol? expr)       expr))


(defn -main
  [& args]
  (println "Todo: Add command line arguments parser"))
