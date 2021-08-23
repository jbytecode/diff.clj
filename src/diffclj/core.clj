(ns diffclj.core
  (:gen-class))

(declare deriv-exp)
(declare deriv-plus)
(declare deriv-minus)
(declare deriv-product)
(declare deriv-divide)
(declare deriv-power)
(declare deriv-log)
(declare deriv-sqrt)
(declare deriv-list)
(declare deriv)

(declare simplify-log)
(declare simplify-power)
(declare simplify-exp)
(declare simplify-divide)
(declare simplify-product)
(declare simplify-plus)
(declare simplify-minus)
(declare simplify-list)
(declare simplify)


;; Wrapper functions for 
;; sqrt, log, pow, etc
(defn sqrt [x] (Math/sqrt x))
(defn log  [x] (Math/log x))
(defn pow  [x y] (Math/pow x y))
(defn exp  [x] (Math/exp x))


(defn number-and-zero? [x]
  (and
   (number? x)
   (zero? x)))

;; Derivative of sum of two terms
(defn deriv-plus [expr]
  (list
   '+
   (deriv (second expr))
   (deriv (last expr))))

;; Derivative of subtraction of two terms
(defn deriv-minus [expr]
  (list
   '-
   (deriv (second expr))
   (deriv (last expr))))

;; Derivative of production of two terms 
(defn deriv-product [expr]
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
(defn deriv-divide [expr]
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
(defn deriv-power [expr]
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
(defn deriv-log [expr]
  (list
   '/
   (deriv (second expr))
   (second expr)))

;; Derivate of sqrt(a) where a is either
;; a constant or a function
(defn deriv-sqrt [expr]
  (list
   '*
   (deriv (second expr))
   (list
    '/
    1
    (list '* 2 (list 'sqrt (second expr))))))

;; Derivative of exp(a) where a is either
;; a constant or a function
(defn deriv-exp [expr]
  (list
   '*
   (deriv (second expr))
   (list
    'exp
    (second expr))))

;; Derivation of (binaryop first-operand second operand) type 
;; expression like (* 'x 2) or (/ 'x (* x 2))
(defn deriv-list [expr]
  (let
   [op    (first expr)]
    (cond
      (= op '+)              (deriv-plus expr)
      (= op '-)              (deriv-minus expr)
      (= op '*)              (deriv-product expr)
      (= op '/)              (deriv-divide expr)
      (= op 'pow)            (deriv-power expr)
      (= op 'log)            (deriv-log  expr)
      (= op 'sqrt)           (deriv-sqrt expr)
      (= op 'exp)            (deriv-exp  expr)
      true                   (throw
                              (Exception.
                               (str "[ERROR] Function not defined: " op))))))

;; Main dispatcher function for 
;; derivatives of a single variable
;; function
(defn deriv [expr]
  (cond
    (number? expr)       0
    (list? expr)         (deriv-list expr)
    (symbol? expr)       1))






;; %%%%%%%%%%%%%%%%%%%%%%%
;; Functions for simplify
;; %%%%%%%%%%%%%%%%%%%%%%%

(defn simplify-plus [expr]
  (let
   [par1     (simplify (second expr))
    par2     (simplify (last expr))]
    (cond
      (number-and-zero? par1)               par2
      (number-and-zero? par2)               par1
      (and (number? par1) (number? par2))   (+ par1 par2)
      true                                  (list '+ par1 par2))))

(defn simplify-minus [expr]
  (let
   [par1     (simplify (second expr))
    par2     (simplify (last expr))]
    (cond
      (number-and-zero? par2)              par1
      (= par1 par2)                        0
      (and (number? par1) (number? par2))  (- par1 par2)
      true                                 (list '- par1 par2))))

(defn simplify-product [expr]
  (let
   [par1     (simplify (second expr))
    par2     (simplify (last expr))]
    (cond
      (number-and-zero? par1)               0
      (number-and-zero? par2)               0
      (and (number? par1) (number? par2))  (* par1 par2)
      true                                 (list '* par1 par2))))


(defn simplify-divide [expr]
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

      (and (number? par1)
           (number? par2))              (/ par1 par2)
      true                              (list '/ par1 par2))))


(defn simplify-exp [expr]
  (let
   [par1     (simplify (second expr))]
    (cond
      (number-and-zero? par1)       1
      true                          (list 'exp par1))))


(defn simplify-log [expr]
  (let
   [par1     (simplify (second expr))]
    (cond
      (and (number? par1)
           (= par1 1))                0
      true                            (list 'log par1))))


(defn simplify-power [expr]
  (let
   [par1     (simplify (second expr))
    par2      (simplify (last expr))]
    (cond
      (number-and-zero? par2)       1
      (number-and-zero? par1)       0
      true                          (list 'pow par1 par2))))


(defn simplify-list [expr]
  (let
   [op    (first expr)]
    (cond
      (= op '+)            (simplify-plus expr)
      (= op '-)            (simplify-minus expr)
      (= op '*)            (simplify-product expr)
      (= op '/)            (simplify-divide expr)
      (= op 'exp)          (simplify-exp expr)
      (= op 'pow)          (simplify-power expr)
      (= op 'log)          (simplify-log expr)
      true                 (throw
                            (Exception.
                             (str "[ERROR] Function not defined: " op))))))


;; Main dispatcher function for
;; simplifying expressions
(defn simplify [expr]
  (cond
    (number? expr)       expr
    (list? expr)         (simplify-list expr)
    (symbol? expr)       expr))

(defn -main
  [& args]
  (println "Todo: Add command line arguments parser"))

