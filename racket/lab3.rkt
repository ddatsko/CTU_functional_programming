#lang racket

(define (f-all-pairs f lst1 lst2)
  (apply append (map
   (lambda (x) (map (curry f x) lst2))
   lst1)))


(define (get-coef m) (car m)) ; first component
(define (get-exp m) (cadr m)) ; second component


(define (add-mon m1 m2)
  (list (+ (get-coef m1) (get-coef m2)) ; sum coefficients
        (get-exp m1))                   ; keep exponent
  )
 
(define (mult-mon m1 m2)
  (list (* (get-coef m1) (get-coef m2)) ; multiply coefficients
        (+ (get-exp m1) (get-exp m2)))  ; sum exponents
  )


(define (same-exp? m1 m2)
  (eqv? (get-exp m1) (get-exp m2)))


(define (add-mono-pol mon pol)
  (define same-mon (filter (curry same-exp? mon) pol))
  (define rest (filter (compose not (curry same-exp? mon)) pol))
  (if (null? same-mon)
      (cons mon pol)
      (cons (add-mon (car same-mon) mon) rest)))


(define (normalize p)
  (define (non-zero-coef? m) (not (= 0 (get-coef m)))) 
  (sort
   (filter non-zero-coef? p)
   (lambda (p1 p2) (< (get-exp p1) (get-exp p2)))))


(define (add-pol p1 p2)
  (normalize (foldl add-mono-pol p1 p2)))


(define (mult-pol p1 p2)
  (normalize (foldl add-mono-pol '() (f-all-pairs mult-mon p1 p2))))


; Tasks


(define (scalar-mult scalar vect)
  (map (curry * scalar) vect))

(define (add-vectors vectors)
  (apply (curry map +) vectors))

(define (linear-combination vectors scales)
  (add-vectors (map scalar-mult scales vectors)))


; Matrix mult

(define (matrix-transpose m)
  (apply map list m))

(define (matrix-mult m1 m2)
  (matrix-transpose (map (lambda (x) (linear-combination (matrix-transpose m1) x)) (matrix-transpose m2))))


(define (matrix-power p m)
  (foldl matrix-mult m (make-list (- p 1) m)))




