#lang racket
(require "lambda-calculus.rkt")


(define T '(λ x : (λ y : x)))
(define F '(λ x : (λ y : y)))
 
(define CONS 
  '(λ a : (λ b : (λ z : ((z a) b))))
  )

(define SWAP `(λ z : ((,CONS (z ,F)) (z ,T))))
 
(eval `(((,CONS a) b) ,T))
(eval `(((,CONS a) b) ,F))



(eval `(,SWAP ((,CONS a) b)))