#lang racket
(require rackunit)
(require 2htdp/image)

;(provide img->mat
;         ascii-art)

(define (split-list n lst [acc '()])
  (cond
    ([null? lst] (reverse (map reverse acc)))
    ([null? acc] (split-list n (cdr lst) `((,(car lst)))))
    (else (if
           (eqv? (length (car acc)) n)
           (split-list n lst (cons '() acc))
           (split-list n (cdr lst) (cons (cons (car lst) (car acc)) (cdr acc)))))))


(define (RGB->grayscale color)
    (+ (* 0.3 (color-red color))
    (* 0.59 (color-green color))
    (* 0.11 (color-blue color)))
)

