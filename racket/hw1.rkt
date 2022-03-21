#lang racket
(require rackunit)
(require 2htdp/image)

(provide img->mat ascii-art)


(define (split-list n lst [acc '()])
  (cond
    ([null? lst] (if (= (length (car acc)) n) (reverse (map reverse acc)) (reverse (map reverse (cdr acc)))))
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

(define (img->mat img)
  (split-list (image-width img) (map RGB->grayscale (image->color-list img))))


(define (average lst)
  (/ (apply + lst) (length lst)))

(define (average-lst lsts)
  (map (lambda (l) (/ l (length lsts))) (apply (curry map +)  lsts)))


(define (average-group mat width height)
  (map average-lst (split-list height (map (compose (curry map average) (curry split-list width)) mat))))


(define (ascii-art width height chars)
  (define chars-list (string->list chars))
  (define d (length chars-list))
  (define (intensity->ascii i)
    (list-ref chars-list (inexact->exact (floor (/ (* d (- 255 (floor i))) 256)))))

  (lambda (img)
    (apply string-append
           (map (lambda (row) (string-append (apply string (map intensity->ascii row)) "\n"))
                (average-group (img->mat img) width height)))))
                 
;((ascii-art 2 2 " .,:;ox%#@") example)

