#lang racket

; Exercise 1

(define (permutations lst)
  (define (interleave left right el [acc '()])
    (if (null? right)
        (cons (append left (list el)) acc)
        (interleave (append left `(,(car right))) (cdr right) el (append acc (list(append left (list el) right))))))

  (if (= (length lst) 1)
      `((,(car lst)))
  (apply append (map (lambda (x) (interleave '() x (car lst))) (permutations (cdr lst))))))


; Exercise 2

(define (variable tree) (car tree))
(define (left-subtree tree) (cadr tree))
(define (right-subtree tree) (caddr tree))
(define (is-leaf tree) (= (length tree) 1))

(define bool-tree '(x1 (x2 (x3 1 0) (x3 0 1)) (x2 (x3 0 0) (x3 1 1))))

; Easy

(define (evaluate-bool-tree tree values)
  (if (null? values)
      tree
      (if (= (car values) 0)
          (evaluate-bool-tree (left-subtree tree) (cdr values))
          (evaluate-bool-tree (right-subtree tree) (cdr values)))))

; Higher-order functions
(define (evaluate-bool-tree-high tree values)
  (define (num-to-subtree n)
    (if (= n 0) left-subtree right-subtree))
  ((apply compose (map num-to-subtree (reverse values))) tree))


; satisficing evaluations
(define (s-e tree [acc '()])
  (define (assign key val) `(,(list key val)))
    (if (number? tree)
        (if (eqv? tree 1)
            (list (reverse acc))
            '())
        (append (s-e (left-subtree tree) (append (assign 0 (variable tree)) acc))
                (s-e (right-subtree tree) (append (assign 1 (variable tree)) acc)))))
    


; Tasks

; Task 1. sub-seq

(define (suffixes lst [acc '()])
  (if (null? lst)
      acc
      (suffixes (cdr lst) (append (list lst) acc))))

(define (prefixes lst)
  (map reverse (suffixes (reverse lst))))


(define (sub-seq lst)
  (if (null? lst)
      '()
  (append (prefixes lst) (sub-seq (cdr lst)))))



; Task 2 tournament
(define tournament '(F (D (A (A) (B)) (D (C) (D))) (F (F (E) (F)) (G (G) (H)))))


(define (get-defeated tr)
  (if (is-leaf tr)
      '()
      (let( [left (variable (left-subtree tr))]
            [right (variable (right-subtree tr))]
            [current (variable tr)])
         (if (equal? left current)
             (cons right (get-defeated (left-subtree tr)))
             (cons left (get-defeated (right-subtree tr)))))))


(get-defeated tournament)



   
