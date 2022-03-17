#lang racket


; Exercises

(define (stream-add s1 s2)
  (stream-cons (+ (stream-first s1) (stream-first s2)) (stream-add (stream-rest s1) (stream-rest s2))))

(define fib-stream
  (stream-cons 1 (stream-cons 1
                              (stream-add fib-stream (stream-rest fib-stream)))))


(struct graph (nodes edges))

; Graph
(define gr (graph '(1 2 3 4 5 6) '((1 2) (1 5) (2 3) (2 5) (3 4) (4 5) (4 6))))


(define (edge? g)
  (lambda (p)
    (or (member p (graph-edges g)) (member (reverse p) (graph-edges g))))) 


(define (permutations lst)
  (define (interleave left right el [acc '()])
    (if (null? right)
        (cons (append left (list el)) acc)
        (interleave (append left `(,(car right))) (cdr right) el (append acc (list(append left (list el) right))))))

  (if (= (length lst) 1)
      `((,(car lst)))
  (apply append (map (lambda (x) (interleave '() x (car lst))) (permutations (cdr lst))))))


(define (check-path g)
  (lambda (lst)
    (define but-last (take lst (- (length lst) 1)))
    (if (andmap (edge? g) (map list but-last (cdr lst)))
        lst
        #f)))


(define (hamiltonian-path g)
  (define paths (filter (lambda (x) (not (equal? x #f))) (map (check-path g) (permutations (graph-nodes g)))))
  (if (null? paths)
      #f
      (car paths)))
  

; Tasks

(define (infinite-stream x)
  (stream-cons x (infinite-stream x)))

(define (stream-mul s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2)) (stream-mul (stream-rest s1) (stream-rest s2))))

(define stream-factorial
  (stream* 1 (stream-mul (in-naturals 1) stream-factorial)))

(define (power-stream x)
  (stream* 1 (stream-mul (power-stream x) (infinite-stream x))))

(define (exp-stream x)
  (define (combine s1 s2)
    (stream-cons (/ (stream-first s1) (stream-first s2)) (combine (stream-rest s1) (stream-rest s2))))
  (combine (power-stream x) stream-factorial))


; Min Vertex Cover
(define (stream-merge s1 s2 cmp)
  (cond
    ([stream-empty? s1] s2)
    ([stream-empty? s2] s1)
    ([cmp (stream-first s1) (stream-first s2)]
     (stream-cons (stream-first s1) (stream-merge (stream-rest s1) s2 cmp)))
    (else (stream-cons (stream-first s2) (stream-merge s1 (stream-rest s2) cmp)))))
 

(define (sub-seq lst)
  (if (null? lst)
      (stream '())
      (let ([el (car lst)]
            [rest-sub-seq (sub-seq (cdr lst))])
        (stream-merge rest-sub-seq
                (stream-map ((curry cons) el) rest-sub-seq)
                (lambda (x y) (< (length x) (length y)))))))


(define (check-vertex-cover g)
  (define edges (graph-edges g))
  (lambda (lst)
    (if (andmap (lambda (e) (or (member (car e) lst) (member (cadr e) lst))) edges)
        lst
        #f)))


(define (min-vertex-cover g)
  (define nodes (graph-nodes g))
  (stream-first (stream-filter identity (stream-map (check-vertex-cover g) (sub-seq nodes)))))
  

(define test_s1 (stream 1 2 3 4 5))
(define test_s2 (stream 0 4 3 2 5))


