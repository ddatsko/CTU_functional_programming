#lang racket



(struct tape (left val right) #:transparent)
 
(define (fresh-tape size)
  (tape '() 0 (make-list (- size 1) 0)))

(define (change op t)
  (tape (tape-left t) (op (tape-val t) 1) (tape-right t)))


(define (move dir t)
  (match (cons dir t)
    [(cons 'left (tape '() _ _)) (error "out of bounds")]
    [(cons 'right (tape _ _ '())) (error "out of bounds")]
    [(cons 'left (tape left val right)) (tape (reverse (cdr (reverse left))) (car (reverse left)) (cons val right))]
    [(cons 'right (tape left val right)) (tape (append left (list val)) (car right) (cdr right))]))





(define add-prg
  '(@ > @ [- < + >] < *))
 
; program reading non-negative numbers on the first two positions and displaying their product
(define mul-prg
  '(@ > @ < [- > [- > + > + < <] > [- < + >] < <] > > > *))
 
; constant defining the size of tape
(define SIZE 10)

 
; defines a global tape used by the interpreter
(define test-tape (fresh-tape SIZE))


; evaluates comma command, i.e., (car input) -> tape[ptr]
(define (eval-comma prg input t)
  (cond
    ([null? input] (error "Empty input"))
    (else (eval-prg prg (cdr input) (tape (tape-left t) (car input) (tape-right t))))))  ; recursive call processing further commands



  
; evaluates all the commands beside comma
(define (eval-cmd cmd prg input t)
  (if (eqv? cmd '*) (printf "~a " (tape-val t)) (printf ""))

  (eval-prg prg input (match cmd
    ['+ (change + t)] 
    ['- (change - t)]
    ['< (move 'left t)]
    ['> (move 'right t)]
    [_ t])))   ; recursive call processing further commands


(define (eval-cycle cycle prg input t)
  (if (= (tape-val t) 0)
      (eval-prg prg input t)
      (eval-prg (append cycle (append (list cycle) prg)) input t)))

 
(define (eval-prg prg input t)
  ;(displayln t)
  (match prg
    [(list) (cons input t)]                ; are all commands processed? if yes, return remaining input
    [(list '@ rest ...) (eval-comma rest input t)]
    [(list (? list? cmd) rest ...) (eval-cycle cmd rest input t)]
    [(list cmd rest ...) (eval-cmd cmd rest input t)]))  
 
; executes the given program with the given input
(define (run-prg prg input t)
  (eval-prg prg input t)       ; evaluate program
  (printf "done~n"))

