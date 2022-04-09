#lang racket

(provide execute)

(define circle (curry format  "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>"))

(define rect (curry format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>"))

(define line (curry format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>"))


(struct function (name vars body) #:transparent)


(define (get-function-from-env env name)
  (cond
    [(empty? env) `()] ; Valid program should never get here
    [(equal? (function-name (car env)) name) (car env)]
    [else (get-function-from-env (cdr env) name)]))



(define (get-function global-env local-env name)
  (let ([local-func (get-function-from-env local-env name)])
    (if (null? local-func) (get-function-from-env global-env name) local-func)))

(define (get-value global-env local-env name)
  (if (or (string? name) (number? name)) name
  (car (function-body (get-function global-env local-env name))))) 

(define (process-num-exp global-env local-env num-exp)
  
 ; (println num-exp)
  (cond
    [(number? num-exp) num-exp]
    [(not (list? num-exp))  (get-value global-env local-env num-exp)]
    
    [(equal? (car num-exp) '+) (+ (process-num-exp global-env local-env (cadr num-exp)) (process-num-exp global-env local-env (caddr num-exp)))]
    [(equal? (car num-exp) '-) (- (process-num-exp global-env local-env (cadr num-exp)) (process-num-exp global-env local-env (caddr num-exp)))]
    [(equal? (car num-exp) '*) (* (process-num-exp global-env local-env (cadr num-exp)) (process-num-exp global-env local-env (caddr num-exp)))]
    [(equal? (car num-exp) '/) (/ (process-num-exp global-env local-env (cadr num-exp)) (process-num-exp global-env local-env (caddr num-exp)))]
    [(equal? (car num-exp) 'floor) (floor (process-num-exp global-env local-env (cadr num-exp)))]
    [(equal? (car num-exp) 'cos) (cos (process-num-exp global-env local-env (cadr num-exp)))]
    [(equal? (car num-exp) 'sin) (sin (process-num-exp global-env local-env (cadr num-exp)))]
    [else (get-value global-env local-env num-exp)]))

(define (process-arg global-env local-env arg)
  (if (string? arg) arg (process-num-exp global-env local-env arg)))




(define (process-definition definition)
  (function (caadr definition) (cdadr definition) (cddr definition)))
  

(define (process-prg prg)
  (if (null? prg) '()
      (append `(,(process-definition (car prg))) (process-prg (cdr prg)))))

(define (make-local-env global-env local-env arguments values)
  (map (lambda (arg val) (apply function `(,arg () (,(process-arg global-env local-env val))))) arguments values))

; Application - function body.
; All the arguments are processed and put into local or global env
(define (process-application application global-env local-env)
  
 ; (println local-env)
 ; (println application)
 ; (println "HERE")
  (match application
    ((cons 'circle _) (apply circle (map (curry process-num-exp global-env local-env) (cdr application))))
    ((cons 'rect _) (apply rect (map (curry process-num-exp global-env local-env) (cdr application))))
    ((cons 'line _) (apply line (map (curry process-num-exp global-env local-env) (cdr application))))
    (_  (process-expression application global-env local-env))))
  


(define (process-function-application application global-env local-env)
  (if (member (car application) '(line circle rect)) (process-application application global-env local-env)
  (let* ([func (get-function global-env local-env (car application))]
         [new-local-env (make-local-env global-env local-env (function-vars func

                                                                            ) (cdr application))])
    (apply string-append (cons "" (map (lambda (exp) (process-application exp global-env new-local-env)) (function-body func)))))))
    


(define (process-bool-exp exp global-env local-env)
  (let ((args (map (curry process-num-exp global-env local-env) (cdr exp))))
    (match (car exp)
      ('= (apply equal? args))
      ('> (apply > args))
      ('< (apply < args))
      (_ #f))))


(define (process-expression expression global-env local-env)
  ;(println expression)
  (match expression
    ((cons 'if _) (if (process-bool-exp (cadr expression) global-env local-env)
                      (process-expression (caddr expression) global-env local-env)
                      (process-expression (cadddr expression) global-env local-env)))
    ((cons 'when _) (if (process-bool-exp (cadr expression) global-env local-env)
                      (apply string-append (map (lambda (exp) (process-expression exp global-env local-env)) (cddr expression)))
                      ""))
    (_ (process-function-application expression global-env local-env))))
                                                                                   
(define (execute width height prg expr)
  (let* ((global-env (process-prg prg))
        (content (process-expression expr global-env '())))
    (apply string-append `(,(format "<svg width=\"~a\" height=\"~a\">" width height) ,content "</svg>"))))
