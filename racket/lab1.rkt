#lang racket

; Number of digits
(define (num-of-digits n [counted 0])
  (if (= n 0)
      (if (= counted 0) 1 counted)
      (num-of-digits (quotient n 10) (+ counted 1))))


; Repr helper function for num->str functions
(define (repr x)
  (if (> x 9)
      (integer->char (+ (char->integer #\A) (- x 10)))
      (integer->char (+ (char->integer #\0) x))))

; num->str function
(define (num->str n [radix 10])
  (if (= n 0)
      ""
      (string-append  (num->str (quotient n radix) radix) (string (repr (remainder n radix))))))

; num->str function using tail recursion. Works fine for 0 unlike the previous one
(define (num->str_tail n [radix 10] [acc ""])
  (if (= n 0)
      (if (equal? acc "")
          "0"
          acc)
      (num->str_tail (quotient n radix) radix (string-append (string (repr (remainder n radix))) acc))))
  

; Get string of consecutive ASCII characters starting with first and ending with last
(define (consecutive-chars first last)
  (if (equal? first last)
      (string last)
      (string-append (string first) (consecutive-chars (integer->char (+ (char->integer first) 1)) last))))



