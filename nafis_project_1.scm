#lang racket

;; Main Funtions


; 1. myEqual? - returns #t if two lists are equal, else #f
(define (myEqual? n m)
  (cond ((= n m) #t)
        (else #f)
  )
)

; 2. head - returns the value of the head of the list given
;(define (head-aux n counter remain)
;  (cond ((not (= remain 0)) counter)
;        ((> (quotient n (expt 2 counter)) 0)
;            ((head-aux n (+ counter 1) (remainder n (expt 2 counter)))
;        )
;  )
;)
;  )









;; Helper Functions

;; isPrime()
(define (prime? n)
  (define (F n i) "helper"
    (cond ((< n (* i i)) #t)
          ((zero? (remainder n i)) #f)
          (else
           (F n (+ i 1)))))
 "primality test"
 (cond ((< n 2) #f)
     (else
      (F n 2))))

;; nthPrime()
(define (nth-Prime? n)
  (define (get-n n iter count rsf)
    (cond ((= n count) rsf)
          ((prime? iter) (get-n n (+ 1 iter) (+ 1 count) iter))
          (else (get-n n (+ 1 iter) count rsf))
    )
  )
  (get-n n 2 0 -1)
)









