#lang racket

;; Main Funtions


; 1. myEqual? - returns #t if two lists are equal, else #f
(define (myEqual? n m)
  (cond ((= n m) #t)
        (else #f)
  )
)

; 2. head - returns the value of the head of the list given
(define (head num)
  (define (head-aux num exp)
    (cond ((> (remainder num (expt 2 exp)) 0) (- exp 1))
          (else (head-aux num (+ 1 exp))
                )
          )
    )
  ;; 2 is used as the base of the exponent because 2 is the prime for the
  ;; head of the list.
  (head-aux num 1)
)

; 3. ref - returns the kth number on the list
(define (ref num k)
  (define base (nth-Prime? k))
    (define (ref-aux num exp)
      (cond ((> (remainder num (expt base exp)) 0) (- exp 1))
          (else (ref-aux num (+ 1 exp))
                )
          )
      )
    
  ;; 2 is used as the base of the exponent because 2 is the prime for the
  ;; head of the list.
  (ref-aux num 1)
)









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









