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

; 4. tail - returns a number representing the list without the head
(define (tail n)
  ;num representing only the tail end of the old list
  (define tail-num (/ n (expt 2 (head n))) )

  ;index-old keeps track of the old list from the 2nd element
  ;index-new keeps track of the new list from the 1st element
  (define (tail-aux num exp rsf base index-old index-new)
    (cond ((= num 1) (* 1 rsf))
          ((> (remainder num (expt base exp)) 0)
           (tail-aux
                 (/ num (expt base (- exp 1)) )
                 1
                 (* rsf (expt (nth-Prime? index-new) (- exp 1) ))
                 (nth-Prime? (+ index-old 1))
                 (+ index-old 1)
                 (+ index-new 1)
                 ) )
          (else (tail-aux num (+ 1 exp) rsf base index-old index-new))
          )
    )
  (tail-aux tail-num 1 1 3 1 0)
)

;5. insert-to-head - returns a num a new value at the beginning
(define (insert-to-head n p)
  (define (ith-aux num rsf j k)
        (cond ( (= (ref num k) 0) rsf)
              (else (ith-aux num
                             (* rsf (expt (nth-Prime? j) (ref num k)))
                             (+ j 1)
                             (+ k 1)
                    )
              )
        )
    )
  (ith-aux n (expt 2 p) 1 0)
)

;6. len - returns the length of the list
(define (len n)
  (define (get-len n length-so-far)
    (cond ((= n 1) length-so-far)
          (else (let ((kth (ref n length-so-far)) (currPrime (nth-Prime? length-so-far)))
                  (get-len (/ n (expt currPrime kth)) (+ 1 length-so-far))))))

  (get-len n 0))





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
  (get-n n 2 -1 -1)
)









