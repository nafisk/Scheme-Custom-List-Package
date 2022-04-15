#lang racket

;
; Helper Functions
;

;; 1. isPrime()
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

;; 2. nthPrime()
(define (nth-Prime? n)
  (define (get-n n iter count rsf)
    (cond ((= n count) rsf)
          ((prime? iter) (get-n n (+ 1 iter) (+ 1 count) iter))
          (else (get-n n (+ 1 iter) count rsf))
    )
  )
  (get-n n 2 -1 -1) ;; Try and see if you can change the function from 1 based to 0 based index
)

;; 3. get-num() - takes list and created the num
(define (get-num list1)
  (define (aux list1 number-so-far counter)
    (let ((curr-prime (nth-Prime? counter)))
    (cond ((null? list1) number-so-far)
          (else(aux (cdr list1) (* number-so-far (expt curr-prime (car list1))) (+ counter 1))))
    )
  )
 (aux list1 1 0))


;; 4. get-list() - decodes the num to the list
(define (get-list n)
  (define (aux n myList)
    (cond ((= n 1) myList)
          (else (aux (tail n) (append myList (list (head n)))))))   
    (aux n '()))


;; 5. swap() - returns a num with two swapped numbers in the list
(define (swap n swap-i with-j)
  (define (swap-aux num i j new-index rsf)
    (cond ((= new-index (len num)) rsf)
          ((= new-index i)
           (swap-aux num i j (+ new-index 1) (* rsf (expt (nth-Prime? new-index) (ref num j)))))
          ((= new-index j)
           (swap-aux num i j (+ new-index 1) (* rsf (expt (nth-Prime? new-index) (ref num i)))))
          (else (swap-aux num i j (+ new-index 1) (* rsf (expt (nth-Prime? new-index) (ref num new-index)))))
          )
    )
(swap-aux n swap-i with-j 0 1)
)







;; ---------------------------------------------------------------------------------------------------------

;
; Main Funtions
;


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

; 5. insert-to-head - returns a num a new value at the beginning
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

; 6. len - returns the length of the list
(define (len n)
  (define (get-len n length-so-far)
    (cond ((= n 1) length-so-far)
          (else (let ((kth (ref n length-so-far)) (currPrime (nth-Prime? length-so-far)))
                  (get-len (/ n (expt currPrime kth)) (+ 1 length-so-far))))))

  (get-len n 0))


; 7. Snoc- returns a number inserting q at the end of the list
(define (snoc n q)
  (let ((length (len n)))
        (* n (expt (nth-Prime? length) q))))


; 8. last - returns a the rightmost element in list s.
(define (last n)
  (let ((length (len n)))
    (ref n (- length 1))))

; 9. insert-at - inserts the x in yth position
 (define (insert-at n x y)
   ;n represents the list
   ;x represents the index
   ;y represents the digit
   (define (aux-insert-at num index value rsf count-num count-prime length-num)
       (cond ((= index length-num) (* num (expt (nth-Prime? index) value))) ; add val to end
             ((= count-num length-num) rsf) ; terminating
             ((= index count-num) (aux-insert-at num ; adding val to index
                                                 -1
                                                 value
                                                 (* rsf (expt (nth-Prime? count-prime) value))
                                                 count-num
                                                 (+ count-prime 1)
                                                 length-num) 
             )
             (else (aux-insert-at num ; adding next num to rsf
                                  index
                                  value
                                  (* rsf (expt (nth-Prime? count-prime) (ref num count-num)))
                                  (+ count-num 1)
                                  (+ count-prime 1)
                                  length-num))
       )
   )
   (aux-insert-at n x y 1 0 0 (len n))
)


; 10. myappend - returns t appended to s. s + t = st
(define (myappend first-num second-num)
  (define (aux-append t rsf prime-count t-count end)
      (cond ((= prime-count end) rsf)
            (else (aux-append t
                              (* rsf (expt (nth-Prime? prime-count) (ref t t-count)))
                              (+ prime-count 1)
                              (+ t-count 1)
                              end ))
      )
  )
  (aux-append second-num first-num (len first-num) 0 (+ (len first-num) (len second-num)))
)


;11. myreverse - inputs a number representing a list s and which outputs the number representing the reverse of s
(define (myreverse n)
  (define (aux n length number-so-far)
    (let ((head-num (head n)))
    (cond ((= n 1) number-so-far)
          (else (aux (tail n) (- length 1) (* number-so-far (expt (nth-Prime? (- length 1)) head-num)))))))
(aux n (len n) 1))

;;Testing 
;(define temp (list 3 2 1 9))
;(define p (get-num temp))
;(display temp)
;(display "\n")
;(display (get-list (myreverse p)))
;(display "\n")
;(equal? (get-list (myreverse p)) (reverse temp)) ;;Returns true if two lists are equal. 
;--End of testing


;12. palin? - which inputs a number representing a list s and which determines whether s is a palindrome
(define (palin? n)
  (define (aux n reversed-n)
    (cond ((not (= (head n) (head reversed-n))) #f)
          ((= (tail n) 1) #t)
        (else (aux (tail n) (tail reversed-n)))))
 (aux n (myreverse n)))
;Test: (palin? (get-num (list 2 0 2))) --> #t
;;     (palin? (get-num (list 1 2 5))) --> #f 


;; 13. Sort - returns sorted num of the inputed list {Uses helper swap function}




; Ask about mid week office hours
; Ask Professor if bubble sort is allowed
; Ask professor if the proof has to be similar to what we're done so far or
;     do we need to add/remove anything different
; Can we re-use some of the list functions for sets? E.G len() to find the length of a set?
; Ask professor about subsets: (a, b, c), (c, d). Also, ask if we need to deal with empty sets like lists
;


