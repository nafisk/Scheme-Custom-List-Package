#lang racket

;
; Helper Functions
;

;;; Questions:
;;; 1. ASK IF WE NEED TO HAVE THE ENCODE/DECODE FUNCTION IN THE FINALS SUBMISSION, DO WE SUBMIT OUR TESTING FUNCTIONS?
;;; 2. IS AN EMPTY LIST AN ELEMENT-OF/SUBSET-OF AN EMPTY LIST?
;;; 3. DO WE NEED TO ADD CONDITION TO HANDLE EMPTY SET BELONGING TO EMPTY SET?
;;; 4. How to come up with an invariant if multiple values are changing? 
;;; 5. If a procedure doesn't recurse, but just implemts a recursive function, how to go about with the proofs? 


;; testing: (get-list (myreverse (get-num '(5 2)) ))

;; 1. isPrime()
(define (prime? n)
  (define (prime?-aux n i)
    (cond ((< (sqrt n) i) #t)
          ((zero? (remainder n i)) #f)
          (else
           (prime?-aux n (+ i 1)))))
 (cond ((< n 2) #f)
     (else (prime?-aux n 2)))
 )



;; 2. nthPrime()
(define (nth-Prime? n)
  (define (get-n n iter count rsf)
    (cond ((< n count) rsf)
          ((prime? iter) (get-n n (+ 1 iter) (+ 1 count) iter))
          (else (get-n n (+ 1 iter) count rsf))
    )
  )
  (get-n n 1 0 2)
)



;; 3. swap() - returns a num with two swapped numbers in the list
(define (swap n swap-i with-j)
  (define (swap-aux num i j new-index rsf)
    (cond ((= new-index (len num)) rsf) ;terminating cond
          ((= new-index i) ;swapped exponent with j
                     (swap-aux num i j (+ new-index 1) (* rsf (expt (nth-Prime? new-index) (ref num j)))))
          ((= new-index j) ;swapped exponent with i
                     (swap-aux num i j (+ new-index 1) (* rsf (expt (nth-Prime? new-index) (ref num i)))))
          (else (swap-aux num i j (+ new-index 1) (* rsf (expt (nth-Prime? new-index) (ref num new-index)))))
          ) ; nornally calcs the num
    )
(swap-aux n swap-i with-j 0 1)
)


;;; TESTING FUNCTIONS:

;; 4. get-num() - takes list and created the num / ENCODE
(define (get-num list1)
  (define (aux list1 number-so-far counter)
    (let ((curr-prime (nth-Prime? counter)))
    (cond ((null? list1) number-so-far)
          (else(aux (cdr list1) (* number-so-far (expt curr-prime (car list1))) (+ counter 1))))
    )
  )
 (aux list1 1 0))



;; 5. get-list() - decodes the num to the list / DECODE
(define (get-list n)
  (define (aux n myList)
    (cond ((= n 1) myList)
          (else (aux (tail n) (append myList (list (head n)))))))   
    (aux n '()))



;-----------------------------------------------------------------------------------------------------------;;----------;
;-----------------------------------------------------Lists-------------------------------------------------------------;



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
  (define (head-aux exp)
    (cond ((> (remainder num (expt 2 exp)) 0) (- exp 1))
          (else (head-aux (+ 1 exp))
                )
          )
    )
  ;; 2 is used as the base of the exponent because 2 is the prime for the
  ;; head of the list.
  (head-aux 0)
)



; 3. ref - returns the kth number on the list
(define (ref num k)
    (define (ref-aux num exp)
      (let ((base (nth-Prime? k)))
        (cond ((> (remainder num (expt base exp)) 0) (- exp 1))
          (else (ref-aux num (+ 1 exp))
                )
          )
        )
      )
  ;; head of the list.
  (ref-aux num 0)
)



; 4. tail - returns a number representing the list without the head
(define (tail n)
  ;num representing only the tail end of the old list
  (define tail-num (/ n (expt 2 (head n))) )

  ;index-old keeps track of the old list from the 2nd element
  ;index-new keeps track of the new list from the 1st element
  (define (tail-aux num exp rsf base index-old index-new)
    (cond ((= num 1) rsf)
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

;;Slightely simplified tail:
(define (tail n)
  ;num representing only the tail end of the old list
  (define tail-num (/ n (expt 2 (head n))) )

  ;index-old keeps track of the old list from the 2nd element
  ;index-new keeps track of the new list from the 1st element
  (define (tail-aux num rsf base index-old index-new)
    (let ((exp (ref num index-old)))
    (cond ((= num 1) rsf)
           (else (tail-aux
                 (/ num (expt base exp ) )
                 (* rsf (expt (nth-Prime? index-new) exp))
                 (nth-Prime? (+ index-old 1))
                 (+ index-old 1)
                 (+ index-new 1)
                 ) )
          ))
    )
  (tail-aux tail-num 1 3 1 0)
)

; 5. insert-to-head - returns a num a new value at the beginning
;OLD
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

;; CHECK GI AND DESIGN ROLES 
(define (insert-to-head num val)
  (define (intert-to-head-aux rsf prime-counter index)
    (cond ((= prime-counter (+ (len num) 1)) rsf)
          (else (intert-to-head-aux (* rsf (expt (nth-Prime? prime-counter)
                                                 (ref num index)))
                                    (+ prime-counter 1)
                                    (+ index 1))
                ))
    )
  (intert-to-head-aux (expt 2 val) 1 0)
)



; 6. len - returns the length of the list
;;Original Funciton 
;(define (len n)
;  (define (get-len n length-so-far)
;    (cond ((= n 1) length-so-far)
;          (else (let ((kth (ref n length-so-far))
;                      (currPrime (nth-Prime? length-so-far)))
;                  (get-len (/ n (expt currPrime kth))
;                           (+ 1 length-so-far))))))
;          (else (get-len (tail n)
;                         (+ 1 length-so-far)))))
;  (get-len n 0))

;Audit and testing 
(define (len n)
  (define (get-len n length-so-far)
    (cond ((= n 1) length-so-far)
          (else (get-len (tail n)
                         (+ 1 length-so-far)))))
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
;
; (define (insert-at n x y)
;   ;n represents the list
;   ;x represents the index
;   ;y represents the digit
;   (define (aux-insert-at num index value rsf count-num count-prime length-num)
;       (cond ((= index length-num) (* num (expt (nth-Prime? index) value))) ; add val to end
;             ((= count-num length-num) rsf) ; terminating
;             ((= index count-num) (aux-insert-at num ; adding val to index
;                                                 -1
;                                                 value
;                                                 (* rsf (expt (nth-Prime? count-prime) value))
;                                                 count-num
;                                                 (+ count-prime 1)
;                                                 length-num) 
;             )
;             (else (aux-insert-at num ; adding next num to rsf
;                                  index
;                                  value
;                                  (* rsf (expt (nth-Prime? count-prime) (ref num count-num)))
;                                  (+ count-num 1)
;                                  (+ count-prime 1)
;                                  length-num))
;       )
;   )
;   (aux-insert-at n x y 1 0 0 (len n))
;)


(define (insert-at num val yth-position)
   (define (insert-at-aux rsf old-index curr-index)
     (cond ((= curr-index (+ (len num) 1)) rsf)
           ((= curr-index yth-position) (insert-at-aux (* rsf (expt (nth-Prime? curr-index) val))
                                            old-index
                                            (+ curr-index 1)))
           (else (insert-at-aux (* rsf (expt (nth-Prime? curr-index) (ref num old-index)))
                                            (+ old-index 1)
                                            (+ curr-index 1))
                 )
           )
   )
   (insert-at-aux 1 0 0)
)



; 10. myappend - returns t appended to s. s + t = st
(define (myappend first-num second-num)
  (define (aux-append t rsf insert-at-index t-count added-len)
      (cond ((= insert-at-index added-len) rsf)
            (else (aux-append t
                              (* rsf (expt (nth-Prime? insert-at-index) (ref t t-count)))
                              (+ insert-at-index 1)
                              (+ t-count 1)
                              added-len))
      )
  )
  (aux-append second-num first-num (len first-num) 0 (+ (len first-num) (len second-num)))
)



; 11. myreverse - inputs a number representing a list s and which outputs the number representing the reverse of s
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


; 12. palin? - which inputs a number representing a list s and which determines whether s is a palindrome
(define (palin? n)
  (define (aux not-yet-processed reversed-n)
    (cond ((not (= (head not-yet-processed) (head reversed-n))) #f)
          ((= (tail not-yet-processed) 1) #t)
        (else (aux (tail not-yet-processed) (tail reversed-n)))))
 (aux n (myreverse n)))
;Test: (palin? (get-num (list 2 0 2))) --> #t
;;     (palin? (get-num (list 1 2 5))) --> #f 



;; 13. sort - returns sorted num of the inputed list {Uses helper swap function}
(define (sort num)
  (define (selection-sort i j min-index rsf)

    (cond ((= i (len rsf)) rsf) ; when i reaches the end and ends sort
          ((= j (len rsf)) (selection-sort (+ i 1) (+ i 2) (+ i 1) (swap rsf i min-index)) )
          ; swapping min with i when j reaches end
          ((< (ref rsf j) (ref rsf min-index)) (selection-sort i (+ j 1) j rsf)) ; sets new min if found
          (else (selection-sort i (+ j 1) min-index rsf)) ; increments j if nothing special
          )
    )
  ;; handles lists with only single element
  (cond ((= (len num) 1) num)
        (else (selection-sort 0 1 0 num)
              )
  )
)



;---------------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------Set-------------------------------------------------------------;



; 14. element-of? - returns true if number k is in n
(define (element-of? n k)
  (cond ((and (= n 1) (= k 1)) #t) ; {} belongs to {} ;;;;;;;;;;;;;;;
        ((= n 1) #f)
        ((= (head n) k) #t)
        (else (element-of? (tail n) k))))

;; gi: keep finding the tails until you find k

;;;; ADD CONDITION FOR EMPTY SET?


; 15. subset-of? - returns true if s is in t, else #f
(define (subset-of? s t)
  (define (subset-of?-aux s t s-iter t-iter)
        (cond ((= t-iter (len t)) #f)
              ((= s-iter (len s)) #t)
              ((= (ref s s-iter) (ref t t-iter)) (subset-of?-aux s t (+ s-iter 1) 0))
              (else (subset-of?-aux s t s-iter (+ t-iter 1))
               )
         )
    )
  (subset-of?-aux s t 0 0)
)



; 16. equal-set? - returns true sets are equal from all unordered places
(define (equal-set? set-s set-t)
  (define (equal-set?-aux s t s-iter t-iter)
        (cond ((= t-iter (len t)) #f)
              ((= s-iter (len s)) #t)
              ((= (ref s s-iter) (ref t t-iter)) (equal-set?-aux s t (+ s-iter 1) 0))
              (else (equal-set?-aux s t s-iter (+ t-iter 1))
               )
         )
    )
  (cond ((not (= (len set-s) (len set-t))) #f)
        (else (equal-set?-aux set-s set-t 0 0) ) )
)



; 17. union-of-sets - returns the num of the union of two sets
(define (union-of-sets s t)
  (define (union-aux rsf t-counter prime-counter)
      (cond ((= t-counter (len t)) rsf)
            ((element-of? s (ref t t-counter))
                 (union-aux rsf (+ t-counter 1) prime-counter))
            (else (union-aux (* rsf (expt (nth-Prime? prime-counter) (ref t t-counter)))
                             (+ t-counter 1)
                             (+ prime-counter 1))
             )
       )
  )
  (union-aux s 0 (len s))
)



; 18. intersection-of-sets - returms the intersecting values of set s and t
(define (intersection-of-sets s t)
  (define (intersection rsf s-iter t-iter prime-counter)
      (cond ((= s-iter (len s)) rsf)
            ((= t-iter (len t)) (intersection rsf (+ s-iter 1) 0 prime-counter))
            ((element-of? t (ref s s-iter)) (intersection (* rsf (expt (nth-Prime? prime-counter) (ref s s-iter)))
                                                          (+ s-iter 1)
                                                          (+ t-iter 1)
                                                          (+ prime-counter 1)))
            (else (intersection rsf s-iter (+ t-iter 1) prime-counter)
                  )
       )
  )
  (intersection 1 0 0 0)
)
