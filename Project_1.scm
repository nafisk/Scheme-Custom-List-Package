#lang racket

;; The City College of New York
;; Computer Science Department
;; CSC 33500 – Programming Language Paradigms
;; Professor Douglas Troeger
;; Project 1 - Scheme Custom List Package
;; by
;; Nafis Khan, email: nafisrizwank@gmail.com
;; Deepankar Chakraborty, email: dchakra001@citymail.cuny.edu


;; Introduction:
; The Scheme Custom List Package consists of a different way of handling lists and sets. Instead of using list
; primitives such as car, cdr, etc. it uses a different rule of keeping track of all the elements. All the elements
; in a list are represented as a positive interger number by taking the product of the power of primes where the
; powers are the elements of the lists or sets themselves. The index of the elements in the list and the index of
; the first prime both start at zero index.
;
; This project contains all completed procedures from all the sections in the prompt, e.g. lists, sets and mixed lists,
; and everything is laid out accordingly to the structure of the prompt. Additionally, the requirement says to work
; with only positive integers but the functions that are written in this package can also handle lists with elements
; that are the number 0.
;
; Regarding the handling of nested lists and mixed lists, although we have not written any specific functions for it,
; we have given our reasoning behind our thought process and how the implementation track could be if tried.



;-----------------------------------------------------------------------------------------------------------------------;
;--------------------------------------------------Helper Functions-----------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------;


;; 1. prime? - returns if an interger is prime or not

; Pre-condition: Given a non-negative integer, 
; Post-condition: Return #t if the input integer is a prime number 
;
; Design Idea: 
; A number is a prime number if and only if it has exactly two factors: 1, and the number itself. Therefore, in order to
; check if a number is prime, one needs to show that the number has no more than two factors. A naive way of solving this
; problem would be to iterate through all numbers starting from 2 to n-1 and check for every number if it divides n with
; a remainder =0. If we find any number other than 1 and n, that divides n, we can return false. 
; However, an efficient approach to solve this problem would be to iterate from 2 to √n. This is because any non-prime
; number can be factored in as two numbers x and y. Suppose q is a non-prime number, q= x * y. However, x and y both can’t
; be greater than the square root of q, √q, because, then the product of x*y would be greater than n. So, in any factorization
; of q, at least one of the factors will be smaller than the square root of q, √q. And if we reach the floor value of the √q,
; and don’t find any factors less than or equal to √q, it can be said that q must be a prime. In our procedure, we used the
; 2nd technique to determine the primality of a number. 
; Let’s consider a number n, 
; Possible divisors:   2 3.......|..... sqrt(n) 
;                     Already 
;                   Processed    | Not-yet-processed
;           
; GI:
; Let's consider a variable i, where i is, 2<= i <= already-processed-numbers. Our guess invariant is that, n is a prime
; number, for an integer i, 2<= i <= already-processed-numbers, if we have (n remainder i) is not equal (!=) to 0. Additionally,
; n not a prime if, there exists a number i 2<= i <already-processed-numbers, that results in (n remainder i) == 0.
; 
;
; Weak Enough?: 
; By definition, numbers, less than 2 are not prime. So with the help of a wrapper function, we returned #f for any inputs
; less than 2, or if n<2 = #f.  
; We start by setting the value of the i > 1, because a number can’t be divided by 0, and all numbers are divisible by 1. So, i=2, at the initial call. 
;
; Strong Enough?:
; Our function can terminate in two ways: 
; Our stopping condition is when i reaches the value equal to sqrt(n). At that point, our gI becomes, for integers, 2<=i<= sqrt(n), we have n reminder i is not equal to (!=) to 0. But clearly, at this point, we have checked all the possible divisors for number n, and so n is a prime number, thus returns true. 
; Secondly, if for an i, 2<= i <= already-processed-numbers, the result of (n reminder i) equals to 0, then the function correctly returns false, because n is not a prime number. 
;
; Preservable?:
; In order to preserve the gI and proceed towards termination, we increment the value of i by 1. i → i+1. 

(define (prime? n)
  (define (prime?-aux n i)
    (cond ((< (sqrt n) i) #t)
          ((zero? (remainder n i)) #f)
          (else
           (prime?-aux n (+ i 1)))))
 (cond ((< n 2) #f)
     (else (prime?-aux n 2)))
 )

; Tests:


;-----------------------------------------------------------------------------------------------------------------------;


;; 2. nth-Prime? - returns the nth prime starting from 2

; Pre-condition: Given a non-negative integer n, n>=0, 
; Post-condition: Returns the prime number located at the nth position.  
;
; Design Idea: The design idea for this procedure is to increase a counter from 0 to nth index (step of 1) and maintain a variable
; rsf, so that when ever we encounter a prime number we increment the counter and update the rsf to the most recent encountered
; prime number. We will use a variable called, iter, to check if it is a prime, and increment counter by 1 and hold its value
; in rsf if iter is a prime. 
;
; GI: The guess invariant is that, for an integer, count >= 0, rsf will hold the prime number value of the count index.
; Rsf = (prime number of count index)
;
; Weak Enough?: At the beginning, we initialized count as 0, and rsf as 2, because the 0th index prime number is 2. 
;
; Strong Enough?: The function terminates when the count is equal to n, count = n. At that point, the rsf will hold the prime
; number of the count index, which is equal to the nth index. So essentially, at termination, the rsf will indeed hold the value
; of the nth prime number.
;
; Preservable?: The variable iter is the number that we are checking for primality. If iter is a prime number, we increment counter
; and iter by 1 and hold the value of iter in our rsf.  
;
; In this procedure, we used our custom function (prime?) to determine if a number is prime. The precondition for the function prime
; is given a non-negative integer, it returns if the input is a prime. We pass the value of iter in the prime? function, and since
; iter is an integer and always greater than 1 in the (nth-Prime?) function, the precondition of the prime? function holds when we
; called the function to check for primality. 


(define (nth-Prime? n)
  (define (get-n n iter count rsf)
    (cond ((< n count) rsf)
          ((prime? iter) (get-n n (+ 1 iter) (+ 1 count) iter))
          (else (get-n n (+ 1 iter) count rsf))
    )
  )
  (get-n n 1 0 2)
)

; Tests:


;-----------------------------------------------------------------------------------------------------------------------;


;; 3. swap - returns a num with two swapped numbers in the list

; Pre-condition: Given a number num representing a list, swap-i and swap-j representing the element in the ith and jth
; index of that list
; Post-condition: Returns a number with the element in the ith and jth position swapped
;
; Design Idea: According to the rules to create a list, a number that represents a list is created using the product
; of the power of primes. In the iterative process of this helper function, it uses this concept for all elements, except
; for manipulating indexes in two places. To swap elements in i and j indexes, and make a list from it, we need the jth
; element as the exponent in the ith prime and ith element as the exponent of the jth prime. This is done while transferring
; all elements from the old list to a new list. This results in the construction of a list where the ith and jth elements
; are swapped and all other elements remain the same returning a num that represents the new list.
;
; GI: From the design idea, we can say that rsf is the product of the power of the primes with element in index i and j flipped
;
; Weak Enough?: For the initial case, if a number representing a list has a length of 1, then the swap function will automatically
; just return the rsf, or the number itself. In all the other cases, the index of the i starts from the first element and the
; index of j starts from the second element therefore holding the GI and proving that the function is weak enough.
;
; Strong Enough?: The terminating case of this swap procedure is very simple. As the i and jth elements are swapped, and the
; procedure continues adding elements to the rsf. When the procedure reaches the last element in the given num n representing
; the list, in other words when the index of the new list has reached the length of the old list the procedure terminates
; due to no more elements being available and returns the rsf.
;
; Preservable?: The GI is preserved by adding elements from the old list to the new list using the product of the power of
; the primes rules that we have been using both for when switching the elements in i and j and when not.

(define (swap n swap-i with-j)
  (define (swap-aux num i j new-index rsf)
    (cond ((= new-index (len num)) rsf)
          ((= new-index i) ;swapped exponent with j
                     (swap-aux num i j (+ new-index 1) (* rsf (expt (nth-Prime? new-index) (ref num j)))))
          ((= new-index j) ;swapped exponent with i
                     (swap-aux num i j (+ new-index 1) (* rsf (expt (nth-Prime? new-index) (ref num i)))))
          (else (swap-aux num i j (+ new-index 1) (* rsf (expt (nth-Prime? new-index) (ref num new-index)))))
          )
    )
(swap-aux n swap-i with-j 0 1)
)

; Tests: 


;-----------------------------------------------------------------------------------------------------------------------;
;-------------------------------------------------Testing Functions-----------------------------------------------------;



; No Proofs are provided for the testing functions but the constructure of them have been considered and tested
; to be able to test using them for all the other functions.

;; 4. get-num() - takes list and created the num / ENCODE
(define (get-num list1)
  (define (aux list1 number-so-far counter)
    (let ((curr-prime (nth-Prime? counter)))
    (cond ((null? list1) number-so-far)
          (else(aux (cdr list1) (* number-so-far (expt curr-prime (car list1))) (+ counter 1))))
    )
  )
 (aux list1 1 0))


;-----------------------------------------------------------------------------------------------------------------------;


;; 5. get-list() - decodes the num to the list / DECODE
(define (get-list n)
  (define (aux n myList)
    (cond ((= n 1) myList)
          (else (aux (tail n) (append myList (list (head n)))))))   
    (aux n '()))


;-----------------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------Lists-------------------------------------------------------------;


;; 1. myEqual? - returns #t if two lists are equal, else #f

; Pre-condition: Given two integer numbers, n representing a list s and m representing a list t, where n and m are >=1. 
; Post-condition: Checks whether s and t are the same list, and returns #t, only if both are the same list. 
;
; Since we are using the product of the power of the prime numbers to represent our lists with a single integer number,
; every list will have a unique prime number representation. Hence, two different lists can not have the same product of
; the prime number representing them. So, two lists will be the same if and only if the representing numbers n and m are
; the same. Otherwise, the lists are different. In our function, we are returning true, if m and n are equal(=), meaning
; that the two lists are the same, otherwise false, meaning that the lists are different.

(define (myEqual? n m)
  (cond ((= n m) #t)
        (else #f)
  )
)

; Tests:
; (myEqual? (get-num (list 1 4 3)) (get-num (list 8 2 9))) 
; Correctly returns #f 

; (myEqual? (get-num (list 1 4 3)) (get-num (list 1 4 3))) 
; Correctly returns #t 


;-----------------------------------------------------------------------------------------------------------------------;


;; 2. head - returns the value of the head of the list given

; Pre-condition: Given a number n, which represents a list, s. 
; Post-condition: Returns the number which is at the 0th position or at the head of the list. 
;
; Design Idea: We have represented our list as an integer number, n, calculated by taking the exponent of kth index prime
; with the kth index on the list, and multiplying them for all k indexes in list, s. So, in order to get the first element
; from the num, n, we need to find the highest power of the first prime, 2, that will divide our number, n, evenly, without
; a reminder, in other word the floor value of m = floor(log(base 2) 288), and m will be the first element in our list. In
; our function, we will have a variable exp, and divide n with an incrementing power of 2 starting at exp=0, until we find
; the highest power, in other words until the 2^(exp) no longer divides n evenly, thus leaving a remainder > 0. 
;
; Guess Invariant: So, the guess invariant can be that: the variable exp, while making it an exponent of the 0th prime, 2,
; or 2^(exp), it divides the number representing the list, num, evenly for x, 0<= exp<=x. X is a counter from 0, that evenly
; divides the list num by 2^x or num/2^x.  
;
; Weak Enough?: At the start of the iterative call, the exp is 0. It is always true that taking 0 as an exponent of our base,
; 2, will divide the number, n, evenly. 
;
; Strong Enough?: Our stopping condition is when the reminder, which is found by dividing the number, num, by the 2^(exp)
; greater than 0, OR (remainder (num && (2^exp) >  0). At this point, since the remainder is greater than 0, means we have
; already found the Highest exponent that evenly divides our number, num. So, our exp becomes, 0<=exp-1 <= head element of
; list s. And the highest exponent is the previous value of the exp, since dividing the num with the current value of exp
; will not result in a reminder equal to 0, thus giving us the correct answer.  
;
; Preservable?: If for a value of an exp, the remainder is equal to 0 and our gI holds true, then we need to check for the
; next exp value for which if our invariant holds true. Thus, to make it preservable, we need to increment the value of exp
; for the next call.

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

; Audit and Testing (use both lines): 
; (define list1 (list 5 3 2))
; (= (car list1) (head (get-num list1))) 
; Here, we can use the list primitive car for testing. (car list1) will return the head of the list or 5, and we are cheking
; if our own head function indeed return the head, of the list of, 5. 
; After running we see that the test returns true, which means our function head indeed works. 


;-----------------------------------------------------------------------------------------------------------------------;


; 3. ref - returns the kth number on the list

; Pre-condition: Given a number representing a list s, and an non-negative integer index value k, 
; Post-Condition: Returns the number in the kth position of s. 
;
; Stronger pre-condition: Although k can be any positive integer value k, the function wouldn’t make sense if someone wants
; to access an element which is outside the range of the indices of the list. So, k has to be between 0 and (length of list s - 1),
; 0 <= k <= (length-1)
;
; Design Idea: This function follows a similar pattern to our previous function head. The difference in this function is that,
; in order to find the element in the kth position of the list, we need to find the highest power of the kth prime that will
; divide our number, n, evenly without a reminder. In our function, we will maintain a variable exp, and divide n with an
; incrementing power of kth prime starting at exp=0, until we find the highest power of the kth index prime number, in other
; words, until (kth prime)^(exp) no longer divides n evenly, thus leaving a reminder >0.
;
; GI:  So, the guess invariant can be that: the variable exp, while making it an exponent of the kth prime, k-prime, divides
; the number representing the list, n, evenly for x, 0<= exp<=x. Where X is a counter from 0, that evenly divides the list num
; by (kth prime)^x or num/(kth-prime)^x
;
; Weak Enough?: At the start of the iterative call, the exp is 0. It is always true that, taking 0 as an exponent of our base,
; 2, will divide the number, n, evenly. 
;
; Strong Enough?: Our stopping condition is when the reminder, which is found by dividing the number, num, by the (kth prime)^(exp)
; greater than 0, OR (remainder of (num && kth-prime^exp) >  0). At this point, since the reminder is greater than 0, means we
; have already found the Highest exponent that evenly divides our number, num. So, our exp becomes, 0<=exp-1<= (kth element
; list s). And the highest exponent is the previous value of the exp, since dividing the num with the previous value of exp will
; not result in a reminder equal to 0, thus giving us the correct answer.  
;
; Preservable?:  If for a value of an exp, the reminder is equal to 0 and our gI holds true, then we need to check for the next
; exp value for which if our invariant holds true. Thus, to make it preservable, we need to increment the value of exp for the
; next call, exp → exp +1. 

(define (ref num k)
    (define (ref-aux num exp)
      (let ((base (nth-Prime? k)))
        (cond ((> (remainder num (expt base exp)) 0) (- exp 1))
          (else (ref-aux num (+ 1 exp))
                )
          )
        )
      )
  ; head of the list.
  (ref-aux num 0)
)

; Audit and Testing (use both lines): 
; Case 1: 
; (ref (get-num (list 1 4 5)) 1) 
; Corrrectly returns the 1st index, 4. 

; Case 2: 
; (ref (get-num (list 44 6 6)) 0)
; Correctly returns the first index, 44. 


;-----------------------------------------------------------------------------------------------------------------------;


; 4. tail - returns a number representing the list without the head

; Pre-condition: Given a positive integer number n which represents a list. 
; Post-condition: Returns a number representing the list without the head or initial element.
;
; Design Idea: The design idea of this iterative process consists of handling two lists to get the tail. The first list is given
; to us as the parameter num and the second is the one that we create to store the tail with an index starting from 0. Initially,
; the value of the head is removed from the tail by dividing num with 2^head. We can get the head of the list using our custom
; function (head). After removing the head, this gives us the num that represents the tail in the old list. But to get a num
; that represents the tail only where the elements in tail start from the index 0, the elements in the old list tail are stored
; in a new list where the index of the tail now starts from 0 instead of 1, or simply put, shifted one unit to the left. This
; continues on with all the rest of the elements in the tail which in tern returns us a value that represents a list containing
; only the elements in the tail of the original num.
;
; Guess Invariant: Let us consider a int, i, which represents the index of our tail list, and it is between 0<= i< old-index in
; the original index. Our guess invariant, rsf, will be the number representing the new tail list, where, i is the index in the
; original list.
;
; Weak Enough?: At the start of the function, we removed the head of our original list, bt dividing it by 2^(head element). 
; Strong Enough?: The function terminates when the number representing the tail of the original list reaches 1, or becomes an
; empty list because we removed one element at a time from the head of the old-list, and inserted it onto our rsf. At this point,
; our gI becomes, rsf = product, Π of ( prime at new index)^ (element at old-index), where the old-index ranges from 1 (not 0
; head) to the index of the last element in the original list. That means the rsf does indeed represent a list indexed at 0,
; obtained from s by removing its first element, and keeping the rest of the elements. 
;
; Preservable?: In order to preserve, rsf, we obtaine the new-index prime number and raise the power to the old-index element from
; our original list, and multiply it with the rsf.

; Audit and Testing:

; Initial Test: 
; (define myList '(3 6 5))
; (define tailList (get-list (tail (get-num myList))))
; (equal? (cdr myList) tailList) 
; Returns contract violation, 
; Mainly because the gI for the variable exp wan’t preserved. 
; Auditing our initial function shows that, our gI doesn’t remain preserved after the initial call. Because, the value of exp,
; doesn’t represent the correct element from the list that should get added to the tail list. 

;(define (tail n)
;  ;num representing only the tail end of the old list
;  (define tail-num (/ n (expt 2 (head n))) )
;
;  ;index-old keeps track of the old list from the 2nd element
;  ;index-new keeps track of the new list from the 1st element
;  (define (tail-aux num exp rsf base index-old index-new)
;    (cond ((= num 1) rsf)
;          ((> (remainder num (expt base exp)) 0)
;           (tail-aux
;                 (/ num (expt base exp))
;                 1
;                 (* rsf (expt (nth-Prime? index-new)  exp))
;                 (nth-Prime? (+ index-old 1))
;                 (+ index-old 1)
;                 (+ index-new 1)
;                 ))
;          (else (tail-aux num (+ 1 exp) rsf base index-old index-new))))
;  (tail-aux tail-num 1 1 3 1 0))


; An effective way to solve this problem would be to use our custom function ref to get the element from our old list, and use
; it in the computation. The corrected code can be used seen below:

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

; Case 1: 
; Our tail function is equivalent to the primitive list function, cdr. So, we can test the outcome of out tail function against the result of the cdr function with the function equal? To see if it returns the right answer.  

; (define myList '(3 6 5))
; (define tailList (get-list (tail (get-num myList))))
; (equal? (cdr myList) tailList) 
; --> Returns #t, 

; Case 2: 
; (define myList2 '(0 65 56))
; (define tailList (get-list (tail (get-num myList2))))
; (equal? (cdr myList2) tailList) 
; Returns #t 



;-----------------------------------------------------------------------------------------------------------------------;



; 5. insert-to-head - returns a num a new value at the beginning

; Pre-cond: Given a number, n, representing a list, s, and a second positive integer p,
; Post-cond: Returns the number representing the list obtained by inserting p at the head of the list s. 
;
; Design Idea: We are representing a list as a number, n, which holds the product of powers of prime numbers. Additionally,
; the kth index prime is raised to the kth index number from the elements in the given num representing the list and multiplied
; with the (k+1)th index prime raised to the (k+1)th index list number. In order to adhere to this rule, if we want to add a
; number at the front of a list, we need to make sure that the kth index prime is indeed raised to the kth index element from
; the list. Hence, when we add a number at the beginning, or at the 0th index, we can start by taking the power of 0th index
; prime with our input integer p, 2^p, and then the rest of the numbers on the list needs to be shifted to the right to preserve
; the identity of the list representation as to the product of powers of prime numbers. We keep track of the number of elements
; being added into the rsf in a variable called prime-counter, and our function terminates when we iterate through all the
; elements in list s, and insert them into the rsf, or when prime counter reaches length of our list,s + 1. 
;
; Guess Invariant: The guess invariant, rsf, is the product of powers of prime numbers, for a list with first element as p,
; and subsequently, the element i, where i is an element in the list s, and 0th index element <=i  < (length-1) index element
; of list s. 
; Rsf = ( 2^(p) * product of Π (k+1th indexed prime)^ kth index element in list,s), where k is 0 <=k  < (prime-count) 
;
;
; Weak Enough?: We can start by setting,  rsf, a number representing the list starting at the 2^(p), where p is the number to
; be inserted at the head, and prime-counter to 1, since the first number has already been added to the rsf. 
;
; Strong Enough?:
; Our termination argument is when prime-counter reacher length of list s + 1. At this point the prime-counter becomes length
; of list s + 1. 
; At this point gI becomes, rsf = 2^p * product of, Π((kth indexed prime)^ kth index element in list,s), where k is
; 0 <= k < (length of list s -1), which means our rsf indeed returns the number representing the list obtained by inserting p
; at the head of the list s. 
;
; Preservable?: In order to keep the gI true, we increment the prime counter 1, when we insert a new value from the list to rsf. 
; And we also multiply rsf to rsf= rsf * (current index+1 prime number)^ current-index element in the list.


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

; Testing:
; We can utilize the list primitive cons function to check the correctness of our program.
; (define myList (list 3 4 5))
; (equal? (cons 9 myList) (get-list (insert-to-head (get-num myList) 9)))
; The list after inserting, (9 3 4 5) 
; --> returns #t  


;-----------------------------------------------------------------------------------------------------------------------;



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
  (define (selection-sort sorted-index j min-index rsf)

    (cond ((= sorted-index (len rsf)) rsf) ; when i reaches the end and ends sort
          ((= j (len rsf)) (selection-sort (+ sorted-index 1) (+ sorted-index 2) (+ sorted-index 1) (swap rsf sorted-index min-index)) )
          ; swapping min with i when j reaches end
          ((< (ref rsf j) (ref rsf min-index)) (selection-sort sorted-index (+ j 1) j rsf)) ; sets new min if found
          (else (selection-sort sorted-index (+ j 1) min-index rsf)) ; increments j if nothing special
          )
    )
  ;; handles lists with only single element
  (cond ((= (len num) 1) num)
        (else (selection-sort 0 1 0 num)
              )
  )
)



;---------------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------Sets------------------------------------------------------------;



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
