# Scheme-Custom-List-Package

[Google Document](https://docs.google.com/document/d/19h2Z1moOObMAWEGFSmHPZzvxmRgOOb_M6cIg8BmYzPU/edit?usp=sharing)

## Explanation: 

* First:

  Indexing of prime numbers:
  You are all familiar with the prime numbers.  Let's index them starting at 0: thus 2 is the 0th prime, 3 is the first prime,
  and so on.

  | Prime | Index | 
  | ------ | ------ | 
  | 2 | 0 | 
  | 3 | 1 | 
  | 5 | 2 | 
  ...

* Second:

  Indexing of lists: 
  We follow Scheme and use 0-based indexing for lists.  Thus, in the list (1 2 3), the 0th element is 1, the 1st element is 2,
  and the 2nd element is 3.
  
  | List | Index | 
  | ------ | ------ | 
  | 1 | 0 | 
  | 2 | 1 | 
  | 3 | 2 | 
  ...

* Third

  Lists represented as numbers:
  * The empty list is represented by 1

  * If j is the kth element of a nonempty list s, and if p is the kth prime number, s (... j ...), k = index of j, and index of prime number p
  then (1) p^j = (expt p j) is a factor of (num s), and (2) no higher power of p is a factor of (num s).

  Let's have a few examples

  if s is the list (5), (num s) would be the number 2^5 = 32
  if s is (5 2), (num s) would be (2^5) * (3^2) = 32 * 9 = 288
  if s is (5 2 8), (num s) = 288 * (5^8) = 112500000
  if s is (5 2 8 2), (num s) = 112500000 * (7^2) = 5512500000

  As the last example makes clear, lists are not sets, even if one ignores the ordering of a list's elements:
  lists can have multiple occurrences of the same element.

  One can decode (num s) to answer questions about s.  For example, if s is (5 2 8 2), we can use (num s) to see that
  the index 2 element of s is 8 by computing that the highest power of the 2nd prime (namely, 5) which divides (num s)
  is 8.  




## MAIN REQUIREMENTS:



Build a package for working with lists of positive numbers represented this way.  At a minimum,
your package must include the following [NAMED AS I HAVE WRITTEN], along with several helper functions:

- [ ] a function **myequal?** which inputs numbers n representing a list s and m representing a list t, and which checks whether s and
  t are the same list (and returns #t  only if s and t are the same lists)

- [ ]  a function **head** which inputs a number n which represents a list s and which returns the number in the
  first position of s, that is, the head of s

- [ ]  more generally, a function **ref** which inputs a number n representing a list s and which returns the number
  in the kth position of s

- [ ] a function **tail** which inputs a number n which represents a list s and which returns the number representing the tail
  of s, that is, the list obtained from s by removing its first element

- [ ] a function **insert-at-head** which inputs a number n representing a list s and a second number p, and which returns the number
  representing the list obtained by inserting p at the head of the list s 

- [ ] a function **len** which inputs a number n which represents a list s and which returns the number of elements of s

- [ ] a function **snoc** which inputs a number n which represents a list s and a second number q, and which returns the number
  representing the list obtained by inserting q at the end of the list s

- [ ] a function **last** which inputs a number n which represents a non-empty list s and which returns the rightmost element of s

- [ ] a function **insert-at** which inputs a number n representing a list s, a second number x, and a third number y and which returns
  the number representing the list obtained by inserting x in the yth position of s.  You will need preconditions
  to ensure that the number y makes sense as a position in s

- [ ] a function **myappend** which inputs numbers m and n representing lists s and t, respectively, and which returns the number
  representing the list formed by appending s and t

- [ ] a function **myreverse** which inputs a number representing a list s and which outputs the number representing the reverse
  of s

- [ ] a function **palin?** which inputs a number representing a list s and which determines whether s is a palindrome

- [ ] a function **sort** which inputs a number representing a list s and which outputs the number representing the list
  formed by sorting (smallest to largest) the elements of s

--- 

- Can this method be used to represent lists of lists of positive integers, such as ((1) (2 3) (3 1 5))?
If so, show with a developed scheme program, how you would do it.  If not, explain why, in detail.

- Can this method be used to represent lists which contain _both_ positive integers and lists of positive integers?
If so, explain briefly how you would do it.  If not, explain why, in detail.

- Note that you are not to make use in any of your functions of scheme's functions for manipulating lists!!! Not cons, car, cdr, list, null?, append ...

- But as we learn about lists, you might wish to use Scheme's list primitives to develop functions for testing
the functions you write above.

- You may use any of the numerical primitives supplied by scheme.

- Beyond this, you may use, in your submitted project, ONLY those parts of scheme which have been discussed in class --
note that though we have mentioned list and cons, we have not yet discussed them in any depth: again, do not use
these functions in your submitted project.

---

* Fourth

  Sets represented as numbers: 
  Give a similar way to use numbers to represent finite sets of positive integers.    

  Use your representation to build a package for manipulation of such sets by defining:

  - [ ] a function element-of? which inputs a number n representing a set s and a number p, and which checks whether p is an element of s

  - [ ] a function subset-of? which inputs two numbers m and n representing sets s and t, respectively, and which determines
    whether s is a subset of t

  - [ ] a function equal-sets? which inputs two numbers m and n representing sets s and t, respectively, and which determines
    whether s and t are equal 

  - [ ] a function which inputs numbers m and n representing two sets and which returns the number representing the union
    of the input sets

  - [ ] a function which inputs numbers m and n representing two sets and which returns the number representing the intersection
    of the input sets

--- 

- Again, you are NOT to make use, in your submitted project, of scheme's functions for manipulating lists, or of any feature of scheme not discussed so far in class (except the numerical primitives, any of which may be used, whether discussed in class or not).

- Proofs, at an appropriate level of detail, are expected.  We will talk about how the document is to be organized in class, but you should view it much as you would view a term paper.

- The general idea of what I want has come to be known (after D. Knuth) as Literate Programming. (Look it up!)
