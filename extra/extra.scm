;; Copyright 2022 Alan Tseng
;; 
;; This file is part of Extra.
;; 
;; Extra is free software: you can redistribute it and/or modify it under the 
;; terms of the GNU General Public License as published by the Free Software 
;; Foundation, either version 3 of the License, or (at your option) any later 
;; version.
;; 
;; Extra is distributed in the hope that it will be useful, but WITHOUT ANY 
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
;; details.
;; 
;; You should have received a copy of the GNU General Public License along with 
;; Extra. If not, see <https://www.gnu.org/licenses/>.

;; Tested on Guile Scheme but it's portable (only need map, filter, error; no SRFI).

(define-module (extra extra)
  #:export
  (butlast
   lfold
   rfold
   lscan
   rscan
   split-runs
   intersperse
   split-at
   cartesian-product
   remove-from
   in?
   unique
   unique?
   duplicates
   union
   intersection
   difference
   disjoint?
   combinations
   permutations
   power-set
   all?
   any?
   zip
   enumerate
   take
   drop
   take-until
   range
   windows
   chunks
   ))

(define (butlast lst)
  "List containing every element except the last."
  (cond ((null? lst) (error "Need at least one element."))
	((= (length lst) 1) '())
	(else (cons (car lst)
		    (butlast (cdr lst))))))
;; (butlast '(1 2 3))

(define (lfold f init lst)
  "Calls f on init and successive elements of lst."
  (if (null? lst)
      init
      (lfold f (f init (car lst))
	     (cdr lst))))
;; (lfold cons '() '(1 2 3))

(define (rfold f init lst)
  "Like lfold but the elements are drawn from lst starting from the right."
  (lfold f init (reverse lst)))
;; (rfold cons '() '(1 2 3))

(define (lscan lst)
  "Returns successive prefixes of lst."
  (let loop ((in lst)
	     (out '()))
    (if (null? in)
	(reverse (map reverse out))
	(loop (cdr in)
	      (append (list (cons (car in)
				  (if (null? out)
				      '()
				      (car out))))
		      out)))))
;; (lscan '(1 2 3)) ;=> '((1) (1 2) (1 2 3))

(define (rscan lst)
  "Returns successive suffixes of lst."
  (map reverse (lscan (reverse lst))))
;; (rscan '(1 2 3)) '((3) (2 3) (1 2 3))

(define (split-first-run lst =?)
  "Breaks off the first run of identical elements of lst."
  (let loop ((streak '())
	     (the-rest lst))
    (cond ((null? the-rest) (values streak the-rest))
	  ;; Move element from the-rest into streak
	  ((null? streak)
	   (loop (list (car the-rest))
		 (cdr the-rest)))
	  ;; The streak matches the next element
	  ((=? (car streak) (car the-rest))
	   (loop (cons (car the-rest) streak)
		 (cdr the-rest)))
	  ;; Next element doesn't match. Just return.
	  (else (values streak the-rest)))))
;; (split-first-run '(3 3 1 2 2) =) ; (3 3) (1 2 2)

(define (split-runs lst =?)
  "Breaks lst into runs of identical elements."
  (let loop ((in lst)
	     (out '()))
    (if (null? in)
	(reverse out)
	;; Split off the first run then recurse
	(call-with-values
	    (lambda ()
	      (split-first-run in =?))
	  (lambda (head tail)
	    (loop tail (cons head out)))))))
;; (split-runs '(1 1 2 2 3) =) ; ((1 1) (2 2) (3))

(define (distribute f x lst)
  (map (lambda (y) (f x y))
       lst))
;; (distribute list 1 '(a b c))

(define (intersperse lst x)
  "Inserts x between elements of lst."
  (do ((temp lst (cdr temp))
       (prev (car lst) (car temp))
       (out '() (append out
			(if (= (length temp) 1)
			    (list (car temp))
			    ;; Insert separator only when > 1 elements left
			    (list (car temp) x)))))
      ((null? temp) out)))
;; (intersperse '(1 2 3) 'x)

(define (split-at lst divider?)
  "Splits lst into sublists that are separated by divider elements.
divider? is a procedure that returns #t if its argument is a divider element."
  (let loop ((temp lst)
	     (x '())
	     (x-list '()))
    (cond ((null? temp)
	   (reverse (map reverse (cons x x-list))))
	  ;; Move x to x-list
	  ((divider? (car temp))
	   (loop (cdr temp) '()
		 (cons x x-list)))
	  ;; Add element to x but not x-list
	  (else (loop (cdr temp)
		      (cons (car temp) x)
		      x-list)))))
;; (split-at '(a b c 1 2 d 3) number?) ; ((a b c) () (d) ())
;; (split-at '(a b c) number?) ; ((a b c))

(define (prod2 f lst1 lst2)
  (lfold (lambda (acc x)
	   (append acc (distribute f x lst2)))
	 '()
	 lst1))
;; (prod2 list '(1 2 3) '(a b))
;; (prod2 cons '(1 2 3) '(()))

(define (cartesian-product . lists)
  "Returns lists of elements drawn from each list."
  (rfold (lambda (acc x)
	   (prod2 cons x acc))
	 '(())
	 lists))
;; (cartesian-product '(a b) '(1 2) '(c))
;; => '((a 1 c) (a 2 c) (b 1 c) (b 2 c))

(define (remove-from x lst =?)
  "Removes elements that are equal to x from lst."
  (filter (lambda (y)
	    (not (=? x y)))
	  lst))

(define (in? x lst =?)
  "Whether x is an element of lst as compared using =?"
  (let loop ((temp lst))
    (cond ((null? temp) #f)
	  ((=? x (car temp)) #t)
	  (else (loop (cdr temp))))))
;; (in? 3 '(1 2 3 4) =) #t
;; (in? 3 '(1 2) =) #f

(define (unique lst =?)
  "Returns list of the unique elements of lst as compared using =?. O(n^2) time."
  (let loop ((in lst)
	     (out '()))
    (cond ((null? in) (reverse out))
	  (else (loop (remove-from
		       (car in) (cdr in) =?)
		      (cons (car in) out))))))
;; (unique '(1 3 2 3 2 1) =)  ;=> (1 3 2)

(define (unique? lst =?)
  "Returns #t iff every element of lst are unique as compared using =?. O(n^2) time."
  (let loop ((temp lst))
    (cond ((null? temp) #t)
	  ((in? (car temp) (cdr temp) =?) #f)
	  (else (loop (cdr temp))))))
;; (unique? '(1 2 a 2) equal?)

(define (duplicates lst =?)
  "Returns list of duplicate elements in lst. O(n^2) time."
  (let loop ((in lst)
	     (out '()))
    (cond ((null? in) (reverse out))
	  ;; Duplicate
	  ((in? (car in) (cdr in) =?)
	   (loop (cdr in) (cons (car in) out)))
	  ;; Not duplicate
	  (else (loop (cdr in) out)))))
;; (duplicates '(1 2 3 2 1) =) ;=> (1 2)

(define (union-2 lst1 lst2 =?)
  (unique (append lst1 lst2) =?))

(define (union =? . lists)
  "Union of lists where elements are compared by =?"
  (if (null? lists)
      '()
      (lfold (lambda (x y)
	       (union-2 x y =?))
	     (car lists)
	     (cdr lists))))
;; (union = '(1 2) '(4 1 3 2)) ; (1 2 4 3)

(define (intersection-2 lst1 lst2 =?)
  (duplicates (append (unique lst1 =?)
		      (unique lst2 =?))
	      =?))

(define (intersection =? . lists)
  "Returns list of elements that are present in every input list."
  (if (null? lists)
      '()
      (lfold (lambda (x y)
	       (intersection-2 x y =?))
	     (car lists)
	     (cdr lists))))
;; (intersection = '(2 4 6) '(1 2 3 5 7 4) '(0 4))
					; (4)

(define (difference lst1 lst2 =?)
  "Returns list of elements of lst1 that aren't in lst2."
  (lfold (lambda (acc x)
	   (remove-from x acc =?))
	 lst1
	 lst2))
;; (difference '(1 2 3 4 5 6 7) '(2 3 5 7) =)

(define (disjoint-2? lst1 lst2 =?)
  (null? (intersection =? lst1 lst2)))

(define (disjoint? =? . lists)
  "Returns #t iff every pair of lists in lists have no elements in common."
  (do ((temp lists (cdr temp))
       (acc '() (union =? acc (car temp))))
      ((or (null? temp)
	   (not (disjoint-2? acc (car temp) =?)))
       (if (null? temp)
	   #t
	   #f))))
;; (disjoint? =) #t
;; (disjoint? = '(1)) #t
;; (disjoint? equal? '(()) '(())) #f
;; (disjoint? = '(1 2) '(3 4)) #t
;; (disjoint? = '(1 3 5) '(2 4) '(-1 3)) #f

(define (combinations lst r)
  "Returns sublists of lst with length r while preserving order."
  (let ((n (length lst)))
    (cond ((= n 0) '())
	  ((= r 1) (map list lst))
	  ((< n r) (error "List doesn't have enough elements."))
	  ((= n r) (list lst))
	  (else (append (distribute cons (car lst) ; take first element
				    (combinations
				     (cdr lst) (- r 1)))
			;; Don't take the first element
			(combinations (cdr lst) r))))))
;; (combinations '(1 2 3 4) 2)

(define (take-out lst index)
  "Removes lst[index]. Returns that element
and the rest of the list."
  (do ((front '() (cons (car back) front))
       (back lst (cdr back))
       (i 0 (+ i 1)))
      ((= i index)
       (values (car back)
	       (append (reverse front)
		       (cdr back))))))
;; (take-out '(1 2 3 4) 2) ; 3, (1 2 4)

(define (lfold-range f init n)
  (do ((acc init (f acc i))
       (i 0 (+ i 1)))
      ((= i n)
       acc)))
;; (lfold-range cons '() 3)
;; (lfold cons '() '(0 1 2))

(define (permutations lst r)
  "Returns list of sublists of lst with length r,
not preserving order."
  (let ((n (length lst)))
    (cond ((= n 0) '())
	  ((= r 1) (map list lst))
	  ((< n r) (error "List doesn't have enough elements."))
	  (else (lfold-range
		 (lambda (acc i) ; for each i, append
		   (append acc
			   (call-with-values
			       (lambda () (take-out lst i))
			     (lambda (el excl)
			       ;; Take the element at index i
			       ;; Permutate among the remaining elements
			       (distribute cons el
					   (permutations excl (- r 1)))))))
		 '() n)))))
;; (combinations '(a b c) 2) ;; '((a b) (a c) (b c))
;; (permutations '(a b c) 2) ; '((a b) (a c) (b a) (b c) (c a) (c b))

(define (power-set lst)
  "Returns list of subsets of lst."
  (lfold-range (lambda (acc i)
		 (append acc (combinations lst i)))
	       '(())
	       (+ (length lst) 1)))
;; (power-set '(1 2 3))
	; '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))

(define (all? pred lst)
  "Whether pred(x) is true for all x in lst."
  (cond ((null? lst) #t)
	((not (pred (car lst))) #f)
	(else (all? pred (cdr lst)))))
;; (all? even? '(0 2))

(define (any? pred lst)
  "Whether pred(x) is true for any x in lst."
  (cond ((null? lst) #f)
	((pred (car lst)) #t)
	(else (any? pred (cdr lst)))))
;; (any? number? '(a d 3))

(define (zip-2 kons lst1 lst2)
  (do ((l1 lst1 (cdr l1))
       (l2 lst2 (cdr l2))
       (out '() (cons (kons (car l1) (car l2))
		      out)))
      ((or (null? l1) (null? l2))
       (reverse out))))
;; (zip-2 '(1 2 3) '(a b c d))

(define (zip . lists)
  "Takes successive elements from each list in lists.
If the lists have unequal lengths, then the output has
the length of the shortest input list."
  (lfold (lambda (acc x)
	   (zip-2 (lambda (y z)
		    (append y (list z)))
		  acc x))
	 (map list (car lists))
	 (cdr lists)))
;; (zip '(1 2) '(a b) '(c d)) ;=> ((1 a c) (2 b d))

(define (enumerate lst)
  "Returns a list of cons pairs where each pair
consists of a counter and an element of lst."
  (do ((i 0 (+ i 1))
       (temp lst (cdr temp))
       (out '() (cons (cons i (car temp))
		      out)))
      ((null? temp) (reverse out))))
;; (enumerate '(a b c d)) ;=> ((0 . a) (1 . b) (2 . c) (3 . d))

(define (take lst n)
  "Takes the first n elements of lst.
Returns the first n elements and the remainder of the list."
  (do ((i 0 (+ i 1))
       (head '() (cons (car tail) head))
       (tail lst (cdr tail)))
      ((or (= i n) (null? tail))
       (values (reverse head) tail))))
;; (take '(1 2 3 4 5) 3) ;=> (1 2 3), (4 5)

(define (drop lst n)
  "Removes the first n elements of lst.
Returns the remainder of the list and the first n elements."
  (call-with-values
      (lambda ()
	(take lst n))
    (lambda (head tail)
      (values tail head))))

(define (take-until pred lst)
  "Takes elements x of lst until pred(x) is true.
Returns the elements taken and the elements remaining."
  (do ((out '() (cons item out))
       (temp (cdr lst) (cdr temp))
       (item (car lst) (car temp)))
      ((or (null? temp) (pred item))
       (values (reverse out)
	       (cons item temp)))))
;; (take-until number? '(a b c 1 2)) ;=> '(a b c), (1 2)

(define (range n)
  "Returns the list [0..n-1]."
  (do ((i 0 (+ i 1))
       (out '() (cons i out)))
      ((= i n) (reverse out))))
;; (range 3) ;=> '(0 1 2)

(define (windows lst k)
  "Returns list of overlapping k-length windows along lst."
  (let loop ((temp lst)
	     (out '()))
    (if (< (length temp) k)
	(reverse out)
	(loop (cdr temp)
	      (cons (take temp k) out)))))
;; (windows (range 5) 2) ;=> '((0 1) (1 2) (2 3) (3 4))

(define (chunks lst k)
  "Breaks lst into non-overlapping chunks of length k or shorter."
  (let loop ((temp lst)
	     (out '()))
    (if (null? temp)
	(reverse out)
	;; Take the first k values
	(call-with-values
	    (lambda () (take temp k))
	  (lambda (head tail)
	    (loop tail (cons head out)))))))
;; (chunks (range 5) 2) ;=> ((0 1) (2 3) (4))
