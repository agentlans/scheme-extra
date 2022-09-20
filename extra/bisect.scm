(define-module (extra bisect)
  #:export
  (;; Call like (func a x lo hi less-than)
   bisect-right
   bisect-left
   left=
   right=
   find<
   find<=
   find>
   find>=
   ;; Call like (array-func arr x less-than)
   array-left=
   array-right=
   array-<
   array-<=
   array->
   array->=
   ))

;; Bisection algorithms on vectors.
;; Based on https://docs.python.org/3/library/bisect.html

  "Let 
S be a set of objects.
a: a monotone function from non-negative integers -> S
x in S
lo, hi: non-negative integers
less-than: (S, S) -> boolean"

(define (half x)
  (floor (/ x 2)))

(define (bisect-right a x lo hi less-than)
  "Finds non-negative integer i such that
a(j) <= x for all j < i
a(j) > x for all j >= i
where j is a non-negative integer."
  (let loop ((lo lo)
	     (hi hi))
    (cond ((< lo 0) (error "lo must be non-negative"))
	  ((>= lo hi) lo)
	  (else (let ((mid (half (+ lo hi))))
		  (if (less-than x (a mid))
		      (loop lo mid)
		      (loop (+ mid 1) hi)))))))

(define (bisect-left a x lo hi less-than)
  "Finds non-negative integer i such that
a(j) < x for all j < i
a(j) >= x for all j >= i
where j is a non-negative integer."
  (let loop ((lo lo)
	     (hi hi))
    (cond ((< lo 0) (error "lo must be non-negative"))
	  ((>= lo hi) lo)
	  (else (let ((mid (half (+ lo hi))))
		  (if (less-than (a mid) x)
		      (loop (+ mid 1) hi)
		      (loop lo mid)))))))

(define-macro (define-finder name f condit expr)
  `(define (,name a x lo hi less-than)
     (let ((i (,f a x lo hi less-than)))
       (if ,condit ,expr (error "Value error.")))))

(define-syntax def-array-func
  (syntax-rules ()
    ((def-array-func array-func-name func-name)
     (define (array-func-name a x less-than)
       (func-name (lambda (y) (vector-ref a y))
		  x 0 (vector-length a) less-than)))))

(define (!= x y)
  (not (= x y)))

(define (equiv? less-than x y)
  (and (not (less-than x y))
       (not (less-than y x))))

;; Finds the greatest integer i such that a(i) = x.
(define-finder left=
  bisect-left
  (equiv? less-than (a i) x)
  i)

;; Finds the least integer i such that a(i) > x.
(define-finder right=
  bisect-right
  (equiv? less-than (a (- i 1)) x)
  i)

;; left= and right= define a range such that
;; every element in the range is equal to x.

;; Finds the greatest integer i such that
;; a(i) < x or a(i) <= x.
;; Returns a(i) and i.
(define-finder find<
  bisect-left i (values (a (- i 1)) i))

(define-finder find<=
  bisect-right i (values (a (- i 1)) i))

;; Finds the least integer i such that
;; a(i) > x or a(i) >= x.
;; Returns a(i) and i.
(define-finder find>
  bisect-right (!= i hi) (values (a i) i))

(define-finder find>=
  bisect-left (!= i hi) (values (a i) i))

;; Like the find* functions, but for arrays
(def-array-func array-left= left=)
(def-array-func array-right= right=)
(def-array-func array-< find<)
(def-array-func array-<= find<=)
(def-array-func array-> find>)
(def-array-func array->= find>=)

;; (array-right= #(0 1 1 1 1 3 4) 1 <) ;=> 5
