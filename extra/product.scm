(define-module (extra product)
  #:use-module (srfi srfi-1)
  #:export (product))

(define (product-0 x lst)
  (map (lambda (y) (cons x y))
       lst))

(define (product-1 lst1 lst2)
  (fold append '()
	(map (lambda (x) (product-0 x lst2))
	     lst1)))
;; (product-1 '(1 2) '(a b))

(define (product . lists)
  "Cartesian product of lists."
  (reverse (fold product-1
		 '(())
		 (reverse lists))))
;; (product '(a b) '(1 2) '(c d))
;=> '((a 1 c) (a 1 d) (a 2 c) (a 2 d) ...)
