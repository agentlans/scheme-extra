(define-module (extra control)
  #:use-module (ice-9 match) ; for destructuring in for-each
  #:export (
  dotimes
  for
  do-until
  infinite-loop
  for-each-loop
  ))

;; Control structures not found in standard Scheme

(define-syntax-rule (dotimes (var n) body...)
  (do ((var 0 (+ var 1)))
      ((= var n))
    body...))

(define-syntax-rule (for (var start condition incr-expr) body ...)
  (do ((var start incr-expr))
    ((not condition))
    (begin body ...)))

#| ;; Guile already has this
(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      (begin body ...)
      (loop))))
|#

(define-syntax-rule (do-until condition body ...)
  (let loop ()
    (begin body ...)
    (unless condition
      (loop))))

(define-syntax-rule (infinite-loop body ...)
  (while #t (begin body ...)))

(define-syntax-rule (for-each-loop (pattern lst) body ...)
  (do ((temp lst (cdr temp)))
     ((null? temp))
     (match (car temp)
       (pattern (begin body ...))
       (t (error "Element doesn't match pattern")))))

