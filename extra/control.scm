(define-module (extra control)
  #:export (dotimes))

;; Control structures not found in standard Scheme

(define-syntax-rule (dotimes (var n) body...)
  (do ((var 0 (+ var 1)))
      ((= var n))
    body...))
