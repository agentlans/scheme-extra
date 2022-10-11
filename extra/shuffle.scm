
(define (vector-swap! vec i j)
  "Swaps vec[i] and vec[j]."
  (let ((temp (vector-ref vec i)))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j temp)
    vec))

(define (random-int i)
  "Returns uniform random integer 0 <= x <= i."
  (random (+ i 1)))

(define (shuffle! vec)
  "Shuffles array in-place using the Fisher-Yates shuffle.
See https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_modern_algorithm"
  (let ((n (vector-length vec)))
    (do ((i (- n 1) (- i 1)))
	((= i 1) vec)
      (let ((j (random-int i)))
	(vector-swap! vec j i)))))
;; (let ((v (vector 0 1 2 3 4))) (shuffle! v))
