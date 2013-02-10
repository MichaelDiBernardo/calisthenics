;;; Various mathematical procedures developed in and used frequently throughout SICP.
;;;
;;; Author: Michael DiBernardo (mikedebo@gmail.com)

;; Take x to the power of 2.
(define (square x)
  (* x x))

;; Compute n! via an iterative process.
(define (fact n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (add1 counter)
                   max-count)))
  (fact-iter 1 1 n))
  
;; Compute the nth Fibonacci number via an iterative process.
(define (fibonacci n)
  (define (fib-iter fn+1 fn count)
    (if (= count 0)
        fn
        (fib-iter (+ fn+1 fn) fn+1 (- count 1))))
  (fib-iter 1 0 n))

;; Does a divide b?
(define (divides? a b)
  (= 0 (remainder b a)))

;; Deterministic algorithm for determining primality of n in O(sqrt(n)) time.
(define (prime? n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  ; Actual implementation of prime?
  (= n (smallest-divisor n)))

;; Probabilistic algorithm for determining primality of n in O(log(n)) time 
;; using Fermat's test. If n fails the test, then it is certainly not prime.
;; However, if n passes the test, it may still not be prime. (The numbers for
;; which this occurs are identified as "Carmichael numbers" in SICP.)
(define (fast-prime? n times)
  ; Compute (b^e) mod m without generating intermediate results much bigger than m.
  (define (expmod b e m)
    (cond ((= e 0) 1)
          ((even? e)
           (remainder (square (expmod b (/ e 2) m))
                      m))
          (else
           (remainder (* b (expmod b (- e 1) m))
                      m))))
  ; Implementation of the Fermat test.
  (define (fermat-test x)
    (define (try-it a)
      (= (expmod a x x) a))
    (try-it (+ 1 (random (- x 1)))))
  ; Implementation of fast-prime?
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (sub1 times)))
        (else false)))

;; Find the fixed point of a function f: R -> R. That
;; is, find an x such that f(x) = x.
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  ; Implementation of fixed-point.
  (try first-guess))
    