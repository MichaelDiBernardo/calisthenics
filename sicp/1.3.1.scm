; Recursive process to compute the sum of term-func(i) for i = a to b by next.
(define (sum term-func a next-func b)
  (if (> a b)
      0
      (+ (term-func a)
         (sum term-func (next-func a) next-func b))))

; Computes an approximation to pi/8 that improves as b
; gets larger (provides that a = 1).
(define (pi-sum a b)
  (sum (lambda (i) (/ 1.0 (* i (+ 2 i))))
       1
       (lambda (i) (+ i 4))
       b))

; Take the definite integral using the approximation 
; S_a^b f = [ f(a + dx/2) + f(a + dx + dx/2) + f(a + 2dx + dx/2) + ...] dx
; = [ f((a + dx/2) + 0dx) + f((a + dx/2) + 1dx) + f((a + dx/2) + 2dx) + ...]dx
; (let c = (a + dx/2)):
; = [f(c) + f(c + dx) + f(c + 2dx) + f(c + 3dx)]dx
; = sum f(c + idx) where i = 0 to (b - a)/dx, roughly
(define (definite-integral f dx a b)
  (* dx
     (sum f 
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)))

; Disparate helper functions that are fed as inputs into sum, product, etc.
(define (inc x) (+ x 1))

; The identity function x -> x.
(define (id x) x)

; The cube function x -> x^3.
(define (cube x) (* x x x))

; Compute an approximation of the definite integral of x from a to b.
; The larger the n, the more accurate the approximation.
(define (simpsons-rule f a b n)
  (define (coeff k)
    (cond ((or (= k 0) (= k n)) 0)
          ((even? k) 2)
          (else 4)))
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k) (* (coeff k) (y k)))
  (* (/ h 3.0) (sum term 0 inc n)))

; Iterative process to compute the sum of term(i) for i = a to b by next.
(define (isum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; Recursive process to compute the product of term(i) for i = a to b by next.
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

; Iterative process to compute the product of term(i) for i = a to b by next.
(define (iproduct term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

; The factorial function, implemented as a call to 'product'.
(define (fact n)
  (product id 1 inc n))

; Another approximation to pi, as derived from
; pi/4 = 2 * 4 * 4 * 6 * 6 ... / 3 * 3 * 5 * 5 * 7 * 7 ...
;      = (8/9 * 24/25 * 80/81 * ... * (2i + 1)^2 - 1 / (2i + 1)^2.
(define (pi-approx-prod n)
  (define (term k)
    (let ((z (expt (+ (* 2 k) 1) 2)))
      (/ (- z 1) z)))
  (* 4.0 (product term 1 inc n)))

; Folds over the implicit sequence of terms from a to b,
; where term is mapped to each element in the sequence, and the
; transformed elements are combined using 'combiner'. It's like
; generate, map, and reduce all in one!
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

; Same as accumulate but implemented with a recursive process.
(define (iaccumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) 
              (combiner (term a) result))))
  (iter a null-value))

; Sum implemented in terms of accumulate.
(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

; Product implemented in terms of accumulate.
(define (prod-acc term a next b)
  (accumulate * 1 term a next b))

; Same as accumulate except that elements in the sequence from a to b
; that do not pass the unary predicate 'filter' are not combined.
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (let ((evaluated-term (if (filter a) null-value (term a))))
        (combiner evaluated-term
                  (filtered-accumulate filter combiner null-value term (next a) next b)))))


