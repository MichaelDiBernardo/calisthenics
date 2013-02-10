; Given a, b, c, returns a single-arg function that computes
; x^3 + ax^2 + bx + c when you supply it with x.
(define (cubic a b c)
  (lambda (x) (+ (expt x 3 )
                 (* a (expt x 2))
                 (* b x)
                 c)))

; Given a function 'proc', returns a function f(x) = proc(proc(x)).
(define (double proc)
  (lambda (x) (proc (proc x))))

; Given single-arg functions f and g, returns a function h(x) = f(g(x)).
(define (compose f g)
  (lambda (x) (f (g x))))

; Given a single-arg function f, returns a function that computes
; f composed with itself n times. So, repeated(x^2, 2) = x^4, for
; example.
(define (repeated f n)
  (define (iter composed n)
    (if (<= n 0)
        composed
        (iter (compose f composed) (- n 1))))
  (iter f (- n 1)))

; Given a single-arg function f(x), returns a function
; that computes the average of f(x - dx), f(x) and f(x + dx)
; for a given x.
(define (smooth f dx)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3.0)))

; Given dx, returns a function that will return a smoothed f
; without having to supply dx. I did it this way because we haven't
; learned how to explicitly curry in scheme yet. In something like ML
; I'd just do something like:
;  let unary-smooth f = smooth ~dx=0.001
(define (unary-smooth dx)
  (lambda (f) (smooth f dx))) 

; Given f, n, and dx, returns a function that will
; will be smoothed n times with dx as the interval. 
(define (nfold-smooth f n dx)
  ((repeated (unary-smooth dx) n) f)) 

; The ubiquitous square, to be used as fodder to these factory functions.
(define (square x) (* x x))

; Given a unary predicate and a unary function, returns a unary function I(x)
; that will iteratively apply 'improve' to the initial guess x until it satisfies
; 'good-enough?'.
(define (iterative-improve good-enough? improve)
  (define (iter current-guess)
      (if (good-enough? current-guess)
          current-guess
          (iter (improve current-guess))))
  (lambda (guess)
    (iter guess)))