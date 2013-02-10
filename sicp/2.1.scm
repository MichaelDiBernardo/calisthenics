(require (lib "list.ss" "SRFI" "1"))

; Constructors and selectors for rationals.
(define (make-rat n d) 
  (define (both-true predicate a b)
    (and (predicate a)
         (predicate b)))
  (define (both-positive a b)
    (both-true (lambda (x) (>= x 0)) a b))
  (define (both-negative a b)
    (both-true (lambda (x) (< x 0)) a b))
  (cond ((both-positive n d) (cons n d))
        ((both-negative n d) (cons (- n) (- d)))
        ((> n 0) (cons (- n) (- d)))
        (else (cons n d))))

(define (numer r) (car r))

(define (denom r) (cdr r))

; Constructors and selectors for points and segments.
(define (make-point x y) (cons x y))

(define (x-of point) (car point))

(define (y-of point) (cdr point))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment segment) (car segment))

(define (end-segment segment) (cdr segment))

(define (midpoint-segment segment)
  (define (pair-avg a b) (/ (+ a b) 2))
  (let ((seg-start (start-segment segment))
        (seg-end (end-segment segment)))
    (make-point (pair-avg (x-of seg-start) (x-of seg-end))
                (pair-avg (y-of seg-start) (y-of seg-end)))))

; Passed on 2.3, boring.

; Exercise 2.4
; let X = (cons a b) --> (lambda (m) (m a b))
; Then
;   (car X) -> (X (lambda (p q) p)
;           -> ((lambda (m) (m a b)) (lambda (p q) p))
;           -> ((lambda (p q) p) a b) [Call this step S]
;           -> a
; Obviously this implies that (cdr x) should be defined as
; (define (cdr x) (lambda (p q) q)
; since that would leave at step S the following:
;    ((lambda (p q) q) a b) -> b
(define (weird-cons x y)
  (lambda (m) (m x y)))

(define (weird-car z)
  (z (lambda (p q) p)))

(define (weird-cdr z)
  (z (lambda (p q ) q)))

; Exercise 2.5
; Define a pair of integers (a, b) to be represented by the number 2^a * 3^b.
(define (23cons x y)
  (* (expt 2 x) (expt 3 y)))

; To extract a, repeatedly divide by 3 until you have 2^a, then take log base
; 2 to get a. I'm sure there's a better/faster way to do this, but at least it's concise.
(define (23car pair)
  (log-b 2 (repeatedly-divide-by 3 pair)))

; To extract b, repeatedly divide by 2 until you have 3^b, the take log base 3. 
(define (23cdr pair)
  (log-b 3 (repeatedly-divide-by 2 pair)))

(define (repeatedly-divide-by divisor dividend)
  (define (iter current-dividend)
    ; Use the multivalue return quotient/remainder proc to simultaneously get the
    ; quotient and the remainder. 
    (let-values (((quotient remainder) (quotient/remainder current-dividend divisor)))
      (if (= remainder 0)
          (iter quotient)
          current-dividend)))
  (iter dividend))

; I can't believe I couldn't find log for arbitrary bases in the stdlib!
(define (log-b base num)
  (/ (log num) (log base)))

; Pass on exercise 2.6, Church numerals fry the fragile repl in my brain. 

;;; Extended exercise on interval arithmetic.

;; Interval constructor and selectors.
(define (make-interval lower-bound upper-bound) 
  (cons lower-bound upper-bound))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

; Intervals API.

; Add two intervals. The minimum is the sum of the two lower bounds, and the maximum
; is the sum of the two upper bounds.
(define (add-interval first-interval second-interval)
  (make-interval (+ (lower-bound first-interval) (lower-bound second-interval))
                 (+ (upper-bound first-interval) (upper-bound second-interval))))

; Subtract two intervals. The minimum occurs when the upper bound of the second is
; subtracted from the lower bound of the first, and vice-versa.
(define (subtract-interval first-interval second-interval)
  (make-interval (- (lower-bound first-interval) (upper-bound second-interval))
                 (- (upper-bound first-interval) (lower-bound second-interval))))

; Straight from the book. 
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; This one is too. Kinda funky, eh?
(define (div-interval x y)
  ; Exercise 2.10 -- add check for straddling zero.
  (define (straddles-zero interval)
    (and (<= (lower-bound interval) 0)
         (<= 0 (upper-bound interval ))))
  (if (straddles-zero y)
      (error "Divisor interval spans zero.")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

; Pass on Exercise 2.9 out of boredom. No scribblies please.

; Exercise 2.12: Constructor to make an interval based on a center
; and a percentage tolerance.
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percent)
  (let ((width (abs (* (/ percent 100) center))))
    (make-center-width center width)))

(define (percent interval)
  (* (/ (width interval) (center interval)) 100.0))

; The rest is a bunch of boring stuff about floating point error.

; Exercise 2.17: Function that returns last element of a list.
(define (last-pair l)
  (cond ((null? l) null)
        ((null? (cdr l)) (car l))
        (else (last-pair (cdr l)))))

; Exercise 2.18: Reverse a list.
(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l))
              (list (car l)))))

; Pass 2.19, boring.

; Exercise 2.20. At first I misread the question and thought they just wanted a predicate
; that would tell you if all the elems had the same parity. But then I noticed that they
; actually wanted you to return those elements, so I did that too.
(define (same-parity? . params)
  (define (parity-bit n) (remainder n 2))
  (let ((first-elem-parity (parity-bit (car params))))        
    (reduce (lambda (a b) (and a b)) ; 'and' is a special form, not a procedure, so need to wrap it in a lambda.
            #t
            (map (lambda (n) (= (parity-bit n) first-elem-parity))
                 params))))

(define (same-parity . params)
  (define (parity-bit n) (remainder n 2))
  (let ((first-elem-parity (parity-bit (car params))))
    (filter (lambda (n) (= (parity-bit n) first-elem-parity))
            params)))


; Exercise 2.21
(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items))

; Exercise 2.23
; Yes this is stupidly inefficient, but ain't she pretty?
(define (my-for-each proc items)
  (define (ignore expr)
    #t)
  (ignore (map proc items)))

; Exercise 2.27
(define (deep-reverse l)
  (if (null? l)
      null ; base case
      ; If the last-element-to-be is a list, we gotta deep-reverse it too.
      (let* ((last-elem (car l))
             ; Rebind last-elem to be a deep-reversed version if it was a list.
             (last-elem (if (list? last-elem)
                            (deep-reverse last-elem)
                            last-elem)))
        (append (deep-reverse (cdr l))
                (list last-elem)))))
                              
; Exercise 2.28. I think they're asking for a list of the leaves as found by a pre-order traversal.
(define (fringe tree)
  (if (list? tree)
      ; Reduce-right to prevent list reversal.
      (reduce-right append '() (map fringe tree))
      (list tree)))

; Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))

(define (branch-length rod)
  (car rod))

(define (branch-structure rod)
  (cadr rod))

(define (total-weight mobile)
  (if (list? mobile)
      ; Recursive case: Add weights of left and right structures.
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      ; Base case: This is just a weight, so return it.
      mobile))

(define (balanced? mobile)
  ; Compute the torque on a single branch.
  (define (torque branch)
    (* (branch-length branch) (total-weight (branch-structure branch)))) 
  (if (not (list? mobile))
      ; Base case: A single weight must be balanced.
      #t
      ; Recursive case: Torque must be same on left and right branch,
      ; and all submobiles must be balanced.
      (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

; To change the mobile stuff to use pairs instead of lists as underlying implementation, change
; all list? predicates to pair?, and change all cadrs to cdrs. Donesville.

; 2.30: Boring.

; Exercise 2.31:
(define (tree-map proc tree)
  (if (list? tree)
      ; Tree case: Recursively tree-map the proc across all subtrees.
      (map (lambda (subtree) (tree-map proc subtree))
           tree)
      ; Leaf case: Apply the proc to the leaf.
      (proc tree)))