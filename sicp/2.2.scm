(load "debo-seq.ss")
(load "debo-math.ss")

; Exercise 2.32 - Generate all subsets of a given set (represented a list).
(define (subsets s)
  (if (null? s)
      (list '()) ; Empty set is a subset of any set.
      ; Find all the subsets of the given set, minus one element.
      (let ((rest (subsets (cdr s)))) 
        ; Glue that missing element to each of the resulting subsets.
        (append rest (dmap (lambda (subset)
                            (cons (car s)
                                  subset)) rest)))))

; Exercise 2.33

(define (map-via-accum p seq)
  (fold-right (lambda (x y) (cons (p x) y)) '() seq))

(define (length-via-accum seq)
  (fold-right (lambda (element length-of-rest)
                (+ 1 length-of-rest)) 0 seq))

; Exercise 2.34
; Given a list of coeffecients (a b c d ..), evaluate the polynomial a + bx + cx^2 ... 
; at x using Horner's rule.
(define (horner-eval x coeff-seq)
  (fold-right (lambda (this-coeff higher-terms-sum) 
                (+ (* higher-terms-sum x) this-coeff))
              0
              coeff-seq))

; Exercise 2.35
(define (count-leaves-via-accum t)
  (fold-right sum 
              0
              (map (lambda (tree)
                     (if (atom? tree)
                         1
                         (count-leaves tree))))))

; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs)) ; If the first sequence is empty, assume all are empty.
      '()
      (cons (fold-right op init (map car seqs))      ; Decapitate each subseq and accum them.
            (accumulate-n op init (map cdr seqs))))) ; Resume the process on the rest of each subseq.
                     
; Exercise 2.37
(define (dot-product v w)
  ; This uses the "superduper" version of map.
  (fold-right + 0 (map * v w)))

; Multiply an MxN matrix against a length-N vector (A*v, NOT v*A).
(define (matrix-*-vector A v)
  (map (lambda (row)
         (dot-product row v)) A))

; Given A, compute A^T.
(define (transpose A)
  (accumulate-n cons '() A))

; Compute the matrix product AB. A must have dimensions MxN and B must have dimensions NxC.
(define (matrix-*-matrix A B)
  (let ((cols (transpose B)))
    ; This map operates on 'A'
    (map (lambda (row)
           ; This map operates on 'cols'
           (map (lambda (col)
                  (dot-product row col)) 
                cols)) 
         A)))

; Exercise 2.38
; (fold-left op l) = (fold-right op l) <=> (op x y) = (op y x) forall x, y in l.

; Exercise 2.39
(define (reverse-via-fr sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-via-fl sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

; Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             ; Given an i, create the
             (map (lambda (j) (list i j)) (enumerate-interval i n)))
           (enumerate-interval 1 n)))

; Exercise 2.41
; Find all i, j, k <= n such that (i + j + k) = s. 
(define (ordered-triples-of-sum n s)
  ; Procedure that takes a set of subsets as input, and furnishes those
  ; subsets that have cardinality 3.
  (define (subsets-of-size n set)
    (dfilter (lambda (subset)
               (= n (length subset)))
             set))
  ; Find all triples that sum to S by generating the power set of the
  ; enumerated interval [1..n], applying the 'subsets-of-size' filter
  ; to pull out the triples, and then applying a sum filter to pull out
  ; those triples that sum to S.
  (dfilter (lambda (triple)
             (= s (apply + triple)))
           (subsets-of-size 3 (subsets (enumerate-interval 1 n)))))

; Exercise 2.42.
(define (queens board-size)
  ; The empty board.
  (define empty-board '())
  ; Adjoin a (row, col) position to a list of board configurations.
  (define (adjoin-position row col rest-of-queens)
    (cons (list row col) rest-of-queens))
  ; Check if the queen in the first position of the configuration is 'safe'
  ; wrt to all other queens.
  (define (safe? positions)
    ; Procedure to determine if one position is threatened by another.
    (define (threatened? p1 p2)
      (let ((xdiff (abs (- (car p1) (car p2))))
            (ydiff (abs (- (cadr p1) (cadr p2)))))
        (or (= xdiff 0)
            (= ydiff 0)
            (= xdiff ydiff))))
    ; Make sure all positions are not threatened wrt to the first.
    (let ((new-queen (car positions)))
      (fold-right andp 
                  #t 
                  (dmap (lambda (other-pos)
                          (not (threatened? new-queen other-pos)))
                        (cdr positions)))))
  ; Actual implementation of n-queens problem.
  (define (queen-cols k)
    ; If we're not placing any queens...
    (if (= k 0)
        ; ... the only solution is an empty board.
        (list empty-board)
        ; Otherwise, take only the board configurations that have all 
        ; queens in a safe position from...
        (dfilter (lambda (positions) (safe? positions))
                 ; ... this list of configurations:
                 (flatmap
                  ; Given a list of safe configurations of size k-1, create a list
                  ; of all possible configurations of size k that can be created by
                  ; adding a single queen in a new column added to the safe configuration.
                  (lambda (rest-of-queens)
                    (dmap (lambda (new-row)
                            ; Adjoin the position r, k for all possible rows in the board.
                            (adjoin-position new-row k rest-of-queens))
                          (enumerate-interval 1 board-size)))
                  (queen-cols (- k 1))))))
  (queen-cols board-size))

; Exercise 2.54
(define (254equal? a b)
  (cond ((and (symbol? a) (symbol? b)) 
         (eq? a b))
        ((and (list? a) (list? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        (else #f)))

; Exercise 2.55
; (car ''junk) -> (car (quote (quate junk))) -> (car '(quote junk)) -> quote
; SCHEME RULEZ

; Exercise 2.56. This stuff is all commented out to allow for the arbitrary-number-of-terms
; implementation in 2.57.
;(define (variable? expr)
;  (symbol? expr))
;
;(define (same-variable? x y)
;  (eq? x y))
;
;(define (is-op-expr? op-symbol expr)
;  (and (pair? expr) (eq? (car expr) op-symbol)))
;
;(define (make-op-expr op-symbol first second)
;  (list op-symbol first second))
;
;; Checks if expr is (1) a number and (2) equal to num. False otherwise.
;(define (=number? expr num)
;  (and (number? expr) (= expr num)))
;
;(define (make-sum addend augend)
;  ; See if we can simplify the addend and augend to a literal expression.
;  (cond ((=number? addend 0) augend)
;        ((=number? augend 0) addend)
;        ((and (number? addend) (number? augend) (+ addend augend)))
;        (else (make-op-expr '+ addend augend))))
;
;(define (sum? expr)
;  (is-op-expr? '+ expr))
;
;(define (addend expr)
;  (cadr expr))
;
;(define (augend expr)
;  (caddr expr))
;
;(define (make-product multiplier multiplicand)
;  (cond ((or (=number? multiplier 0) (=number? multiplicand 0)) 0)
;        ((=number? multiplier 1) multiplicand)
;        ((=number? multiplicand 1) multiplier)
;        ((and (number? multiplier) (number? multiplicand)) (* multiplier multiplicand))
;        (else (make-op-expr '* multiplier multiplicand))))
;
;(define (product? expr)
;  (is-op-expr? '* expr))
;
;(define (multiplier expr)
;  (cadr expr))
;
;(define (multiplicand expr)
;  (caddr expr))
;
;(define (make-exponentiation base exponent)
;  (cond ((=number? exponent 0) 1)
;        ((=number? exponent 1) base)
;        ((=number? base 1) 1)
;        ((=number? base 0) 0)
;        ((and (number? base) (number? exponent)) (expt base exponent))
;        (else (make-op-expr '** base exponent))))
;
;(define (exponentiation? expr)
;  (is-op-expr? '** expr))
;
;(define (base expn)
;  (cadr expn))
;
;(define (exponent expn)
;  (caddr expn))
;
;(define (deriv expr var)
;  (cond ((number? expr) 0)
;        ((variable? expr)
;         (if (same-variable? expr var) 1 0))
;        ((sum? expr)
;         (make-sum (deriv (addend expr) var)
;                   (deriv (augend expr) var)))
;        ((product? expr)
;         (make-sum
;          (make-product (multiplier expr)
;                        (deriv (multiplicand expr) var))
;          (make-product (deriv (multiplier expr) var)
;                        (multiplicand expr))))
;        ((exponentiation? expr)
;         (make-product
;          (make-product
;           (exponent expr)
;           (make-exponentiation (base expr) 
;                                (make-sum (exponent expr) -1)))
;          (deriv (base expr) var)))
;        (else
;         (error "Unknown expression type -- DERIV" expr))))



; Exercise 2.57.
(define (variable? expr)
  (symbol? expr))

(define (same-variable? x y)
  (eq? x y))

(define (is-op-expr? op-symbol expr)
  (and (pair? expr) (eq? (car expr) op-symbol)))

(define (has-an-operator? expr)
  (memq (car expr) '(+ * **)))

; This function deals with the possibility that 'second' is itself a
; multi-element expression.
(define (make-op-expr op-symbol first second)
  (cond ((null? second) first) 
        ; If second is a list, it could just be a list of operands, or it could
        ; be an operator expression.
        ((pair? second) 
         (if (has-an-operator? second)
             ; If it's an operator expression, you can use it as-is in the new expression.
             (list op-symbol first second)
             ; If it's not, then it's just a list of operands that need to be in this
             ; new expression.
             (append (list op-symbol first) second)))
        ; If second is an atom, just make a binary operator expression. Ugly
        ; replication :S
        (else (list op-symbol first second))))

; Checks if expr is (1) a number and (2) equal to num. False otherwise.
(define (=number? expr num)
  (and (number? expr) (= expr num)))

(define (make-sum addend augend)
  ; See if we can simplify the addend and augend to a literal expression.
  (cond ((=number? addend 0) augend)
        ((=number? augend 0) addend)
        ((and (number? addend) (number? augend) (+ addend augend)))
        (else (make-op-expr '+ addend augend))))

(define (sum? expr)
  (is-op-expr? '+ expr))

(define (addend expr)
  (cadr expr))

(define (augend expr)
  (make-sum (caddr expr) (cdddr expr)))

(define (make-product multiplier multiplicand)
  (cond ((or (=number? multiplier 0) (=number? multiplicand 0)) 0)
        ((=number? multiplier 1) multiplicand)
        ((=number? multiplicand 1) multiplier)
        ((and (number? multiplier) (number? multiplicand)) (* multiplier multiplicand))
        (else (make-op-expr '* multiplier multiplicand))))

(define (product? expr)
  (is-op-expr? '* expr))

(define (multiplier expr)
  (cadr expr))

(define (multiplicand expr)
  (make-product (caddr expr) (cdddr expr)))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 1) 1)
        ((=number? base 0) 0)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (make-op-expr '** base exponent))))

(define (exponentiation? expr)
  (is-op-expr? '** expr))

(define (base expn)
  (cadr expn))

(define (exponent expn)
  (cddr expn))

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
          (make-product (multiplier expr)
                        (deriv (multiplicand expr) var))
          (make-product (deriv (multiplier expr) var)
                        (multiplicand expr))))
        ((exponentiation? expr)
         (make-product
          (make-product
           (exponent expr)
           (make-exponentiation (base expr) 
                                (make-sum (exponent expr) -1)))
          (deriv (base expr) var)))
        (else
         (error "Unknown expression type -- DERIV" expr))))

