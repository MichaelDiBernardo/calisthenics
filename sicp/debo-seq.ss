;; Procedures that operate on sequences, both flat (lists) and nested (trees).

;; LIST OPS
; In-house implementation of map. Only maps 1 sequence.
(define (dmap proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (dmap proc (cdr items)))))
      
; Same as dmap, except has no return value. Intended for applying
; procs with side-effects against the elements of a list.
(define (dfor-each proc items)
  (define (ignore x)
    '())
  (ignore (dmap proc items)))

; Filter out elements in 'items' that match unary predicate 'pred'.
(define (dfilter pred items)
  (cond ((null? items) '())
        ((pred (car items))
         (cons (car items)
               (dfilter pred (cdr items))))
        (else (dfilter pred (cdr items)))))

; Accumulate items with binary proc 'proc', using 'initial' as a base.
(define (fold-right proc initial items)
  (if (null? items)
      initial
      (proc (car items)
            (fold-right proc initial (cdr items)))))

; Accumulate items with binary proc 'proc', in opposite accumulation order
; of fold-right.
(define (fold-left proc initial items)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (proc result (car rest))
              (cdr rest))))
  (iter initial items))

; Map proc across a sequence, and accumulate results with 'append'. 
; Proc must be of type fun anything -> list. 
(define (flatmap proc items)
  (fold-right append '() (dmap proc items)))

; Given n sequences S1 S2 ... Sn all with length l, produce the sequence 
; ( [accum(op, S1_1, S2_1, ... Sn_1)], [accum(op, S1_2, S2_2, ..., Sn_2)], ...
;   [accum(op, S1_l, S2_l, ... Sn_l])
(define (fold-right-n op init seqs)
  (if (null? (car seqs)) ; If the first sequence is empty, assume all are empty.
      '()
      (cons (fold-right op init (map car seqs))      ; Decapitate each subseq and accum them.
            (fold-right-n op init (map cdr seqs))))) ; Resume the process on the rest of each subseq.

;; TREE OPS
; Enumerate all leaves in a tree.
(define (fringe tree)
  (if (list? tree)
      ; Reduce-right to prevent list reversal.
      (fold-right append '() (dmap fringe tree))
      (list tree)))

; Count the number of leaves in a tree.
(define (count-leaves tree)
  (length (fringe tree)))

; Map a function against all leaves in a tree. Could also be implemented
; as (map proc (fringe tree))?
(define (tree-map proc tree)
  (if (list? tree)
      ; Tree case: Recursively tree-map the proc across all subtrees.
      (map (lambda (subtree) (tree-map proc subtree))
           tree)
      ; Leaf case: Apply the proc to the leaf.
      (proc tree)))

;; GENERATORS

; Enumerate the integer-valued interval [low, high].
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (add1 low) high))))
                  
; Pythonic name for enumerate interval.
(define range enumerate-interval)

; Generate all possible sublists of a list.
(define (sublists s)
  (if (null? s)
      (list '()) ; Empty set is a subset of any set.
      ; Find all the subsets of the given set, minus one element.
      (let ((rest (sublists (cdr s))))
        ; Glue that missing element to each of the resulting subsets.
        (append rest (dmap (lambda (subset)
                            (cons (car s)
                                  subset)) rest)))))

;; COMMON LAMBDAS 

; AND is a special form, so you can't pass it as proc in a function arg.
; This simply wraps it as a binary proc, but note that all args will be evaluated
; (no short-circuiting as in the special form).
(define (andp a b)
  (and a b))