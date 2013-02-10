(load "debo-seq.ss")
(load "debo-math.ss")
(load "debo-binary-tree.ss")

; Unordered sets.
(define empty-set '())

(define (uo-element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (uo-element-of-set? x (cdr set)))))

(define (uo-adjoin-set x set)
  (if (uo-element-of-set? x set)
      set
      (cons x set)))

(define (uo-intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) empty-set)
        ((uo-element-of-set? (car s2) s1) (cons (car s2)
                                            (uo-intersection-set s1 (cdr s2))))
        (else (uo-intersection-set s1 (cdr s2)))))

; Exercise 2.59.
(define (uo-union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((uo-element-of-set? (car s2) s1) (uo-union-set s1 (cdr s2)))
        (else (cons (car s2) (uo-union-set s1 (cdr s2))))))

; Ordered sets.
(define (o-element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (o-element-of-set? x (cdr set)))))

(define (o-intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (o-intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (o-intersection-set (cdr set1) set2))
              ((< x2 x1)
               (o-intersection-set set1 (cdr set2)))))))

; Exercise 2.60.
(define (o-adjoin-set x set)
  ; Adjoining any element to the null set is just that element.
  (if (null? set) (list x)
      ; Otherwise, inspect the first element of the set.
      (let ((first-elem (car set)))
        ; If the element to insert (x) is already in the set, we're done.
        (cond ((= x first-elem) set)
              ; If x should be inserted after the first element, glue the
              ; first element to the result of sticking x in the rest of the set.
              ((> x first-elem) (cons first-elem (o-adjoin-set x (cdr set))))
              ; Otherwise, x should be the first element of this set. 
              (else (cons x set))))))

; Exercise 2.61.
(define (o-union-set s1 s2)
  ; If either set is null, just glue them together so we don't have to figure out
  ; _which_ one is null.
  (if (or (null? s1) (null? s2))
      (append s1 s2)
      ; Let x be the smallest elem in s1 and y be the smallest elem in s2.
      (let ((x (car s1))
            (y (car s2)))
        ; If they're the same, glue x to the union of the rest of s1 and the rest of s2.
        (cond ((= x y) (cons x (o-union-set (cdr s1) (cdr s2))))
              ; If x is less than y, x should definitely be in the union. So glue it to
              ; the union of the rest of s1 and all of s2.
              ((< x y) (cons x (o-union-set (cdr s1) s2)))
              ; Otherwise we're in the opposite situation. The solution is symmetric.
              (else (cons y (o-union-set s1 (cdr s2))))))))

; BST-sets. 
(define (bst-element-of-set? x set)
  (if (null? set) #f
      (let ((cur-elem (item set)))
        (cond ((< x cur-elem) (bst-element-of-set? x (left set)))
              ((> x cur-elem) (bst-element-of-set? x (right set)))
              (else #t)))))
        
(define (bst-adjoin-set x set)
  (if (null? set)
      (create-leaf x)
      (let ((cur-elem (item set)))
        (cond ((= x cur-elem) set)
              ((< x cur-elem)
               (create-tree cur-elem (bst-adjoin-set x (left set)) (right set)))
              (else 
               (create-tree cur-elem (left set) (bst-adjoin-set x (right set))))))))

(define (bst-union-set s1 s2)
  (list->tree 
   (o-union-set (tree->list s1) (tree->list s2))))

(define (bst-intersection-set s1 s2)
  (list->tree 
   (o-intersection-set (tree->list s1) (tree->list s2))))

;; Huffman trees.

; Leaves
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (h-adjoin-set x set)
   (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (h-adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (h-adjoin-set (make-leaf (car pair)    ; symbol
                                 (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
  
; Exercise 2.68
(define (encode message tree)
  (define (encode-symbol sym tree)
    ; Encode-symbol-rec actually implements the traversal.
    (define (encode-symbol-rec tree bits-accum)
      (define (is-leaf-with-symbol tree)
        (and (leaf? tree) (eq? sym (symbol-leaf tree))))
      (cond ((is-leaf-with-symbol (left-branch tree)) 
             (reverse (cons 0 bits-accum)))
            ((is-leaf-with-symbol (right-branch tree)) 
             (reverse (cons 1 bits-accum)))
            ((memq sym (symbols (left-branch tree))) 
             (encode-symbol-rec (left-branch tree) (cons 0 bits-accum)))
            ((memq sym (symbols (right-branch tree)))
             (encode-symbol-rec (right-branch tree) (cons 1 bits-accum)))
            (else (error "Bad symbol -- ENCODE MESSAGE"))))
    
    ; Implementation of encode-symbol using encode-symbol-rec.
    (encode-symbol-rec tree '()))    
  
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; Exercise 2.69
;(define (generate-huffman-tree pairs)
;  (define (successive-merge leaf-set)
;    )
;  (successive-merge (make-leaf-set pairs)))