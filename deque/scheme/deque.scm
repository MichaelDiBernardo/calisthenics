;;; Deque public interface
; Deques are currently implemented as a pair of lists. The first list represents 
; the 'front elements' of the deque, and is in the expected order. The second list
; represents the 'rear elements' of the deque, and is in reverse order. Thus, 
; taking the head of the queue simply returns the head of the first list, and 
; taking the tail of the queue returns the head of the second list.
;
; This works fine as long as there are elements in both lists. For any deque of
; length >= 2, an invariant is maintained that requires the front and rear list to 
; each have at least one element. If this invariant is violated by a pop-head or
; pop-tail, the invariant is restored by taking half of the elements in the 'full'
; list and moving them to the 'empty' list. For deques of length 1, the element
; in question can be in either list.

;; Create an empty deque.
(define (make-deque)
  ; See implementation details below for information on how deques are implemented.
  (cons (list) (list)))

;; Create a deque from a list.
(define (list->deque l)
  ; Split the list into equal parts and make each part into a chunk of the deque.
  (let ((splitlist (_balance-lists (list) l)))
    (cons (first splitlist) (reverse (rest splitlist)))))

;; Create a list from a deque.
(define (deque->list d)
  (append (_front d) (reverse (_rear d))))

;; Get the length of the given deque. 
(define (deque-length deque)
  (+ (length (_front deque)) (length (_rear deque))))

;; Add an element to the head of the deque.
(define (push-head deque element)
  (_restore-invariant
   (let ((new-front (cons element (_front deque))))
    (cons new-front (_rear deque)))))

;; Add an element to the tail of the deque.
(define (push-tail deque element)
  (_restore-invariant
   (let ((new-rear (cons element (_rear deque))))
    (cons (_front deque) new-rear))))

;; Return a pair of (the head of the queue, and the queue minus its head).
(define (pop-and-return-head deque)
  (cons (head deque) (pop-head deque)))

;; Return the queue minus its head.
(define (pop-head deque)
  (cond ((= (deque-length deque) 0) raise 'DequeEmpty)
        ((= (deque-length deque) 1) (make-deque))
        (else (_restore-invariant (cons (rest (_front deque)) (_rear deque))))))

;; Return a pair of (the tail of the queue, and the queue minus its tail).
(define (pop-and-return-tail deque)
  (cons (tail deque) (pop-tail deque)))

;; Return the queue minus its tail.
(define (pop-tail deque)
  (cond ((= (deque-length deque) 0) raise 'DequeEmpty)
        ((= (deque-length deque) 1) (make-deque))
        (else (_restore-invariant (cons (_front deque) (rest (_rear deque)))))))

;; Get the head of the deque
(define (head deque)
  (if (_invariant-base-case? deque)
      (_get-single-element deque)
      (first (_front deque))))
  
;; Get the tail of the deque
(define (tail deque)
  (if (_invariant-base-case? deque)
      (_get-single-element deque)
      (first (_rear deque))))

;;; Deque private helper methods. 
;; Gets the elements that make up the "front part" of the deque.
(define _front first)

; Gets the elements that make up the "rear part" of the deque, in reverse order.
(define _rear rest)

; Is the deque in the 'base case' of the invariant? (i.e. length is 0 or 1).
(define (_invariant-base-case? deque)
  (<= (length deque) 1))

; Shift elements from 'tail' into 'head' until they're roughly the same length, then
; return a pair of the balanced (head, tail).
(define (_balance-lists head tail)
    (if (>= (length head) (length tail))
        (cons (reverse head) tail)
        (_balance-lists (cons (car tail) head) (cdr tail)))) 

; Restores the invariant 
(define (_restore-invariant deque)       
  (cond ((< (deque-length deque) 2) deque)
        ((= 0 (length (_front deque)))
        ; The rear queue is in reverse order, so reverse it before shifting elements to front..
            (let ((balanced (_balance-lists (list) (reverse (_rear deque)))))
              (cons (first balanced) (reverse (rest balanced)))))
        ; The front queue is in proper order, but we need to add to rear in reverse order.
        ((= 0 (length (_rear deque)))
            (let ((balanced (_balance-lists (list) (reverse (_front deque)))))
              (cons (reverse (rest balanced)) (first balanced))))
        (else deque)))

; Get the single element in the deque when it is in its base case, if one exists.
; If not, raise DequeEmpty. If the deque is not in the invariant base case, an
; assertion error is raised.
(define (_get-single-element deque)
  (cond ((= (deque-length deque) 0) raise 'DequeEmpty)
        ((= (deque-length deque) 1) 
         (if (= (length (_front deque)) 1)
             (first (_front deque))
             (first (_rear deque))))
        (else (raise 'PreconditionViolated))))
  