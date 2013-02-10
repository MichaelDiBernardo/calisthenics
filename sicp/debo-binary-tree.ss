; An empty branch.
(define empty-branch '())

; Create a tree with an item and the given left and right trees as branches.
(define (create-tree item left-branch right-branch)
  (list item left-branch right-branch))

; Create a leaf with the given item.
(define (create-leaf item)
  (list item empty-branch empty-branch))

; Get the item at the root of this tree.
(define (item tree)
  (car tree))

; Get the left subtree of this tree.
(define (left tree)
  (cadr tree))

; Get the right subtree of this tree.
(define (right tree)
  (caddr tree))

; Return the in-order traversal of the given binary tree.
(define (tree->list tree)
  (if (null? tree) '()
      (append (tree->list (left tree))
              (list (item tree))
              (tree->list (right tree)))))

; Turn the given list into a balanced BST.
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (create-tree this-entry left-tree right-tree)
                      remaining-elts))))))))