(load "curves.scm")
(load "utils.scm")
(load "drawing.scm")

; Stuff to make this assignment not suck.
(define (print-point point)
  (printf "(~s, ~s)~n" (x-of point) (y-of point)))

(define pp print-point)

(define (point-math-bin-op op p1 p2)
  (make-point (op (x-of p1) (x-of p2))
              (op (y-of p1) (y-of p2))))

(define (pt- p1 p2)
  (point-math-bin-op - p1 p2))

; Exercise 2: 

; (a) unit-line-at is of type (scheme-num) -> Curve

; (b) Vertical-line
; (c) of type (Point, scheme-num) -> Curve
(define (vertical-line start-point length)
  (lambda (t) 
    (make-point (x-of start-point) 
                (+ (y-of start-point) (* t length)))))

; Exercise 3: of type Curve -> Curve
(define (reflect-through-y-axis curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (- (x-of ct))
                  (y-of ct)))))

; Exercise 4: of type (Curve, Curve) -> Curve.
; Given a curve c1 and a curve c2, this will create
; a continous curve C where C[0..0.5] is a squashed 
; version of c1, and C[0.5..1.0] is a squashed version of c2
; where c2's start point has been translated to coincide with
; c1's endpoint.
(define (connect-ends c1 c2)
  (lambda (t)
    (let* ((c1-end-point (c1 1.0))
           (c2-start-point (c2 0.0))
           (required-translation (pt- c1-end-point c2-start-point))
           (translated-c2 ((translate (x-of required-translation) 
                                      (y-of required-translation))
                           c2)))
      (if (< t (/ 1 2))
          (c1 (* 2 t))
          (translated-c2 (* 2 (- t 0.5)))))))
          