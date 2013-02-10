; A "digitized integer" is to integers what a character sequence is to strings. 
; That is, a dig-int is a sequence of single-digit integers that represents the
; number you would get if you concatenated the digits. You can perform all of
; the operations on dig-ints that you would expect to be able to perform on
; strings -- e.g. a subsequence of a dig-int is itself an int, you can concatenate
; dig-ints, and so on.
;
; Internally, the digitized-int is actually stored as a string, and conversion to
; integers is done as needed.
;
; Author: Michael DiBernardo (mikedebo@gmail.com)

;; Create a new digitized integer. Note that the integer? predicate isn't strong
;; enough to ensure that a number can be digitized: it must have no decimal point.
;; For example, (integer? 47.0) is true, but 47.0 is not valid input for this
;; constructor ( (floor 47.0) would be, though).
(define (digitize-integer num) 
  (number->string num))

;; Take a "substring" of the digits as an integer. For example,
;; (digit-sequence (digitize-integer 123) 0 2) is the integer 12.
(define digit-subseq substring)

;; How many digits does this integer have?
(define number-of-digits string-length)

;; Glue two dig-ints together.
(define concat-digits string-append)

;; Reverse the digits in the dig-int e.g. 246 becomes 642.
(define (reverse-digits dig-int)
  (list->string (reverse (string->list dig-int))))

;; Coerces a digitized integer back into a vanilla integer.
(define integerize string->number)

;; Add an integer to a digitized number. Could take a HOF to apply any func but
;; we don't need that much flexibility right now.
(define (add-to-dig-int dig-int increment)
  (digitize-integer (+ (integerize dig-int) increment)))

;; Check for equality.
(define digits-equal? string=?)