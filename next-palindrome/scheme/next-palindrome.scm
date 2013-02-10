(load "digitized-integer.scm")
;; Finds the next palindrome greater than or equal to 'num'. 'num' must be a 
;; nonnegative integer.
;; 
;; This algorithm doesn't make a heck of a lot of sense unless you read the
;; proof. See URL for details. But, basically, for some integer 'num', we
;; segment 'num' into digits: num = d_n . d_(n-1) ... d_1. We then partition
;; 'num' into 3 integers formed by 'substrings' of its digits:
;; - 'middle-digit' is the digit in the center of the number if 'num' is odd, 
;;   or nil otherwise.
;; - 'left-digits' is the most significant digit to the floor (n/2)th digit.
;; - 'left-and-middle-digits' is the left-digits with the middle digit 
;;   concatenated to its end.
;; - 'right-digits' is the ceil (n/2)th digit to the end of 'num'.
;; Now, you have to take it on faith that:
;; - if the integer represented by left-and-middle-digits concat 
;;   reverse(left-digits) is bigger than 'num', it's the next biggest palindrome.
;; - otherwise, if you increment the integer represented by left-and-middle-digits
;;   by one and concat it to the reverse of left-digits (possibly adding one in the
;;   the case where n is even), that is the next greatest palindrome.
;;
;; Author: Michael DiBernardo (mikedebo@gmail.com).
(define (next-palindrome num)
  (if (not (integer? num))
      (raise 'InvalidArgumentException)
      (let* (
             (digits (digitize-integer num))
             (n (number-of-digits digits))
             (left-digits (digit-subseq digits 0 (floor (/ n 2))))
             (left-and-middle-digits (digit-subseq digits 0 (ceiling (/ n 2))))
             (right-digits (digit-subseq digits (+ (floor (/ n 2)) 1) n))
             (potential-next-palindrome (integerize (concat-digits left-and-middle-digits (reverse-digits left-digits)))))
        (cond
          ((digits-equal? left-digits right-digits) num)
          ((> potential-next-palindrome num) potential-next-palindrome)
          (else 
           (let ((bigger-left-and-middle (add-to-dig-int left-and-middle-digits 1)))
             (if (even? n)
                 (integerize (concat-digits bigger-left-and-middle (reverse-digits bigger-left-and-middle)))
                 ; Add one to left-and-middle, and then pull out all but the last digit in the sum and
                 ; reverse-and-append that because there may have been a carry.
                 (integerize 
                  (concat-digits bigger-left-and-middle 
                                 (reverse-digits 
                                  (digit-subseq bigger-left-and-middle 0 (floor (/ n 2)))))))))))))