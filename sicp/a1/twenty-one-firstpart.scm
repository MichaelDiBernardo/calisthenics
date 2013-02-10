;;; Scheme code for Twenty-One Simulator [PS2 Fall '90]
(define (twenty-one player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal))))
    (let ((player-hand
           (play-hand player-strategy
                      (make-new-hand (deal))
                      (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
          0                                ; ``bust'': player loses
          (let ((house-hand 
                 (play-hand house-strategy
                            house-initial-hand
                            (hand-up-card player-hand))))
            (cond ((> (hand-total house-hand) 21)
                   1)                      ; ``bust'': house loses
                  ((> (hand-total player-hand)
                      (hand-total house-hand))
                   1)                      ; house loses
                  (else 0)))))))           ; player loses

(define (play-hand strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (else my-hand)))                ; stay


(define (deal) (+ 1 (random 10)))

(define (make-new-hand first-card)
  (make-hand first-card first-card))

(define (make-hand up-card total)
  (cons up-card total))

(define (hand-up-card hand)
  (car hand))

(define (hand-total hand)
  (cdr hand))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (+ new-card (hand-total hand))))

(define (hit? your-hand opponent-up-card)
  (newline)
  (print "Opponent up card ")
  (print opponent-up-card)
  (newline)
  (print "Your Total: ")
  (print (hand-total your-hand))
  (newline)
  (print "Hit? ")
  (user-says-y?))

(define (user-says-y?) (eq? (read) 'y))

;;; MY STUFF

; Creates a strategy that will keep asking for cards until the total
; hand meets or exceeds hand-value.
(define (stop-at hand-value)
  (lambda (my-hand opponent-up-card)
    (< (hand-total my-hand) hand-value)))

; Plays a specified number of games of twenty-one using the given strategies,
; and returns the number of games won by the player against the house.
(define (test-strategy player-strategy house-strategy n)
  (define (iter games-won i)
    (if (>= i n)
        games-won
        (iter (+ (twenty-one player-strategy house-strategy) games-won)
              (+ i 1))))
  (iter 0 0))

; Decorates a strategy for a player with name 'player-name' so that information
; about the plays being taken are printed to the screen.
(define (watch-player player-name strategy)
  (define (watch my-hand opponent-up-card)
    (let ((hit-decision (strategy my-hand opponent-up-card)))
          ; Print out what's going on.
          (printf "=== Player ~s plays! ===~n" player-name)
          (printf "   Total (Up): ~v (~v)~n" (hand-up-card my-hand) (hand-total my-hand))
          (printf "   OUC: ~v~n" opponent-up-card)
          (if hit-decision
              (printf "He hits!~n")
              (printf "He stays.~n"))
          ; Return the actual result of the strategy. We don't want to call it
          ; twice in case the result is random.
          hit-decision))
  watch)

; Louis' strategy.
(define (louis my-hand opponent-up-card)
  (let ((my-up-card (hand-up-card my-hand))
        (my-total (hand-total my-hand)))            
    (cond ((> my-total 16) #f)
          ((= my-total 12) (< opponent-up-card 4))
          ((= my-total 16) (not (= opponent-up-card 10)))
          (else (> opponent-up-card 6)))))

; Compound strategy that will hit iff both first-strategy and second-strategy
; would hit in the same situation.
(define (both first-strategy second-strategy)
  (lambda (my-hand opponent-up-card)
    (and (first-strategy my-hand opponent-up-card)
         (second-strategy my-hand opponent-up-card))))