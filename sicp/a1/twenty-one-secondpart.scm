;;; Scheme code for Twenty-One Simulator [PS2 Fall '90]
;;; Modified to accomodate card and card-set datatypes, and a finite-size deck.
(require (lib "list.ss" "SRFI" "1"))
; (require srfi/1) for PLT v4

;;; Data declarations

;;  Card. Cards are suitless for the purposes of 21.
; Create a card with the given face-value.
(define (make-card face-value)
  face-value)

; Get the face-value of the card.
(define (card-value card)
  card)

;; Card-set. 
; Create an empty card-set.
(define (make-card-set)
  '())

; Create a card set from a list of values.
(define (make-card-set-from-list values)
  values)

; Count how many cards are in a set. 
(define (num-cards-in-set set)
  (length set))

; Glue two decks together.
(define (merge-decks left-deck right-deck)
  (append left-deck right-deck))

; Split a deck into two decks at the given index. This function returns
; two values.
(define (cut-deck-at deck index)
  (split-at deck index))

; The empty deck.
(define empty-deck '())

; Generate a blackjack deck, where the cards 1..10 have values
; 1..10, and all the face cards have value 10 as well.
(define (make-blackjack-deck) 
  ; We iterate over the range 0..52, and any cards over the index of 40
  ; are face cards.
  (define (face-card-seed? seed) (>= seed 40))
  ; We stop at 52 cards.
  (define (stop? seed) (>= seed 52))
  ; Any card in the first 40 gets mapped to the index divided by 4. The rest
  ; are face cards, and they just get 10.
  (define (map-seed-to-card seed) 
    (if (face-card-seed? seed)
        10        
        (+ (quotient seed 4) 1)))
  ; Call the higher-order generator.
  (make-card-set-from-list (unfold stop?
                                   map-seed-to-card
                                   add1
                                   0)))

; Shuffle a deck by cutting it in half, and then alternately taking a random number
; (between 1 and 5 inclusive) of cards from first the left pile and then the right.
(define (shuffle-deck deck)
  (define (shuffle-impl shuffled pile-to-take-from other-pile)
    (let ((number-to-take (+ 1 (random 5))))
      ; If we're trying to take more cards than are available...
      (if (>= number-to-take (num-cards-in-set pile-to-take-from))
          ; Glue all the remaining cards onto the shuffled deck.
          (merge-decks (merge-decks shuffled pile-to-take-from) other-pile)
          ; Otherwise cut the pile to take from in two, with the left side of the deck having the number
          ; of cards we want to put into the shuffled deck.
          (let-values (((left-of-split right-of-split) (cut-deck-at pile-to-take-from number-to-take)))
            ; Now glue the taken cards onto the shuffled deck, and iterate on the _other_ pile.
            (shuffle-impl (merge-decks shuffled left-of-split) other-pile right-of-split)))))
  ; Split the deck evenly in half and call the shuffle implementation.
  (let-values (((left-half-of-deck right-half-of-deck) (cut-deck-at deck (quotient (num-cards-in-set deck) 2))))
    (shuffle-impl empty-deck left-half-of-deck right-half-of-deck)))

; Repeatedly call the shuffle procedure on a deck for n iterations.
(define (shuffle-n deck n)
  (define (iter index intermediate-deck)
    (if (= n index)
        intermediate-deck
        (iter (add1 index) 
              (shuffle-deck intermediate-deck))))
  (iter 0 deck))

; Add a card to a set.
(define add-card cons)

; Get the total value of all cards in the set.
(define (card-set-total card-set)
  (reduce + 0 card-set))

;; Hands. Hands are just card-sets themselves.
(define (make-new-hand first-card)
  (make-card-set-from-list (list first-card)))

; Take the top card of a hand.
(define (hand-up-card hand)
  (first hand))

; Get the total value of all cards in a hand.
(define (hand-total hand) 
  (card-set-total hand))

; Add a card to a hand.
(define (hand-add-card hand card)
  (add-card card hand))

;;; Game. All that needed to be changed to accomodate the change of representation for
;;; hands was the 'deal' method, which now calls make-card instead of returning an int
;;; explicitly. 
;;;
;;; Some more work was needed to incorporate a discrete deck, basically any
;;; code that used to call (deal) and bind the result to something now has to handle the
;;; two simultaneous return values from 'deal-from', which returns the dealt card and the
;;; rest of the deck.
;;; 
;;; We assume that it is impossible for the deck to run out in the course of a 
;;; game.
(define (twenty-one player-strategy house-strategy)
  (let*-values (((deck) (shuffle-n (make-blackjack-deck) 100)) 
                ((first-house-card deck) (deal-from deck))
                ((first-player-card deck) (deal-from deck))
                ((house-initial-hand) (make-new-hand first-house-card))
                ((player-hand deck) (play-hand player-strategy
                                               (make-new-hand first-player-card)
                                               (hand-up-card house-initial-hand)
                                               deck)))
    (if (> (hand-total player-hand) 21)
        0                                ; ``bust'': player loses
        (let*-values (((house-hand deck) 
                         (play-hand house-strategy
                                    house-initial-hand
                                    (hand-up-card player-hand)
                                    deck)))
          (cond ((> (hand-total house-hand) 21)
                 1)                      ; ``bust'': house loses
                ((> (hand-total player-hand)
                    (hand-total house-hand))
                 1)                      ; house loses
                (else 0))))))            ; player loses

; Plays a hand and returns two values, the hand itself and the
; rest of the deck that is left over at the end.
(define (play-hand strategy my-hand opponent-up-card deck)
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
           (let*-values (((dealt-card deck) (deal-from deck)))
             (play-hand strategy
                        (hand-add-card my-hand dealt-card)
                        opponent-up-card
                        deck)))
        (else (values my-hand deck))))                ; stay

; Deal the top card from the deck.
(define (deal-from deck)
  ; Cheating / breaking abstraction, could use cut-deck-at here but then we get
  ; a single-element list as dealt card.
  (car+cdr deck))

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
          (printf "   Up (Total): ~v (~v)~n" (hand-up-card my-hand) (hand-total my-hand))
          (print  "   Hand: ")
          (print my-hand)
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



