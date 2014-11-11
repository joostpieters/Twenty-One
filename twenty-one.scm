(define (best-total sent)
  (define (total sent)
    (cond ((empty? sent) 0)
        ((member? (first (first sent)) '(a)) (+ 11 (total (bf sent))))
        ((member? (first (first sent)) '(j q k)) (+ 10 (total (bf sent))))
        ((= (count (first sent)) 3) (+ 10 (total (bf sent))))
        (else (+ (first (first sent)) (total (bf sent))))))
  (ace-value (total sent) sent))

(define (ace-value val sent)
  (cond ((and (> val 21)(= (count-ace sent) 0)) val)
        ((and (> val 21)(= (count-ace sent) 1)) (- val 10))
        ((and (> val 21)(<= val 31)(>= (count-ace sent) 1)) (- val 10))
        ((and (> val 21)(= (count-ace sent) 2)) (- val 20))
        ((and (> val 21)(<= val 41)(>= (count-ace sent) 2)) (- val 20))
        ((and (> val 21)(= (count-ace sent) 3)) (- val 30))
        ((and (> val 21)(<= val 51)(>= (count-ace sent) 3)) (- val 30))
        ((and (> val 21)(= (count-ace sent) 4)) (- val 40))
        (else val)))

(define (count-ace sent)
  (cond ((empty? sent) 0)
  ((equal? 'a (first (first sent))) ( + 1 (count-ace (bf sent))))
  (else (count-ace (bf sent)))))


(define (stop-at-17 hand dealers-card)
  (< (best-total hand) 17))


(define (play-n strategy n)
  (cond ((= n 0) 0)
    (else (+ (twenty-one strategy) (play-n strategy (- n 1))))))


(define (dealer-sensitive hand dealers-card)
    (or (and (or (member? (first dealers-card) '(a 7 8 9 10 j q k)) (= (count dealers-card) 3)) (< (best-total hand) 17))
        (and (member? (first dealers-card) '(2 3 4 5 6)) (< (best-total hand) 12))))


(define (stop-at n)
  (lambda (hand dealers-hand) (< (best-total hand) n)))



(define (heartful hand)
  (cond ((empty? hand) #f)
    ((equal? (last (first hand)) 'h) #t)
    (else (heartful (bf hand)))))


(define (valentine hand dealers-hand)
  (or
    (and (heartful hand) (< (best-total hand) 19)) 
    (< (best-total hand) 17)))


(define (suits hand)
  (every last hand))

(define (has-suit? suit hand)
  (member? suit (suits hand))) 


(define (suit-strategy suit strategy-no-suit strategy-yes-suit)
  (lambda (hand dealer-card) 
  (if (has-suit? suit hand)
   (strategy-yes-suit hand dealer-card)
   (strategy-no-suit hand dealer-card))))

(define (valentine2 hand dealer-card)
  ((suit-strategy 'h (stop-at 17) (stop-at 19)) hand dealer-card))

(define (majority strategy-one strategy-two strategy-three)
  (lambda (hand dealer-card)
    (if (or
      (and (strategy-one hand dealer-card) (strategy-two hand dealer-card))
      (and (strategy-one hand dealer-card) (strategy-three hand dealer-card))
      (and (strategy-two hand dealer-card) (strategy-three hand dealer-card)) 
      (and (strategy-two hand dealer-card) (strategy-one hand dealer-card) (strategy-three hand dealer-card)))
      #t
      #f)))


(define (reckless strategy)
  (lambda (hand dealer-card)
    (strategy (bl hand) dealer-card)))

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
          ((< (best-total dealer-hand-so-far) 17)
           (play-dealer customer-hand
                        (se dealer-hand-so-far (first rest-of-deck))
                        (bf rest-of-deck)))
          ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
          ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
          (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
          ((strategy customer-hand-so-far dealer-up-card)
           (play-customer (se customer-hand-so-far (first rest-of-deck))
                          dealer-up-card
                          (bf rest-of-deck)))
          (else
           (play-dealer customer-hand-so-far
                        (se dealer-up-card (first rest-of-deck))
                        (bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
                   (first (bf (bf deck)))
                   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
          (se (first in) (shuffle (se (bf in) out) (- size 1)))
          (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
        deck
        (move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )
