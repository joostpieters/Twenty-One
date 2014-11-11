#|(load "twenty-one.scm")|#

;don't worry about how this function works. All you have to know
;is its domain and range
(define (run-test test-num func-name actual-result expected-result)
  (format #t "Testing case ~A for ~A: " test-num func-name)
  (if (not (equal? expected-result actual-result))
      (format #t "Failed.~%Expected: ~A~%Got: ~A~%~%"
              expected-result actual-result)
      (format #t "Passed!~%")))

;; Tests for best-total
(define (test-best-total)
  (run-test 1 "best-total"               ;; Test 1 for best-total
            (best-total '(2c 6d 3s 8h))  ;; Code to run for test 1
            19)                          ;; Expected output

  (run-test 2 "best-total"
            (best-total '(ad 8s)) ;; in this hand the ace counts as 11
            19)

  (run-test 3 "best-total"
            (best-total '(ad 8s 5h)) ;; here it must count as 1 to avoid busting
            14)

  (run-test 4 "best-total"
            (best-total '(ad as 9h)) ;; here one counts as 11 and the other as 1
            21)

  (run-test 5 "best-total"
            (best-total '(ad as ac ah 5d))
            19)

  (run-test 6 "best-total"
            (best-total '(3s ad as))
            15)

  (run-test 7 "best-total"
            (best-total '(3s ad as ah))
            16)

  (run-test 8 "best-total"
            (best-total '(ad 8s 8c 5h))
            22)

  (run-test 9 "best-total"
            (best-total '(10d 8s 5s))
            23)

  (run-test 10 "best-total"
            (best-total '(ad 10d))
            21)

  (run-test 11 "best-total"
            (best-total '(ad kd qd))
            21)

  (run-test 12 "best-total"
            (best-total '(as ah ad ac))
            14)

  (run-test 13 "best-total"
            (best-total '(10d ad))
            21)

  (run-test 14 "best-total"
            (best-total '(4c ah ad))
            16)

  (run-test 15 "best-total"
            (best-total '(ks kh kd kc))
            40)
  ;; Add more tests here.  Best-total has many different cases, so you
  ;; should have a *lot* of tests.  For example, you may want to test
  ;; that face cards (jacks/queens/kings) work, that aces work at the
  ;; beginning, the end, and the middle of the hand, and so on.

)
(test-best-total)

;; Tests for stop-at-17
(define (test-stop-at-17)
  (run-test 1 "stop-at-17"
            (stop-at-17 '(kh 6d) 'ah)
            #t)

  (run-test 2 "stop-at-17"
            (stop-at-17 '(kh 7d) '4d)
            #f)

  (run-test 3 "stop-at-17"
            (stop-at-17 '(ah kd) '5d)
            #f)

  (run-test 4 "stop-at-17"
            (stop-at-17 '(4d 3d) '5d)
            #t)

  (run-test 5 "stop-at-17"
            (stop-at-17 '(10d 6d) 'ad)
            #t)

  (run-test 6 "stop-at-17"
            (stop-at-17 '(10h 20d) '5d)
            #f)

  (run-test 7 "stop-at-17"
            (stop-at-17 '(ah 5d) '10h)
            #t)

  (run-test 8 "stop-at-17"
            (stop-at-17 '(10d 3h) 'ac)
            #t)

  (run-test 9 "stop-at-17"
            (stop-at-17 '(4d 3d 2d ad) '5d)
            #f)

  (run-test 10 "stop-at-17"
            (stop-at-17 '(5d 4d 3d 2d) '5c)
            #t)
  ;; Add more tests here

)
;; Uncomment the following line to run tests for stop-at-17
(test-stop-at-17)


;; Tests for play-n
(define (test-play-n)
  (run-test 1 "play-n"
            (play-n (lambda (x y) #t) 5)
            -5)
  ;; Since play-n is random, it is hard to write good tests for it.  You
  ;; should run play-n and make sure it returns reasonable outputs, but
  ;; you do not need to write more tests here for play-n.
)
;; Uncomment the following line to run tests for play-n
(test-play-n)

;; Tests for dealer-sensitive
(define (test-dealer-sensitive)
  (run-test 1 "dealer-sensitive"
            (dealer-sensitive '(4s 3s qh) '5h)
            #f)

  (run-test 2 "dealer-sensitive"
            (dealer-sensitive '(4s 2s qh) '10h)
            #t)

  (run-test 3 "dealer-sensitive"
            (dealer-sensitive '(as ah) '8h)
            #t)

  (run-test 4 "dealer-sensitive"
            (dealer-sensitive '(4s 3s) '3h)
            #t)

  (run-test 5 "dealer-sensitive"
            (dealer-sensitive '(qd kh) 'qc)
            #f)

  (run-test 6 "dealer-sensitive"
            (dealer-sensitive '(10h 5d) '10c)
            #t)

  (run-test 7 "dealer-sensitive"
            (dealer-sensitive '(10h 5d) 'ac)
            #t)

  (run-test 8 "dealer-sensitive"
            (dealer-sensitive '(10h 3c) '2d)
            #f)
  ;; Add more tests here

)
;; Uncomment the following line to run tests for dealer-sensitive
(test-dealer-sensitive)


;; Tests for stop-at
(define (test-stop-at)
  (run-test 1 "stop-at"
            ((stop-at 4) '(4d) '7h)
            #f)

  (run-test 2 "stop-at"
            ((stop-at 15) '(4d kd) '6h)
            #t)

  (run-test 3 "stop-at"
            ((stop-at 11) '(ad) '3h)
            #f)

  (run-test 4 "stop-at"
            ((stop-at 20) '(10d 10c) '7h)
            #f)

  (run-test 5 "stop-at"
            ((stop-at 11) '(10d) 'ac)
            #t)

  (run-test 6 "stop-at"
            ((stop-at 5) '(4d) '7h)
            #t)

  (run-test 7 "stop-at"
            ((stop-at 15) '(10d ad) '2h)
            #f)

  (run-test 8 "stop-at"
            ((stop-at 21) '(ad 9h) '7h)
            #t)

  (run-test 9 "stop-at"
            ((stop-at 21) '(10d kd) '7h)
            #t)
  ;; Add more tests here

)
;; Uncomment the following line to run tests for stop-at
(test-stop-at)


;; Tests for valentine
(define (test-valentine)
  (run-test 1 "valentine"
            (valentine '(qd 7h) 'jc)
            #t)

  (run-test 2 "valentine"
            (valentine '(qd 7c) 'jc)
            #f)

  (run-test 3 "valentine"
            (valentine '(10d 7h) '3c)
            #t)

  (run-test 4 "valentine"
            (valentine '(ad 4h) '3h)
            #t)

  (run-test 5 "valentine"
            (valentine '(3d 5d 9d) '5c)
            #f)

  (run-test 6 "valentine"
            (valentine '(ad 6h) 'ah)
            #t)

  (run-test 7 "valentine"
            (valentine '(ad 10h) 'jc)
            #f)

  (run-test 8 "valentine"
            (valentine '(4h 3d kd ad) 'jc)
            #t)

  (run-test 9 "valentine"
            (valentine '(4c 2d kd) 'jd)
            #t)

  (run-test 10 "valentine"
            (valentine '(10h 8d) 'jc)
            #t) 
  ;; Add more tests here

)
;; Uncomment the following line to run tests for valentine
(test-valentine)


;; Tests for suit-strategy
(define (test-suit-strategy)
  (run-test 1 "suit-strategy"
            ((suit-strategy 'c (stop-at 7) (stop-at 10)) '(4c 5d) '9h)
            #t)

  (run-test 2 "suit-strategy"
            ((suit-strategy 'c (stop-at 7) (stop-at 10)) '(4d 5d) '9h)
            #f)

  (run-test 3 "suit-strategy"
            ((suit-strategy 'd (stop-at 15) (stop-at 17)) '(4c 5d) '9h)
            #t)

  (run-test 4 "suit-strategy"
            ((suit-strategy 'h (stop-at 10) (stop-at 13)) '(4c 5d) '9h)
            #t)

  (run-test 5 "suit-strategy"
            ((suit-strategy 'h (stop-at 7) (stop-at 10)) '(4h 5d) '9h)
            #t)

  (run-test 6 "suit-strategy"
            ((suit-strategy 'h (stop-at 11) (stop-at 13)) '(5d 7d) '9h)
            #f)


  ;; Add more tests here

)
;; Uncomment the following line to run tests for suit-strategy
(test-suit-strategy)

;; Tests for valentine2
;; You can copy over your tests for valentine and change them to use
;; valentine2 instead of valentine.
(define (test-valentine2)
  (run-test 1 "valentine2"
            (valentine2 '(qd 7h) 'jc)
            #t)

  (run-test 2 "valentine2"
            (valentine2 '(qd 7c) 'jc)
            #f)

  (run-test 3 "valentine2"
            (valentine2 '(10d 7h) '3c)
            #t)

  (run-test 4 "valentine2"
            (valentine2 '(ad 4h) '3h)
            #t)

  (run-test 5 "valentine2"
            (valentine2 '(3d 5d 9d) '5c)
            #f)

  (run-test 6 "valentine2"
            (valentine2 '(ad 6h) 'ah)
            #t)

  (run-test 7 "valentine2"
            (valentine2 '(ad 10h) 'jc)
            #f)

  (run-test 8 "valentine2"
            (valentine2 '(4h 3d kd ad) 'jc)
            #t)

  (run-test 9 "valentine2"
            (valentine2 '(4c 2d kd) 'jd)
            #t)

  (run-test 10 "valentine2"
            (valentine2 '(10h 8d) 'jc)
            #t) 

  ;; Add more tests here
)
;; Uncomment the following line to run tests for valentine2
(test-valentine2)


;; Tests for majority
(define (test-majority)
  (run-test 1 "majority"
            ((majority stop-at-17 dealer-sensitive (stop-at 14)) '(kd 6h) '9h)
            #t)

  (run-test 2 "majority"
            ((majority stop-at-17 dealer-sensitive (stop-at 13)) '(kd 6h) '6h)
            #f)

  (run-test 3 "majority"
            ((majority stop-at-17 dealer-sensitive (stop-at 10)) '(10d 5h as) '10h)
            #t)

  (run-test 4 "majority"
            ((majority stop-at-17 dealer-sensitive (stop-at 14)) '(kd ah) '2h)
            #f)

  (run-test 5 "majority"
            ((majority stop-at-17 dealer-sensitive (stop-at 5)) '(8h 2c) '3h)
            #t)

  (run-test 6 "majority"
            ((majority stop-at-17 dealer-sensitive (stop-at 10)) '(8h 9h 3c) '10h)
            #f)
  ;; Add more tests here

)
;; Uncomment the following line to run tests for majority
(test-majority)


;; Tests for reckless
(define (test-reckless)
  (run-test 1 "reckless"
            ((reckless (stop-at 12)) '(ad 8s) '10h)
            #t)

  (run-test 2 "reckless"
            ((reckless (stop-at 10)) '(2d 3s) '9h)
            #t)

  (run-test 3 "reckless"
            ((reckless (stop-at 5)) '(ad as) '10h)
            #f)

  (run-test 4 "reckless"
            ((reckless (stop-at 10)) '(3s 5s 2h) 'ah)
            #t)

  (run-test 5 "reckless"
            ((reckless (stop-at 15)) '(kd 3s) '3h)
            #t)

  ;; Add more tests here

)
;; Uncomment the following line to run tests for play-n
(test-reckless)


;; Tests for jokers
(define (test-joker)
  (run-test 1 "joker"
            (best-total '(2c 6d 3s 8h))
            19)

  (run-test 2 "joker"
            (best-total '(r 2d))
            13)

  (run-test 3 "joker"
            (best-total '(r 3s r))
            21)

  (run-test 4 "joker"
            (best-total '(ad r))
            21)

  (run-test 5 "joker"
            (best-total '(ac r r))
            21)

  (run-test 6 "joker"
            (best-total '(ad ac as ah r r))
            21)

  (run-test 7 "joker"   
            (best-total '(10h 9d r))
            21)

  (run-test 8 "joker"
            (best-total '(kd r ah))
            21)

  (run-test 9 "joker"
            (best-total '(9d r r 3d 2d))
            21)

  (run-test 10 "joker"
            (best-total '(jd r 3s))
            21)

  (run-test 11 "joker"
            (best-total '(8h r))
            19)

  (run-test 12 "joker"
            (best-total '(ac ad 3s))
            15)

  (run-test 13 "joker"
            (best-total '(r 2s 3s))
            16)

  (run-test 14 "joker"
            (best-total '(3s 4h r))
            18)

  (run-test 15 "joker"
            (best-total '(as 6s ah))
            18)

  (run-test 16 "joker"
            (best-total '(10d 9h r r))
            21)

  (run-test 17 "joker"
            (best-total '(r qc 8h 7s ad))
            27)

  (run-test 18 "joker"
            (best-total '(r r  qd kc))
            22)
  ;; Add more tests here

)
;; Uncomment the following lines to run tests for jokers
 (load "joker.scm")
  (test-joker)
