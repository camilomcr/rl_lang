#lang br/quicklang

;; Define Board class
(define Board
  (class object%
    (init n-init reds-init blues-init snakes-init stairs-init p-init gamma-init)
    (super-new)  ; Call the superclass constructor
    
    ;; Define board parameters
    (define n n-init)
    (define reds reds-init)
    (define blues blues-init)
    (define snakes snakes-init)
    (define stairs stairs-init)
    (define p p-init)
    (define gamma gamma-init)
    
    ;; Method to set dice probabilities
    (define/public (setDiceProbabilities new-p)
      (set! p new-p))
    
    ;; Method to set gamma
    (define/public (setGamma new-gamma)
      (set! gamma new-gamma))
    
    )
)

;; Instantiate the class
(define blues (list 80 100))
(define reds (list 23 37 45 67 89))
(define stairs '((8 26)(21 82)(43 77)(50 91)(54 93)(62 96)(66 87)(80 100)))
(define snakes '((98 28)(95 24)(92 51)(83 19)(73 1)(69 33)(64 36)(59 17)(55 7)(52 11)(48 9)(46 5)(44 22)))
(define p (list 0.1 0.2 0.3 0.1 0.1 0.2))
(define n 10)
(define eps 1e-8)
(define gamma 0.9)
(define epsilon 0.1)
(define alpha 0.1)
(define episodes 500000)

(define board (new Board [n-init n] [reds-init reds] [blues-init blues] [snakes-init snakes] [stairs-init stairs] [p-init p] [gamma-init gamma]))

;; Use the object's methods
;; (send board display-blues)
