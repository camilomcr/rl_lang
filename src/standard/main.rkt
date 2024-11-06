#lang br/quicklang

(require "Board.rkt")
(require "Learning_functions.rkt")

;; Instantiate the class
(define blues (list 80 100))
(define reds (list 23 37 45 67 89))
(define stairs '((8 26)(21 82)(43 77)(50 91)(54 93)(62 96)(66 87)(80 100)))
(define snakes '((98 28)(95 24)(92 51)(83 19)(73 1)(69 33)(64 36)(59 17)(55 7)(52 11)(48 9)(46 5)(44 22)))
(define p (vector 1/6 1/6 1/6 1/6 1/6 1/6))
(define n 10)
(define eps 1e-4)
(define gamma 0.9)
(define alpha 0.1)
(define epsilon 0.1)
(define num_episodes 500000)

(define board (new Board [n-init n] [reds-init reds] [blues-init blues] [snakes-init snakes] [stairs-init stairs] [p-init p] [gamma-init gamma]))

;; define transition probabilities of the MDP
(define transitionProbabilities (send board buildMDP))

;; Get optimal policy according to policy iteration
(define-values (V policyPI) (policy-iteration eps transitionProbabilities reds blues n gamma))
(define-values (Q policyQL) (QLearning board n reds blues epsilon alpha gamma num_episodes))
(displayln policyPI)
(displayln policyQL)