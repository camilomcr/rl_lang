#lang reader "rl_lang.rkt"

(require "Board.rkt")
(require "Learning_functions.rkt")

;; Instantiate the class
(define snakes '((98 28)(95 24)(92 51)(83 19)(73 1)(69 33)(64 36)(59 17)(55 7)(52 11)(48 9)(46 5)(44 22)))
(define stairs '((8 26)(21 82)(43 77)(50 91)(54 93)(62 96)(66 87)(80 100)))
(define blues (list 80 100))
(define reds (list 23 37 45 67 89))
(define n 10)

(define-hyperparameters hyperparameters)
(add-hyperparameter hyperparameters 'n n)
(add-hyperparameter hyperparameters 'p (vector 1/6 1/6 1/6 1/6 1/6 1/6))
(add-hyperparameter hyperparameters 'snakes snakes)
(add-hyperparameter hyperparameters 'stairs stairs)
(add-hyperparameter hyperparameters 'blues blues)
(add-hyperparameter hyperparameters 'reds reds)

(reward-function reward-fn (s a sp)
                 [(list?(member sp blues)) 1]
                 [(list?(member sp reds)) -1])

;; Helper function to find a snake or ladder endpoint
(define (find-end state pairs)
  (for/or ([pair pairs])
    (if (= state (first pair))
        (second pair)
        #f))) 

;; Function to calculate the next position with all game rules
(define (getSp cell)
  (let* ([adjusted-cell (cond
                          [(> cell 100) (- 100 (- cell 100))]  ; Bounce if > 100
                          [(< cell 1) (+ 1 (- 1 cell))]       ; Bounce if < 1
                          [else cell])]
         ;; Check for snakes and ladders at the adjusted state
         [snake-end (find-end adjusted-cell snakes)]
         [ladder-end (find-end adjusted-cell stairs)])
    (cond
      [(number? snake-end) snake-end]   ; Go to the snake's tail if on snake's head
      [(number? ladder-end) ladder-end] ; Go to the ladder's top if on ladder's base
      [else adjusted-cell]))) ; Otherwise, stay at the adjusted state


(states-function states-fn (s a dice)
                 [(= a 1) (getSp (+ s dice))]
                 [(= a 0) (getSp (- s dice))])

(define board (new Board [hyperparameters-init hyperparameters] [reward-fn-init reward-fn] [states-fn-init states-fn]))

;; define transition probabilities of the MDP
(define transitionProbabilities (send board buildMDP))

;; Get optimal policy according to policy iteration
(define-values (V policyPI) (policy-iteration (hash-ref hyperparameters 'eps) transitionProbabilities reds blues n (hash-ref hyperparameters 'gamma)))
(define-values (Q policyQL) (QLearning hyperparameters reward-fn states-fn))
(displayln policyPI)
(displayln policyQL)