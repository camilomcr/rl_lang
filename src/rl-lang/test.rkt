#lang reader "rl_lang.rkt"

(reward-function my-reward (s a sp)
                 [(and (= s 1) (= a 0)) 10]
                 [(and (= s 2) (= a 1) (= sp 3)) 5]
                 [(= s 4) -1])

(states-function states (s-actual x1 x2 x3)
                 [(> x1 10) 1]
                 [(and (= x1 2) (= x2 1) (= x3 3)) 2]
                 [(= x3 4) 3]
                 [(and (= s-actual 3) (= x1 1) (< x2 5) (>= x3 2)) 4])
 
(define-hyperparameters hyperparameters)
(set-hyperparameter hyperparameters 'gamma 0.1)
(add-hyperparameter hyperparameters 'dropout-rate 0.5)

(rl-env cars (hyperparameters my-reward states))

(displayln "Rewards: ")
(displayln (my-reward 1 0 2)) ; prints 10
(displayln (my-reward 2 1 3)) ; prints 5
(displayln (my-reward 4 0 1)) ; prints -1
(displayln (my-reward 5 0 1)) ; prints 0 (default case)

(displayln "States: ")
(displayln (states 2 11 0 0)) ; prints 1
(displayln (states 1 2 1 3))  ; prints 2
(displayln (states 0 0 0 4))  ; prints 3
(displayln (states 3 1 4 3))  ; prints 4
(displayln (states 0 0 0 0))  ; prints #f (default case)

(displayln "Hyperparameters: ")
(displayln (hash-ref hyperparameters 'eps))        ; prints 0.1
(displayln (hash-ref hyperparameters 'gamma))          ; prints 0.1
(displayln (hash-ref hyperparameters 'dropout-rate))   ; prints 0.5

(displayln "Environment: ")
(define hyperp (rl-environment-hyperparameters cars))
(displayln cars)
(displayln (rl-environment-hyperparameters cars))        ; Prints the hyperparameters hash
(displayln (hash-ref hyperp 'eps))        ; 0.0001
(displayln ((rl-environment-reward-function cars) 1 0 2)) ; Prints 10
(displayln ((rl-environment-states-function cars) 2 11 0 0)) ; Prints 1