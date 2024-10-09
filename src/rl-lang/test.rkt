#lang reader "rl_lang.rkt"

(reward-function my-reward (s a sp)
  [(and (= s 1) (= a 0)) 10]
  [(and (= s 2) (= a 1) (= sp 3)) 5]
  [(= s 4) -1])

;; Now `my-reward` is a function that takes states, actions, and next states:
(displayln (my-reward 1 0 2)) ; prints 10
(displayln (my-reward 2 1 3)) ; prints 5
(displayln (my-reward 4 0 1)) ; prints -1
(displayln (my-reward 5 0 1)) ; prints 0 (default case)
