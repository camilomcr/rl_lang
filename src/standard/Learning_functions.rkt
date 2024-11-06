#lang br/quicklang

(require math)
(require "Board.rkt")
(require racket/random)

(define (policy-iteration eps transitionProbabilities reds blues n gamma)
  ; Initialization
  (define states (range 1 (+ (* n n) 1)))
  (define V (make-vector (* n n) 0))
  (define pi (for/list ([i (range 100)]) (random 2))) ; Random initial policy
  (define actions '(0 1))
  (define rewards-possibles '(0 1 2)) ; index 0 -> reward -1, index 1 -> reward 0, index 2 -> reward 1
  (define policy-stable? #t)


  (let loop ()  ; Define the named `loop` with no parameters
    ; Policy Evaluation
    (set! V (policy-evaluation V eps transitionProbabilities pi reds blues n gamma))

    ; Policy Improvement
    (set! policy-stable? #t)
    (for ([s states])
      (define old-action (list-ref pi (- s 1)))
      (define A (make-vector (length actions) 0))
      (for ([a actions])
        (for ([sp states])
          (for ([r rewards-possibles])
            (vector-set! A a
                         (+ (vector-ref A a)
                            (* (let ([indx (vector (- s 1)  a r (- sp 1))])
                                 (array-ref transitionProbabilities indx)
                                 )
                               (+ (- r 1) (* gamma (vector-ref V (- sp 1))))))))
          )
        )
      ; Update policy
      (define new-action (argmax-v A))
      (when (not (= old-action new-action))
        (set! policy-stable? #f))
      (set! pi (list-set pi (- s 1) new-action)))

    ; Check if the policy is stable; if not, recurse
    (if policy-stable?
        (values V pi) ; Return the state values and the optimal policy
        (loop)) ; Recursive call to `loop` to continue if not stable
    )

  )


(define (policy-evaluation V eps transitionProbabilities policy reds blues n gamma)
  ; Initialization
  (define states (range 1 (+ (* n n) 1)))
  (define rewards-possibles '(0 1 2)) ; index 0 -> reward -1, index 1 -> reward 0, index 2 -> reward 1

  (let loop ()
    (define delta 0)
    (define Vprev (vector-copy V))

    ; Iterate through states
    (for ([s states])
      (define vp (vector-ref V (- s 1)))
      (if (or (member s reds) (member s blues))
          (vector-set! V (- s 1) 0)
          (let ([sum 0])
            (for ([sp states])
              (for ([r rewards-possibles])
                ;; (displayln (let ([indx (vector (- s 1) (list-ref policy (- s 1)) r (- sp 1))])(array-ref transitionProbabilities indx)))
                (set! sum
                      (+ sum
                         (* (let ([indx (vector (- s 1) (list-ref policy (- s 1)) r (- sp 1))])
                              (array-ref transitionProbabilities indx)
                              )
                            (+ (- r 1) (* gamma (vector-ref Vprev (- sp 1))))
                            )
                         )
                      )
                )
              )
            (vector-set! V (- s 1) sum)
            ;; (displayln sum)
            )
          )
      (set! delta (max delta (abs (- vp (vector-ref V (- s 1))))))
      )
    (if (< delta eps)
        V
        (loop))
    )
  )

(define (r)
  (/ (random 4294967087)
     4294967086.0))

(define (QLearning board n reds blues epsilon alpha gamma num_episodes)
  
  (define Q (build-vector (* n n) (lambda (_) (make-vector 2 0.0)))) ; Initialize Q(s, a) with zeros
  (define policy (make-vector (* n n) 0)) ; Initialize policy vector with zeros

  ;; Loop through each episode
  (for ([episode (in-range num_episodes)])
    (define state (+ 1 (random (* n n)))) ; Random initial state from 1 to n^2
    ;; Continue until we reach a terminal state
    (when (and (not (member state reds)) (not (member state blues)))
      (let loop()
      
        (define action (epsilon-greedy-policy Q state epsilon)) ; Select action
        (define-values (reward next-state) (send board do-action action state)) ; Perform action, get reward and next state
      

        ;; Update Q-value using the Q-learning formula
        (define current-q (vector-ref (vector-ref Q (- state 1)) action))
        (define max-next-q (apply max (vector->list (vector-ref Q (- next-state 1)))))
        (vector-set! (vector-ref Q (- state 1)) action
                     (+ current-q (* alpha (- (+ reward (* gamma max-next-q)) current-q))))

        ;; Update the state to the next state
        (set! state next-state)
      
        (when (and (not (member state reds)) (not (member state blues)))
          (loop)
          )
        ))
    )

  ;; Extract policy from Q by choosing the action with the highest Q-value for each state
  (for ([s (in-range (* n n))])
    (vector-set! policy s (argmax-v (vector-ref Q s))))

  ;; Return the Q-values and policy
  (values Q policy))

;; Epsilon-greedy policy function
(define (epsilon-greedy-policy Q state epsilon)
  (if (< (r) epsilon)
      (random 2)  ; Return a random action (either 0 or 1)
      (argmax-v (vector-ref Q (- state 1)))
      )
  )  ; Return the action with the highest Q-value

; Helper function to find the index of the maximum value in a list
(define (argmax-v vec)
  (define max-index 0)
  (define max-value (vector-ref vec 0))
  
  (for ([i (in-range 1 (vector-length vec))])
    (let ([current-value (vector-ref vec i)])
      (when (> current-value max-value)
        (set! max-value current-value)
        (set! max-index i))))
  
  max-index)

; Helper function to update a list at a specific index
(define (list-set lst idx val)
  (append (take lst idx) (list val) (drop lst (+ idx 1))))

(provide policy-iteration)
(provide QLearning)