#lang reader "rl_lang.rkt"

(require math)
(provide Board)

;; Define Board class
(define Board
  (class object%
    (init hyperparameters-init reward-fn-init states-fn-init)
    (super-new)  ; Call the superclass constructor
    
    ;; Define board parameters
    (rl-env environment (hyperparameters-init reward-fn-init states-fn-init))

    ;; Helper function to create a 4D array
    (define (make-4d-array dim1 dim2 dim3 dim4 init-val)
      (define indx (vector dim1 dim2 dim3 dim4))
      (array->mutable-array(make-array indx init-val))
      )

    (define/public (spInInitCell sp)
      (define hyperp (rl-environment-hyperparameters environment))
      (define snakes (hash-ref hyperp 'snakes))
      (define stairs (hash-ref hyperp 'stairs))
      (define reds (hash-ref hyperp 'reds))
      (define blues (hash-ref hyperp 'blues))
      
      (cond
        ;; If sp is in reds or blues, return #f
        [(or (member sp reds) (member sp blues)) #f]
        
        ;; Check if sp is the initial cell of a snake
        [(for/first ([snake snakes] #:when (= (first snake) sp))
           #t)]

        ;; Check if sp is the initial cell of a stair
        [(for/first ([stair stairs] #:when (= (first stair) sp))
           #t)]

        ;; Default case: return #f if not found
        [else #f]))

    ;; Method to get the initial cell for a snake or stair given their final cell
    (define/public (getInitCell sp)
      
      (define hyperp (rl-environment-hyperparameters environment))
      (define snakes (hash-ref hyperp 'snakes))
      (define stairs (hash-ref hyperp 'stairs))
      
      (cond
        ;; Check if sp is the final cell of a snake
        [(for/first ([snake snakes] #:when (= (second snake) sp))
           (first snake))]
        
        ;; Check if sp is the final cell of a stair
        [(for/first ([stair stairs] #:when (= (second stair) sp))
           (first stair))]

        ;; Default case: return 0 if not found
        [else 0]))
    
    ;; Method to build the Markov Decision Process (MDP)
    (define/public (buildMDP)
      (define reward-fn (rl-environment-reward-function environment))
      (define hyperp (rl-environment-hyperparameters environment))
      (define states-fn (rl-environment-states-function environment))
      (define n (hash-ref hyperp 'n))
      (define p (hash-ref hyperp 'p))
      (define reds (hash-ref hyperp 'reds))
      (define blues (hash-ref hyperp 'blues))
      
      (define actions (vector 0 1))  ;; 0 backwards, 1 forward
      (define states (range 1 (+ (* n n) 1)))  ;; States from 1 to n^2
      (define rewardsPossibles (range 3))  ;; Indexes for rewards
      (define num-states (expt n 2))  ;; This is n^2, the number of states
      (define num-actions 2)          ;; Actions: forward (1) and backward (0)
      (define num-rewards 3)  
      (define transitionProbabilities (make-4d-array num-states num-actions num-rewards num-states 0))
      
      (for-rl ([s states] [a (in-vector actions)] [sp states] [r rewardsPossibles]) 

              (if (send this spInInitCell sp)
                  ;; If sp is in an initial cell, set transition probability to 0 and skip further logic
                 
                  (let ([indx (vector (- s 1) a r (- sp 1))])
                    (array-set! transitionProbabilities indx 0)
                    )
                  ;; Else continue with the rest of the code
                  (let ([initCell (send this getInitCell sp)])  ;; Fall on initCell to go to sp, 0 if not possible
                    ;; Further logic goes here using initCell
                    (when initCell
                      (when (and (not (= initCell 0))
                                 (not (member initCell reds))
                                 (not (member initCell blues))
                                 (= (reward-fn s a sp) (- r 1)))
  
                        ;; Bounce going backwards (a=0)
                        (when (and (= a 0) (< (- s 6) 1))
                          (let ([diceNeeded (+ (- initCell 1) (- s 1))])
                            (when (and (<= diceNeeded 6) (> diceNeeded 0) (not (= (- initCell 1) 0)))
                              (let ([indx (vector (- s 1) a r (- sp 1))])
                                (array-set! transitionProbabilities indx (+ (array-ref transitionProbabilities indx) (vector-ref p (- diceNeeded 1))))
                                )
                              )))

                        ;; Bounce going forward (a=1)
                        (when (and (= a 1) (> (+ s 6) (expt n 2)))
                          (let ([diceNeeded (+ (- (expt n 2) initCell) (- (expt n 2) s))])
                            (when (and (<= diceNeeded 6) (> diceNeeded 0) (not (= (- (expt n 2) initCell) 0)))
                              (let ([indx (vector (- s 1) a r (- sp 1))])
                                (array-set! transitionProbabilities indx (+ (array-ref transitionProbabilities indx) (vector-ref p (- diceNeeded 1))))
                                )
                              )))

                        ;; We can go from s to initCell with a 6-face dice
                        (let ([dif (- initCell s)])
                          (when (and (<= (abs dif) 6) (not (= dif 0)))
                            (when (or (and (= a 0) (< dif 0)) (and (= a 1) (> dif 0)))
                              (let ([indx (vector (- s 1) a r (- sp 1))])
                                (array-set! transitionProbabilities indx (+ (array-ref transitionProbabilities indx) (vector-ref p (- (abs dif) 1))))
                                )
                              ))))
                      
                      (when (= (reward-fn s a sp) (- r 1))

                        ;; Bounce going backwards (a=0)
                        (when (and (= a 0) (< (- s 6) 1))
                          (let ([diceNeeded (+ (- sp 1) (- s 1))])
                            (when (and (<= diceNeeded 6) (> diceNeeded 0) (not (= (- sp 1) 0)))
                              (let ([indx (vector (- s 1) a r (- sp 1))])
                                (array-set! transitionProbabilities indx (+ (array-ref transitionProbabilities indx) (vector-ref p (- diceNeeded 1))))
                                )
                              )))

                        ;; Bounce going forward (a=1)
                        (when (and (= a 1) (> (+ s 6) (expt n 2)))
                          (let ([diceNeeded (+ (- (expt n 2) sp) (- (expt n 2) s))])
                            (when (and (<= diceNeeded 6) (> diceNeeded 0) (not (= (- (expt n 2) sp) 0)))
                              (let ([indx (vector (- s 1) a r (- sp 1))])
                                (array-set! transitionProbabilities indx (+ (array-ref transitionProbabilities indx) (vector-ref p (- diceNeeded 1))))
                                )
                              )))

                        ;; We can go from s to sp with a 6-face dice
                        (let ([dif (- sp s)])
                          (when (and (<= (abs dif) 6) (not (= dif 0)))
                            (when (or (and (= a 0) (< dif 0)) (and (= a 1) (> dif 0)))
                              (let ([indx (vector (- s 1) a r (- sp 1))])
                                (array-set! transitionProbabilities indx (+ (array-ref transitionProbabilities indx) (vector-ref p (- (abs dif) 1))))
                                )
                              ))))



                      )))
              )
      transitionProbabilities
      )
    )
  )