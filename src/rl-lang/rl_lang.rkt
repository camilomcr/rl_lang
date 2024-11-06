#lang br/quicklang

(require syntax/parse)

(define (read-syntax path port)
  ;; Read all expressions (datums) from the input port into a list
  (define exprs (let loop ((acc '()))
                  (define next (read port))
                  (if (eof-object? next)
                      (reverse acc)
                      (loop (cons next acc)))))

  ;; Wrap the expressions in a module for evaluation
  (define module-datum
    `(module rl-lang-module racket
       
       ;; Include the for-rl macro
       (define-syntax-rule (for-rl ([s states] [a (in-vector actions)] [sp spStates] [r rewardsPossibles]) body)
         (for ([s states])
           (for ([a (in-vector actions)])
             (for ([sp spStates])
               (for ([r rewardsPossibles])
                 body)))))

       ;; Macro for the definition of reward function
       (define-syntax-rule (reward-function name (s a sp) [condition reward] ...)
         (define (name s a sp)
           (cond
             [condition reward] ...
             [else 0])))

       ;; Macro for the definition of states function
       (define-syntax-rule (states-function name (params ...) [condition result] ...)
         (define (name params ...)
           (cond
             [condition result] ...
             [else #f]))) ; Default case to return #f if no conditions match

       ;;Macro for the definition of hyperparameters
       (define-syntax-rule (define-hyperparameters name)
         (define name
           (make-hash (list
                       (cons 'learning-rate 0.01)
                       (cons 'gamma 0.9)
                       (cons 'alpha 0.1)
                       (cons 'eps 1e-4)
                       (cons 'epsilon 0.1)
                       (cons 'batch-size 32)
                       (cons 'num-episodes 500000)
                       (cons 'max-steps 200)
                       (cons 'hidden-layers '(64 64))))))


       ;; Procedure to set (or update) a hyperparameter
       (define (set-hyperparameter hyperparameters key value)
         (hash-set! hyperparameters key value))

       ;; Procedure to add a new hyperparameter if it doesn't already exist
       (define (add-hyperparameter hyperparameters key value)
         (unless (hash-has-key? hyperparameters key)
           (hash-set! hyperparameters key value)))

       ;; Define the structure for an RL environment
       (struct rl-environment (hyperparameters reward-function states-function) 
         #:transparent)

       ;; Macro to define an RL environment with hyperparameters, reward-function, and states-function
       (define-syntax-rule (rl-env name (hyperparameters reward-function states-function))
         (define name (rl-environment hyperparameters reward-function states-function)))

       ;; Include user expressions
       ,@exprs))

  ;; Convert the module datum to syntax and return it
  (datum->syntax #f module-datum))

;; Provide the read-syntax function for use as a reader
(provide read-syntax)
