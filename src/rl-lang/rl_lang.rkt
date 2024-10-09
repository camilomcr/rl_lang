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
       (define-syntax-rule (for-rl ([s states] [a (in-vector actions)] [sp states] [r rewardsPossibles]) body)
         (for ([s states])
           (for ([a (in-vector actions)])
             (for ([sp states])
               (for ([r rewardsPossibles])
                 body)))))

       ;; Include the reward-function macro
       (define-syntax (reward-function stx)
         (syntax-parse stx
           [(_ reward-var (state action next-state) (clauses ...))
            (define reward-var
              (lambda (s a sp)
                (cond
                  ;; Process each clause to form the conditional statements
                  ,@(for/list ([clause (in-list (syntax->list #'(clauses ...)))])
                      (define condition (first clause))
                      (define value (second clause))
                      `(,(syntax->datum condition) ,value))
                  (else 0))))]))

       ;; Include user expressions
       ,@exprs))

  ;; Convert the module datum to syntax and return it
  (datum->syntax #f module-datum))

;; Provide the read-syntax function for use as a reader
(provide read-syntax)
