#lang br/quicklang

(define (read-syntax path port)
  ;; Read all expressions (datums) from the input port into a list
  (define exprs (let loop ((acc '()))
                  (define next (read port))
                  (if (eof-object? next)
                      (reverse acc)
                      (loop (cons next acc)))))

  ;; Wrap the expressions in a module for evaluation
  (define module-datum
    `(module rl-lang-module br/quicklang
       ,@exprs))

  ;; Convert the module datum to syntax and return it
  (datum->syntax #f module-datum))

;; Provide the read-syntax function to be used as a reader
(provide read-syntax)

