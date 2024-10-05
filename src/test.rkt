#lang racket
(require math)

(define (make-4d-vector dim1 dim2 dim3 dim4 init-val)
  (define indx (vector dim1 dim2 dim3 dim4))
  (array->mutable-array(make-array indx init-val))
  )


;; Example dimensions
(define n 10) ;; Adjust n as needed for your use case
(define num-states (expt n 2))  ;; Number of states (n^2)
(define num-actions 2)          ;; Number of actions
(define num-rewards 3)          ;; Number of rewards

;; Initialize the 4D transitionProbabilities vector with zeros
(define transitionProbabilities (make-4d-vector num-states num-actions num-rewards num-states 0))
(define test (+ n num-rewards))
;; Example of accessing an element in the 4D vector
(define s 5)  ;; Example state (adjust as needed)
(define a 0)  ;; Example action (0 or 1)
(define r 1)  ;; Example reward (0, 1, or 2)
(define sp 6) ;; Example state prime
(define p (vector 0.1 0.2 0.3 0.1 0.1 0.2))
(define dif -1)
(define indx (vector 0 0 1 1))
(displayln indx)
(displayln (array-ref transitionProbabilities indx))

(array-set! transitionProbabilities indx (+ (array-ref transitionProbabilities indx) (vector-ref p (- (abs dif) 1))))
;; Accessing an element in the transitionProbabilities vector
 (define value (array-ref transitionProbabilities indx))

 (displayln value) ;; Display the value (should be 0 initially)