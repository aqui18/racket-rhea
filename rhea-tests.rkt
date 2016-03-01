#lang racket/base

(require rackunit rackunit/text-ui
         "rhea.rkt")

(provide rhea-tests)

(define cassowary-epsilon 1e-5)
(define (approx? a b)
  (and (>= a (- b cassowary-epsilon))
       (<= a (+ b cassowary-epsilon))))
(define (sat? solver) (check-equal? #t (send solver solve)))

(define (add-delete1-test)
  (test-case
   ""
   (define solver (new cassowary%))
   (define x (new cassowary-variable% [initial-value 100.0]))

   (send solver add-stay x)
   
   (define c10 (send x <= 10))
   (define c20 (send x <= 20))
   
   (send solver add-constraint c10)
   (send solver add-constraint c20)
   (sat? solver)
   (check approx? (send x value) 10)
   
   (send solver remove-constraint c10)
   (sat? solver)
   (check approx? (send x value) 20)
   
   (send solver remove-constraint c20)
   (sat? solver)
   (check approx? (send x value) 100)
   
   (define c10again (send x <= 10))
   (send solver add-constraint c10)
   (send solver add-constraint c10again)
   (sat? solver)
   (check approx? (send x value) 10)
   
   (send solver remove-constraint c10)
   (sat? solver)
   (check approx? (send x value) 10)
   
   (send solver remove-constraint c10again)
   (sat? solver)
   (check approx? (send x value) 100)))

(define (add-delete2-test)
  (test-case
   ""
   (define solver (new cassowary%))
   (define x (new cassowary-variable% [initial-value 100.0]))
   (define y (new cassowary-variable% [initial-value 120.0]))

   (send solver add-stay x (send solver weak-strength))
   (send solver add-stay y (send solver strong-strength))
   
   (define c10 (send x <= 10))
   (define c20 (send x <= 20))
   
   (send solver add-constraint c10)
   (send solver add-constraint c20)
   (sat? solver)
   (check approx? (send x value) 10)
   (check approx? (send y value) 120)
   
   (send solver remove-constraint c10)
   (sat? solver)
   (check approx? (send x value) 20)
   (check approx? (send y value) 120)

   (define cxy (send (send x * 2) = y))
   (send solver add-constraint cxy)
   (sat? solver)
   (check approx? (send x value) 20)
   (check approx? (send y value) 40)
   
   (send solver remove-constraint c20)
   (sat? solver)
   (check approx? (send x value) 60)
   (check approx? (send y value) 120)
   
   (send solver remove-constraint cxy)
   (sat? solver)
   (check approx? (send x value) 100)
   (check approx? (send y value) 120)
   
   (define cxy2 (send (send x * 1) = 1000))
   (send solver add-constraint cxy2)
   (sat? solver)
   (check approx? (send x value) 1000)
   (check approx? (send y value) 120)
   ))

(define (test-edit2vars)
  (define x (make-object cassowary-variable% 20))
  (define y (make-object cassowary-variable% 30))
  (define z (make-object cassowary-variable% 120))

  (define solver (new cassowary%))

  (send solver add-stay x)
  (send solver add-stay z)
  (send solver add-constraint (send z = (send (send x * 2) + y)))
  (sat? solver)
  (check approx? (send x value) 20)
  (check approx? (send y value) 80)
  (check approx? (send z value) 120)

  
  (send solver add-edit-var x)
  (send solver add-edit-var y)
  (send solver begin-edit)
  (send solver edit-value x 10)
  (send solver edit-value y 5)
  (send solver resolve)
  (check approx? (send x value) 10)
  (check approx? (send y value) 5)
  (check approx? (send z value) 25)

  (send solver edit-value x -10)
  (send solver edit-value y 15)
  (send solver resolve)
  (check approx? (send x value) -10)
  (check approx? (send y value) 15)
  (check approx? (send z value) -5)
  (send solver end-edit)
  (check approx? (send x value) -10)
  (check approx? (send y value) 15)
  (check approx? (send z value) -5)
)

(define (test-inconsistent1)
  (define x (new cassowary-variable%))
  (define solver (new cassowary%))
  (send solver add-constraint (send x = 10))
  (sat? solver)
  (check-equal? #f (send solver add-constraint (send x = 5)))
  )

(define (test-suggest)
  (define x (new cassowary-variable%))
  (define solver (new cassowary%))

  (send solver add-constraint (send x <= 10))
  (sat? solver)
  (check approx? (send x value) 10)

  (check-equal? (send solver suggest-value x 4) #t)
  (check approx? (send x value) 4)
)


(define rhea-tests 
  (test-suite
   "run all rhea tests"
   (add-delete1-test)
   (add-delete2-test)
   (test-edit2vars)
   (test-inconsistent1)
   (test-suggest)
   ))

(time (run-tests rhea-tests))
