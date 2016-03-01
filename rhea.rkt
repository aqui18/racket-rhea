#lang racket/base
(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         racket/class
         racket/runtime-path)

(provide (all-from-out racket/class))
(provide cassowary% cassowary-constraint% cassowary-expression% cassowary-variable%)

(define-runtime-path librhea "ext/librhea")

(define-ffi-definer define-rhea (ffi-lib (path->string librhea)))

(define _solver-ptr (_cpointer 'solver))
(define _variable-ptr (_cpointer 'variable))
(define _expression-ptr (_cpointer 'expression))
(define _constraint-ptr (_cpointer 'expression))
(define _strength-ptr (_cpointer 'expression))

(define-rhea solver_delete (_fun _solver-ptr -> _void) #:wrap (deallocator))
(define-rhea solver_new (_fun -> _solver-ptr) #:wrap (allocator solver_delete))
(define-rhea solver_add_constraint (_fun _solver-ptr _constraint-ptr -> _int))
(define-rhea solver_remove_constraint (_fun _solver-ptr _constraint-ptr -> _void))
(define-rhea solver_add_stay (_fun _solver-ptr _variable-ptr _strength-ptr -> _void))
(define-rhea solver_suggest (_fun _solver-ptr _variable-ptr _double -> _int))
(define-rhea solver_add_edit_var (_fun _solver-ptr _variable-ptr -> _void))
(define-rhea solver_begin_edit (_fun _solver-ptr -> _void))
(define-rhea solver_edit_value (_fun _solver-ptr _variable-ptr _double -> _void))
(define-rhea solver_resolve (_fun _solver-ptr -> _int))
(define-rhea solver_end_edit (_fun _solver-ptr -> _int))
(define-rhea solver_solve (_fun _solver-ptr -> _int))

;; variables
(define-rhea variable_new (_fun _double -> _variable-ptr))
(define-rhea variable_expression (_fun _variable-ptr -> _expression-ptr))

;; variable and expression operations
(define-rhea expression_value (_fun _expression-ptr -> _double))
(define-rhea expression_plus (_fun _expression-ptr _expression-ptr -> _expression-ptr))
(define-rhea expression_plus_double (_fun _expression-ptr _double -> _expression-ptr))
(define-rhea expression_minus (_fun _expression-ptr _expression-ptr -> _expression-ptr))
(define-rhea expression_minus_double (_fun _expression-ptr _double -> _expression-ptr))
(define-rhea expression_times (_fun _expression-ptr _expression-ptr -> _expression-ptr))
(define-rhea expression_times_double (_fun _expression-ptr _double -> _expression-ptr))
(define-rhea expression_divide (_fun _expression-ptr _expression-ptr -> _expression-ptr))
(define-rhea expression_divide_double (_fun _expression-ptr _double -> _expression-ptr))
(define-rhea expression_leq (_fun _expression-ptr _expression-ptr -> _expression-ptr))
(define-rhea expression_leq_double (_fun _expression-ptr _double -> _expression-ptr))
(define-rhea expression_geq (_fun _expression-ptr _expression-ptr -> _expression-ptr))
(define-rhea expression_geq_double (_fun _expression-ptr _double -> _expression-ptr))
(define-rhea expression_equals (_fun _expression-ptr _expression-ptr -> _expression-ptr))
(define-rhea expression_equals_double (_fun _expression-ptr _double -> _expression-ptr))

;; constraints
(define-rhea constraint_change_strength (_fun _constraint-ptr _strength-ptr -> _void))

;; strengths
(define-rhea strength_required (_fun -> _strength-ptr))
(define-rhea strength_strongest (_fun -> _strength-ptr))
(define-rhea strength_stronger (_fun -> _strength-ptr))
(define-rhea strength_strong (_fun -> _strength-ptr))
(define-rhea strength_medium (_fun -> _strength-ptr))
(define-rhea strength_weak (_fun -> _strength-ptr))
(define-rhea strength_weaker (_fun -> _strength-ptr))
(define-rhea strength_weakest (_fun -> _strength-ptr))

;; solver
(define cassowary%
  (class object%
    (define llsolver (solver_new))
    
    (super-new)
    
    (define/public (begin-edit) (solver_begin_edit llsolver))
    
    (define/public (end-edit)
      (eq? (solver_end_edit llsolver) 0))
    
    (define/public (add-edit-var var)
      (send var make-edit-var llsolver))
    
    (define/public (add-constraint c [priority (required-strength)])
      (send c enable llsolver priority))
    
    (define/public (remove-constraint c)
      (send c disable llsolver))
    
    (define/public (add-stay var [priority (weak-strength)])
      (send var add-stay-constraint llsolver priority))
    
    (define/public (suggest-value var v)
      (send var suggest-value llsolver v))
    
    (define/public (edit-value var v)
      (send var edit-value llsolver v))
    
    (define/public (solve)
      (eq? (solver_solve llsolver) 0))
    
    (define/public (resolve)
      (eq? (solver_resolve llsolver) 0))
    
    (define/public (required-strength) (strength_required))
    (define/public (strongest-strength) (strength_strongest))
    (define/public (stronger-strength) (strength_stronger))
    (define/public (strong-strength) (strength_strong))
    (define/public (medium-strength) (strength_medium))
    (define/public (weak-strength) (strength_weak))
    (define/public (weaker-strength) (strength_weaker))
    (define/public (weakest-strength) (strength_weakest))))

(define cassowary-expression%
  (class object%
    (init llptr)
    
    (super-new)
    
    (define llexpression llptr)
    
    ;; unfortunately, all of Rhea's functions for combining expressions
    ;; mutate the receiver.
    ;; TODO: learn how Racket ppl generate parameterized methods
    (define/public (+ v)
      (make-object cassowary-expression% (cond
                                           [(number? v) (expression_plus_double llexpression (exact->inexact v))]
                                           [else (expression_plus llexpression (send v get-llexpression))])))
    
    (define/public (- v)
      (make-object cassowary-expression% (cond
                                           [(number? v) (expression_minus_double llexpression (exact->inexact v))]
                                           [else (expression_minus llexpression (send v get-llexpression))])))
    
    (define/public (* v)
      (make-object cassowary-expression% (cond
                                           [(number? v) (expression_times_double llexpression (exact->inexact v))]
                                           [else (let* ((result (expression_times llexpression (send v get-llexpression))))
                                                   (if (not result)
                                                       (raise 'non-linear-constraint)
                                                       result))])))
    
    (define/public (/ v)
      (make-object cassowary-expression% (cond
                                           [(number? v) (expression_divide_double llexpression (exact->inexact v))]
                                           [else (let* ((result (expression_divide llexpression (send v get-llexpression))))
                                                   (if (not result)
                                                       (raise 'non-linear-constraint)
                                                       result))])))
    
    (define/public (<= v)
      (make-object cassowary-constraint%
        (cond
          [(number? v) (expression_leq_double llexpression (exact->inexact v))]
          [else (expression_leq llexpression (send v get-llexpression))])))
    
    (define/public (>= v)
      (make-object cassowary-constraint%
        (cond
          [(number? v) (expression_geq_double llexpression (exact->inexact v))]
          [else (expression_geq llexpression (send v get-llexpression))])))
    
    (define/public (= v)
      (make-object cassowary-constraint%
        (cond
          [(number? v) (expression_equals_double llexpression (exact->inexact v))]
          [else (expression_equals llexpression (send v get-llexpression))])))
    
    (define/public (get-llexpression)
      llexpression)
    
    (define/public (value)
      (expression_value llexpression))))

(define cassowary-variable%
  (class object%
    (init [initial-value 0.0])
    
    (super-new)
    
    (define llvariable (variable_new (exact->inexact initial-value)))
    
    (define/public (get-expression)
      (make-object cassowary-expression% (variable_expression llvariable)))
    
    (define/public (add-stay-constraint llsolver [priority (strength_weak)])
      (solver_add_stay llsolver llvariable priority))
    
    (define/public (make-edit-var llsolver)
      (solver_add_edit_var llsolver llvariable))
    
    (define/public (suggest-value llsolver v)
      (eq? (solver_suggest llsolver llvariable (exact->inexact v)) 0))
    
    (define/public (edit-value llsolver v)
      (solver_edit_value llsolver llvariable (exact->inexact v)))
    
    ;; TODO: Learn how Racket ppl generate parameterized forwarding methods
    (define/public (+ v) (send (get-expression) + v))
    (define/public (- v) (send (get-expression) - v))
    (define/public (* v) (send (get-expression) * v))
    (define/public (/ v) (send (get-expression) / v))
    (define/public (<= v) (send (get-expression) <= v))
    (define/public (>= v) (send (get-expression) >= v))
    (define/public (= v) (send (get-expression) = v))
    (define/public (value) (send (get-expression) value))
    (define/public (get-llexpression) (send (get-expression) get-llexpression))))

(define cassowary-constraint%
  (class object%
    (init constraintptr)
    
    (super-new)
    
    (define llconstraint constraintptr)
    
    (define/public (change-strength prio)
      (constraint_change_strength llconstraint prio))
    
    (define/public (enable llsolver [priority (strength_required)])
      (change-strength priority)
      (eq? 0 (solver_add_constraint llsolver llconstraint)))
    
    (define/public (disable llsolver)
      (solver_remove_constraint llsolver llconstraint))))
