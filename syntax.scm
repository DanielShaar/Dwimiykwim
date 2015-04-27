;;; -*- Mode:Scheme -*-

(declare (usual-integrations))


;;; Self-evaluating entities

(define (self-evaluating? exp)
  (or (number? exp)
      (eq? exp #t)
      (eq? exp #f)
      ;; Our prompt (viz., "EVAL==> ") is a string.
      (string? exp)))


;;; Variables

(define (variable? exp) (symbol? exp))
(define (same-variable? var1 var2) (eq? var1 var2))  ;; Nice abstraction


;;; Special forms (in general)

(define ((special-form? tag) exp)
  (and (pair? exp)
       (eq? (car exp) tag)))


;;; Quotations

(define quoted? (special-form? 'quote))
(define (text-of-quotation quot) (cadr quot))


;;; Assignment---SET!

(define assignment? (special-form? 'set!))
(define (assignment-variable assn) (cadr assn))
(define (assignment-value assn) (caddr assn))


;;; Definitions

(define definition? (special-form? 'define))

(define (definition-variable defn)
  (if (variable? (cadr defn))
      ;; (define foo ...)
      (cadr  defn)
      ;; (define (foo args...) body)
      (caadr defn)))

(define (definition-value defn)
  (if (variable? (cadr defn))
      ;; (define foo ...)
      (caddr defn)
      ;; (define (foo args...) body) => (define foo (lambda (args...) body))
      (cons 'lambda
            (cons (cdadr defn)
                  (cddr  defn)))))


;;; LAMBDA expressions

(define lambda? (special-form? 'lambda))
(define (lambda-parameters lambda-exp) (cadr lambda-exp))

(define (lambda-body lambda-exp)
  (let ((full-body (cddr lambda-exp)))
    (sequence->begin full-body)))

(define declaration? pair?)

(define (parameter-name var-decl)
  (if (pair? var-decl)
      (car var-decl)
      var-decl))

(define (sequence->begin seq)
  (cond ((null? seq) seq)
        ((null? (cdr seq)) (car seq))
        ((begin? (car seq)) seq)
        (else (make-begin seq))))

(define (make-begin exp) (cons 'begin exp))


;;; If conditionals

(define if? (special-form? 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'the-unspecified-value))

(define (make-if pred conseq alternative)
  (list 'IF pred conseq alternative))


;;; COND Conditionals

(define cond? (special-form? 'cond))

(define (clauses cndl) (cdr cndl))

(define (no-clauses? clauses) (null? clauses))
(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))

(define (else-clause? clause) (eq? (predicate clause) 'else))
(define (predicate clause) (car clause))
(define (actions clause) (sequence->begin (cdr clause)))

(define (cond->if cond-exp)
  (define (expand clauses)
    (cond ((no-clauses? clauses)
           (list 'error "COND: no values matched"))
          ((else-clause? (car clauses))
           (if (no-clauses? (cdr clauses))
               (actions (car clauses))
               (error "else clause isn't last -- INTERP" exp)))
          (else
           (make-if (predicate (car clauses))
                    (actions (car clauses))
                    (expand (cdr clauses))))))
  (expand (clauses cond-exp)))


;;; BEGIN expressions (a.k.a. sequences)

(define begin? (special-form? 'begin))
(define (begin-actions begin-exp) (cdr begin-exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
;; Non-tail-recursive vers.
(define no-more-exps? null?)


;;; LET expressions

(define let? (special-form? 'let))

(define (let-bound-variables let-exp) (map car (cadr let-exp)))

(define (let-values let-exp) (map cadr (cadr let-exp)))

(define (let-body let-exp) (sequence->begin (cddr let-exp)))

(define (let->combination let-exp)
  (let ((names (let-bound-variables let-exp))
        (values (let-values let-exp))
        (body (let-body let-exp)))
    (cons (list 'lambda names body) values)))


;;; Procedure applications -- NO-ARGS? and LAST-OPERAND? added

(define (application? exp)
  (pair? exp))

;; Added for tail recursion
(define (no-args? exp)
  (and (pair? exp)
       (null? (cdr exp))))

;; Changed from 5.2.1
(define (args-application? exp)
  (and (pair? exp)
       (not (null? (cdr exp)))))

(define (operator app) (car app))
(define (operands app) (cdr app))

;; Added for tail recursion
(define (last-operand? args)
  (null? (cdr args)))

(define (no-operands? args) (null? args))
(define (first-operand args) (car args))
(define (rest-operands args) (cdr args))
