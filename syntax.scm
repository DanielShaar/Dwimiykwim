;;; -*- Mode:Scheme -*-

(declare (usual-integrations))


;;; Applications

(define (application? exp) (pair? exp))
(define (operator app) (car app))
(define (operands app) (cdr app))


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

(define (special-form? tag)
  (lambda (exp)
    (and (pair? exp)
         (eq? (car exp) tag))))


;;; Quote

(define quoted? (special-form? 'quote))
(define (text-of-quotation quot) (cadr quot))


;;; Set!

(define assignment? (special-form? 'set!))
(define (assignment-variable assn) (cadr assn))
(define (assignment-value assn) (caddr assn))


;;; Definitions

(define definition? (special-form? 'define))

(define (definition-variable defn)
  (if (variable? (cadr defn))
      ;; (define foo ...)
      (cadr defn)
      ;; (define (foo args...) body)
      (caadr defn)))

(define (definition-value defn)
  (if (variable? (cadr defn))
      ;; (define foo ...)
      (caddr defn)
      ;; (define (foo args...) body) => (define foo (lambda (args...) body))
      (cons 'lambda
            (cons (cdadr defn)
                  (cddr defn)))))

(define (make-define name body) (cons 'define name body))

;;; Begin (a.k.a. sequences)

(define begin? (special-form? 'begin))
(define (begin-actions begin-exp) (cdr begin-exp))
(define (make-begin exp) (cons 'begin exp))


;;; Lambda

(define (sequence->begin seq)
  (cond ((null? seq) seq)
        ((null? (cdr seq)) (car seq))
        ((begin? (car seq)) seq)
        (else (make-begin seq))))

(define lambda? (special-form? 'lambda))
(define lambda-parameters cadr)
(define lambda-body (compose sequence->begin cddr))


;;; Madblock

(define madblock? (special-form? 'madblock))
(define madblock-inherit? (special-form? 'madblock-inherit))
(define madblock-actions cdr)
(define (make-madblock exp) (cons 'madblock exp))


;;; Infer

(define infer? (special-form? 'infer))
(define infer-madlab cadr)
(define (infer-required-args exp)
  (if (>= (length exp) 3)
      (caddr exp)
      ''()))


;;; Madlab (order-agnostic lambda :D)

(define (madlab? exp)
  (or ((special-form? 'madlab) exp)
      (and (lambda? exp)
           (list? (lambda-parameters exp))
           (every pair? (lambda-parameters exp)))))
;; (define madlab? (special-form? 'madlab))

;; This is the same as lambda for now.
(define madlab-parameters lambda-parameters)

(define (madlab-body exp)
  ;; Put the parameter names at the beginning of the madblock so the madlab
  ;; inputs are available for inference.
  (make-madblock (append (map car (madlab-parameters exp)) (cddr exp))))


;;; If

(define if? (special-form? 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'the-unspecified-value))

(define (make-if pred conseq alternative)
  (list 'IF pred conseq alternative))


;;; Cond

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


;;; Let

(define let? (special-form? 'let))

(define (let-bound-variables let-exp) (map car (cadr let-exp)))

(define (let-values let-exp) (map cadr (cadr let-exp)))

(define (let-body let-exp) (sequence->begin (cddr let-exp)))

(define (let->combination let-exp)
  (let ((names (let-bound-variables let-exp))
        (values (let-values let-exp))
        (body (let-body let-exp)))
    (cons (list 'lambda names body) values)))
