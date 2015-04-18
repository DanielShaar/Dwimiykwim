;;;; Separating analysis from execution.
;;;   Generic analysis, but not prepared for
;;;   extension to handle nonstrict operands.


(define (eval exp env)
  ((analyze exp) env))

(define analyze
  (make-generic-operator
   1 'analyze
   (lambda (exp)
     (cond ((application? exp) (analyze-application exp))
           (else (error "Unknown expression type" exp))))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(defhandler analyze analyze-self-evaluating self-evaluating?)


(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(defhandler analyze analyze-quoted quoted?)


(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(defhandler analyze analyze-variable variable?)


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (pproc env) (cproc env) (aproc env)))))

(defhandler analyze analyze-if if?)


(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze (lambda-body exp))))
    (lambda (env)
      (make-compound-procedure vars bproc env))))

(defhandler analyze analyze-lambda lambda?)


(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define execute-application
  (make-generic-operator
   2 'execute-application
   (lambda (proc args)
     (error "Unknown procedure type" proc))))

(defhandler execute-application
  (lambda (proc args)
    ;; Underlying scheme doesn't know about tags,
    ;; so get rid of them.
    (apply-primitive-procedure proc (map tag-unwrap args)))
  strict-primitive-procedure?)


(define (match-arguments vars vals)
  (cond
   ((null? vars) (if (null? vals)
                     '()
                     (error "Too many arguments given" vars vals)))
   ((pair? vars) (if (pair? vals)
                     (cons (cons (car vars) (car vals))
                           (match-arguments (cdr vars) (cdr vals)))
                     (error "Too few arguments given" vars vals)))
   ((symbol? vars) (list (cons vars vals)))
   (else (error "Bad argument variable specification"))))

(define (list-of-pairs->pair-of-lists pairs)
  (cons (map car pairs) (map cdr pairs)))

(defhandler execute-application
  (lambda (proc args)
    ((procedure-body proc)
     (let ((vars-vals
            (list-of-pairs->pair-of-lists
             (match-arguments (procedure-parameters proc) args))))
       (extend-environment
        (car vars-vals)
        (cdr vars-vals)
        (procedure-environment proc)))))
  compound-procedure?)


(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (if (null? exps) (error "Empty sequence"))
  (let ((procs (map analyze exps)))
    (loop (car procs) (cdr procs))))

(defhandler analyze
  (lambda (exp)
    (analyze-sequence (begin-actions exp)))
  begin?)


(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(defhandler analyze analyze-assignment assignment?)


(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(defhandler analyze analyze-definition definition?)


(define *tag* '*tag*)

(define (tagged? x)
  (and (pair? x) (eq? *tag* (car x))))

(define (tag-unwrap x)
  (if (tagged? x)
      (caddr x)
      x))

(define tags
  (make-tag-aware
   (lambda (x)
     (if (tagged? x)
         (cadr x)
         '()))))

(define tag
  (make-tag-aware
   (lambda (x . names)
     (list *tag* (append names ((tag-aware-unwrap tags) x)) (tag-unwrap x)))))

(defhandler execute-application
  (lambda (proc args)
    (apply-primitive-procedure (tag-aware-unwrap proc) args))
  tag-aware-procedure?)

;; TODO: make this work: (apply f arg1 arg2 rest-of-args).
(define apply (make-tag-aware execute-application))

;; TODO: fix this.
(define cons
  (make-tag-aware cons))


;;; Macros (definitions are in syntax.scm)

(defhandler analyze (compose analyze cond->if) cond?)

(defhandler analyze (compose analyze let->combination) let?)
