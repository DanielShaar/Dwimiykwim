;;; Generic operations.

(define (eval exp env)
  ((analyze exp) env))

(define analyze
  (make-generic-operator
   1 'analyze
   (lambda (exp)
     (cond ((application? exp) (analyze-application exp))
           (else (error "Unknown expression type" exp))))))

(define execute-application
  (make-generic-operator
   2 'execute-application
   (lambda (proc args)
     (error "Unknown procedure type" proc))))


;;; Applications

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(defhandler execute-application
  (lambda (proc args)
    ;; Underlying scheme doesn't know about tags,
    ;; so get rid of them.
    (apply-primitive-procedure proc (map untag args)))
  strict-primitive-procedure?)


;;; Self-evaluating entities

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(defhandler analyze analyze-self-evaluating self-evaluating?)


;;; Variables

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(defhandler analyze analyze-variable variable?)


;;; Quote

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(defhandler analyze analyze-quoted quoted?)


;;; If

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (pproc env) (cproc env) (aproc env)))))

(defhandler analyze analyze-if if?)


;;; Set!

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(defhandler analyze analyze-assignment assignment?)


;;; Definitions

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(defhandler analyze analyze-definition definition?)


;;; Lambda

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze (lambda-body exp))))
    (lambda (env)
      (make-compound-procedure vars bproc env))))

(defhandler analyze analyze-lambda lambda?)

(define (match-argument-trees vars vals)
  (cond
   ((null? vars) (if (null? vals)
                     '()
                     (error "Too many arguments given" vars vals)))
   ((pair? vars) (if (pair? vals)
                     (cons (list (car vars) (car vals))
                           (match-argument-trees (cdr vars) (cdr vals)))
                     (error "Too few arguments given" vars vals)))
   ((symbol? vars) (list (list vars vals)))
   (else (error "Bad argument variable specification"))))

(defhandler execute-application
  (lambda (proc args)
    ((compound-procedure-bproc proc)
     (extend-environment-twos
      (match-argument-trees (compound-procedure-vars proc) args)
      (compound-procedure-env proc))))
  compound-procedure?)


;;; Madlab (order-agnostic lambda :D)

;;; Takes in an alist mapping argument variable names to predicates that they
;;; must satisfy and a list of argument values. Returns a unique perfect
;;; matching of the arugment variables to their matched values if such a
;;; matching exists and #f otherwise.
(define (match-predicates-with-arguments varpreds args)
  (let* ((vars-to-args
          (map (lambda (varpred)
                 (cons (car varpred)
                       (filter (lambda (arg)
                                 ;; Use execute-application to support
                                 ;; predicates defined in the interpreted
                                 ;; language.
                                 (execute-application (cadr varpred)
                                                      (list arg)))
                               args)))
               varpreds)))
    (unique-perfect-matching (map car varpreds) args vars-to-args)))

(define (analyze-madlab exp)
  ;; A varpred is a two-element list (v p?).
  (let* ((params (madlab-parameters exp))
         (bproc (analyze (madlab-body exp)))
         (varpreds (lambda (env)
                     (map (lambda (varpred)
                            (list (car varpred)
                                  ((analyze (cadr varpred)) env)))
                          params))))
    (lambda (env)
      (make-madlab-procedure (varpreds env) bproc env))))

(defhandler analyze analyze-madlab madlab?)

(defhandler execute-application
  (lambda (proc args)
    ((madlab-procedure-bproc proc)
     (extend-environment-twos
      (match-predicates-with-arguments (madlab-procedure-varpreds proc) args)
      (madlab-procedure-env proc))))
  madlab-procedure?)


;;; Begin (a.k.a. sequences)

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


;;; Tags

(define (tags x)
  (if (tagged? x)
      (%tagged-tags x)
      '()))

(define (untag x)
  (if (tagged? x)
      (%tagged-data x)
      x))

(define (tag x . names)
  (%make-tagged (untag x)
                (append names (tags x))))

(defhandler execute-application
  (lambda (proc args)
    (apply-primitive-procedure (%tag-aware-proc proc) args))
  tag-aware?)


;;; Macros (definitions are in syntax.scm)

(defhandler analyze (compose analyze cond->if) cond?)

(defhandler analyze (compose analyze let->combination) let?)
