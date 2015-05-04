;;; Generic operations.

(define (eval exp env)
  ((analyze exp) env))

(define analyze
  (make-generic-operator
   1
   'analyze
   (lambda (exp)
     (cond ((application? exp) (analyze-application exp))
           (else (error "Unknown expression type" exp))))))

(define execute-application
  (make-generic-operator
   2
   'execute-application
   (lambda (proc args)
     (error "Unknown procedure type" proc))))


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

(define (apply-tag-aware proc args)
  (apply (%tag-aware-proc proc) args))

(defhandler execute-application apply-tag-aware tag-aware?)


;;; Applications

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (apply-primitive proc args)
  ;; Underlying scheme doesn't know about tags,
  ;; so get rid of them.
  (apply proc (map untag args)))

(defhandler execute-application apply-primitive primitive-procedure?)


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
      (let ((val (vproc env)))
        (define-variable! var val env)
        ;; Fun hack: by returning val, we get nice synergy with madblocks.
        val))))

(defhandler analyze analyze-definition definition?)


;;; Begin (a.k.a. sequences)

(define (sequentially proc1 proc2)
  (lambda (env) (proc1 env) (proc2 env)))

(define (sequence-loop first-proc rest-procs)
  (if (null? rest-procs)
      first-proc
      (sequence-loop (sequentially first-proc (car rest-procs))
                     (cdr rest-procs))))

(define (analyze-sequence exps)
  (if (null? exps)
      (error "Empty sequence")
      (let ((procs (map analyze exps)))
        (sequence-loop (car procs) (cdr procs)))))

(defhandler analyze (compose analyze-sequence begin-actions) begin?)


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


;;; Madblock

(define *inference-ctx* '())

(define (vars-to-args varpreds args)
  (map (lambda (varpred)
         (cons (car varpred)
               (filter (lambda (arg)
                         ;; Use execute-application to support predicates
                         ;; defined in the interpreted language.
                         (execute-application (cadr varpred) (list arg)))
                       args)))
       varpreds))

(define (analyze-madstep exp)
  (let ((proc (analyze exp)))
    (lambda (env)
      (let ((result (proc env)))
        (set! *inference-ctx* (cons result *inference-ctx*))
        result))))

(define (analyze-madsequence exps)
  (define (add-result-to-ctx proc)
    (lambda (env)
      (let ((result (proc env)))
        (set! *inference-ctx* (cons result *inference-ctx*))
        result)))
  (if (null? exps)
      (error "Empty sequence")
      (let* ((procs (map analyze-madstep exps))
             (seqproc (sequence-loop (car procs) (cdr procs))))
        (lambda (env)
          ;; Start with a fresh inference context.
          (fluid-let ((*inference-ctx* '()))
            (seqproc env))))))

(defhandler analyze (compose analyze-madsequence madblock-actions) madblock?)


;;; Infer

;; Like match-predicates-with-arguments but only needs a unique semiperfect
;; matching (all variable-predicate pairs matched). All of args-required is
;; guaranteed to be matched, and the uniqueness checking accounts for this.
(define (infer-arguments varpreds args-required ctx)
  (let* ((args (remove (lambda (arg) (memq arg args-required)) ctx))
         (edges (vars-to-args varpreds (append args-required args)))
         (vars (map car varpreds))
         (matching (unique-semiperfect-matching-with-required vars
                                                              args-required
                                                              args
                                                              edges)))
    (or matching
        (call-with-current-continuation
         (make-inference-debugger vars args edges)))))

(define (make-inference-debugger . args) (error "TODO"))

(define (analyze-infer exp)
  (let ((mproc (analyze (infer-madlab exp)))
        (aprocs (map analyze (infer-required-args exp))))
    (lambda (env)
      (let ((madlab (mproc env)))
        ((madlab-procedure-bproc madlab)
         (extend-environment-twos
          (let ((args-required (map (lambda (aproc) (aproc env)) aprocs)))
            (infer-arguments (madlab-procedure-varpreds madlab)
                             args-required
                             *inference-ctx*))
          (madlab-procedure-env madlab)))))))

(defhandler analyze analyze-infer infer?)


;;; Madlab (order-agnostic lambda :D)

;;; Takes in an alist mapping argument variable names to predicates that they
;;; must satisfy and a list of argument values. Returns a unique perfect
;;; matching of the arugment variables to their matched values if such a
;;; matching exists and #f otherwise.
(define (match-predicates-with-arguments varpreds args)
  (let ((vars (map car varpreds))
        (edges (vars-to-args varpreds args)))
    (or (unique-perfect-matching vars args edges)
        ;; For now, we're focusing on debugging inference.
        (error "No unique matching of predicates with arguments." edges))))

(define (analyze-madlab exp)
  ;; A varpred is a two-element list (v p?).
  (let* ((varpreds (madlab-parameters exp))
         (bproc (analyze (madlab-body exp)))
         (vpprocs (map (lambda (varpred)
                         ;; The car is the variable symbol and the cadr is the
                         ;; predicate expression.
                         (let ((pproc (analyze (cadr varpred))))
                           (lambda (env)
                             (list (car varpred)
                                   (pproc env)))))
                       varpreds)))
    (lambda (env)
      (make-madlab-procedure (map (lambda (vpproc) (vpproc env)) vpprocs)
                             bproc
                             env))))

(defhandler analyze analyze-madlab madlab?)

(define (apply-madlab proc args)
  ((madlab-procedure-bproc proc)
   (extend-environment-twos
    (match-predicates-with-arguments (madlab-procedure-varpreds proc) args)
    (madlab-procedure-env proc))))

(defhandler execute-application apply-madlab madlab-procedure?)


;;; Macros (definitions are in syntax.scm)

(defhandler analyze (compose analyze cond->if) cond?)

(defhandler analyze (compose analyze let->combination) let?)
