;;; Generic operations.

(define (eval exp env)
  ((analyze exp) env))

(define execute-application
  (make-generic-operator
   2
   'execute-application
   (lambda (proc args)
     (error "Unknown procedure type" proc))))

(define analyze
  (make-generic-operator
   1
   'analyze
   (lambda (exp)
     (cond ((application? exp) (analyze-application exp))
           (else (error "Unknown expression type" exp))))))


;;; Debugging

;; It's a fluid-let hack....
(define *history* '())


;;; Tags

(define (tags x)
  (if (tagged? x)
      (%tagged-tags x)
      '()))

(define (clear-tags x)
  (if (tagged? x)
      (%tagged-data x)
      x))

(define (tag names x)
  (make-tagged (clear-tags x)
               (append names (tags x))))

(define (untag names x)
  (make-tagged (clear-tags x)
               (remove (lambda (name) (memq name names)) (tags x))))

(define (apply-tag-aware proc args)
  (apply (%tag-aware-proc proc) args))

(defhandler execute-application apply-tag-aware tag-aware?)


;;; Applications

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (fluid-let ((*history* (cons exp *history*)))
        (execute-application (fproc env)
                             (map (lambda (aproc) (aproc env))
                                  aprocs))))))

(define (primitivize-if-procedure x)
  (if (any-procedure? x)
      (lambda args
        (execute-application x args))
      x))

(define (apply-primitive proc args)
  ;; Underlying scheme doesn't know about tags,
  ;; so get rid of them.
  (apply proc (map (compose primitivize-if-procedure clear-tags) args)))

(defhandler execute-application apply-primitive primitive-procedure?)

(define (apply-tagged proc args)
  (execute-application (clear-tags proc) args))

(defhandler execute-application apply-tagged tagged?)


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
      #!unspecific)))

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

(defhandler analyze
  (compose analyze-sequence begin-actions)
  begin?)


;;; Lambda

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze (lambda-body exp))))
    (lambda (env)
      (make-compound-procedure vars bproc env))))

(defhandler analyze analyze-lambda lambda?)

(define (match-argument-trees original-vars original-vals)
  (let loop ((vars original-vars)
             (vals original-vals))
    (cond
     ((null? vars) (if (null? vals)
                       '()
                       (begin
                         (pp (list (list 'vars original-vars)
                                   (list 'vals original-vals)))
                         (error "Too many arguments given"))))
     ((pair? vars) (if (pair? vals)
                       (cons (list (car vars) (car vals))
                             (loop (cdr vars) (cdr vals)))
                       (begin
                         (pp (list (list 'vars original-vars)
                                   (list 'vals original-vals)))
                         (error "Too few arguments given"))))
     ((symbol? vars) (list (list vars vals)))
     (else (error "Bad argument variable specification")))))

(define (apply-compound proc args)
  ((compound-procedure-bproc proc)
   (extend-environment-twos
    (match-argument-trees (compound-procedure-vars proc) args)
    (compound-procedure-env proc))))

(defhandler execute-application apply-compound compound-procedure?)


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
        ;; Ignore the dummy return value of
        (if (not (equal? result #!unspecific))
            (set! *inference-ctx* (cons result *inference-ctx*)))
        result))))

(define (analyze-madsequence inherit?)
  (lambda (exps)
    (if (null? exps)
        (error "Empty sequence")
        (let* ((procs (map analyze-madstep exps))
               (seqproc (sequence-loop (car procs) (cdr procs))))
          (lambda (env)
            ;; Start with a fresh inference context.
            (fluid-let ((*inference-ctx* (if inherit?
                                             *inference-ctx*
                                             '())))
              (seqproc env)))))))

(defhandler analyze
  (compose (analyze-madsequence #f) madblock-actions)
  madblock?)

(defhandler analyze
  (compose (analyze-madsequence #t) madblock-actions)
  madblock-inherit?)


;;; Infer

;; Like match-predicates-with-arguments but only needs a unique semiperfect
;; matching (all variable-predicate pairs matched). All of args-required is
;; guaranteed to be matched, and the uniqueness checking accounts for this.
(define (infer-arguments varpreds args-required ctx printable-exp)
  (let* ((args (remove (lambda (arg) (memq arg args-required)) ctx))
         (edges (vars-to-args varpreds (append args-required args)))
         (vars (map car varpreds))
         (matching (unique-semiperfect-matching-with-required vars
                                                              args-required
                                                              args
                                                              edges)))
    (or matching
        (debug-inference vars args-required args edges printable-exp))))

(define (analyze-infer exp)
  (let ((mproc (analyze (infer-madlab exp)))
        (aproc (analyze (infer-required-args exp))))
    (lambda (env)
      (let ((madlab (mproc env)))
        ((madlab-procedure-bproc madlab)
         (extend-environment-twos
          (let ((args-required (aproc env)))
            (infer-arguments (madlab-procedure-varpreds madlab)
                             args-required
                             *inference-ctx*
                             *history*))
          (madlab-procedure-env madlab)))))))

(defhandler analyze analyze-infer infer?)


;;; Madlab (order-agnostic lambda :D)

;; Takes in an alist mapping argument variable names to predicates that they
;; must satisfy and a list of argument values. Returns a unique perfect
;; matching of the arugment variables to their matched values if such a
;; matching exists and #f otherwise.
(define (match-predicates-with-arguments varpreds args)
  (let ((vars (map car varpreds))
        (edges (vars-to-args varpreds args)))
    (or (unique-perfect-matching vars args edges)
        ;; For now, we're focusing on debugging inference.
        (begin
          (pp (list 'edges edges))
          (error "No unique matching of predicates with arguments.")))))

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


;;; And and or

(define (analyze-or-clauses clauses)
  (lambda (env)
    (let loop ((cprocs (map analyze clauses)))
      (and (not (null? cprocs))
           (or ((car cprocs) env)
               (loop (cdr cprocs)))))))

(defhandler analyze (compose analyze-or-clauses or-clauses) or?)

(define (analyze-and-clauses clauses)
  (lambda (env)
    (let loop ((cprocs (map analyze clauses)))
      (or (null? cprocs)
          (if (null? (cdr cprocs))
              ((car cprocs) env)
              (and ((car cprocs) env)
                   (loop (cdr cprocs))))))))

(defhandler analyze (compose analyze-and-clauses and-clauses) and?)


;;; Macros (definitions are in syntax.scm)

(defhandler analyze (compose analyze cond->if) cond?)

(defhandler analyze (compose analyze let->combination) let?)
