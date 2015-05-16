;;; -*- Mode:Scheme -*-

(declare (usual-integrations))

(define the-unspecified-value (list 'the-unspecified-value))


;;; Primitive procedures (inherited from Scheme)

(define primitive-procedure? procedure?)


;;; Compound procedures

(define-record-type <compound-procedure>
  (make-compound-procedure vars bproc env)
  compound-procedure?
  (vars compound-procedure-vars)
  (bproc compound-procedure-bproc)
  (env compound-procedure-env))


;;; Madlab procedures

(define-record-type <madlab-procedure>
  (make-madlab-procedure preds bproc env)
  madlab-procedure?
  (preds madlab-procedure-varpreds)
  (bproc madlab-procedure-bproc)
  (env madlab-procedure-env))


;;; Tagged data and tag-aware procedures

(define-record-type <tag-aware>
  (%make-tag-aware proc)
  tag-aware?
  (proc %tag-aware-proc))

(define-record-type <tagged>
  (%make-tagged data tags)
  tagged?
  (data %tagged-data)
  (tags %tagged-tags))

(define (make-tagged data tags)
  (if (null? tags)
      data
      (%make-tagged data tags)))


;;; Procedures in general

(define (any-procedure? f)
  (or (primitive-procedure? f)
      (tag-aware? f)
      (compound-procedure? f)
      (madlab-procedure? f)))


;;; Environments

(define (extend-environment-twos var-val-twos base-environment)
  (let ((vars-vals (list-of-twos->two-lists var-val-twos)))
    (extend-environment (car vars-vals)
                        (cadr vars-vals)
                        base-environment)))

;; Environments are chains of frames, which are made of vectors.
(define (extend-environment variables values base-environment)
  (if (fix:= (length variables) (length values))
      (vector variables values base-environment)
      (if (fix:< (length variables) (length values))
	  (error "Too many arguments supplied" variables values)
	  (error "Too few arguments supplied" variables values))))

(define (environment-variables env) (vector-ref env 0))
(define (environment-values env) (vector-ref env 1))
(define (environment-parent env) (vector-ref env 2))

(define the-empty-environment '())

(define (lookup-variable-value var env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
	(lookup-scheme-value var)
	(let scan
	    ((vars (vector-ref env 0))
	     (vals (vector-ref env 1)))
	  (cond ((null? vars) (plp (vector-ref env 2)))
		((eq? var (car vars)) (car vals))
		(else (scan (cdr vars) (cdr vals))))))))

(define (define-variable! var val env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- DEFINE" var) ;should not happen.
      (let scan
	  ((vars (vector-ref env 0))
	   (vals (vector-ref env 1)))
	(cond ((null? vars)
	       (vector-set! env 0 (cons var (vector-ref env 0)))
	       (vector-set! env 1 (cons val (vector-ref env 1))))
	      ((eq? var (car vars))
	       (set-car! vals val))
	      (else
	       (scan (cdr vars) (cdr vals)))))))

(define (set-variable-value! var val env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let scan
	    ((vars (vector-ref env 0))
	     (vals (vector-ref env 1)))
	  (cond ((null? vars) (plp (vector-ref env 2)))
		((eq? var (car vars)) (set-car! vals val))
		(else (scan (cdr vars) (cdr vals))))))))


;;; Extension to make underlying Scheme values available to interpreter

(define (lookup-scheme-value var)
  (lexical-reference generic-evaluation-environment var))
