;;; Definitions

(define lib:cons (%make-tag-aware cons))
(define lib:cons* (%make-tag-aware cons*))
(define lib:list (%make-tag-aware list))

(define lib:apply
  (%make-tag-aware
   (lambda (f . args)
     ;; The cons* allows the usage (apply f arg1 arg2 ... rest-of-args).
     (execute-application f (apply cons* args)))))

(define lib:procedure?
  (%make-tag-aware
   (lambda (f)
     (or (procedure? f)
         (tag-aware? f)
         (compound-procedure? f)
         (madlab-procedure? f)))))


;;; Bindings

(define library-exps '(

(define cons lib:cons)
(define cons* lib:cons*)
(define list lib:list)
(define apply lib:apply)
(define procedure? lib:procedure?)

(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))))

(define (filter keep? xs)
  (if (null? xs)
      '()
      (let ((x (car xs))
            (rest (filter keep? (cdr xs))))
        (if (keep? x)
            (cons x rest)
            rest))))

(define (identity x) x)

(define (any? x) #t)

(define (compose f g)
  (lambda args
    (f (apply g args))))

(define (partial-apply f . args)
  (lambda more-args
     (apply f (append args more-args))))

))
