;;;; Definitions


;;; MIT Scheme

(define lib:cons (%make-tag-aware cons))
(define lib:cons* (%make-tag-aware cons*))
(define lib:list (%make-tag-aware list))

(define lib:apply
  (%make-tag-aware
   (lambda (f . args)
     ;; The cons* allows the usage (apply f arg1 arg2 ... rest-of-args).
     (execute-application f (apply cons* args)))))

;; It's not tag-aware, so it strips tags, which is what we want.
(define lib:procedure? any-procedure?)


;;; Dwimiykwim

(define lib:tag (%make-tag-aware tag))
(define lib:tags (%make-tag-aware tags))
(define lib:untag (%make-tag-aware untag))
(define lib:clear-tags identity)


;;;; Bindings


(define library-exps '(


;;; MIT Scheme

(define cons lib:cons)
(define cons* lib:cons*)
(define list lib:list)
(define apply lib:apply)
(define procedure? lib:procedure?)


;;; Utils

(define (identity x) x)

(define (any? x) #t)

(define (compose f g)
  (lambda args
    (f (apply g args))))

(define (partial-apply f . args)
  (lambda more-args
     (apply f (append args more-args))))


;;; Dwimiykwim

(define tag lib:tag)
(define tags lib:tags)
(define untag lib:untag)
(define clear-tags lib:clear-tags)

(define (~~? name)
  (lambda (x)
    (member name (tags x))))

(define (?? proc . args)
  (infer proc args))

(define (??:apply proc . args)
  (lambda more-args
    (infer proc (append args more-args))))

(define (~~ names x)
  (tag (if (list? names)
           names
           (list names))
       x))

(define (~~:delq names x)
  (untag (if (list? names)
             names
             (list names))
         x))

))
