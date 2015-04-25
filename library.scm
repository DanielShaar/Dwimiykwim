(define library-exps '(


(define cons tag-aware-cons)
(define cons* tag-aware-cons*)
(define list tag-aware-list)
(define apply tag-aware-apply)

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


))
