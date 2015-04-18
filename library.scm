(define library-exps '(


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
