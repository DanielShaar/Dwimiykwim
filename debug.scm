(define (index-ctx ctx)
  (let loop ((new-ctx '())
             (rem-ctx ctx)
             (index 0))
    (if (null? rem-ctx)
        new-ctx
        (loop (cons (cons (car rem-ctx) index) new-ctx)
              (cdr rem-ctx)
              (+ index 1)))))

(define (get-index ctx-value indexed-ctx)
  (let loop ((rem-ctx indexed-ctx))
    (if (eqv? ctx-value (caar rem-ctx))
        (cdar rem-ctx)
        (loop (cdr rem-ctx)))))

(define (get-indeces ctx-values indexed-ctx)
  (map (lambda (ctx-value) (get-index ctx-value indexed-ctx)) ctx-values))

;; Takes in the context, vars, args, and edges, and returns and prints out a
;; list of the vars along with the ids and items in the context each var
;; corresponds to.
(define (debug-inference ctx vars args edges)
  (let ((indexed-ctx (index-ctx ctx)))
    (map
     (lambda (var)
       (pp (list var (get-indeces (cadr (assq var edges)) indexed-ctx))))
     vars)))
    
