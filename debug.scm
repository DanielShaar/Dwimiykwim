(define (index-ctx ctx)
  (map cons ctx (iota (length ctx))))

(define (get-indeces ctx-values indexed-ctx)
  (map (lambda (ctx-value) (cdr (assq ctx-value indexed-ctx))) ctx-values))

;; Takes in the context, vars, args, and edges, and returns and prints out a
;; list of the vars along with the ids and items in the context each var
;; corresponds to.
(define (debug-inference ctx vars args edges)
  (let ((indexed-ctx (index-ctx ctx)))
    (for-each
     (lambda (var)
       (pp (list var (get-indeces (cdr (assq var edges)) indexed-ctx))))
     vars)))
    
