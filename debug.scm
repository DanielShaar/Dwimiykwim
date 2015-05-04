(define (index-ctx ctx)
  (map cons ctx (iota (length ctx))))

(define (get-indices ctx-values indexed-ctx)
  (map (lambda (ctx-value) (cdr (assq ctx-value indexed-ctx))) ctx-values))

(define (split-by-indices ctx-values indexed-ctx)
  (map (lambda (ctx-value) (cdr (assq ctx-value indexed-ctx))) ctx-values))

filter

;; Takes in the context, vars, args, and edges, and returns and prints out a
;; list of the vars along with the ids and items in the context each var
;; corresponds to.
(define (debug-inference vars args-required args edges)
  (let* ((ctx (append args-required args))
         (indexed-ctx (index-ctx (append args-required args)))
         (required (get-indices args-required indexed-ctx))
         (optional (get-indices args indexed-ctx))
         (vars-to-indices (map (lambda (var)
                                 (cons var
                                       (get-indices (cdr (assq var edges))
                                                    indexed-ctx)))
                               vars)))
    (newline)
    ;; Tawimiydkwim = then ask what I mean if you don't know what I mean.
    (write-string "=== Dwimiykwim Tawimiydkwim ===")
    (newline)
    (newline)
    (write-string "Context:\n")
    (for-each (lambda (value-index)
                (pp (list (cdr value-index) (car value-index))))
              indexed-ctx)
    (newline)
    (write-string "Edges:\n")
    (for-each pp vars-to-indices)
    (newline)
    ;; TODO: factor out this part.
    (write-string "Required:\n")
    (pp required)
    (newline)
    (let* ((new-required (prompt-for-command-expression "New required:\n"))
           (ctx-partition ((partition (lambda (value-index)
                                        (memq (cdr value-index) new-required))
                                      indexed-ctx)
                           list))
           (new-args-required (map car (car ctx-partition)))
           (new-args (map car (cadr ctx-partition))))
      (debug-inference vars new-args-required new-args edges))))
