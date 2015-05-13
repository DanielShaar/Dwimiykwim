(define (index-ctx ctx)
  (map cons ctx (iota (length ctx))))

(define (get-indices ctx-values indexed-ctx)
  (map (lambda (ctx-value) (cdr (assq ctx-value indexed-ctx))) ctx-values))

(define (split-by-indices ctx-values indexed-ctx)
  (map (lambda (ctx-value) (cdr (assq ctx-value indexed-ctx))) ctx-values))

(define (prompt-with-expr-until-valid expr pred)
  (let loop ((input (prompt-for-command-expression expr)))
    (if (pred input)
        input
        (begin
          (pp "Invalid required list!")
          (loop (prompt-for-command-expression expr))))))

(define (update-inference indexed-ctx vars edges)
  (let* ((new-required
          (prompt-with-expr-until-valid "New required:\n"
                                        (lambda (index-list)
                                          (and (list? index-list)
                                               (every number? index-list)))))
         (ctx-partition ((partition (lambda (value-index)
                                      (memq (cdr value-index) new-required))
                                    indexed-ctx)
                         list))
         (new-args-required (map car (car ctx-partition)))
         (new-args (map car (cadr ctx-partition))))
    (if (unique-semiperfect-matching-with-required vars
                                                   new-args-required
                                                   new-args
                                                   edges)
        (begin
          (write-string "Unambiguous matching! Terminate debugging mode!\n")
          (error "Disambiguate code and run again!"))
        (begin
          (write-string "Ambiguous matching!\n")
          (update-inference indexed-ctx vars edges)))))

;; Takes in the context, vars, args, and edges, and returns and prints out a
;; list of the vars along with the ids and items in the context each var
;; corresponds to.
(define (debug-inference vars args-required args edges printable-exp)
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
    (write-string "=== Dwimiykwim Tawimiydkwim ===\n")
    (newline)
    ;; Removed for now because the expression isn't yet useful when we use ??
    ;; instead of infer.
    ;; (write-string "Expression:\n")
    ;; (pp printable-exp)
    ;; (newline)
    (write-string "Context:\n")
    (for-each (lambda (value-index)
                (pp (list (cdr value-index) (car value-index))))
              indexed-ctx)
    (newline)
    (write-string "Edges:\n")
    (for-each pp vars-to-indices)
    (newline)
    (write-string "Required:\n")
    (pp required)
    (newline)
    (update-inference indexed-ctx vars edges)))
