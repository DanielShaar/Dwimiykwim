(define (match args vars-to-predicates)
  ;; Takes in a list of potential arguments and an alist mapping
  ;; argument-variable names to predicates that they must satisfy. Returns a
  ;; unique perfect matching of the arguments to their location in the function
  ;; call if such a matching exists.
  (let* ((vars
          (map car vars-to-predicates))
         (args-to-vars
          (map (lambda (arg)
                 (cons arg
                       (map car
                            (filter (lambda (var-to-predicate)
                                      ((cdr var-to-predicate) arg))
                                    vars-to-predicates)
                            args))))))
    (unique-perfect-matching args vars args-to-vars)))
