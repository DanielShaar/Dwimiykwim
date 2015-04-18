;;;; Read-eval-print loop for extended Scheme interpreter

(declare (usual-integrations write write-line pp eval))

(define write
  (make-generic-operator 1 'write
                         (access write user-initial-environment)))

(define write-line
  (make-generic-operator 1 'write-line
                         (access write-line user-initial-environment)))

(define pp
  (make-generic-operator 1 'pretty-print
                         (access pp user-initial-environment)))

(define (procedure-printable-representation procedure)
  `(compound-procedure
    ,(procedure-parameters procedure)
    ,(procedure-body procedure)
    <procedure-environment>))

(defhandler write
  (compose write procedure-printable-representation)
  compound-procedure?)

(defhandler write-line
  (compose write-line procedure-printable-representation)
  compound-procedure?)

(defhandler pp
  (compose pp procedure-printable-representation)
  compound-procedure?)

(define the-global-environment 'not-initialized)

(define (init)
  (set! the-global-environment
        (extend-environment '() '() the-empty-environment))
  (for-each (lambda (exp) (eval exp the-global-environment)) library-exps)
  (repl))

(define (repl)
  (if (eq? the-global-environment 'not-initialized)
	  (error "Interpreter not initialized. Run (init) first."))
  (let ((input (prompt-for-command-expression "eval> ")))
    (write-line (eval input the-global-environment))
    (repl)))

(define go repl)

(define (load-library filename)
  (if (eq? the-global-environment 'not-initialized)
      (error "Interpreter not initialized. Run (init) first."))
  (with-input-from-file filename
    (lambda ()
      (let lp ((input (read)))
        (if (not (eof-object? input))
            (begin
              (write-line (eval input the-global-environment))
              (lp (read)))
            'done)))))
