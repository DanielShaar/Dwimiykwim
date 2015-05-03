;;;; Read-eval-print loop for extended Scheme interpreter

(declare (usual-integrations write write-line pp eval))

(define printable-representation
  (make-generic-operator 1 'printable-representation identity))

(define write
  (compose (access write user-initial-environment)
           printable-representation))

(define write-line
  (compose (access write-line user-initial-environment)
           printable-representation))

(define pp
  (compose (access pp user-initial-environment)
           printable-representation))

(defhandler printable-representation
  (lambda (procedure)
    (vector '<compound-procedure>
            (compound-procedure-vars procedure)
            (compound-procedure-bproc procedure)))
  compound-procedure?)

(defhandler printable-representation
  (lambda (procedure)
    (vector '<madlab-procedure>
            (madlab-procedure-varpreds procedure)
            (madlab-procedure-bproc procedure)))
  madlab-procedure?)

(defhandler printable-representation
  (lambda (x)
    (vector '<tagged>
            (%tagged-data x)
            (%tagged-tags x)))
  tagged?)

(defhandler printable-representation
  (lambda (x)
    (cons (printable-representation (car x))
          (printable-representation (cdr x))))
  pair?)

(define the-global-environment 'not-initialized)

(define (init)
  (set! the-global-environment
        (extend-environment '() '() the-empty-environment))
  (for-each (lambda (exp) (eval exp the-global-environment)) library-exps)
  (repl))

(define (repl)
  (if (eq? the-global-environment 'not-initialized)
	  (error "Interpreter not initialized. Run (init) first."))
  (let* ((input (prompt-for-command-expression ";dwimiykwim>\n"))
         (output (eval input the-global-environment)))
    (write-string ";=> ")
    (write-line output)
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
