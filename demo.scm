(define (exp? exp)
  (null? (tags exp)))

(define (binop? exp)
  (if (pair? exp)
      (member (car exp) '(+ - * /))
      #f))

(define (binop-op (exp exp?)) (car exp))
(define (binop-left (exp exp?)) (cadr exp))
(define (binop-right (exp exp?)) (caddr exp))

(define (apply-binop (op symbol?)
                     (left (has-tag? 'left))
                     (right (has-tag? 'right)))
  ((cadr (assoc op
                (list (list '+ +)
                      (list '- -)
                      (list '* *)
                      (list '/ /))))
   left
   right))

;; ;dwimiykwim>
;; (apply-binop '+ (tag 4 'left) (tag 2 'right))
;; ;=> 6

;; ;dwimiykwim>
;; (apply-binop '- (tag 4 'left) (tag 2 'right))
;; ;=> 2

;; ;dwimiykwim>
;; (apply-binop '- (tag 4 'right) (tag 2 'left))
;; ;=> -2

(define (eval (exp exp?))
  (cond
   ((number? exp)
    exp)
   ((binop? exp)
    (madblock
     exp
     (tag (eval (infer binop-left)) 'left)
     (tag (eval (infer binop-right)) 'right)
     (infer binop-op)
     (infer apply-binop)))))
