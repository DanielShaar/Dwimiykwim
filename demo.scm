;;;; Demo: an evaluator for arithmetic expressions with let bindings


;;; Arithmetic

(define (binop? exp)
  (and (pair? exp)
       (member (car exp) '(+ - * /))))

(define (binop-op (exp binop?))
  (car exp))

(define (binop-left (exp binop?))
  (cadr exp))

(define (binop-right (exp binop?))
  (caddr exp))

(define (apply-binop (op symbol?)
                     (left (~~? 'left))
                     (right (~~? 'right)))
  ((cadr (assq op
               (list (list '+ +)
                     (list '- -)
                     (list '* *)
                     (list '/ /))))
   left
   right))

;; ;dwimiykwim>
;; (apply-binop '+ (~~ 'left 4) (~~ 'right 2))
;; ;=> 6

;; ;dwimiykwim>
;; (apply-binop '- (~~ 'left 4) (~~ 'right 2))
;; ;=> 2

;; ;dwimiykwim>
;; (apply-binop '- (~~ 'right 4) (~~ 'left 2))
;; ;=> -2

(define (exp? x)
  (null? (tags x)))

(define (primitive? (exp exp?))
  (number? exp))

(define (operation? (exp exp?))
  (binop? exp))

(define (eval (exp exp?))
  (cond
   ((?? primitive?)
    exp)
   ((?? operation?)
    (madblock-inherit
     (~~ 'left (?? eval (?? binop-left)))
     (~~ 'right (?? eval (?? binop-right)))
     (?? binop-op)
     (?? apply-binop)))))

;; ;dwimiykwim>
;; (eval '(- (+ 3 4) 9))
;; ;=> -2


;;; Let bindings

(define ctx?
  (~~? 'ctx))

(define empty-ctx
  (~~ 'ctx '()))

(define (list-of p?)
  (lambda (xs)
    (and (list? xs)
         (every p? xs))))

(define (extend-ctx (ctx ctx?)
                    (vars (list-of symbol?))
                    (vals (list-of number?)))
  (~~ 'ctx (append (map list vars vals)
                   (~~:delq 'ctx ctx))))

(define (lookup (ctx ctx?)
                (var symbol?))
  (cadr (assq var ctx)))

(define (let? exp)
  (and (pair? exp)
       (eq? (car exp) 'let)))

(define (let-vars (exp let?))
  (map car (cadr exp)))

(define (let-vals (exp let?))
  (map cadr (cadr exp)))

(define (let-body (exp let?))
  (caddr exp))

;; ;dwimiykwim>
;; (let-vars '(let ((x (+ 2 2))) (+ x 2)))
;; ;=> (x)

;; ;dwimiykwim>
;; (let-vals '(let ((x (+ 2 2))) (+ x 2)))
;; ;=> ((+ 2 2))

;; ;dwimiykwim>
;; (let-body '(let ((x (+ 2 2))) (+ x 2)))
;; ;=> (+ x 2)

;; ;dwimiykwim>
;; (let? '(let ((x (+ 2 2))) (+ x 2)))
;; ;=> #t

;; ;dwimiykwim>
;; (let? '(not-let ((x (+ 2 2))) (+ x 2)))
;; ;=> #f

(define (variable? (exp exp?))
  (symbol? exp))

(define (declaration? (exp exp?))
  (let? exp))

(define (eval (ctx ctx?)
              (exp exp?))
  (cond
   ((?? primitive?)
    exp)
   ((?? operation?)
    (madblock-inherit
     (~~ 'left (?? eval (?? binop-left)))
     (~~ 'right (?? eval (?? binop-right)))
     (?? binop-op)
     (?? apply-binop)))
   ((?? variable?)
    (?? lookup))
   ((?? declaration?)
    (madblock-inherit
     (map (??:apply eval) (?? let-vals))
     (?? let-vars)
     (eval (?? extend-ctx) (?? let-body))))))

;; ;dwimiykwim>
;; (eval empty-ctx '(let ((x (+ 2 2)) (y (- 6 3))) (+ (* x x) (* y y))))
;; ;=> 25
