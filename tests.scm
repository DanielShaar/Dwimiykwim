;;; This file is set up such that you can run the tests by just pressing M-o
;;; (xscheme-send-buffer), though you'll need to check that the answers are
;;; correct manually for now. (This is mainly to catch whether things start
;;; throwing errors.)


(load "load")


;;; MIT Scheme

(perfect-matching
 '((x1 y1 y2) (x2 y1 y2 y3) (x3 y1))
 '()
 '(x1 x2 x3)
 '(y1 y2 y3))
;Value: (((x3) (x2 y1 y2) (x1 y1)) ((y1 x3) (y3 x2) (y2 x1)))

(unique-perfect-matching
 '(x1 x2 x3)
 '(y1 y2 y3)
 '((x1 y1 y2) (x2 y1 y2 y3) (x3 y1)))
;Value: ((y1 x3) (y3 x2) (y2 x1))

(unique-perfect-matching
 '(x1 x2 x3)
 '(y1 y2 y3)
 '((x1 y1 y2) (x2 y1 y2 y3) (x3 y1 y3)))
;Value: #f

(define varpreds (list (list 'a symbol?) (list 'b number?) (list 'c string?)))
;Value: varpreds

(match-predicates-with-arguments varpreds '(1 "hi" foo))
;Value: ((a foo) (c "hi") (b 1))

(match-predicates-with-arguments varpreds '(1 foo bar))
;Value: #f


;;; Dwimiykwim

(init)

;dwimiykwim>
(define mad-map (madlab ((xs list?) (f procedure?)) (map f xs)))
;=> ok

;dwimiykwim>
(mad-map list '(1 2 3 4))
;=> ((1) (2) (3) (4))

;dwimiykwim>
(mad-map '(1 2 3 4) list)
;=> ((1) (2) (3) (4))

;dwimiykwim>
(mad-map '(1 2 3 4) (partial-apply * 4))
;=> (4 8 12 16)

;dwimiykwim>
(mad-map (partial-apply * 4) '(34 3425 254368))
;=> (136 13700 1017472)

;dwimiykwim>
((partial-apply mad-map list) '(1 2 3 4))
;=> ((1) (2) (3) (4))

;dwimiykwim>
(mad-map (partial-apply mad-map '(1 2 3 4)) (list (partial-apply * 42) list))
;=> ((42 84 126 168) ((1) (2) (3) (4)))

;dwimiykwim>
(define greeting (tag "hello" 'some 'tags))
;=> ok

;dwimiykwim>
greeting
;=> #(<tagged> "hello" (some tags))

;dwimiykwim>
(tags greeting)
;=> (some tags)

;dwimiykwim>
(untag greeting)
;=> "hello"

;dwimiykwim>
(mad-map (list (tag 4 'four) (tag 6 'six) (tag 7 'eight 'gotcha)) tags)
;=> ((four) (six) (eight gotcha))

;dwimiykwim>
(define (x-then-y (x (has-tag? 'x)) (y (has-tag? 'y)))
  (list x y))
;=> ok

;dwimiykwim>
(x-then-y (tag 4 'y) (tag 3 'x))
;=> (#(<tagged> 3 (x)) #(<tagged> 4 (y)))
