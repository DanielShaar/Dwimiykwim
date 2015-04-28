;;; MIT Scheme tests

(define args '(1 "hi" foo))
;Value: args

(define varpreds (list (list 'a symbol?) (list 'b number?) (list 'c string?)))
;Value: varpreds

(match-predicates-with-arguments varpreds args)
;Value: ((a foo) (c "hi") (b 1))


;;; Dwimiykwim tests

(init)

; dwimiykwim>
(define mad-map (madlab ((xs list?) (f procedure?)) (map f xs)))
; => ok

; dwimiykwim>
(mad-map list '(1 2 3 4))
; => ((1) (2) (3) (4))

; dwimiykwim>
(mad-map '(1 2 3 4) list)
; => ((1) (2) (3) (4))

; dwimiykwim>
(mad-map '(1 2 3 4) (partial-apply * 4))
; => (4 8 12 16)

; dwimiykwim>
(mad-map (partial-apply * 4) '(34 3425 254368))
; => (136 13700 1017472)

; dwimiykwim>
((partial-apply mad-map list) '(1 2 3 4))
; => ((1) (2) (3) (4))

; dwimiykwim>
(mad-map (partial-apply mad-map '(1 2 3 4)) (list (partial-apply * 42) list))
; => ((42 84 126 168) ((1) (2) (3) (4)))
