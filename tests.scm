;;; This file is set up such that you can run the tests by just pressing M-o
;;; (xscheme-send-buffer), though you'll need to check that the answers are
;;; correct manually for now. (This is mainly to catch whether things start
;;; throwing errors.)

(load "load")

;Loading "load.scm"...
;  Loading "utils.scm"... done
;  Loading "bipartite-matching.scm"... done
;  Loading "ghelper.scm"... done
;  Loading "syntax.scm"... done
;  Loading "rtdata.scm"... done
;  Loading "analyze.scm"... done
;  Loading "library.scm"... done
;  Loading "repl.scm"... done
;... done
;Value 2: #[environment 2]


;;; MIT Scheme

(flip-all '((x1 y1 y2) (x2 y1 y2 y3) (x3 y1)))
;Value 3: ((y1 x3 x2 x1) (y3 x2) (y2 x2 x1))

(semiperfect-matching
 '((x1 y1 y2) (x2 y1 y2 y3) (x3 y1))
 '()
 '(x1 x2 x3)
 '(y1 y2 y3))
;Value 4: (((x3) (x2 y1 y2) (x1 y1)) ((y1 x3) (y3 x2) (y2 x1)))

(unique-perfect-matching '(foo bar)
                         '(foo bar)
                         '((foo foo) (bar bar)))
;Value 5: ((bar bar) (foo foo))

(unique-perfect-matching
 '(x1 x2 x3)
 '(y1 y2 y3)
 '((x1 y1 y2) (x2 y1 y2 y3) (x3 y1)))
;Value 6: ((x3 y1) (x2 y3) (x1 y2))

(unique-perfect-matching
 '(x1 x2 x3)
 '(y1 y2 y3)
 '((x1 y1 y2) (x2 y1 y2 y3) (x3 y1 y3)))
;Value: #f

(unique-semiperfect-matching-with-required
 '(x1 x2 x3)
 '(y4)
 '(y1 y2 y3)
 '((x1 y1 y2) (x2 y1 y2 y3) (x3 y1 y4)))
;Value: #f

(unique-semiperfect-matching-with-required
 '(x1 x2 x3)
 '(y4)
 '(y1 y2 y3)
 '((x1 y1 y2) (x2 y1 y2 y3 y4) (x3 y1)))
;Value 7: ((x3 y1) (x1 y2) (x2 y4))

(define varpreds (list (list 'a symbol?) (list 'b number?) (list 'c string?)))
;Value: varpreds

(match-predicates-with-arguments varpreds '(1 "hi" foo))
;Value 8: ((c "hi") (b 1) (a foo))


;;; Dwimiykwim

(init)

;dwimiykwim>
(define madmap (madlab ((xs list?) (f procedure?)) (map f xs)))
;=> #(<madlab-procedure> ((xs #[compiled-procedure 9 ("list" #x11) #x3 #x4d58e]) (f #[tag-aware 10])) #[compound-procedure 11])

;dwimiykwim>
(madmap list '(1 2 3 4))
;=> ((1) (2) (3) (4))

;dwimiykwim>
(madmap '(1 2 3 4) list)
;=> ((1) (2) (3) (4))

;dwimiykwim>
(madmap '(1 2 3 4) (partial-apply * 4))
;=> (4 8 12 16)

;dwimiykwim>
(madmap (partial-apply * 4) '(34 3425 254368))
;=> (136 13700 1017472)

;dwimiykwim>
((partial-apply madmap list) '(1 2 3 4))
;=> ((1) (2) (3) (4))

;dwimiykwim>
(madmap (partial-apply madmap '(1 2 3 4)) (list (partial-apply * 42) list))
;=> ((42 84 126 168) ((1) (2) (3) (4)))

;dwimiykwim>
(define greeting (~~ '(some tags) "hello"))
;=> #(<tagged> "hello" (some tags))

;dwimiykwim>
(tags greeting)
;=> (some tags)

;dwimiykwim>
(clear-tags greeting)
;=> "hello"

;dwimiykwim>
(madmap (list (~~ 'four 4) (~~ 'six 6) (~~ '(eight gotcha) 7)) tags)
;=> ((four) (six) (eight gotcha))

;dwimiykwim>
(define (x-then-y (x (has-tag? 'x)) (y (has-tag? 'y)))
  (list x y))
;=> #(<madlab-procedure> ((x #[compound-procedure 12]) (y #[compound-procedure 13])) #[compound-procedure 14])

;dwimiykwim>
(x-then-y (~~ 'y 4) (~~ 'x 3))
;=> (#(<tagged> 3 (x)) #(<tagged> 4 (y)))

;dwimiykwim>
(madblock
 42
 'its-a-trap
 (partial-apply list 'funky)
 'dont-pick-me-im-a-symbol-not-a-procedure-or-list
 '(1 3 5 7 8 why-not-9?)
 (?? madmap))
;=> ((funky 1) (funky 3) (funky 5) (funky 7) (funky 8) (funky why-not-9?))

;dwimiykwim>
(define (funky-map (xs list?) (f procedure?))
  (define thingy
    (list (?? madmap)
          (?? madmap (compose (partial-apply list 'funky) f))))
  (?? madmap thingy))
;=> #(<madlab-procedure> ((xs #[compiled-procedure 9 ("list" #x11) #x3 #x4d58e]) (f #[tag-aware 10])) #[compound-procedure 15])

;dwimiykwim>
(define (madmap-1-to-5 (f procedure?))
  (define xs '(1 2 3 4 5))
  (?? madmap))
;=> #(<madlab-procedure> ((f #[tag-aware 10])) #[compound-procedure 16])

;dwimiykwim>
(madmap (~~ 'tagged-function (lambda (x) (+ 1 x))) '(1 2 3 4 5))
;=> (2 3 4 5 6)

;dwimiykwim>
(map (~~ 'tagged-madlab
         (lambda ((x number?) (y symbol?))
           (list y x)))
     '(1 b 3 4 e)
     '(a 2 c d 5))
;=> ((a 1) (b 2) (c 3) (d 4) (e 5))

;dwimiykwim>
(funky-map cdr '((1 2 3 4 5) (6 7 8 9 10) (11 12)))
;=> (((7 8 9 10) (12)) ((funky (7 8 9 10)) (funky (12))))

;dwimiykwim>
(madmap-1-to-5 -)
;=> (-1 -2 -3 -4 -5)

;dwimiykwim>
(define (madcons (x (lambda (x) (not (list? x)))) (xs list?))
  (cons x xs))
;=> #(<madlab-procedure> ((x #[compound-procedure 18]) (xs #[compiled-procedure 9 ("list" #x11) #x3 #x4d58e])) #[compound-procedure 19])

;dwimiykwim>
(madblock
 '(1 2 3 4)
 (map (??:apply madcons) '(a b c d)))
;=> ((a 1 2 3 4) (b 1 2 3 4) (c 1 2 3 4) (d 1 2 3 4))
