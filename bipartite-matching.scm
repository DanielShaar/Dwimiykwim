;;; One of two entry points for this file. Takes two lists of vertices, xs and
;;; ys, and an alist of pairs (x . neighbors-of-x). Returns a list of
;;; two-element lists (x y) representing the unique perfect matching, if it
;;; exists, where x is from xs and y is from ys, and returns #f otherwise.
(define (unique-perfect-matching xs ys xs-to-ys)
  ;; We restrict ourselves to having exactly as many xs as ys. The algorithm
  ;; can miss additional matchings if there are more ys than xs, so this
  ;; restriction is important.
  (and-let*
   ((matching (and (= (length xs) (length ys))
                   (semiperfect-matching xs-to-ys '() xs ys)))
    (xs-to-ys (car matching))
    ;; Don't accept if there's another perfect matching.
    ((not (any (lambda (edge-rest)
                 (let ((ys-to-xs (cadr edge-rest))
                       (x (cadar edge-rest))
                       (y (caar edge-rest)))
                   ;; Start where we left off (same xs-to-ys) but with a single
                   ;; edge from the matching removed (modified ys-to-xs) and
                   ;; the vertices that were touching that edge now exposed
                   ;; (both xs and ys are singletons). This works because the
                   ;; modified matching has an augmenting path if and only if
                   ;; it is not maximum.
                   (augmenting-path xs-to-ys ys-to-xs x (list y))))
               ;; Iterate through the ys-to-xs from the matching. Each y-to-xs
               ;; is a (y), meaning y is unmatched, or a (y x), meaning y is
               ;; matched to x. We only want to look at the matched ones.
               (possible-selections (filter (lambda (y-to-xs)
                                              (= (length y-to-xs) 2))
                                            (cadr matching)))))))
   (map reverse (cadr matching))))

;;; Like unique-perfect-matching except there can be more ys than xs and we can
;;; require some of those ys to be in the matching (all xs are already required
;;; to be in the matching). That is,
;;;     (unique-semiperfect-matching-with-required '() xs ys xs-to-ys)
;;; returns the same result as
;;;     (unique-perfect-matching xs ys xs-to-ys)
;;; if xs and ys are equal length, though it's likely slower.
(define (unique-semiperfect-matching-with-required xs ys-required ys xs-to-ys)
  ;; Start by flipping the edges because we're matching ys (specifically, the
  ;; required ones) to xs.
  (and-let*
   ((ys-to-xs (flip-all xs-to-ys))
    (required-matching (semiperfect-matching ys-to-xs '() ys-required xs))
    ;; Flip the edges back for matching xs to ys.
    (xs-to-ys (flip-all (car required-matching)))
    (ys-to-xs (flip-all (cadr required-matching)))
    (xs-exposed (exposed-xs ys-to-xs xs))
    (matching (semiperfect-matching xs-to-ys ys-to-xs xs-exposed ys))
    (xs-to-ys (car matching))
    ;; Don't accpet if there's another semiperfect matching of xs that matches
    ;; all of ys-required.
    ((not (any (lambda (edge-rest)
                 (let ((ys-to-xs (cadr edge-rest))
                       (x (cadar edge-rest))
                       (y (caar edge-rest)))
                   (if (memq y ys-required)
                       ;; The y is required, so look for an augmenting path
                       ;; from y to x, which involves flipping the graph.
                       (let ((xs-to-ys (flip-all ys-to-xs))
                             (ys-to-xs (flip-all xs-to-ys)))
                         (augmenting-path ys-to-xs xs-to-ys y (list x)))
                       ;; The y isn't required, so look for an augmenting path
                       ;; from x to any exposed y.
                       (let ((ys (exposed-ys ys-to-xs ys)))
                         (augmenting-path xs-to-ys ys-to-xs x ys)))))
               (possible-selections (filter (lambda (y-to-xs)
                                              (= (length y-to-xs) 2))
                                            (cadr matching)))))))
   (map reverse (cadr matching))))

(define (possible-selections xs)
  ;; Sends '(a b c d) to '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c))).
  (let loop ((xs-head-rev '())
             (xs-tail xs))
    (if (null? xs-tail)
        '()
        (let ((x (car xs-tail))
              (xs-tail-new (cdr xs-tail)))
          (cons (list x (append (reverse xs-head-rev) xs-tail-new))
                (loop (cons x xs-head-rev) xs-tail-new))))))

(define (exposed-xs ys-to-xs xs)
  (remove (lambda (x)
            ;; Is x the neighbor of any y?
            (memq x (apply append (map cdr ys-to-xs))))
          xs))

(define (exposed-ys ys-to-xs ys)
  (remove (lambda (y) (assq y ys-to-xs)) ys))

;;; Returns a matching in the form of new xs-to-ys and ys-to-xs in which every
;;; x is matched or #f if no such matching exists. In particular, ys-to-xs has
;;; an edge from each matched y to its matched x.
(define (semiperfect-matching xs-to-ys ys-to-xs xs-exposed ys-exposed)
  (cond
   ;; We've matched all the xs. There's exactly one edge from each matched y to
   ;; the x its matched with, so the matching can be extracted from ys-to-xs,
   ;; but we need xs-to-ys for unique-semiperfect-matching-with-required.
   ((null? xs-exposed) (list xs-to-ys ys-to-xs))
   ;; We ran out of ys before running out of xs.
   ((null? ys-exposed) #f)
   (else
    (let* ((x (car xs-exposed))
           (new-xs (cdr xs-exposed))
           ;; Augmenting path from y to x for some exposed y.
           (path (augmenting-path xs-to-ys ys-to-xs x ys-exposed)))
      (and path
           (let* ((new-edges (flip-path xs-to-ys ys-to-xs path))
                  (new-xs-to-ys (car new-edges))
                  (new-ys-to-xs (cadr new-edges))
                  (y (car path))
                  (new-ys (delq y ys-exposed)))
             (semiperfect-matching new-xs-to-ys new-ys-to-xs new-xs new-ys)))))))

(define (odds xs)
  ;; We 0-index lists, obviously.
  (cond
   ((null? xs) '())
   ((null? (cdr xs)) '())
   (else (cons (cadr xs) (odds (cddr xs))))))

(define (augmenting-path xs-to-ys ys-to-xs x ys-exposed)
  (define (search-x old-path x)
    (if (memq x (odds old-path))
        #f
        (let ((new-path (cons x old-path))
              (x-to-ys (assq x xs-to-ys)))
          (and x-to-ys
               (any (partial-apply search-y new-path)
                    (cdr x-to-ys))))))
  (define (search-y old-path y)
    (if (memq y (odds old-path))
        #f
        (let ((new-path (cons y old-path)))
          (if (memq y ys-exposed)
              new-path
              (let ((y-to-xs (assq y ys-to-xs)))
                (and y-to-xs
                     (any (partial-apply search-x new-path)
                          (cdr y-to-xs))))))))
  (search-x '() x))

(define (flip-path old-xs-to-ys old-ys-to-xs old-path)
  (if (or (null? old-path) (null? (cdr old-path)))
      ;; These are flipped because in the last iteration, the path is (x),
      ;; which means old-xs-to-ys is actually ys-to-xs....
      (list old-ys-to-xs old-xs-to-ys)
      (let* ((y (car old-path))
             (x (cadr old-path))
             (new-path (cdr old-path))
             (new-xs-to-ys (remove-edge old-xs-to-ys x y))
             (new-ys-to-xs (add-edge old-ys-to-xs y x)))
        (flip-path new-ys-to-xs new-xs-to-ys new-path))))

(define (add-edge xs-to-ys x y)
  (let* ((x-neighbors (assq x xs-to-ys)))
    (if x-neighbors
        (cons (cons* x y (cdr x-neighbors))
              (del-assq x xs-to-ys))
        (cons (list x y) xs-to-ys))))

(define (remove-edge xs-to-ys x y)
  (let* ((x-neighbors (assq x xs-to-ys)))
    (if x-neighbors
        (cons (cons x (delq y (cdr x-neighbors)))
              (del-assq x xs-to-ys))
        xs-to-ys)))

(define (flip-all xs-to-ys)
  (let loop ((xs-to-ys xs-to-ys)
             (ys-to-xs '()))
    (if (null? xs-to-ys)
        ys-to-xs
        (loop (cdr xs-to-ys)
              (fold-left (let ((x (caar xs-to-ys)))
                           (lambda (ys-to-xs y)
                             (add-edge ys-to-xs y x)))
                         ys-to-xs
                         ;; Edges x connects to.
                         (cdar xs-to-ys))))))
