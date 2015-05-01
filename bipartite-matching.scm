(define (unique-perfect-matching xs ys xs-to-ys)
  ;; The entry point for this file. Takes two lists of vertices, xs and ys, and
  ;; an alist of pairs (x . neighbors-of-x). Returns a list of two-element
  ;; lists (x y) representing the unique perfect matching, if it exists, where
  ;; x is from xs and y is from ys.
  (let ((matching (perfect-matching xs-to-ys '() xs ys)))
    (and matching
         (let ((xs-to-ys (car matching)))
           (not (any (lambda (edge-rest)
                       (let ((ys-to-xs (cadr edge-rest))
                             (xs (cdar edge-rest))
                             (ys (list (caar edge-rest))))
                         ;; Start where we left off (same xs-to-ys) but with a
                         ;; single edge from the matching removed (modified
                         ;; ys-to-xs) and the vertices that were touching that
                         ;; edge now exposed (both xs and ys are singletons).
                         (perfect-matching xs-to-ys ys-to-xs xs ys)))
                     ;; Iterate through the ys-to-xs from the matching. Each
                     ;; y-to-xs is a (y), meaning y is unmatched, or a (y x),
                     ;; meaning y is matched to x.
                     (elem-rest-twos (filter (lambda (y-to-xs)
                                               ;; Only consider matched ys.
                                               (= (length y-to-xs) 2))
                                             (cadr matching))))))
         (map reverse (cadr matching)))))

(define (elem-rest-twos xs)
  ;; Sends '(a b c d) to '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c))).
  (let loop ((xs-head-rev '())
             (xs-tail xs))
    (if (null? xs-tail)
        '()
        (let ((x (car xs-tail))
              (xs-tail-new (cdr xs-tail)))
          (cons (list x (append (reverse xs-head-rev) xs-tail-new))
                (loop (cons x xs-head-rev) xs-tail-new))))))

(define (perfect-matching xs-to-ys ys-to-xs xs-exposed ys-exposed)
  ;; Returns a matching in the form of new xs-to-ys and ys-to-xs in which every
  ;; x is matched or #f if no such matching exists. In particular, ys-to-xs has
  ;; an edge from each matched y to its matched x.
  (if (or (null? xs-exposed) (null? ys-exposed))
      ;; There is exactly one edge from each y to each x when we have no
      ;; remaining exposed vertices, so ys-to-xs gives the matching.
      (list xs-to-ys ys-to-xs)
      (let* ((x (car xs-exposed))
             (new-xs (cdr xs-exposed))
             ;; Augmenting path from y to x for some exposed y.
             (path (augmenting-path xs-to-ys ys-to-xs x ys-exposed)))
        (and path
             (let* ((new-edges (flip xs-to-ys ys-to-xs path))
                    (new-xs-to-ys (car new-edges))
                    (new-ys-to-xs (cadr new-edges))
                    (y (car path))
                    (new-ys (delq y ys-exposed)))
               (perfect-matching new-xs-to-ys new-ys-to-xs new-xs new-ys))))))

(define (augmenting-path xs-to-ys ys-to-xs x ys-exposed)
  (define (search-x old-path x)
    (if (memq x old-path)
        #f
        (let ((new-path (cons x old-path))
              (neighbors (cdr (assq x xs-to-ys))))
          (any (partial-apply search-y new-path) neighbors))))
  (define (search-y old-path y)
    (if (memq y old-path)
        #f
        (let ((new-path (cons y old-path)))
          (if (memq y ys-exposed)
              new-path
              (let ((neighbors (cdr (assq y ys-to-xs))))
                (any (partial-apply search-x new-path) neighbors))))))
  (search-x '() x))

(define (flip old-xs-to-ys old-ys-to-xs old-path)
  (if (or (null? old-path) (null? (cdr old-path)))
      ;; These are flipped because in the last iteration, the path is (x),
      ;; which means old-xs-to-ys is actually ys-to-xs....
      (list old-ys-to-xs old-xs-to-ys)
      (let* ((y (car old-path))
             (x (cadr old-path))
             (new-path (cdr old-path))
             (new-xs-to-ys (remove-edge old-xs-to-ys x y))
             (new-ys-to-xs (add-edge old-ys-to-xs y x)))
        (flip new-ys-to-xs new-xs-to-ys new-path))))

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
