(define (unique-perfect-matching xs ys xs-to-ys)
  ;; The entry point for this file. Takes two lists of vertices, xs and ys, and
  ;; an alist of pairs (x . neighbors-of-x). Returns a list of two-element
  ;; lists (y x) representing the unique perfect matching, if it exists, where
  ;; y is from ys and x is from xs.
  (let ((matching (perfect-matching xs-to-ys '() xs ys)))
    (and matching
         (every (lambda (edges)
                  (not (perfect-matching edges '() xs ys)))
                (edge-removals xs-to-ys matching))
         matching)))

(define (edge-removals xs-to-ys matching)
  (map (lambda (y-to-x)
         (let ((x (cadr y-to-x))
               (y (car y-to-x)))
           (remove-edge xs-to-ys x y)))
       matching))

(define (perfect-matching xs-to-ys ys-to-xs xs-exposed ys-exposed)
  (if (or (null? xs-exposed) (null? ys-exposed))
      ;; There is exactly one edge from each y to each x when we have no
      ;; remaining exposed vertices.
      ys-to-xs
      (let* ((x (car xs-exposed))
             (new-xs (cdr xs-exposed))
             ;; Augmenting path from y to x for some exposed y.
             (path (augmenting-path xs-to-ys ys-to-xs x ys-exposed)))
        (and path
             (let* ((new-edges (flip xs-to-ys ys-to-xs path))
                    (new-xs-to-ys (car new-edges))
                    (new-ys-to-xs (cadr new-edges))
                    (y (car path))
                    (new-ys (remove (partial-apply eq? y) ys-exposed)))
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
        (cons (cons x (remove (partial-apply eq? y) (cdr x-neighbors)))
              (del-assq x xs-to-ys))
        xs-to-ys)))

;; Victory!
;; (perfect-matching '((x1 y1 y2) (x2 y1 y2 y3) (x3 y1)) '() '(x1 x2 x3) '(y1 y2 y3))
;; ;Value 32: ((y1 x3) (y3 x2) (y2 x1))
