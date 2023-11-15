(load "lib/scm/point.scm")
(load "lib/scm/list.scm")
(load "lib/scm/grid.scm")

(define-structure exact-angle quadrant slope)

(define (p2d-exact-angle p2d) (let
    ((x (p2d-x p2d))
     (y (p2d-y p2d))) (cond
    ((and (>= x 0) (<  y 0)) (make-exact-angle 0 (/ x (- y))))
    ((and (>  x 0) (>= y 0)) (make-exact-angle 1 (/ y x)))
    ((and (<= x 0) (>  y 0)) (make-exact-angle 2 (/ (- x) y)))
    ((and (<  x 0) (<= y 0)) (make-exact-angle 3 (/ y x)))
    (else (raise "p2d-exact-angle of 0")))))

(define (exact-angle-lt a b) (or
    (< (exact-angle-quadrant a) (exact-angle-quadrant b))
    (and
        (= (exact-angle-quadrant a) (exact-angle-quadrant b))
        (< (exact-angle-slope a) (exact-angle-slope b)))))

(define (asteroid? v) (equal? v #\#))

(define (asteroid-scan grid p) (let
    ((by-angle (make-table)))
    (table-for-each
        (lambda (q v) (if
            (and (asteroid? v) (not (equal? p q)))
            (let*
                ((delta (p2d-subtract q p))
                 (angle (p2d-exact-angle delta)))
                (table-set! by-angle angle
                    (cons q (table-ref by-angle angle '()))))))
        grid)
    (list-sort
        (lambda (a b) (exact-angle-lt (car a) (car b)))
        (table->list by-angle))))

(define (transpose ls) (letrec*
    ((row (lambda (cols acc remain) (cond
        ((null? cols) (values (reverse acc) (reverse remain)))
        ((null? (car cols)) (row (cdr cols) acc remain))
        (else (row
            (cdr cols)
            (cons (car (car cols)) acc)
            (cons (cdr (car cols)) remain))))))
     (loop (lambda (cols acc) (let-values
        (((r remain) (row cols '() '())))
        (if
            (null? r)
            (reverse acc)
            (loop remain (cons r acc)))))))
    (loop ls '())))

(define (asteroid-vaporize grid l) (let
    ((ordered (map
        (lambda (ps) (list-sort
            (lambda (p q) (< (p2d-manhattan l p) (p2d-manhattan l q)))
            ps))
        (map cdr (asteroid-scan grid l)))))
    (apply append (transpose ordered))))

(let ((grid (read-grid (current-input-port)))
      (best 0)
      (laser '()))
    (table-for-each
        (lambda (k v) (if
            (asteroid? v)
            (let
                ((v (length (asteroid-scan grid k))))
                (if (> v best) (begin (set! best v) (set! laser k))))))
        grid)
    (write best)
    (newline)
    (write (let
        ((n200 (list-ref (asteroid-vaporize grid laser) 199)))
        (+ (* (p2d-x n200) 100) (p2d-y n200))))
    (newline))
