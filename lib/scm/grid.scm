(load "lib/scm/list.scm")

(define (read-grid port) (let
    ((grid (make-table)))
    (for-each-index
        (lambda (row line) (for-each-index
             (lambda (col char) (table-set! grid (make-p2d col row) char))
             (string->list line)))
        (read-all (current-input-port) read-line))
    grid))

(define (grid-top-left grid) (let
    ((min-x '())
     (min-y '()))
    (table-for-each
        (lambda (k v)
            (if (or (null? min-x) (< (p2d-x k) min-x)) (set! min-x (p2d-x k)))
            (if (or (null? min-y) (< (p2d-y k) min-y)) (set! min-y (p2d-y k))))
        grid)
    (make-p2d min-x min-y)))

(define (grid-bottom-right grid) (let
    ((max-x '())
     (max-y '()))
    (table-for-each
        (lambda (k v)
            (if (or (null? max-x) (> (p2d-x k) max-x)) (set! max-x (p2d-x k)))
            (if (or (null? max-y) (> (p2d-y k) max-y)) (set! max-y (p2d-y k))))
        grid)
    (make-p2d max-x max-y)))

(define (display-grid grid) (let*
    ((top-left (grid-top-left grid))
     (bottom-right (grid-bottom-right grid))
     (min-x (p2d-x top-left))
     (min-y (p2d-y top-left))
     (max-x (p2d-x bottom-right))
     (max-y (p2d-y bottom-right)))
    (for-range
        (lambda (row)
            (display (list->string (generate
                (lambda (col) (let
                   ((y (+ min-y row))
                    (x (+ min-x col)))
                   (table-ref grid (make-p2d x y) #\ )))
                (+ (- max-x min-x) 1))))
            (newline))
        (+ (- max-y min-y) 1))))
