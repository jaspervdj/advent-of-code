(load "lib/scm/grid.scm")
(load "lib/scm/point.scm")
(load "lib/scm/intcode.scm")

(define (robot intcode grid) (let*
    ((pos (make-p2d 0 0))
     (dir 'up)
     (color '()))
    (intcode-run
        intcode
        (lambda (k) (k (case (table-ref grid pos #\.) ((#\.) 0) ((#\#) 1))))
        (lambda (x k)
            (if
                (null? color)
                (set! color x)
                (begin
                    (table-set! grid pos (case color ((0) #\.) ((1) #\#)))
                    (set! color '())
                    (set! dir (case x
                        ((0) (turn-left dir))
                        ((1) (turn-right dir))))
                    (set! pos (p2d-move pos dir 1))))
            (k)))))

(let*
    ((grid (make-table))
     (intcode (string->intcode (read-line))))
    (robot intcode grid)
    (write (table-length grid))
    (set! grid (make-table))
    (table-set! grid (make-p2d 0 0) #\#)
    (robot intcode grid)
    (newline)
    (display-grid grid)
    (newline))
