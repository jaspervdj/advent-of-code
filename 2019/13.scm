(load "lib/scm/grid")
(load "lib/scm/intcode")
(load "lib/scm/point")
(load "lib/scm/table")

(define (clear) (print "\033[H\033[2J"))

(let*
    ((grid (make-table))
     (animate #f)
     (outputs '())
     (ball '())
     (paddle '())
     (score 0)
     (intcode (string->intcode (read-line)))
     (patched-intcode (vector-copy intcode))

     (input (lambda (k)
        (if animate (begin
            (clear)
            (println score)
            (display-grid grid)
            (thread-sleep! 0.3)))
        (k (cond
            ((< (p2d-x paddle) (p2d-x ball)) 1)
            ((> (p2d-x paddle) (p2d-x ball)) (- 1))
            (else 0)))))
     (output (lambda (out k)
            (if (< (length outputs) 2)
                (set! outputs (cons out outputs))
                (let*
                    ((tile (case out
                        ((0) #\ )
                        ((1) #\#)
                        ((2) #\X)
                        ((3) #\-)
                        ((4) #\o)))
                     (x (cadr outputs))
                     (y (car outputs))
                     (pos (make-p2d x y)))
                    (set! outputs '())
                    (if (>= x 0)
                        (begin
                            (table-set! grid pos tile)
                            (case tile
                                ((#\-) (set! paddle pos))
                                ((#\o) (set! ball pos))))
                        (set! score out))))
            (k))))

    ; Part 1
    (intcode-run intcode input output)
    (write (table-fold (lambda (k v n) (if (equal? v #\X) (+ n 1) n)) 0 grid))
    (newline)

    ; Part 2
    (vector-set! intcode 0 2)
    (intcode-run intcode input output)
    (write score)
    (newline))
