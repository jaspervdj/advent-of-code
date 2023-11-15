(load "lib/scm/intcode.scm")

(define (run intcode a b) (let
    ((cpy (vector-copy intcode)))
    (vector-set! cpy 1 a)
    (vector-set! cpy 2 b)
    (table-ref (intcode-run cpy void void) 0)))

(define (solve intcode) (letrec*
    ((loop (lambda (a b) (cond
        ((= 19690720 (run intcode a b)) (+ (* 100 a) b))
        ((>= a b)                       (loop 0 (+ b 1)))
        (else                           (loop (+ a 1) b))))))
    (loop 0 0)))

(let* ((intcode (string->intcode (read-line))))
    (write (run intcode 12 2)) (newline)
    (write (solve intcode)) (newline))
