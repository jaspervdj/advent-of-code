(load "lib/scm/intcode.scm")

(define (intcode-run intcode a b) (let
    ((cpy (vector-copy intcode)))
    (vector-set! cpy 1 a)
    (vector-set! cpy 2 b)
    (intcode-run! cpy void void)
    (vector-ref cpy 0)))

(define (solve intcode) (letrec*
    ((loop (lambda (a b) (cond
        ((= 19690720 (intcode-run intcode a b)) (+ (* 100 a) b))
        ((>= a b)                               (loop 0 (+ b 1)))
        (else                                   (loop (+ a 1) b))))))
    (loop 0 0)))

(let* ((intcode (string->intcode (read-line))))
    (write (intcode-run intcode 12 2)) (newline)
    (write (solve intcode)) (newline))
