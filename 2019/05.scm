(load "lib/scm/intcode.scm")

(let*
    ((program (string->intcode (read-line)))
     (out 0))
    (intcode-run! (vector-copy program) (lambda () 1) (lambda (x) (set! out x)))
    (write out)
    (newline)
    (intcode-run! (vector-copy program) (lambda () 5) (lambda (x) (set! out x)))
    (write out)
    (newline))
