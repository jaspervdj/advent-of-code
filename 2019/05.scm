(load "lib/scm/intcode.scm")

(let*
    ((program (string->intcode (read-line)))
     (out 0)
     (run (lambda (n) (intcode-run!
        (vector-copy program)
        (lambda (k) (k n))
        (lambda (x k) (set! out x) (k))))))
    (run 1) (write out) (newline)
    (run 5) (write out) (newline))
