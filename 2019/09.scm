(load "lib/scm/intcode.scm")

(let*
    ((program (string->intcode (read-line)))
     (run (lambda (n) (intcode-run
        program
        (lambda (k) (k n))
        (lambda (x k) (write x) (newline) (k))))))
    (run 1)
    (run 2))
