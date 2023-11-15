(load "lib/scm/list.scm")
(load "lib/scm/intcode.scm")

(define-structure fifo reader queue)

(define (fifo-write fifo x) (if
    (null? (fifo-reader fifo))
    (fifo-queue-set! fifo (append (fifo-queue fifo) (cons x '())))
    (let ((k (fifo-reader fifo))) (fifo-reader-set! fifo '()) (k x))))

(define (fifo-read fifo k) (cond
    ((not (null? (fifo-reader fifo))) (raise "concurrent reads"))
    ((null? (fifo-queue fifo)) (fifo-reader-set! fifo k))
    (else (let ((queue (fifo-queue fifo)))
        (fifo-queue-set! fifo (cdr queue))
        (k (car queue))))))

(define (empty-fifo) (make-fifo '() '()))

(define (for-combinations f l) (cond
    ((null? l) (f '()))
    (else (for-each
        (lambda (x) (for-combinations
            (lambda (c) (f (cons x c)))
            (filter (lambda (y) (not (equal? x y))) l)))
        l))))

(define (run-amplifiers program pss k) (let
    ((fifos (map
        (lambda (s) (let ((fifo (empty-fifo))) (fifo-write fifo s) fifo))
        pss)))
    (for-range
        (lambda (idx) (intcode-run
            (vector-copy program)
            (lambda (k) (fifo-read (list-ref fifos idx) k))
            (lambda (x k) (let
                ((fifo (list-ref fifos (remainder (+ idx 1) (length pss)))))
                (fifo-write fifo x)
                (k)))))
        5)
    (fifo-write (car fifos) 0)
    (fifo-read (car fifos) k)))

(define (max-thrust program pss) (let*
    ((best '()))
    (for-combinations
        (lambda (pss) (run-amplifiers
            program
            pss
            (lambda (thrust) (cond
                ((null? best) (set! best thrust))
                ((> thrust best) (set! best thrust))))))
        pss)
    best))

(let*
    ((program (string->intcode (read-line))))
    (write (max-thrust program '(0 1 2 3 4))) (newline)
    (write (max-thrust program '(5 6 7 8 9))) (newline))
