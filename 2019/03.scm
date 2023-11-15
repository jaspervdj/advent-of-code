(load "lib/scm/list.scm")
(load "lib/scm/point.scm")
(load "lib/scm/string.scm")
(load "lib/scm/table.scm")

(define-structure command dir n)

(define (command-decrement c)
    (make-command (command-dir c) (- (command-n c) 1)))

(define (string->path str) (letrec
    ((string->command (lambda (str) (make-command
        (string-ref str 0)
        (string->number (substring str 1 (string-length str)))))))
    (map string->command (split-string str #\,))))

(define (move-p2d p dir) (case dir
    ((#\U) (p2d-up    p 1))
    ((#\R) (p2d-right p 1))
    ((#\D) (p2d-down  p 1))
    ((#\L) (p2d-left  p 1))
    (else p)))

(define (path->trace path) (letrec
    ((walk (lambda (path p i acc) (cond
        ((null? path) (reverse (cons (cons p i) acc)))
        ((= 0 (command-n (car path))) (walk (cdr path) p i acc))
        (else (walk
            (cons (command-decrement (car path)) (cdr path))
            (move-p2d p (command-dir (car path)))
            (+ i 1)
            (cons (cons p i) acc)))))))
    (walk path (make-p2d 0 0) 0 '())))

(let* ((paths (map string->path (read-all (current-input-port) read-line)))
       (origin (make-p2d 0 0))
       (tables (map list->table (map path->trace paths)))
       (intersects (fold-left
        (lambda (t u) (table-intersect t u (lambda (x y) (+ x y))))
        (car tables)
        (cdr tables))))
    (write (apply min (filter (lambda (d) (> d 0)) (map
        (lambda (kv) (p2d-manhattan origin (car kv)))
        (table->list intersects)))))
    (newline)
    (write (apply min (filter (lambda (d) (> d 0)) (map
        (lambda (kv) (cdr kv))
        (table->list intersects)))))
    (newline))
