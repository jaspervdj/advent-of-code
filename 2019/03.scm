(load "lib/scm/list.scm")
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

(define-structure point x y)

(define (manhattan p1 p2) (+
    (abs (- (point-x p1) (point-x p2)))
    (abs (- (point-y p1) (point-y p2)))))

(define (move-point p dir) (case dir
    ((#\U) (make-point    (point-x p)    (- (point-y p) 1)))
    ((#\R) (make-point (+ (point-x p) 1)    (point-y p)   ))
    ((#\D) (make-point    (point-x p)    (+ (point-y p) 1)))
    ((#\L) (make-point (- (point-x p) 1)    (point-y p)   ))
    (else p)))

(define (path->trace path) (letrec
    ((walk (lambda (path p i acc) (cond
        ((null? path) (reverse (cons (cons p i) acc)))
        ((= 0 (command-n (car path))) (walk (cdr path) p i acc))
        (else (walk
            (cons (command-decrement (car path)) (cdr path))
            (move-point p (command-dir (car path)))
            (+ i 1)
            (cons (cons p i) acc)))))))
    (walk path (make-point 0 0) 0 '())))

(let* ((paths (map string->path (read-all (current-input-port) read-line)))
       (origin (make-point 0 0))
       (tables (map list->table (map path->trace paths)))
       (intersects (fold-left
        (lambda (t u) (table-intersect t u (lambda (x y) (+ x y))))
        (car tables)
        (cdr tables))))
    (write (apply min (filter (lambda (d) (> d 0)) (map
        (lambda (kv) (manhattan origin (car kv)))
        (table->list intersects)))))
    (newline)
    (write (apply min (filter (lambda (d) (> d 0)) (map
        (lambda (kv) (cdr kv))
        (table->list intersects)))))
    (newline))
