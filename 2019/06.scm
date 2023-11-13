(load "lib/scm/string.scm")

(define (parse-orbit str)
    (let ((pieces (split-string str #\)))) (cons (cadr pieces) (car pieces))))

(define (strip-shared-prefix lx ly) (if
    (equal? (car lx) (car ly))
    (strip-shared-prefix (cdr lx) (cdr ly))
    (cons lx ly)))

(letrec*
    ((orbits (map parse-orbit (read-all (current-input-port) read-line)))
     (table (list->table orbits))
     (parent (lambda (obj) (table-ref table obj '())))
     (count (lambda (obj)
        (if (null? (parent obj)) 0 (+ 1 (count (parent obj))))))
     (path (lambda (obj)
        (cons obj (if (null? (parent obj)) '() (path (parent obj))))))
     (suffixes (strip-shared-prefix
        (reverse (path "YOU"))
        (reverse (path "SAN")))))
    (write (apply + (map (lambda (kv) (count (car kv))) (table->list table))))
    (newline)
    (write (+ (length (car suffixes)) (length (cdr suffixes)) -2))
    (newline))
