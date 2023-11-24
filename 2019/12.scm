(load "lib/scm/point")
(load "lib/scm/string")
(load "lib/scm/list")

(define (string->p3d str) (let*
    ((relevant (string-map
        (lambda (char) (if
            (or (char-numeric? char) (equal? char #\-) (equal? char #\,))
            char
            #\ ))
        str))
     (trimmed (list->string (filter
        (lambda (char) (not (char-whitespace? char)))
        (string->list relevant))))
     (ls (map string->number (split-string trimmed #\,))))
    (make-p3d (car ls) (cadr ls) (caddr ls))))

(define-structure moon pos vel)

(define (gravity-sign a b) (cond
    ((< a b) 1)
    ((> a b) (- 1))
    (else 0)))

(define (gravity moon others) (let
    ((sum (lambda (f) (apply + (map
        (lambda (other) (gravity-sign (f (moon-pos moon)) (f (moon-pos other))))
        others)))))
    (make-p3d (sum p3d-x) (sum p3d-y) (sum p3d-z))))

(define (step moons) (map
    (lambda (moon) (let*
        ((vel (p3d-add (moon-vel moon) (gravity moon moons)))
         (pos (p3d-add (moon-pos moon) vel)))
        (make-moon pos vel)))
    moons))

(define (energy moons) (apply + (map
    (lambda (moon) (* (p3d-abs (moon-pos moon)) (p3d-abs (moon-vel moon))))
    moons)))

(define (repeat n f x) (if (<= n 0) x (repeat (- n 1) f (f x))))

(define (solve g f x0) (letrec
    ((zero (g x0))
     (loop (lambda (x i) (if
        (equal? zero (g x))
        i
        (loop (f x) (+ i 1))))))
    (loop (f x0) 1)))

(let*
    ((initial (map string->p3d (read-all (current-input-port) read-line)))
     (moons (map (lambda (pos) (make-moon pos (make-p3d 0 0 0))) initial))
     (solve-for (lambda (f) (solve
        (lambda (ms) (map
            (lambda (m) (cons (f (moon-pos m)) (f (moon-vel m))))
            ms))
        step
        moons))))
    (write (energy (repeat 1000 step moons)))
    (newline)
    (write (lcm (solve-for p3d-x) (solve-for p3d-y) (solve-for p3d-z)))
    (newline))
