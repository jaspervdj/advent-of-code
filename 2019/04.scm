(load "lib/scm/list.scm")
(load "lib/scm/string.scm")

(define-structure range low high)

(define (read-range port) (let*
    ((not-space? (lambda (c) (not (char-whitespace? c))))
     (chars (filter not-space? (read-all port read-char)))
     (ints (map string->number (split-string (list->string chars) #\-))))
    (make-range
        (car ints)
        (cadr ints))))

(define (range-count f range) (letrec
    ((count (lambda (i n) (cond
        ((> i (range-high range)) n)
        ((f i) (count (+ i 1) (+ n 1)))
        (else (count (+ i 1) n))))))
    (count (range-low range) 0)))

(define (number->digits x)
    (map digit-value (string->list (number->string x))))

(define (digits->number v) (string->number
    (apply string-append (map number->string v))))

(define (rle ls) (letrec
    ((loop (lambda (ls x n) (cond
        ((null? ls) (cons (cons x n) '()))
        ((= x (car ls)) (loop (cdr ls) x (+ n 1)))
        (else (cons (cons x n) (loop (cdr ls) (car ls) 1)))))))
    (if (null? ls) '() (loop (cdr ls) (car ls) 1))))

(define (adjacent-pair? ls)
    (any (lambda (t) (>= (cdr t) 2)) (rle ls)))

(define (adjacent-strict-pair? ls)
    (any (lambda (t) (= (cdr t) 2)) (rle ls)))

(define (never-decrease? ls) (letrec
    ((loop (lambda (ls x) (cond
        ((null? ls) #t)
        ((> x (car ls)) #f)
        (else (loop (cdr ls) (car ls)))))))
    (if (null? ls) #t (loop (cdr ls) (car ls)))))

(let ((range (read-range (current-input-port))))
    (write (range-count
        (lambda (n) (let ((digits (number->digits n)))
            (and (never-decrease? digits) (adjacent-pair? digits))))
        range))
    (newline)
    (write (range-count
        (lambda (n) (let ((digits (number->digits n)))
            (and (never-decrease? digits) (adjacent-strict-pair? digits))))
        range))
    (newline))
