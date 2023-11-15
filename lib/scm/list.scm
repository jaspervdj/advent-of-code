(define (filter p? l) (letrec
    ((loop (lambda (l acc) (cond
        ((null? l) (reverse acc))
        ((p? (car l)) (loop (cdr l) (cons (car l) acc)))
        (else (loop (cdr l) acc))))))
    (loop l '())))

(define (fold-left f z l)
    (if (null? l) z (fold-left f (f z (car l)) (cdr l))))

(define (any p? l) (cond
    ((null? l) #f)
    ((p? (car l)) #t)
    (else (any p? (cdr l)))))

(define (for-each-index f l) (letrec
    ((loop (lambda (l i) (cond
        ((pair? l) (f i (car l)) (loop (cdr l) (+ i 1)))))))
    (loop l 0)))

(define (generate f n) (letrec
    ((loop (lambda (i acc) (if
        (>= i 0)
        (loop (- i 1) (cons (f i) acc))
        acc))))
    (loop (- n 1) '())))

; Does this belong here?
(define (for-range f n) (letrec
    ((loop (lambda (i) (cond ((< i n) (f i) (loop (+ i 1)))))))
    (loop 0)))
