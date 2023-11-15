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

; Does this belong here?
(define (for-range f n) (letrec
    ((loop (lambda (i) (cond ((< i n) (f i) (loop (+ i 1)))))))
    (loop 0)))
