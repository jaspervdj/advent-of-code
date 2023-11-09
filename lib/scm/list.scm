(define (filter f l) (letrec
    ((loop (lambda (l acc) (cond
        ((null? l) (reverse acc))
        ((f (car l)) (loop (cdr l) (cons (car l) acc)))
        (else (loop (cdr l) acc))))))
    (loop l '())))

(define (fold-left f z l)
    (if (null? l) z (fold-left f (f z (car l)) (cdr l))))
