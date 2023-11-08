(define (filter f l) (letrec
    ((loop (lambda (l acc) (cond
        ((null? l) (reverse acc))
        ((f (car l)) (loop (cdr l) (cons (car l) acc)))
        (else (loop (cdr l) acc))))))
    (loop l '())))
