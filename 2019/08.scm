(define (split-every l n) (if
    (null? l)
    '()
    (let-values
        (((front back) (split-at l n)))
        (cons front (split-every back n)))))

(define (mininum-on lt f l) (if
    (null? l)
    (raise "mininum-on: empty list")
    (let*
        ((x (car l))
         (fx (f x)))
        (for-each
            (lambda (y) (let
                ((fy (f y)))
                (cond ((lt fy fx) (set! x y) (set! fx fy)))))
            (cdr l))
        x)))

(define (display-layer layer w h) (let
    ((pixel (lambda (p) (if (= 0 p) " " "M"))))
    (for-each
        (lambda (l) (display (apply string-append (map pixel l))) (newline))
        (split-every layer w))))

(define (zip-with f xs ys) (cond
    ((null? xs) '())
    ((null? ys) '())
    (else (cons (f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys))))))

(let*
    ((data (map
        (lambda (c) (string->number (list->string (cons c '()))))
        (string->list (read-line))))
     (w 25)
     (h 6)
     (layers (split-every data (* w h)))
     (count (lambda (l n) (length (filter (lambda (x) (= n x)) l))))
     (layer0 (mininum-on < (lambda (l) (count l 0)) layers))
     (merge (lambda (x y) (if (= 2 x) y x)))
     (merged (fold
        (lambda (layer acc) (zip-with merge acc layer))
        (car layers)
        (cdr layers))))
    (write (* (count layer0 1) (count layer0 2)))
    (newline)
    (display-layer merged w h))
