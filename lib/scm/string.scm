(define (split-string str sep) (letrec
    ((len (string-length str))
     (loop (lambda (acc i j) (cond
        ((< i 0) (cons (substring str 0 j) acc))
        ((eq? sep (string-ref str i)) (loop
            (cons (substring str (+ i 1) j) acc)
            (- i 1) i))
        (else (loop acc (- i 1) j))))))
    (loop '() (- len 1) len)))
