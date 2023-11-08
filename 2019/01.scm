(define (fuel mass) (- (floor (/ mass 3)) 2))

(define (rec-fuel mass)
    (let ((f (fuel mass))) (if (<= f 0) 0 (+ f (rec-fuel f)))))

(let ((masses (map string->number (read-all (current-input-port) read-line))))
    (write (apply + (map fuel masses))) (newline)
    (write (apply + (map rec-fuel masses))) (newline))
