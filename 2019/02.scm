(define (read-intcode port) (let*
    ((not-space? (lambda (c) (not (char-whitespace? c))))
     (chars (filter not-space? (read-all port read-char)))
     (ints (split-string (list->string chars) #\,)))
    (apply vector (map string->number ints))))

(define (intcode-run! mem) (letrec*
    ((binop (lambda (ip f) (let*
        ((lhsp (vector-ref mem (+ ip 1)))
            (lhs (vector-ref mem lhsp))
            (rhsp (vector-ref mem (+ ip 2)))
            (rhs (vector-ref mem rhsp))
            (res (f lhs rhs))
            (outp (vector-ref mem (+ ip 3))))
            (vector-set! mem outp res))))
     (loop (lambda (ip) void (let
        ((opcode (vector-ref mem ip)))
        (cond
            ((= 1 opcode)  (binop ip +) (loop (+ ip 4)))
            ((= 2 opcode)  (binop ip *) (loop (+ ip 4)))
            ((= 99 opcode) void))))))
    (loop 0)))

(define (intcode-run intcode a b) (let
    ((cpy (vector-copy intcode)))
    (vector-set! cpy 1 a)
    (vector-set! cpy 2 b)
    (intcode-run! cpy)
    (vector-ref cpy 0)))

(define (solve intcode) (letrec*
    ((loop (lambda (a b) (cond
        ((= 19690720 (intcode-run intcode a b)) (+ (* 100 a) b))
        ((>= a b)                               (loop 0 (+ b 1)))
        (else                                   (loop (+ a 1) b))))))
    (loop 0 0)))

(let* ((intcode (read-intcode (current-input-port))))
    (write (intcode-run intcode 12 2)) (newline)
    (write (solve intcode)) (newline))
