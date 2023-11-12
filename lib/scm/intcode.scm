(include "list.scm")
(include "string.scm")

(define (string->intcode str) (let*
    ((not-space? (lambda (c) (not (char-whitespace? c))))
     (chars (filter not-space? (string->list str)))
     (ints (split-string (list->string chars) #\,)))
    (apply vector (map string->number ints))))

(define (instr-opcode instr) (case (remainder instr 100)
    ((1)  'add)
    ((2)  'mul)
    ((3)  'ipt)
    ((4)  'opt)
    ((5)  'jit)
    ((6)  'jif)
    ((7)  'ltn)
    ((8)  'equ)
    ((99) 'hlt)))

(define (intcode-run! mem input output) (letrec
    ((loop (lambda (ip) void (let*
       ((instr (vector-ref mem ip))
        (opcode (instr-opcode instr))
        (load (lambda (n) (let
           ((position-mode
               (zero? (remainder (floor (/ instr (expt 10 (+ 1 n)))) 10)))
            (lit (vector-ref mem (+ ip n))))
           (if position-mode (vector-ref mem lit) lit))))
        (store (lambda (n x)
            (let ((out (vector-ref mem (+ ip n)))) (vector-set! mem out x)))))
       (case opcode
           ((add) (store 3 (+ (load 1) (load 2))) (loop (+ ip 4)))
           ((mul) (store 3 (* (load 1) (load 2))) (loop (+ ip 4)))
           ((ipt) (store 1 (input))               (loop (+ ip 2)))
           ((opt) (output (load 1))               (loop (+ ip 2)))
           ((jit) (if (not (zero? (load 1))) (loop (load 2)) (loop (+ ip 3))))
           ((jif) (if (zero? (load 1))       (loop (load 2)) (loop (+ ip 3))))
           ((ltn) (store 3 (if (< (load 1) (load 2)) 1 0)) (loop (+ ip 4)))
           ((equ) (store 3 (if (= (load 1) (load 2)) 1 0)) (loop (+ ip 4)))
           ((hlt) void))))))
    (loop 0)))
