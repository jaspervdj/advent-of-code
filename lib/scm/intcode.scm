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
    ((9)  'rbe)
    ((99) 'hlt)))

(define (param-mode instr n) (case
    (remainder (floor (/ instr (expt 10 (+ 1 n)))) 10)
    ((0) 'position)
    ((1) 'immediate)
    ((2) 'relative)))

(define (intcode-run program input output) (letrec
    ((mem (make-table))
     (relbase 0)
     (loop (lambda (ip) (let*
       ((instr (table-ref mem ip 0))
        (opcode (instr-opcode instr))
        (param-value (lambda (n) (table-ref mem (+ ip n) 0)))
        (load (lambda (n) (case (param-mode instr n)
            ((immediate) (param-value n))
            ((position)  (table-ref mem (param-value n) 0))
            ((relative)  (table-ref mem (+ relbase (param-value n)) 0)))))
        (store (lambda (n x) (case (param-mode instr n)
            ((position) (table-set! mem (param-value n) x))
            ((relative) (table-set! mem (+ relbase (param-value n)) x))))))
       (case opcode
           ((add) (store 3 (+ (load 1) (load 2))) (loop (+ ip 4)))
           ((mul) (store 3 (* (load 1) (load 2))) (loop (+ ip 4)))
           ((ipt) (input (lambda (x) (store 1 x) (loop (+ ip 2)))))
           ((opt) (output (load 1) (lambda () (loop (+ ip 2)))))
           ((jit) (if (not (zero? (load 1))) (loop (load 2)) (loop (+ ip 3))))
           ((jif) (if (zero? (load 1))       (loop (load 2)) (loop (+ ip 3))))
           ((ltn) (store 3 (if (< (load 1) (load 2)) 1 0)) (loop (+ ip 4)))
           ((equ) (store 3 (if (= (load 1) (load 2)) 1 0)) (loop (+ ip 4)))
           ((rbe) (set! relbase (+ relbase (load 1))) (loop (+ ip 2)))
           ((hlt) void))))))
    (for-range
         (lambda (i) (table-set! mem i (vector-ref program i)))
         (vector-length program))
    (loop 0)
    mem))
