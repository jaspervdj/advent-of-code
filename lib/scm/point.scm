(define-structure p2d x y)

(define (p2d-manhattan p1 p2) (+
    (abs (- (p2d-x p1) (p2d-x p2)))
    (abs (- (p2d-y p1) (p2d-y p2)))))

(define (p2d-distance p q) (sqrt (+
    (square (- (p2d-x p) (p2d-x q)))
    (square (- (p2d-y p) (p2d-y q))))))

(define (p2d-up    p n) (make-p2d    (p2d-x p)    (- (p2d-y p) n)))
(define (p2d-right p n) (make-p2d (+ (p2d-x p) n)    (p2d-y p)   ))
(define (p2d-down  p n) (make-p2d    (p2d-x p)    (+ (p2d-y p) n)))
(define (p2d-left  p n) (make-p2d (- (p2d-x p) n)    (p2d-y p)   ))

(define (p2d-subtract p q)
    (make-p2d (- (p2d-x p) (p2d-x q)) (- (p2d-y p) (p2d-y q))))
