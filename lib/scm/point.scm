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

(define (p2d-move p dir n) (case dir
    ((up)    (p2d-up p n))
    ((right) (p2d-right p n))
    ((down)  (p2d-down p n))
    ((left)  (p2d-left p n))))

(define (p2d-subtract p q)
    (make-p2d (- (p2d-x p) (p2d-x q)) (- (p2d-y p) (p2d-y q))))

(define (turn-left dir) (case dir
    ((up)    'left)
    ((right) 'up)
    ((down)  'right)
    ((left)  'down)))

(define (turn-right dir) (case dir
    ((up)    'right)
    ((right) 'down)
    ((down)  'left)
    ((left)  'up)))
