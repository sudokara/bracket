(define (inc [x : Integer]) : Integer
(+ x 1))
(define (inc [x : Integer]) : Integer
(- x 1))
(let ([vecFunc (vector inc inc)])
  ((vector-ref vecFunc 0) 41))
