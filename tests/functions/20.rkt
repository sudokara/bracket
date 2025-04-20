(define (twice [f : (Integer -> Integer)] [x : Integer]) : Integer
  (f (f x)))
(define (inc [y : Integer]) : Integer
  (+ y 1))
(twice inc 3)