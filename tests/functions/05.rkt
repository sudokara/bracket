(define (apply2 [f : (Integer -> Integer)] [x : Integer]) : Integer
  (f (f x)))
(define (inc [x : Integer]) : Integer
(+ x 1))
(apply2 inc 10)