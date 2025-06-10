(define (add-one [x : Integer]) : Integer
  (+ x 1))
(define (compose [f : (Integer -> Integer)] [g : (Integer -> Integer)] [x : Integer]) : Integer
  (f (g x)))
(let ([y 10])
  (compose add-one add-one y))