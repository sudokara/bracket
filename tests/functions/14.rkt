(define (compose [f : (Integer -> Integer)] [g : (Integer->Integer]) : Integer)
  (lambda ([x : Integer]) (f (g x))))