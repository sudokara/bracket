(define vf : (Vector (Integer -> Integer) (Integer -> Integer))
  (vector inc inc))
(define (call-first [v : (Vector (Integer -> Integer) (Integer -> Integer))]
                    [x : Integer]) : Integer
  ((vector-ref v 0) x))
(call-first vf 5)