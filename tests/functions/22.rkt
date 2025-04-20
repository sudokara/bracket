(define (map4 [f : (Integer -> Integer)]
              [v : (Vector Integer Integer Integer Integer)])
  : (Vector Integer Integer Integer Integer)
  (vector
    (f (vector-ref v 0))
    (f (vector-ref v 1))
    (f (vector-ref v 2))
    (f (vector-ref v 3))))
(define (inc [x : Integer]) : Integer
  (+ x 1))
(vector-ref (map4 inc (vector 0 1 2 3)) 3)