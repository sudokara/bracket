(define (make2 [a : Integer] [b : Integer]) 
  : (Vector Integer Integer)
  (vector a b))
(vector-length (make2 3 4))