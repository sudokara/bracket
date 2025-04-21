(let ([v (vector 1 2 3)]) 
  (vector-set! v 2 #f) 
  (vector-length v))