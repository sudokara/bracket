(let ([vv (vector (vector 1 2) (vector 3 4) (vector 5 6))])
  (vector-ref (vector-ref vv 2) 1))