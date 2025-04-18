(let ([v (vector 0 0 0)])
  (begin
    (vector-set! v 0 42)
    (vector-set! v 2 99)
    (vector-ref v 2)))