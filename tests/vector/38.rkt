(let ([v (vector 1 2 3)]) (
    begin
    (vector-set! v 0 42) 
    (vector-ref v 0)))