(let ([x 1]) 
  (begin (begin 
    (set! x 2) 
    (set! x 3) 
    (set! x 4)) 
  x))