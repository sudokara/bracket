(let ([x 1])
  (begin
    (set! x 2)
    (set! x 3)
    (set! x 4)
    (set! x 5)
    x))