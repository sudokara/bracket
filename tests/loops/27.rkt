(let ([x 1])
  (begin
    (set! x 2)
    (set! x (+ x 3))
    (set! x (+ x x))
    x))