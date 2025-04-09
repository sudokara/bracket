(let ([x 10])
  (if #t
    (set! x 20)
    (set! x #f))
  x)