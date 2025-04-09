(let ([x 1])
  (begin
    (if #t
      (set! x 2)
      (set! x 3))
  x)
)