(let ([x 1])
  (begin
    (let ([x 2])
      (set! x 3))
    x))