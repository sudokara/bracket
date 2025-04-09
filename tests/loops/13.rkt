(let ([x 10])
  (let ([y #t])
    (begin
      (set! x 20)
      (set! y #f)
      y)))