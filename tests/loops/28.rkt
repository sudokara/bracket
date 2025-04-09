(let ([x 5])
  (let ([sum 0])
    (begin
      (while (> x 0)
        (begin
          (set! sum (+ sum x))
          (set! x (- x 1))))
      sum)))