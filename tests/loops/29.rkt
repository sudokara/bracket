(let ([i 3])
  (let ([sum 0])
    (begin
      (while (> i 0)
        (begin
          (let ([j i])
            (while (> j 0)
              (begin
                (set! sum (+ sum 1))
                (set! j (- j 1)))))
          (set! i (- i 1))))
      sum)))