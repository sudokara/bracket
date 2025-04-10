(let ([i 0])
  (let ([sum 0])
    (begin
      (while (< i 5)
        (begin
          (let ([j 0])
            (begin
              (while (< j i)
                (begin
                  (set! sum (+ sum 1))
                  (set! j (+ j 1))))
              (set! i (+ i 1))))))
      sum))))