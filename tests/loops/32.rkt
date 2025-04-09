(let ([x #t])
  (let ([count 0])
    (begin
      (while x
        (begin
          (set! count (+ count 1))
          (if (>= count 5)
              (set! x #f)
              (void))))
      count)))