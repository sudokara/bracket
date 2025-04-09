(let ([n 5])
  (let ([fact 1])
    (begin
      (while (> n 0)
        (begin
          (set! fact (* n fact))
          (set! n (- n 1))))
      fact)))