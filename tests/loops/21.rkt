(let ([x 10])
  (let ([y 20])
    (begin
      (set! x 30)
      (set! y 40)
      (+ x y))))