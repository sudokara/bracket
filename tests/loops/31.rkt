(let ([n 5])
  (let ([fact 1])
    (let ([temp 0])
      (begin
        (while (> n 0)
          (begin
            (set! temp 0)
            (let ([i fact])
              (while (> i 0)
                (begin
                  (set! temp (+ temp n))
                  (set! i (- i 1)))))
            (set! fact temp)
            (set! n (- n 1))))
        fact))))