(let ([n 7])
  (let ([a 0])
    (let ([b 1])
      (let ([i 0])
        (let ([temp 0])
          (begin
            (if (eq? n 0)
                a
                (if (eq? n 1)
                    b
                    (begin
                      (while (< i (- n 1))
                        (begin
                          (set! temp (+ a b))
                          (set! a b)
                          (set! b temp)
                          (set! i (+ i 1))))
                      b)))))))))