(let ([x 10])
  (+ x (let ([x 20])
         (+ x (let ([x 30])
                (+ x (let ([x 40]) x)))))))