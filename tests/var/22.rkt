(let ([f (let ([x 1]) (+ x 1))])
  (let ([g (let ([x 2]) (+ x 2))])
    (let ([x 0]) (+ f (+ g x)))))