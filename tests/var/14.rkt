(let ([x (let ([y 10]) (+ y 5))])
  (let ([z (+ x 20)])
    (- z x)))