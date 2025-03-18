(let ([x (let ([x 1]) x)]) 
  (let ([x (let ([x (+ x 1)]) x)])
    (+ x (let ([x 5]) x))))