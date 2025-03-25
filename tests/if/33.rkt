(let ([x (read)])
  (if (< x 0)
      (if (< x (- 10)) (- 2) (- 1))
      (if (> x 10) 2 
          (if (eq? x 0) 0 1))))