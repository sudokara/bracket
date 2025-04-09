(let ([n 10])
  (let ([i 0])
    (let ([found #f])
      (begin
        (while (and (< i n) (not found))
          (begin
            (if (eq? i 5)
                (set! found #t)
                (void))
            (set! i (+ i 1))))
        i))))