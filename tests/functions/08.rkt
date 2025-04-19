(define (foo [x : Integer]) : Integer (+ x 1))
(define (test-shadow [x : Integer]) : Integer
  (let ([foo (read)])
    (foo x)))
(test-shadow 7)