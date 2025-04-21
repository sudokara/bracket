(define (neg [b : Boolean]) : Boolean
  (not b))

(define (get-neg [x : Integer]) : (Boolean -> Boolean)
  neg)

(let ([f (get-neg 1)]) (f #f))