(load "data/test.scm")

(define (g x y)
  (* x y))

(g (f 1 2) (f 3 4))
