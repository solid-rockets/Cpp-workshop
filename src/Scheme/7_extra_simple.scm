(load "6_define-syntax-rule.scm")

(define-syntax-rule
  (! a v)
  (begin
    (set! a v)
    a
  )
)

(define-syntax-rule
  (++ a)
  (! a (+ a 1))
)

(define-syntax-rule
  (-- a)
  (! a (- a 1))
)
