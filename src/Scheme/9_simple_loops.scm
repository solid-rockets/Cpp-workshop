(load "8_define-syntax-rule_literals.scm")

(define-syntax-rule (while c actions ...)
  (let
    (
      (f
        (lambda (f)
          (if c
            (begin actions ... (f f))
            '()))
      )
    )
    (f f)
  )
)

(define-syntax-rule (for (a val) c i actions ...)
  ; Using the star ensures that all of the following pairs are
  ; bound consecutively. This way, the lambda below can use
  ; the initialized variable 'a'.
  (let*
    (
      (a val)
      (f
        (lambda (f)
          (if c
            (begin actions ... (set! a i) (f f))
            '()
          )
        )
      )
    )
    (f f)
  )
)

(define-syntax-rule (loop while c actions ...) (while)
  (let
    (
      (f
        (lambda (f)
          (begin actions ...
            (if c
            (f f)
            '()))
        )
      )
    )
    (f f)
  )
)
