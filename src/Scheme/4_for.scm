(define-syntax for
  (syntax-rules ()
    (
      (_ (a val) c i actions ...)
      (let* ; Using the star ensures that all of the following pairs are
          ; bound consecutively. This way, the lambda below can use
          ; the initialized variable 'a'.
        (
          (a val)
          (f
            (lambda (f)
              (if c
                (begin actions ... (set! a i) (f f))
                '()))
          )
        )
        (f f)
      )
    )
  )
)
