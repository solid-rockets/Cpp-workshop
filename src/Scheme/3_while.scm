(define-syntax while
  (syntax-rules ()
    (
      (_ c actions ...)
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
  )
)
