; Creates a macro definition definer that is more like Racket's
; and, hence, much easier to use for simple cases.
;
(define-syntax define-syntax-rule
  (syntax-rules ()
    (
      (_ (a b ...) c)
      (define-syntax a
        (syntax-rules ()
          (
            (_ b ...) c
          )
        )
      )
    )
  )
)
