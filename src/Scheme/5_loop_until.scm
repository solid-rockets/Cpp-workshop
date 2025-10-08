; Quite similar to the while loop with the following differences:
; 1) while run all statements at least once.
; 2) the 'while' keyword is used after the 'loop' keyword.
;
(define-syntax loop
  (syntax-rules (while)
    (
      (_ while c actions ...)
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
  )
)
