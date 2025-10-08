; Simple macros meant to be my first proper steps with the macro system.
; Will also become quite handy later.
;
; The reason this can work is because macros are not functions -
; they are, instead, expanded within the definition of a function.
; Thus, they can modify the state of a variable without creating
; a separate one.
;
(define-syntax ++
  (syntax-rules ()
    (
      (_ a)
      (begin
        (set! a (+ a 1))
        a
      )
    )
  )
)

(define-syntax --
  (syntax-rules ()
    (
      (_ a)
      (begin
        (set! a (- a 1))
        a
      )
    )
  )
)

(define-syntax !
  (syntax-rules ()
    (
      (_ a v)
      (begin
        (set! a v)
        a
      )
    )
  )
)
