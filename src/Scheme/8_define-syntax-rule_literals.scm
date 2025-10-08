; Creates a macro definition definer that is more like Racket's
; and, hence, much easier to use for simple cases.
;
; Now with added literals.
; Please note that in interactive mode, syntax rule definitions do
; replace earlier defined rules.
;
; To test, run the following commands:
;
; 1) (define-syntax-rule (sum a ...) (+ a ...))
; 2) (sum 1 2 30) => 33
; 3) (define-syntax-rule (op add a ...) (+ a ...) (add))
; 4) (op add 1 2 3) => 6
; 5) (op mul 1 2 3) => Exception
; 6) (define-syntax-rule (op mul a ...) (* a ...) (mul))
; 7) (op mul 1 2 3 4) => 24
; 8) (op add 1 2 3 4) => Exception, later definition replaced an earlier one.
;
(define-syntax define-syntax-rule
  (syntax-rules ()
    (
      (_ (name pattern ...) template)
      '(define-syntax name
        (syntax-rules ()
          (
            (_ pattern ...) template
          )
        )
      )
    )
    (
      (_ (name pattern ...) (literals ...) template)
      '(define-syntax name
        (syntax-rules (literals ...)
          (
            (_ pattern ...) template
          )
        )
      )
    )
  )
)
