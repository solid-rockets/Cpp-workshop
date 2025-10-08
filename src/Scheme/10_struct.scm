; A simple record system for Scheme, based on vectors.
;
; The principle is quite straightforward - quasiquotes and built-in meta
; functionality are used to define functions in a begin block, which is
; created by a macro and run at the top level.
; Thus, the newly created functions are accessible globally and can be used
; to both create a named struct and manage its member fields.
;
(define (make-struct-tree l)
  (define temp "")                       ; For temporary storage.

  (define tree '(begin))                 ; This will be evaluated.

  (define name (symbol->string (car l))) ; Name of the struct.

  (define size (- (length l) 1))         ; Of the struct to be created.
                                         ; Same as the number of members.

  (define (make-sets il i)
    (if (zero? (length il))
      tree
      (begin
        (set! temp (symbol->string (car il)))
        (set! temp (string->symbol (string-append name "-" temp "!")))
        (set!
          tree
          (append tree `((define (,temp v val) (vector-set! v ,i val)))))
        (make-sets (cdr il) (+ i 1))
      )
    )
  )

  (define (make-gets il i)
    (if (zero? (length il))
      tree
      (begin
        (set! temp (symbol->string (car il)))
        (set! temp (string->symbol (string-append name "-" temp)))
        (set! tree (append tree `((define (,temp v) (vector-ref v ,i)))))
        (make-gets (cdr il) (+ i 1))
      )
    )
  )

  (define creator-sym
    (string->symbol
      (string-append "make-" name)
    )
  )

  (set! tree (append tree `((define (,creator-sym) (make-vector ,size)))))
  (make-sets (cdr l) 0)
  (make-gets (cdr l) 0)
)

(define-syntax struct
  (syntax-rules ()
    (
      (_ m ...)
      (eval (make-struct-tree '(m ...)))
    )
  )
)
