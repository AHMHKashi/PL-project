#lang racket

(require "passes/parser.rkt")
(require "passes/lexer.rkt")
(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(require "env.rkt")
(require "errors.rkt")
(require "store.rkt")


; constants
(define FILE_NAME "test.py")
(define NULL (atomic_null_exp))
(define DELIMITER ", ")

(define PASS (pass_flag))
(define CONTINUE (continue_flag))
(define RETURN_VOID (return_void_flag))
(define BREAK (break_flag))


(define (evaluate_print file-name)
  (if (file-exists? file-name)
    (begin
      (initialize-store!)
      (initialize-global-env!)
      (initialize-scope-env!)
      (let* ([lines (file->lines file-name)]
             [code (string-join lines)]
             [lexed (lex-this code)]
             [program (python-parser lexed)])
        (statements program)))
    (report-file-does-not-exist file-name)))
; evaluate
(define (evaluate file-name)
  (if (file-exists? file-name)
    (begin
      (initialize-store!)
      (initialize-global-env!)
      (initialize-scope-env!)
      (let* ([lines (file->lines file-name)]
             [code (string-join lines)]
             [lexed (lex-this code)]
             [program (python-parser lexed)])
        (value-of (statements program))))
    (report-file-does-not-exist file-name)))


(define (value-of stm)
  (begin
    ; (print stm)
    ; (newline)
    (cases statement stm
        (statements (stms) 
          (let loop ([stms stms])
            (if (null? stms)
              (atomic_null_exp)
              (let ([value (value-of (car stms))])
                (cases expression value
                  (break_flag () NULL)
                  (return_void_flag () NULL)
                  (continue_flag () NULL)
                  (pass_flag () NULL)
                  (else (loop (cdr stms)))
                )
              )
            )
          )
        )
        (return_void () RETURN_VOID)
        (pass () PASS)
        (break () BREAK)
        (continue () CONTINUE)
        (print_stmt (exprs) (_print exprs) (displayln "") NULL)
        (if_stmt (cond_exp if_sts else_sts)
          (if (exp->value (value-of-expression cond_exp))
            (value-of (statements if_sts))
            (value-of (statements else_sts))
          )
        )


        (assign (var expr) (let ()
            (update-scope-env! (extend-env var (value-of-expression expr) the-scope-env))
            NULL
        )
          ; (let ([current_variable (apply-env var the-scope-env)]
          ;   (cases expression current_variable
          ;     ; if its new -> we should add it to scope
          ;     [atomic_null_exp (update-scope-env! (extend-env var (newref (value-of-expression expr)) the-scope-env))]
          ;     ; if already exists we should update it
          ;     [else (update-scope-env! (extend-env var (newref)))]
          ;   )
        )

        (global (var) "not implemented: global") 
        ; (global (var) 
        ;   (let ([ref (apply-env var the-global-env)])
        ;     (update-scope-env! (extend-env var ref the-scope-env))
        ;     NULL
        ;   )
        ; )

        (return (expr) "not implemented: global\n")
        (func (name params statements) "not implemented: func\n")
        (for_stmt (iter list_exp sts) "not implemented: for_stmt\n")
    )
  )
)

(define (_print exprs)
  (begin
  ; (print exprs)
  ; (newline)
  (cases expression* exprs
    [empty-expr () NULL]
    [expressions (expr rest-exprs) 
      (cases expression* rest-exprs
        (empty-expr () (pyprint (exprs->first exprs)))
        (expressions (new-expr new-rest-exprs) (pyprint expr) (display DELIMITER) (_print rest-exprs))
      )
    ]
  )
  )
)

(define (pyprint expr)
  (let ((value (value-of-expression expr)))
  (cases expression value
    (atomic_num_exp (num)
      (if (integer? num)
        (display num)
        (display (exact->inexact num))))
    (atomic_bool_exp (bool)
      (if bool
        (display "True")
        (display "False")))
    (atomic_null_exp ()
      (display "None"))
    (atomic_list_exp (lst)
      (display "[")
      (let loop ([lst lst])
        (if (is_null_expression? lst)
          NULL
          (begin
            (pyprint (exprs->first lst))
            (when (not (is_null_expression? (exprs->rest lst)))
              (display DELIMITER))
            (loop (exprs->rest lst)))))
      (display "]"))
    (else (report-type-error 'print))))
)

(define (value-of-expression expr)
  (cases expression expr
      [binary_op (op left right)
        (let ([result (op (exp->value (value-of-expression left)) (exp->value (value-of-expression right)))])
          (if (boolean? result)
            (atomic_bool_exp result)
            (atomic_num_exp result)
          )
        )
      ]
      [unary_op (op operand) 
        (atomic_bool_exp (op (exp->value (value-of-expression operand))))
      ]
      [function_call (func params) "not implemented"]
      [list_ref (ref index) "not implemented"]
      [ref (var) (apply-env var the-scope-env)]

      [atomic_bool_exp (bool) expr]
      [atomic_num_exp (num) expr]
      [atomic_null_exp () expr]
      [atomic_list_exp (l) expr]
      [else expr]
  )
)

; run test
; (print (evaluate_print FILE_NAME))
(evaluate FILE_NAME)


