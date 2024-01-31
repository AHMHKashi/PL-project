#lang racket

(require "passes/parser.rkt")
(require "passes/lexer.rkt")
(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(require "env.rkt")
(require "errors.rkt")
(require "store.rkt")
(require "proc.rkt")

; constants
(define FILE_NAME "test_1.py")
(define NULL (atomic_null_exp))
(define DELIMITER ", ")

(define PASS (pass_flag))
(define CONTINUE (continue_flag))
(define RETURN_VOID (return_flag NULL))
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
                  (return_flag (val) value)
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
            (_assign var expr)
            NULL
        ))

        (global (var) 
          (let ([ref (apply-env var the-global-env)])
            (update-scope-env! (extend-env var ref the-scope-env))
            NULL
          )
        )
      (func (name params statements-list)
            (let* ([thunk-params
                    (letrec ([param-to-list
                              (lambda (p)
                                (cases func_param* p
                                  (empty-param () '())
                                  (func_params (param rest-params)
                                               (cases func_param param
                                                 (with_default (name exp) (cons (list name (a-thunk exp the-scope-env)) (param-to-list rest-params)))
                                                 (else (report-type-error 'value-of))
                                                 ))
                                  )
                                )]) (param-to-list params))]
                   [reference (ref_val (newref (a-proc name thunk-params (statements statements-list))))]
                   )
              (update-global-env! (extend-env name reference the-global-env))
              (update-scope-env! (extend-env name reference the-scope-env))
              NULL)
            )
        (for_stmt (iter list_exp sts)
          (let ([list_exp (exp->value (value-of-expression list_exp))]) ;get the expression* object as list
            (_assign iter NULL)
            (let ([iter-ref (exp->value (apply-env iter the-scope-env))]) ; iter-ref is now the address of our iter. actually a pointer
              (let loop ([list_exp list_exp])
                (if (is_null_expression? list_exp)
                  NULL
                  (begin
                    (setref! iter-ref (exprs->first list_exp))
                    (let ([val (value-of (statements sts))])
                      (cases expression val
                        (break_flag () NULL)
                        (continue_flag () (loop (exprs->rest list_exp)))
                        (return_flag (ret_val) val)
                        (else (loop (exprs->rest list_exp)))
                      )
                    )
                  )
                )
              )
            )
          )
        )

        (return (expr) (return_flag (value-of-expression expr)))
    )
  )
)

(define (_assign var expr) 
  (let ([current_address (apply-env var the-scope-env)])
    (cases expression current_address
      [atomic_null_exp () 
        (let ([reference (ref_val (newref (a-thunk expr the-scope-env)))])
          (when (global-scope? the-scope-env)
              (update-global-env! (extend-env var reference the-global-env)))
          (update-scope-env! (extend-env var reference the-scope-env))
        )
      ]
      [ref_val (num) (setref! num (a-thunk expr the-scope-env))]
      [else report-reference-type-error]
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
        (empty-expr () (_pyprint (exprs->first exprs)))
        (expressions (new-expr new-rest-exprs) (_print rest-exprs) (display DELIMITER) (_pyprint expr) )
      )
    ]
  )
  )
)

(define (_pyprint expr)
(begin
; (printf "pypring expr:")
  ; (print expr)
  ; (newline)
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
              (loop (exprs->rest lst))
              (when (not (is_null_expression? (exprs->rest lst)))
                (display DELIMITER))
              (_pyprint (exprs->first lst))
            
              )))
      (display "]"))
    (else (report-type-error 'print))))
  ))

(define (value-of-expression expr)
  (begin
  ; (printf "value-of-expressison: ")
  ; (print expr)
  ; (newline)
    (cases expression expr
      [binary_op (op left right)
                 (let ([left-val (exp->value (value-of-expression left))])
                 (if (and (equal? op *) (zero? left-val))
                     (atomic_num_exp 0)
                     (let ([result (op left-val (exp->value (value-of-expression right)))])
                       (if (boolean? result)
                           (atomic_bool_exp result)
                           (atomic_num_exp result)
                           )
                       )
                     )
                   )
                 ]
      [unary_op (op operand) 
              (atomic_bool_exp (op (exp->value (value-of-expression operand))))
              ]
    ; [function_call (func args) ""]
    [function_call (func args)
                   (let ([function (value-of-expression func)]
                         [old-scope-env the-scope-env])
                     (update-scope-env! (extend-env-with-functions (empty-env #f)))
                     (cases proc function
                       (a-proc (p-name params p-body)
                               (let loop ([arguments args]
                                          [params params])
                                 (cond
                                   [(null? params) 888]
                                   [else (cases expression* arguments
                                           (empty-expr () (let ([param-def (car params)])
                                                            (update-scope-env! (extend-env
                                                                                (car param-def)
                                                                                (ref_val (newref (cadr param-def)))
                                                                                the-scope-env))
                                                            (loop arguments (cdr params))))
                                           (expressions (expr rest-exprs) (let ([param-def (car params)])
                                                                            (update-scope-env! (extend-env
                                                                                                (car param-def)
                                                                                                (ref_val (newref (a-thunk expr old-scope-env)))
                                                                                                the-scope-env))
                                                                            (loop rest-exprs (cdr params))))
                                           )]))
                               (let ([ret-val (value-of p-body)])
                                 (update-scope-env! old-scope-env)
                                 (cases expression ret-val
                                   (return_flag (val) val)
                                   (atomic_null_exp () NULL)
                                   (else (report-type-error 'value-of)))
                                 ))
                       (else (report-type-error 'value-of))))
                   ]
    [ref (var)
         (let ([val (deref (exp->value (apply-env var the-scope-env)))])
           (if (thunk? val)
               (value-of-thunk val)
               val
               )
           )
         ]
      [list_ref (ref index)

        (let ([lst (exp->value (value-of-expression ref))]
              [index (exp->value (value-of-expression index))])
            ; (printf "ref: ")
            ; (print ref)
            ; (newline)
            ; (printf "ref-val: ")
            ; (print (exp->value ref))
            ; (newline)
            
            (let loop ([lst lst][index index])
                (if (is_null_expression? lst) report-index-out-of-bound
                (if (zero? index) (exprs->first lst) (loop (exprs->rest lst) (- index 1)))
              )
            )
        )
      ]
      [atomic_bool_exp (bool) expr]
      [atomic_num_exp (num) expr]
      [atomic_null_exp () expr]
      [atomic_list_exp (l) expr]
      [else expr]
  )
  )
)



(define (value-of-thunk the-thunk)
  (cases thunk the-thunk
    (a-thunk (exp saved-env)
      (let ([old-scope-env the-scope-env])
        (update-scope-env! saved-env)
        (let ([val (value-of-expression exp)])
          (update-scope-env! old-scope-env)
          val)))))

; run test
(print (evaluate_print FILE_NAME))
(evaluate FILE_NAME)


