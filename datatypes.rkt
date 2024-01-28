#lang racket
(require (lib "eopl.ss" "eopl"))

(define-datatype statement statement?
  (statements (stms (list-of statement?)))
  (assign (var string?) (expr expression?))
  (global (var string?))
  (return (expr expression?))
  (return_void)
  (pass)
  (break)
  (continue)
  (func (name string?) (params func_param*?) (statements list?))
  (if_stmt (cond_exp expression?) (if_sts list?) (else_sts list?))
  (for_stmt (iter string?) (list_exp expression?) (sts list?))
  (print_stmt (expressions expression*?))
  )

(define-datatype func_param func_param?
  (with_default (var string?) (expr expression?))
  )

(define-datatype func_param* func_param*?
  (empty-param)
  (func_params (param func_param?) (rest-params func_param*?))
  )

(define-datatype expression expression?
  (binary_op (op procedure?) (left expression?) (right expression?))
  (unary_op (op procedure?) (operand expression?))
  (function_call (func expression?) (params expression*?))
  (list_ref (ref expression?) (index expression?))
  (ref (var string?))

  (atomic_bool_exp (bool boolean?))
  (atomic_num_exp (num number?))
  (atomic_null_exp)
  (atomic_list_exp (l expression*?))
; added by me
  (continue_flag)
  (break_flag)
  (return_void_flag)
  (pass_flag)
)


(define-datatype expression* expression*?
  (empty-expr)
  (expressions (expr expression?) (rest-exprs expression*?))
)

(define (exprs->first exprs)
  (cases expression* exprs
    (empty-expr () "error, no first exists")
    (expressions (expr rest-exprs) expr)  
  )
)

(define (exprs->rest exprs)
  (cases expression* exprs
    (empty-expr () "error, no rest exists")
    (expressions (expr rest-exprs) rest-exprs)  
  )
)

(define (is_null_expression? exprs)
  (cases expression* exprs
    (empty-expr () #t)
    (else #f)  
  )
)

(define (exp->value expr)
(begin
  ; (print "exp->val: ")
  ; (print expr)
  ; (newline)

  (cases expression expr
    [atomic_bool_exp (bool) bool]
    [atomic_list_exp (l) l]
    [atomic_num_exp (num) num]
    ; [atomic_null_exp () "not supported"]
    ; [binary_op (op left right) "not supported"]
    ; [unary_op (op operand) "not supported"]
    ; [function_call (func params) "not supported"]
    ; [list_ref (ref index) ref]
    [ref (var) var]
    [else "not supported"]
  )
)
)

(provide (all-defined-out))
(#%provide (all-defined))