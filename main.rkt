#lang racket

(require "passes/parser.rkt")
(require "passes/lexer.rkt")

(define (parse-scan prog-string)
  (python-parser (lex-this prog-string))
  )

(define (evaluate file-name)
  (parse-scan (string-join (file->lines file-name)))
  )

(provide (all-defined-out))