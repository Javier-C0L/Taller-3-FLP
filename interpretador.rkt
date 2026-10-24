#lang eopl
;; TALLER 3: INTERPRETADOR CON SLLGEN
;; Curso: Fundamentos de Lenguajes de Programacion

;; 1. ESPECIFICACION LEXICA Y GRAMATICAL BASICA
(define scanner-spec-interpreter
  '((white-sp
     (whitespace) skip)
    (comment
     ("//" (arbno (not #\newline))) skip)
    (identifier
     ("@" (or letter "_") (arbno (or letter digit "_" "?"))) symbol)
    (number
     (digit (arbno digit)) number)
    (number
     ("-" digit (arbno digit)) number)
    (number
     (digit (arbno digit) "." digit (arbno digit)) number)
    (number
     ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (texto
     ((or letter "_") (arbno (or letter digit "_" ":" "-"))) string)))

(define grammar-interpreter
  '((program (expression) un-programa)

    (expression (number) numero-lit)
    (expression ("\"" texto "\"") texto-lit)
    (expression (identifier) var-exp)

    (expression ("(" expression primitive-binary expression ")") primapp-bin-exp)
    (expression (primitive-unary "(" expression ")") primapp-un-exp)

    ;; Primitivas binarias basicas
    (primitive-binary ("+") primitiva-suma)
    (primitive-binary ("~") primitiva-resta)
    (primitive-binary ("/") primitiva-div)
    (primitive-binary ("*") primitiva-multi)
    (primitive-binary ("%") primitiva-modulo)
    (primitive-binary ("concat") primitiva-concat)

    ;; Primitivas unarias basicas
    (primitive-unary ("longitud") primitiva-longitud)
    (primitive-unary ("add1") primitiva-add1)
    (primitive-unary ("sub1") primitiva-sub1)
  ))

(sllgen:make-define-datatypes scanner-spec-interpreter grammar-interpreter)

(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-interpreter grammar-interpreter)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-interpreter grammar-interpreter))