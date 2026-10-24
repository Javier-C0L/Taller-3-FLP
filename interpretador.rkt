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

;; 2. DATATYPES Y MANEJO DE AMBIENTES

(define scheme-value? (lambda (v) #t))

(define-datatype ambiente ambiente?
  (vacio)
  (extendido
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (amb-padre ambiente?)))

(define empty-env
  (lambda () (vacio)))

(define extend-env
  (lambda (syms vals amb)
    (extendido syms vals amb)))

(define init-env
  (lambda ()
    (extend-env '(@a @b @c @d @e)
                (list 1 2 3 "hola" "FLP")
                (empty-env))))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((r (list-index pred (cdr ls))))
              (if (number? r) (+ r 1) #f))))))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (s) (eqv? s sym)) los)))

(define buscar-variable
  (lambda (env search-var)
    (cases ambiente env
      (vacio ()
        (eopl:error 'buscar-variable "Error, la variable no existe: ~s" search-var))
      (extendido (syms vals amb-padre)
        (let ((pos (list-find-position search-var syms)))
          (if (number? pos)
              (list-ref vals pos)
              (buscar-variable amb-padre search-var)))))))

(define APPLY-ENV buscar-variable)