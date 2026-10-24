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
    (primitive-binary (">") primitiva-mayor)
    (primitive-binary ("<") primitiva-menor)
    (primitive-binary (">=") primitiva-mayor-igual)
    (primitive-binary ("<=") primitiva-menor-igual)
    (primitive-binary ("!=") primitiva-diferente)
    (primitive-binary ("==") primitiva-comparador-igual)

    ;; Primitivas unarias basicas
    (primitive-unary ("longitud") primitiva-longitud)
    (primitive-unary ("add1") primitiva-add1)
    (primitive-unary ("sub1") primitiva-sub1)
    (primitive-unary ("neg") primitiva-negacion-booleana)
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


;; 3. LOGICA BOOLEANA Y EVALUACION DE PRIMITIVAS

(define valor-verdad?
  (lambda (val)
    (cond
      ((number? val) (not (zero? val)))
      ((boolean? val) val)
      ((string? val) #t)
      (else #t))))

(define apply-primitive-binary
  (lambda (prim val1 val2)
    (cases primitive-binary prim
      (primitiva-suma () (+ val1 val2))
      (primitiva-resta () (- val1 val2))
      (primitiva-div () (if (and (integer? val1) (integer? val2))
                            (quotient val1 val2)
                            (/ val1 val2)))
      (primitiva-multi () (* val1 val2))
      (primitiva-modulo () (modulo val1 val2))
      (primitiva-concat () (string-append val1 val2))
      (primitiva-mayor () (if (> val1 val2) 1 0))
      (primitiva-menor () (if (< val1 val2) 1 0))
      (primitiva-mayor-igual () (if (>= val1 val2) 1 0))
      (primitiva-menor-igual () (if (<= val1 val2) 1 0))
      (primitiva-diferente () (if (not (equal? val1 val2)) 1 0))
      (primitiva-comparador-igual () (if (equal? val1 val2) 1 0)))))

(define apply-primitive-unary
  (lambda (prim val)
    (cases primitive-unary prim
      (primitiva-longitud () (string-length val))
      (primitiva-add1 () (+ val 1))
      (primitiva-sub1 () (- val 1))
      (primitiva-negacion-booleana () (if (valor-verdad? val) 0 1)))))