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

    ;; Variables locales. declarar ( @x=2; @y=3; ) { cuerpo }
    (expression ("declarar" "(" (separated-list identifier "=" expression ";") ")" "{" expression "}") variableLocal-exp)

    ;; Creación procedimientos. procedimiento (@x, @y) { cuerpo }
    (expression ("procedimiento" "(" (separated-list identifier ",") ")" "{" expression "}") procedimiento-ex)

    ;; Evaluación procedimientos. evaluar @proc (arg1, arg2) finEval
    (expression ("evaluar" expression "(" (separated-list expression ",") ")" "finEval") app-exp)

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

    ;; Primitivas unarias básicas
    (primitive-unary ("longitud") primitiva-longitud)
    (primitive-unary ("add1") primitiva-add1)
    (primitive-unary ("sub1") primitiva-sub1)
    (primitive-unary ("neg") primitiva-negacion-booleana)

    ;; Condicional if. Si test { true-exp } sino { false-exp } 
    (expression ("Si" expression "{" expression "}" "sino" "{" expression "}") condicional-exp)
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
   (amb-padre ambiente?)) 
  (extendido-recursivo ;; ambiente que soporta funciones recursivas
    (proc-names (list-of symbol?)) 
    (idss (list-of (list-of symbol?))) 
    (cuerpos (list-of expression?)) 
    (amb-padre ambiente?)))

(define empty-env
  (lambda () (vacio)))

(define extend-env
  (lambda (syms vals amb)
    (extendido syms vals amb)))

(define extend-env-recursively 
  (lambda (proc-names idss cuerpos amb) 
    (extendido-recursivo proc-names idss cuerpos amb)))

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
              (buscar-variable amb-padre search-var))))))
      (extendido-recursivo (proc-names idss cuerpos amb-padre) 
        (let ((pos (list-find-position search-var proc-names))) 
          (if (number? pos) 
              (cerradura (list-ref idss pos) 
                          (list-ref cuerpos pos) 
                          env) 
              (buscar-variable amb-padre search-var)))))

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

(define-datatype procVal procVal? 
(cerradura 
  (lista-ID (list-of symbol?))
  (exp expression?)
  (amb ambiente?)))

(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids cuerpo amb-guardado)
        (if (= (length ids) (length args)) 
        (eval-expression cuerpo (extend-env ids args amb-guardado))
        (eopl:error 'apply-procedure "Numero incorrecto de argumentos: se esperaban ~s, se recibieron ~s" 
                    (length ids) (length args)))))))

  ;; Definición del eval-program.

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (un-programa (exp)
        (eval-expression exp (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable env id))

      (primapp-bin-exp (exp1 prim exp2)
        (let ((val1 (eval-expression exp1 env))
              (val2 (eval-expression exp2 env)))
          (apply-primitive-binary prim val1 val2)))

      (primapp-un-exp (prim exp1)
        (let ((val (eval-expression exp1 env)))
          (apply-primitive-unary prim val)))

      (condicional-exp (test-exp true-exp false-exp)
        (if (valor-verdad? (eval-expression test-exp env))
            (eval-expression true-exp env)
            (eval-expression false-exp env)))

       ;; Condicional if (Revisar).

      (variableLocal-exp (ids exps cuerpo) (let ((vals (eval-rands exps env)))
      (eval-expression cuerpo (extend-env ids vals env)))) 

      (variableLocalRec-exp (proc-names idss cuerpos cuerpo) 
       (eval-expression cuerpo (extend-env-recursively proc-names idss cuerpos env)))

      (procedimiento-ex (ids cuerpo) 
        (cerradura ids cuerpo env))

      (app-exp (exp exps)
        (let ((proc (eval-expression exp env)) 
              (args (eval-rands exps env)))
          (if (procVal? proc)
              (apply-procedure proc args) 
              (eopl:error 'eval-expression "El valor no es un procedimiento valido: ~s" proc)))))))

(define eval-rands (lambda (exps env) ;; Evaluación de rands (Revisar)
(map (lambda (e) (eval-expression e env)) exps)))

(define interpretador
  (lambda (cadena)
    (eval-program (scan&parse cadena))))

;; 4. Programas en el Lenguaje y Pruebas Automatizadas

(define (probar nombre expresion-str esperado) ;; Revisión de pruebas (revisar)
  (let ((obtenido (interpretador expresion-str)))
    (display " ") (display nombre) (display " =&gt; ") (display obtenido)
    (if (equal? obtenido esperado)
        (begin (display " [OK]") (newline))
        (begin (display " [FALLO - Esperado: ") (display esperado) (display "]") (newline)))))

(display "=== Pruebas con condicional si-sino ===") (newline)
(probar "Si verdadero" "Si (2+3) { 2 } sino { 3 }" 2)
(probar "Si falso" "Si (longitud(@d) ~ 4) { 2 } sino { 3 }" 3)

(display "=== Pruebas con variables locales ===") (newline) 
(probar "declarar 1" "declarar (@x=2;@y=3;@a=7;){ (@a+(@x~@y)) }" 6) 
(probar "declarar 2" "declarar (@x=2;@y=3;@a=7;) { (@a+@b) }" 9)

(display "=== Procedimientos (procedimiento / evaluar) ===") (newline)
(probar "Procedimiento simple" 
"declarar (@x=2; @y=3; @a=procedimiento (@x,@y,@z) { ((@x+@y)+@z) } ;) { evaluar @a (1,2,@x) finEval }" 
5)
(probar "Procedimientos anidados" 
"declarar (@x=procedimiento (@a,@b) { ((@a*@a) + (@b*@b)) }; @y=procedimiento (@x,@y) { (@x+@y) } ;) { (evaluar @x(1,2) finEval + evaluar @y(2,3) finEval) }" 
10)


(define prog-9a "declararRec ( 
  @sumarDigitos (@n) = Si (@n &lt; 10) { @n } sino { ((@n % 10) + evaluar @sumarDigitos((@n / 10)) finEval) } ; ) { 
  evaluar @sumarDigitos(147) finEval }"
)

(define prog-9b-1 "declararRec ( 
  @sumaNaturales (@n) = Si (@n == 0) { 0 } sino { (@n + evaluar @sumaNaturales((@n ~ 1)) finEval) } ; ) { 
  evaluar @sumaNaturales(5) finEval }"
)

(define prog-9b-2 "declararRec ( 
  @sumaNaturales (@n) = Si (@n == 0) { 0 } sino { (@n + evaluar @sumaNaturales((@n ~ 1)) finEval) } ; ) { 
  evaluar @sumaNaturales(10) finEval }"
)

(define prog-9c "declararRec ( 
  @multiplicar (@a, @b) = Si (@b == 0) { 0 } sino { (@a + evaluar @multiplicar(@a, (@b ~ 1)) finEval) } ; ) { 
  evaluar @multiplicar(4, 3) finEval }"
)

(define prog-9d "declararRec ( 
  @sumaParesRango (@a, @b) = Si (@a &gt; @b) { 0 } sino { (Si ((@a % 2) == 0) { @a } sino { 0 } + evaluar @sumaParesRango((@a + 1), @b) finEval) } ; ) { 
  evaluar @sumaParesRango(2, 7) finEval }"
)

(define prog-9e "declarar ( 
  @integrantes = procedimiento () { \\"Maria\_Jhonnier\_Javier\\" } ; @saludar = procedimiento (@f) { procedimiento () { (\\"Hola:\\" concat evaluar @f () finEval) } } ; @decorate = evaluar @saludar (@integrantes) finEval ; ) { 
  evaluar @decorate () finEval }"
)

(define prog-9f "declarar ( 
  @integrantes = procedimiento () { \\"Maria\_Jhonnier\_Javier\\" } ; @saludar = procedimiento (@f) { procedimiento (@mensaje) { ((\\"Hola:\\" concat evaluar @f () finEval) concat @mensaje) } } ; @decorate = evaluar @saludar (@integrantes) finEval ; ) { 
  evaluar @decorate (\\"\_ProfesoresFLP\\") finEval }"
)

;; Verificacion de pruebas -9.

(titulo "Pruebas Punto 9: Programas y Decoradores") 
(probar "9a. sumarDigitos(147)" prog-9a 12) 
(probar "9b. sumaNaturales(5)" prog-9b-1 15) 
(probar "9b. sumaNaturales(10)" prog-9b-2 55) 
(probar "9c. multiplicar(4, 3)" prog-9c 12) 
(probar "9d. sumaParesRango(2, 7)" prog-9d 12) 
(probar "9e. Decorador simple" prog-9e "Hola:Maria_Jhonnier_Javier") 
(probar "9f. Decorador con parametro" prog-9f "Hola:Maria_Jhonnier_Javier_ProfesoresFLP")