#lang eopl
;; TALLER 3: INTERPRETADOR CON SLLGEN
;; Curso: Fundamentos de Lenguajes de Programacion (FLP).

;; - María Fernanda González - 202477325
;; - Jhonnier Andres Becerra Ballesteros - 201940979
;; - Javier Esteban Castrillon - 202510139

;; GitHub: https://github.com/Javier-C0L/Taller-3-FLP

;; 1. Especificación léxica y gramatical.
;; Define las reglas léxicas: reconoce espacios, comentarios, identificadores que comienzan con @, números y textos.
;; SLLGEN las usa para convertir la cadena de entrada en tokens utilizables.
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

;; Define la gramática del lenguaje y relaciona cada construcción sintáctica con su constructor de datatype:
;; operaciones, variables, condicionales, declaraciones, procedimientos y llamadas.
(define grammar-interpreter
  '((program (expression) un-programa)

    (expression (number) numero-lit)
    (expression ("\"" texto "\"") texto-lit)
    (expression (identifier) var-exp)

    (expression ("(" expression primitive-binary expression ")") primapp-bin-exp)
    (expression (primitive-unary "(" expression ")") primapp-un-exp)

    ;; Variables locales. declarar ( @x=2; @y=3; ) { cuerpo }
    ;;(expression ("declarar" "(" (separated-list identifier "=" expression ";") ")" "{" expression "}") variableLocal-exp)
    (expression ("declarar" "(" (arbno identifier "=" expression ";") ")" "{" expression "}") variableLocal-exp)


    ;; Variables recursivas locales: declararRec ( @f(@x) = cuerpo; ) { cuerpo } 
    (expression ("declararRec" "(" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression ";") ")" "{" expression "}") variableLocalRec-exp)

    ;; Creacion procedimientos. procedimiento (@x, @y) { cuerpo }
    (expression ("procedimiento" "(" (separated-list identifier ",") ")" "{" expression "}") procedimiento-ex)

    ;; Evaluacion procedimientos. evaluar @proc (arg1, arg2) finEval
    (expression ("evaluar" expression "(" (separated-list expression ",") ")" "finEval") app-exp)

    ;; Primitivas binarias básicas.
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

;; Genera automáticamente los datatypes de los árboles sintácticos a partir de la gramática, ahorrándonos el proceso de hacerlo a mano.
(sllgen:make-define-datatypes scanner-spec-interpreter grammar-interpreter)

;; Muestra en la consola los datatypes generados por SLLGEN.
;; Es una función auxiliar para inspeccionar la estructura del interpretador.
(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-interpreter grammar-interpreter)))

;; Construye el analizador que recibe una cadena de texto, la separa en tokens y la convierte en un árbol
;; sintáctico según la gramática del lenguaje.
(define scan&parse
  (sllgen:make-string-parser scanner-spec-interpreter grammar-interpreter))

;; 2. Datatypes y manejo de ambientes en el lenguaje.

;; Predicado usado para aceptar los valores almacenados.
;; En esta versión devuelve #t para cualquier valor; no realiza una validación de tipos.
(define scheme-value? (lambda (v) #t))

;; Define las formas posibles de un ambiente: vacío, extendido con variables y valores,
;; o extendido recursivamente con nombres, parámetros y cuerpos de procedimientos.
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

;; Crea y retorna un ambiente vacío.
(define empty-env
  (lambda () (vacio)))

;; Crea un nuevo ambiente que asocia una lista de identificadores con una lista de valores y conserva el ambiente anterior.
(define extend-env
  (lambda (syms vals amb)
    (extendido syms vals amb)))

;; Crea un nuevo ambiente que asocia una lista de identificadores con una lista de valores y conserva el ambiente anterior.
;; En este caso, añadiendo el detalle recursivo que lo diferencia del extend-env
(define extend-env-recursively 
  (lambda (proc-names idss cuerpos amb) 
    (extendido-recursivo proc-names idss cuerpos amb)))

;; Construye el ambiente inicial exigido por el taller, con @a, @b, @c, @d y @e asociadas respectivamente a 1, 2, 3, "hola" y "FLP".
(define init-env
  (lambda ()
    (extend-env '(@a @b @c @d @e)
                (list 1 2 3 "hola" "FLP")
                (empty-env))))

;; Busca de izquierda a derecha el primer elemento de una lista que satisface un predicado.
;; Retorna su índice empezando en cero o #f si no encuentra ninguno.
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((r (list-index pred (cdr ls))))
              (if (number? r) (+ r 1) #f))))))

;; Busca la posición de un símbolo dentro de una lista de símbolos,
;; usando eqv? para comparar. Retorna el índice o #f si el símbolo no aparece.
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (s) (eqv? s sym)) los)))

;; Busca el identificador en el ambiente actual.
;; Si lo encuentra, retorna su valor; si debe continuar, busca en el ambiente padre.
;; Para un ambiente recursivo, construye la cerradura del procedimiento correspondiente.
(define buscar-variable
  (lambda (env search-var)
    (cases ambiente env
      (vacio ()
        (eopl:error 'buscar-variable "Error, la variable no existe: ~s" search-var))
      (extendido (syms vals amb-padre)
        (let ((pos (list-find-position search-var syms)))
          (if (number? pos)
              (list-ref vals pos)
              (buscar-variable amb-padre search-var))))
      (extendido-recursivo (proc-names idss cuerpos amb-padre) 
        (let ((pos (list-find-position search-var proc-names))) 
          (if (number? pos) 
              (cerradura (list-ref idss pos) 
                          (list-ref cuerpos pos) 
                          env) 
              (buscar-variable amb-padre search-var)))))))

;; Alias de buscar-variable, utilizado para consultar el valor de un identificador en un ambiente.
(define APPLY-ENV buscar-variable)


;; 3. Lógica booleana y evaluación de las primitivas.

;; Determina si un valor cuenta como verdadero en los condicionales del lenguaje.
;; El número cero es falso; los demás números son verdaderos.
;; Los booleanos se respetan y los textos y otros valores se consideran verdaderos por defecto.
(define valor-verdad?
  (lambda (val)
    (cond
      ((number? val) (not (zero? val)))
      ((boolean? val) val)
      ((string? val) #t)
      (else #t))))

;; Evalúa una primitiva binaria ya identificada aplicándola a dos valores.
;; Incluye operaciones aritméticas, módulo, concatenación y comparaciones; las comparaciones producen 1 o 0.
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

;; Evalúa una primitiva unaria sobre un valor: longitud de texto, incremento, decremento o negación lógica expresada como 1/0.
(define apply-primitive-unary
  (lambda (prim val)
    (cases primitive-unary prim
      (primitiva-longitud () (string-length val))
      (primitiva-add1 () (+ val 1))
      (primitiva-sub1 () (- val 1))
      (primitiva-negacion-booleana () (if (valor-verdad? val) 0 1)))))

;; Define el valor de procedimiento (cerradura).
;; Guarda los identificadores de sus parámetros, el cuerpo de la función y el ambiente donde se creó, para conservar el ámbito léxico.
(define-datatype procVal procVal? 
(cerradura 
  (lista-ID (list-of symbol?))
  (exp expression?)
  (amb ambiente?)))

;; Aplica una cerradura a los argumentos ya evaluados.
;; Comprueba que coincida la cantidad de argumentos con los parámetros y evalúa el cuerpo en un ambiente extendido con esos parámetros.
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids cuerpo amb-guardado)
        (if (= (length ids) (length args)) 
        (eval-expression cuerpo (extend-env ids args amb-guardado))
        (eopl:error 'apply-procedure "Numero incorrecto de argumentos: se esperaban ~s, se recibieron ~s" 
                    (length ids) (length args)))))))


;; Extrae la expresión del constructor un programa y la evalúa dentro del ambiente incial.
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (un-programa (exp)
        (eval-expression exp (init-env))))))

;; Evalúa cada expresión de una lista de argumentos dentro del mismo ambiente y retorna la lista de valores resultante.
(define eval-rands 
  (lambda (exps env) 
    (map (lambda (e) (eval-expression e env)) exps)))

;; Recorre el árbol sintáctico y evalúa literales, variables, primitivas, condicionales,
;; declaraciones locales, procedimientos y aplicaciones de procedimientos. Evaluador principal.
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

      
      (variableLocal-exp (ids exps cuerpo) 
        (eval-declarar-seq ids exps cuerpo env)) 
        
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


;; Evalúa en orden las declaraciones de variables locales.
;; Cada valor se calcula en el ambiente disponible hasta ese momento y luego se incorpora antes de procesar la siguiente declaración;
;; al terminar, evalúa el cuerpo.
(define eval-declarar-seq 
  (lambda (ids exps cuerpo env) 
    (if (null? ids) 
      (eval-expression cuerpo env) 
      (let ((val (eval-expression (car exps) env))) 
        (eval-declarar-seq (cdr ids) (cdr exps) cuerpo 
                            (extend-env (list (car ids)) (list val) env))))))

;; Función pública del interpretador: recibe el programa como cadena, lo analiza con scan&parse y evalúa el árbol resultante mediante eval-program.
(define interpretador
  (lambda (cadena)
    (eval-program (scan&parse cadena))))

;; 4. Programas en el Lenguaje y Pruebas Automatizadas
;; En esta sección se definen los programas de los puntos 9a–9f y se ejecutan
;; pruebas para comparar los resultados obtenidos con los valores esperados.

;; Ejecuta una expresión de prueba, muestra el nombre y el valor obtenido, y lo compara con el resultado esperado.
;; Informa [OK] si coinciden o [FALLO] si son distintos. (El display es utilizado por nosotros como estudiantes para
;; verificar que lo que estamos haciendo está bien. Piedad 🙏)

(define (probar nombre expresion-str esperado) ;; Revision de pruebas (revisar)
  (let ((obtenido (interpretador expresion-str)))
    (display " ") (display nombre) (display " > ") (display obtenido)
    (if (equal? obtenido esperado)
        (begin (display " [OK]") (newline))
        (begin (display " [FALLO - Esperado: ") (display esperado) (display "]") (newline)))))


;; Sección nueve.


;; Programa del punto 9a: define recursivamente @sumarDigitos y prueba que la suma de los dígitos de 147 sea 12.
(define prog-9a "declararRec ( 
  @sumarDigitos (@n) = Si (@n < 10) { @n } sino { ((@n % 10) + evaluar @sumarDigitos((@n / 10)) finEval) } ; ) { 
  evaluar @sumarDigitos(147) finEval }"
)

;; Primera prueba del punto 9b: define @sumaNaturales recursivamente y calcula la suma de los enteros positivos hasta 5, cuyo resultado esperado es 15.
(define prog-9b-1 "declararRec ( 
  @sumaNaturales (@n) = Si (@n == 0) { 0 } sino { (@n + evaluar @sumaNaturales((@n ~ 1)) finEval) } ; ) { 
  evaluar @sumaNaturales(5) finEval }"
)

;; Segunda prueba del punto 9b: usa la misma definición recursiva de @sumaNaturales para calcular la suma hasta 10, cuyo resultado esperado es 55.
(define prog-9b-2 "declararRec ( 
  @sumaNaturales (@n) = Si (@n == 0) { 0 } sino { (@n + evaluar @sumaNaturales((@n ~ 1)) finEval) } ; ) { 
  evaluar @sumaNaturales(10) finEval }"
)

;; Programa del punto 9c: define @multiplicar mediante sumas sucesivas y recursión, sin usar la primitiva de multiplicación para calcular el producto.
(define prog-9c "declararRec ( 
  @multiplicar (@a, @b) = Si (@b == 0) { 0 } sino { (@a + evaluar @multiplicar(@a, (@b ~ 1)) finEval) } ; ) { 
  evaluar @multiplicar(4, 3) finEval }"
)

;; Programa del punto 9d: recorre recursivamente el rango desde @a hasta @b y suma los valores pares. La prueba solicitada suma 2 + 4 + 6 = 12.
(define prog-9d "declararRec ( 
  @sumaParesRango (@a, @b) = Si (@a > @b) { 0 } sino { (Si ((@a % 2) == 0) { @a } sino { 0 } + evaluar @sumaParesRango((@a + 1), @b) finEval) } ; ) { 
  evaluar @sumaParesRango(2, 7) finEval }"
)

;; Programa del punto 9e: @integrantes retorna los nombres del grupo; @saludar recibe un procedimiento y devuelve otro que antepone "Hola:" al resultado del procedimiento recibido. @decorate guarda ese procedimiento decorado.
(define prog-9e 
  "declarar ( 
    @integrantes = procedimiento () { \"Maria_Jhonnier_Javier\" } ; 
    @saludar = procedimiento (@f) { procedimiento () { (\"Hola:\" concat evaluar @f () finEval) } } ; 
    @decorate = evaluar @saludar (@integrantes) finEval ; 
  ) { 
    evaluar @decorate () finEval 
  }")

;; Programa del punto 9f: extiende el decorador anterior para que el procedimiento devuelto reciba un mensaje y lo concatene al final del saludo y los nombres.
(define prog-9f 
  "declarar ( 
    @integrantes = procedimiento () { \"Maria_Jhonnier_Javier\" } ; 
    @saludar = procedimiento (@f) { procedimiento (@mensaje) { ((\"Hola:\" concat evaluar @f () finEval) concat @mensaje) } } ; 
    @decorate = evaluar @saludar (@integrantes) finEval ; 
  ) { 
    evaluar @decorate (\"_ProfesoresFLP\") finEval 
  }")

;; Ejecuta las pruebas de condicionales, declaraciones locales, procedimientos,
;; recursión y decoradores. Cada llamada a probar informa si el resultado coincide.
;; Verificacion de pruebas 

#| 

  
|#

(display "=== Pruebas con condicional si-sino ===") (newline)
(probar "Si verdadero" "Si (2+3) { 2 } sino { 3 }" 2)
(probar "Si falso" "Si (longitud(@d) ~ 4) { 2 } sino { 3 }" 3)

(display "=== Pruebas con variables locales ===") (newline) 
(probar "declarar 1" "declarar (@x=2;@y=3;@a=7;){ (@a+(@x~@y)) }" 6) 
(probar "declarar 2" "declarar (@x=2;@y=3;@a=7;) { (@a+@b) }" 9)

(display "=== Procedimientos (procedimiento / evaluar) ===") (newline)
(probar "Procedimiento simple" 
"declarar (@x=2; @y=3; @a=procedimiento (@x,@y,@z) { ((@x+@y)+@z) } ;) { evaluar @a (1,2,@x) finEval }" 5)

(probar "Procedimientos anidados" 
"declarar (@x=procedimiento (@a,@b) { ((@a*@a) + (@b*@b)) }; 
@y=procedimiento (@x,@y) { (@x+@y) } ;) { (evaluar @x(1,2) finEval + evaluar @y(2,3) finEval) }" 10)

(display " === Pruebas Punto 9: Programas y Decoradores === \n") 
(probar "9a. sumarDigitos(147)" prog-9a 12) 
(probar "9b. sumaNaturales(5)" prog-9b-1 15) 
(probar "9b. sumaNaturales(10)" prog-9b-2 55) 
(probar "9c. multiplicar(4, 3)" prog-9c 12) 
(probar "9d. sumaParesRango(2, 7)" prog-9d 12) 
(probar "9e. Decorador simple" prog-9e "Hola:Maria_Jhonnier_Javier") 
(probar "9f. Decorador con parametro" prog-9f "Hola:Maria_Jhonnier_Javier_ProfesoresFLP")


;; Pruebas adicionales de las operaciones (revisar).
(probar "Suma" "(2 + 3)" 5)
(probar "Resta con negativo" "(4 ~ -5)" 9)
(probar "Mayor verdadero" "(6 > 2)" 1)
(probar "Mayor falso" "(2 > 6)" 0)
(probar "Longitud" "longitud(@d)" 4)
(probar "Incremento" "add1(5)" 6)
(probar "Decremento" "sub1(5)" 4)
(probar "Negación de verdadero" "neg(5)" 0)

 
