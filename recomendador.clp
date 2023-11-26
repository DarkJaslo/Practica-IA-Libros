; Propuesta de estructura


; Modulo principal
(defmodule MAIN (export ?ALL))

; Modulo obtencion datos
(defmodule preguntas-usuario
    (import MAIN ?ALL)
    (export ?ALL)
)

; Modulo para convertir al problema concreto en abstracto
(defmodule abstraccion-problema
    (import MAIN ?ALL)
    (export ?ALL)
)

; Modulo para solucionar el problema abstracto
(defmodule asociacion-heuristica
    (import MAIN ?ALL)
    (export ?ALL)
)

; Modulo para refinar la solucion (si lo llegamos a necesitar)
(defmodule refinamiento-solucion
    (import MAIN ?ALL)
    (export ?ALL)
)

; Definir template para problema concreto
; (deftemplate...)

; Definir template para problema abstracto
; (deftemplate...)

; Definir template para solucion abstracta
; (deftemplate...)

; Definir template para solucion concreta/refinada
; (deftemplate)

; ...

; Reglas: (ejemplos)

; (defrule abstraccion-problema:miRegla1)