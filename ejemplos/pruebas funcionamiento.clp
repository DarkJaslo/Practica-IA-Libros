;;; ---------------------------------------------------------
;;; ontologia.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ontologia.ttl
;;; :Date 02/12/2023 16:00:29

(defclass Publicador
    (is-a USER)
    (role abstract)
    (pattern-match non-reactive)
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Autopublicador
    (is-a Publicador)
    (role concrete)
    (pattern-match reactive)
)

(defclass Editorial
    (is-a Publicador)
    (role concrete)
    (pattern-match reactive)
)

(defclass Manga
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot pertenece-a
        (type INSTANCE)
        (create-accessor read-write))
    (slot publicado-por
        (type INSTANCE)
        (create-accessor read-write))
    (multislot trata-de
        (type INSTANCE)
        (create-accessor read-write))
    (slot capitulos
        (type INTEGER)
        (create-accessor read-write))
    (slot copias-vendidas
        (type INTEGER)
        (create-accessor read-write))
    ;;; Valores: (baja | media | alta)
    (slot dificultad-lectura
        (type STRING)
        (create-accessor read-write)
        (allowed-values "baja" "media" "alta"))
    ;;; Valores: (acabado | en publicacion | en pausa | cancelado)
    (slot estado-publicacion
        (type STRING)
        (create-accessor read-write)
        (allowed-values "acabado" "en publicacion" "en pausa" "cancelado"))
    (slot fin-publicacion
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Valores: (semanal | quincenal | mensual | bimestral | trimestral | semestral | irregular)
    (slot frecuencia-publicacion
        (type STRING)
        (create-accessor read-write)
        (allowed-values "semanal" "quincenal" "mensual" "bimestral" "trimestral" "semestral" "irregular"))
    (slot inicio-publicacion
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Valores: (digital | fisico | ambos)
    (slot metodo-distribucion
        (type STRING)
        (create-accessor read-write)
        (allowed-values "digital" "fisico" "ambos"))
    ;;; Valores: (gratuito | de pago)
    (slot precio
        (type STRING)
        (create-accessor read-write)
        (allowed-values "gratuito" "de pago"))
    (slot restriccion-edad
        (type INTEGER)
        (create-accessor read-write))
    (slot tiene-anime
        (type SYMBOL)
        (create-accessor read-write))
    (slot valoracion
        (type FLOAT)
        (create-accessor read-write))
    (slot titulo
        (type STRING)
        (create-accessor read-write))
)

(defclass One-shot
    (is-a Manga)
    (role concrete)
    (pattern-match reactive)
)

(defclass Serializado
    (is-a Manga)
    (role concrete)
    (pattern-match reactive)
    (slot tomos
        (type INTEGER)
        (create-accessor read-write))
)

(defclass Autor
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot escribe
        (type INSTANCE)
        (create-accessor read-write))
    (multislot ilustra
        (type INSTANCE)
        (create-accessor read-write))
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Genero
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Tema
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(definstances instances
    ([eiichiro-oda-p] of Autopublicador
         (nombre  "Eiichiro Oda")
    )

    ([eiichiro-oda] of Autor
         (escribe  [one-piece])
         (ilustra  [one-piece])
         (nombre  "Eiichiro Oda")
    )

    ([one-piece] of Serializado
         (tomos  107)
         (publicado-por  [eiichiro-oda-p])
         (capitulos  1099)
         (copias-vendidas  598957)
         (dificultad-lectura  "facil")
         (estado-publicacion  "en publicacion")
         (frecuencia-publicacion  "semanal")
         (inicio-publicacion  "1997-07-22")
         (metodo-distribucion  "ambos")
         (precio  "pago")
         (tiene-anime  "true")
         (titulo  "One Piece")
         (valoracion  9.22)
    )

    ([shueisha] of Editorial
         (nombre  "Shueisha")
    )

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Modulos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; No es lo mas eficiente exportar todo

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Funciones ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; funcion que nos dan en el FAQ
; recibe la pregunta que hacer al usuario y el rango de valores valido y los imprime
; lee el input y si es incorrecto vuelve a escribir la pregunta y leer hasta que
; sea correcto, entonces devuelve el valor leido
(deffunction MAIN::pregunta-numerica (?pregunta ?rangini ?rangfi)
    (format t "%s [%d, %d] " ?pregunta ?rangini ?rangfi)
    (bind ?respuesta (read))
    (while (not(and(> ?respuesta ?rangini)(< ?respuesta ?rangfi))) do
        (format t "%s [%d, %d] " ?pregunta ?rangini ?rangfi)
        (bind ?respuesta (read))
    )
    ?respuesta
)

; funcion que nos dan en el FAQ
(deffunction pregunta-enum (?pregunta $?valores-permitidos)
    (progn$
        (?var ?valores-permitidos)
        (lowcase ?var))
    (format t "%s (%s) " ?pregunta (implode$ ?valores-permitidos))
    (bind ?respuesta (read))
    (while (not (member$ (lowcase ?respuesta) ?valores-permitidos)) do
        (format t "%s (%s) " ?pregunta (implode$ ?valores-permitidos))
        (bind ?respuesta (read))
    )
    ?respuesta
)

; funcion que nos dan en el FAQ
(deffunction pregunta-si-no (?pregunta)
    (format t "%s " ?pregunta)
    (bind ?respuesta (read))
    (if (or (eq (lowcase ?respuesta) si) (eq (lowcase ?respuesta) s))
        then TRUE
        else FALSE
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Templates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate MAIN::usuario
    (slot edad (type INTEGER))
    (slot ve-anime (type SYMBOL))
    (slot leeria-anime-ya-visto (type SYMBOL)
                                (default FALSE))
    (slot mangas-leidos (type SYMBOL) ; esto lo cambiaria un poco
                        (allowed-values pocos normal muchos))
    (slot mangas-animes-vistos (type SYMBOL) ; esto lo cambiaria un poco
                        (allowed-values pocos normal muchos))
    (multislot gusto-generos (type INSTANCE))
    (multislot gusto-temas (type INSTANCE))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reglas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Funcion que solo activa el modulo preguntas-usuario
; salience para que se llame la primera
(defrule MAIN::haz-preguntas-usuario
    (declare (salience 10))
    =>
    (assert (usuario))
    (focus preguntas-usuario)
)

(defrule preguntas-usuario::pregunta-edad
    ; control para que no se llame mas de una vez
    (not (edad-preguntada))
    ; coge la direccion del fact usuario para modificarlo luego
    ?usr <- (usuario)
    =>
    ; modifica la edad del usuario con el resultado que devuelve la funcion pregunta-numerica
    (modify ?usr (edad (pregunta-numerica "¿Cuantos años tienes?" 0 150)))
    (assert (edad-preguntada))
)

(defrule preguntas-usuario::pregunta-ve-anime
    ; control de orden
    (edad-preguntada)
    ; control para que no se llame mas de una vez
    (not (ve-anime-preguntado))
    ; coge la direccion del fact usuario para modificarlo luego
    ?usr <- (usuario)
    =>
    ; modifica la edad del usuario con el resultado que devuelve la funcion pregunta-numerica
    (modify ?usr (ve-anime (pregunta-si-no "¿Ves anime?")))
    (assert (ve-anime-preguntado))
)

(defrule preguntas-usuario::pregunta-animes-ya-vistos
    ; control para que no se llame mas de una vez
    (not (animes-ya-vistos-preguntado))
    ; coge la direccion del fact usuario para modificarlo luego
    ?usr <- (usuario)
    ; solo se pregunta si ha respondido "si" a ver anime
    (usuario (ve-anime TRUE))
    =>
    ; modifica la edad del usuario con el resultado que devuelve la funcion pregunta-numerica
    (modify ?usr (leeria-anime-ya-visto (pregunta-si-no "¿Leerias el manga de animes que ya has visto?")))
    (assert (animes-ya-vistos-preguntado))
)

; Flujo preguntas 1
(defrule preguntas-usuario::pregunta-mangas-leidos
    ; control de orden
    (ve-anime-preguntado)
    ; o no ve anime o ha respondido que le parece bien leer mangas de animes que ya ha visto
    
    (or (usuario (ve-anime FALSE)) (and (animes-ya-vistos-preguntado) (usuario (leeria-anime-ya-visto TRUE))))
    ; control para que no se llame mas de una vez
    (not (mangas-leidos-preguntado))
    ; coge la direccion del fact usuario para modificarlo luego
    ?usr <- (usuario)
    =>
    ; modifica la edad del usuario con el resultado que devuelve la funcion pregunta-numerica
    (modify ?usr (mangas-leidos (pregunta-enum "¿Cuantos mangas has leido aproximadamente?" pocos normal muchos)))
    (assert (mangas-leidos-preguntado))
)

; Flujo preguntas 2
(defrule preguntas-usuario::pregunta-mangas-animes-vistos
    ; control de orden
    (animes-ya-vistos-preguntado)
    ; no quiere leer mangas de animes que ya ha visto
    (usuario (leeria-anime-ya-visto FALSE))
    ; control para que no se llame mas de una vez
    (not (mangas-animes-vistos-preguntado))
    ; coge la direccion del fact usuario para modificarlo luego
    ?usr <- (usuario)
    =>
    ; modifica la edad del usuario con el resultado que devuelve la funcion pregunta-numerica
    (modify ?usr (mangas-animes-vistos (pregunta-enum "¿Cuantos mangas/animes has leido aproximadamente?" pocos normal muchos)))
    (assert (mangas-animes-vistos-preguntado))
)
