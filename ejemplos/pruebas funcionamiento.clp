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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Templates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate MAIN::usuario
    (slot edad (type INTEGER))
    (multislot gusto-generos (type INSTANCE))
    (multislot gusto-temas (type INSTANCE))
)

(defrule MAIN::haz-preguntas-usuario
    (declare (salience 10))
    =>
    (focus preguntas-usuario)
)

(defrule preguntas-usuario::pregunta-edad
    ; poner un hecho que ahora este sin instanciar pero se instancie aqui (para que no se vuelva a llamar??)
    ?usr <- (usuario)
    =>
    ;esto siguiente se puede hacer mejor con una funcion pero ahora mismo no se hacer funciones aun
    (printout t "¿Cuantos años tienes?" crlf)
    (modify ?usr (edad (read)))
)