;;; ---------------------------------------------------------
;;; ontologia.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ontologia.ttl
;;; :Date 04/12/2023 08:50:00

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
    (role abstract)
    (pattern-match non-reactive)
    (multislot pertenece-a
        (type INSTANCE)
        (create-accessor read-write)
        (visibility public))
    (slot publicado-por
        (type INSTANCE)
        (create-accessor read-write)
        (visibility public))
    (multislot trata-de
        (type INSTANCE)
        (create-accessor read-write)
        (visibility public))
    (slot capitulos
        (type INTEGER)
        (create-accessor read-write)
        (visibility public))
    (slot copias-vendidas
        (type INTEGER)
        (create-accessor read-write)
        (visibility public))
    ;;; Valores: (baja | media | alta)
    (slot dificultad-lectura
        (type STRING)
        (create-accessor read-write)
        (visibility public)
        (allowed-values "baja" "media" "alta"))
    (slot inicio-publicacion
        (type SYMBOL)
        (create-accessor read-write)
        (visibility public))
    ;;; Valores: (digital | fisico | ambos)
    (slot metodo-distribucion
        (type STRING)
        (create-accessor read-write)
        (visibility public)
        (allowed-values "digital" "fisico" "ambos"))
    (slot restriccion-edad
        (type INTEGER)
        (create-accessor read-write)
        (visibility public))
    (slot tiene-anime
        (type SYMBOL)
        (create-accessor read-write)
        (visibility public))
    (slot titulo
        (type STRING)
        (create-accessor read-write)
        (visibility public))
    (slot valoracion
        (type FLOAT)
        (create-accessor read-write)
        (visibility public))
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
    ;;; Valores: (acabado | en publicacion | en pausa | cancelado)
    (slot estado-publicacion
        (type STRING)
        (create-accessor read-write)
        (visibility public)
        (allowed-values "acabado" "en publicacion" "en pausa" "cancelado"))
    ;;; Valores: (semanal | quincenal | mensual | bimestral | trimestral | semestral | irregular)
    (slot frecuencia-publicacion
        (type STRING)
        (create-accessor read-write)
        (allowed-values "semanal" "quincenal" "mensual" "bimestral" "trimestral" "semestral" "irregular"))
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