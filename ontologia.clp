;;; ---------------------------------------------------------
;;; ontologia.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ontologia.ttl
;;; :Date 01/12/2023 21:52:47

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

(defclass Editorial
    (is-a Autor)
    (role concrete)
    (pattern-match reactive)
)

(defclass Manga
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot publicado-por
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
        (create-accessor read-write))
        (allowed-values "baja" "media" "alta")
    ;;; Valores: (acabado | en publicacion | en pausa | cancelado)
    (slot estado-publicacion
        (type STRING)
        (create-accessor read-write))
        (allowed-values "acabado" "en publicacion" "en pausa" "cancelado")
    (slot fin-publicacion
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Valores: (semanal | quincenal | mensual | bimestral | trimestral | semestral | irregular)
    (slot frecuencia-publicacion
        (type STRING)
        (create-accessor read-write))
        (allowed-values "semanal" "quincenal" "mensual" "bimestral" "trimestral" "semestral" "irregular")
    (slot inicio-publicacion
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Valores: (digital | fisico | ambos)
    (slot metodo-distribucion
        (type STRING)
        (create-accessor read-write))
        (allowed-values "digital" "fisico" "ambos")
    ;;; Valores: (gratuito | de pago)
    (slot precio
        (type STRING)
        (create-accessor read-write))
        (allowed-values "gratuito" "de pago")
    (slot restriccion-edad
        (type INTEGER)
        (create-accessor read-write))
    (slot tiene-anime
        (type SYMBOL)
        (create-accessor read-write))
    (slot valoracion
        (type FLOAT)
        (create-accessor read-write))
    (multislot genero
        (type STRING)
        (create-accessor read-write))
    (multislot tema
        (type STRING)
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

(defclass Publicador
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Autor-Publicador
    (is-a Publicador)
    (role concrete)
    (pattern-match reactive)
)

(defclass Editorial
    (is-a Publicador)
    (role concrete)
    (pattern-match reactive)
)

(definstances instances
    ([eiichiro-oda] of Autor
         (escribe  [one-piece])
         (ilustra  [one-piece])
         (nombre  "Eiichiro Oda")
    )

    ([one-piece] of Serializado
         (tomos  107)
         (publicado-por  [shueisha])
         (capitulos  1099)
         (copias-vendidas  598957)
         (dificultad-lectura  "facil")
         (estado-publicacion  "en publicacion")
         (frecuencia-publicacion  "semanal")
         (inicio-publicacion  "1997-07-22T00:00:00")
         (metodo-distribucion  "ambos")
         (precio  "pago")
         (tiene-anime  "true")
         (valoracion  9.22)
         (genero  "Accion" "Aventura")
         (tema  "Super poderes")
         (titulo  "One Piece")
    )

    ([shueisha] of Editorial
         (nombre  "Shueisha")
    )

)
