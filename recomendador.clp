;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ontologia ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Instancias ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definstances instancias
([one-piece] of Serializado
	(titulo  "One Piece")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [aventura]  [comedia]  [fantasia])
	(trata-de  [superpoderes])
	(tomos  107)
	(capitulos  1100)
	(copias-vendidas  528000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1997-07-22")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  9.22)
	(restriccion-edad  12)
)
([hunter-x-hunter] of Serializado
	(titulo  "Hunter x Hunter")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [aventura]  [fantasia])
	(trata-de  [superpoderes])
	(tomos  37)
	(capitulos  400)
	(copias-vendidas  84000000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "en pausa")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "1998-03-03")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.73)
	(restriccion-edad  12)
)
([jojo-no-kimyou-na-bouken-part-5-ougon-no-kaze] of Serializado
	(titulo  "JoJo no Kimyou na bouken part 5: Ougon no kaze")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [aventura]  [drama]  [sobrenatural])
	(trata-de  [delincuencia]  [historico]  [superpoderes])
	(tomos  17)
	(capitulos  155)
	(copias-vendidas  10000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1995-11-28")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.17)
	(restriccion-edad  12)
)
([jojo-no-kimyou-na-bouken-part-4-diamond-wa-kudakenai] of Serializado
	(titulo  "JoJo no Kimyou na bouken part 4: Diamond wa kudakenai")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [slice-of-life]  [misterio]  [sobrenatural])
	(trata-de  [escolar]  [superpoderes])
	(tomos  18)
	(capitulos  174)
	(copias-vendidas  100000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1992-04-21")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.57)
	(restriccion-edad  12)
)
([jojo-no-kimyou-na-bouken-part-3-stardust-crusaders] of Serializado
	(titulo  "JoJo no Kimyou na bouken part 3: Stardust crusaders")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [aventura]  [sobrenatural]  [suspense])
	(trata-de  [superpoderes]  [historico])
	(tomos  16)
	(capitulos  152)
	(copias-vendidas  94600)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1989-04-21")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.1)
	(restriccion-edad  12)
)
([jojo-no-kimyou-na-bouken-part-2-battle-tendency] of Serializado
	(titulo  "JoJo no Kimyou na bouken part 2: Battle tendency")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [aventura]  [suspense])
	(trata-de  [superpoderes]  [vampiros]  [historico])
	(tomos  7)
	(capitulos  69)
	(copias-vendidas  102000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1987-04-21")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.15)
	(restriccion-edad  12)
)
([jojo-no-kimyou-na-bouken-part-1-phantom-blood] of Serializado
	(titulo  "JoJo no Kimyou na bouken part 1: Phantom blood")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [aventura]  [suspense])
	(trata-de  [superpoderes]  [vampiros]  [historico])
	(tomos  5)
	(capitulos  44)
	(copias-vendidas  125000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1985-04-21")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.6)
	(restriccion-edad  12)
)
([kaguya-sama-wa-kokurasetai-tensai-tachi-no-renai-zunousen] of Serializado
	(titulo  "Kaguya-sama wa kokurasetai: Tensai-tachi no renai zunousen")
	(publicado-por  [shueisha])
	(pertenece-a  [romance]  [comedia]  [psicologico])
	(trata-de  [escolar])
	(tomos  28)
	(capitulos  281)
	(copias-vendidas  22000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2015-05-19")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.91)
	(restriccion-edad  12)
)
([world-trigger] of Serializado
	(titulo  "World Trigger")
	(publicado-por  [shueisha])
	(pertenece-a  [ciencia-ficcion]  [accion])
	(trata-de  [superpoderes])
	(tomos  17)
	(capitulos  200)
	(copias-vendidas  15000000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2013-01-01")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.81)
	(restriccion-edad  8)
)
([naruto] of Serializado
	(titulo  "Naruto")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [aventura]  [fantasia]  [drama])
	(trata-de  [artes-marciales]  [tradicional-japones]  [superpoderes]  [venganza])
	(tomos  72)
	(capitulos  700)
	(copias-vendidas  250000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1999-01-01")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.07)
	(restriccion-edad  12)
)
([shingeki-no-kyojin] of Serializado
	(titulo  "Shingeki no Kyojin")
	(publicado-por  [kodansha])
	(pertenece-a  [accion]  [drama])
	(trata-de  [gore]  [guerra]  [supervivencia]  [postapocaliptico])
	(tomos  34)
	(capitulos  141)
	(copias-vendidas  140000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2009-03-03")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.55)
	(restriccion-edad  16)
)
([death-note] of Serializado
	(titulo  "Death Note")
	(publicado-por  [shueisha])
	(pertenece-a  [sobrenatural]  [suspense]  [psicologico])
	(trata-de  [detectives]  [personajes-adultos])
	(tomos  12)
	(capitulos  108)
	(copias-vendidas  30000000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2003-07-23")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.69)
	(restriccion-edad  16)
)
([detective-conan] of Serializado
	(titulo  "Detective Conan")
	(publicado-por  [shogakukan])
	(pertenece-a  [aventura]  [comedia]  [misterio])
	(trata-de  [detectives])
	(tomos  104)
	(capitulos  1111)
	(copias-vendidas  270000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "1994-01-05")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.29)
	(restriccion-edad  10)
)
([made-in-abyss] of Serializado
	(titulo  "Made in Abyss")
	(publicado-por  [takeshobo])
	(pertenece-a  [aventura]  [ciencia-ficcion]  [drama]  [fantasia]  [horror]  [misterio]  [psicologico])
	(trata-de  [gore]  [supervivencia])
	(tomos  12)
	(capitulos  66)
	(copias-vendidas  1965000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2012-10-20")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.77)
	(restriccion-edad  18)
)
([pretty-soldier-sailor-moon] of Serializado
	(titulo  "Pretty Soldier Sailor Moon")
	(publicado-por  [kodansha])
	(pertenece-a  [fantasia]  [romance])
	(trata-de  [mahou-shoujo]  [mitologia])
	(tomos  18)
	(capitulos  52)
	(copias-vendidas  46000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "1991-12-28")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.2)
	(restriccion-edad  7)
)
([one-punch-man] of Serializado
	(titulo  "One Punch Man")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [comedia]  [ciencia-ficcion])
	(trata-de  [parodia]  [personajes-adultos]  [superpoderes])
	(tomos  29)
	(capitulos  185)
	(copias-vendidas  15000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2012-06-14")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.75)
	(restriccion-edad  12)
)
([berserk] of Serializado
	(titulo  "Berserk")
	(publicado-por  [hakusensha])
	(pertenece-a  [accion]  [aventura]  [drama]  [fantasia]  [horror]  [sobrenatural]  [psicologico])
	(trata-de  [gore]  [guerra]  [mitologia]  [tragedia]  [venganza])
	(tomos  42)
	(capitulos  364)
	(copias-vendidas  60000000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "1989-08-25")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  9.47)
	(restriccion-edad  18)
)
([bobobo-bo-bo-bobo] of Serializado
	(titulo  "Bobobo-bo Bo-bobo")
	(publicado-por  [shueisha])
	(pertenece-a  [aventura]  [comedia]  [ciencia-ficcion])
	(trata-de  [humor-absurdo]  [parodia])
	(tomos  21)
	(capitulos  230)
	(copias-vendidas  7000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2001-02-20")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.49)
	(restriccion-edad  10)
)
([yotsuba-to] of Serializado
	(titulo  "Yotsuba to!")
	(publicado-por  [ascii-media-works])
	(pertenece-a  [comedia]  [slice-of-life])
	(trata-de  [familiar]  [reconfortante])
	(tomos  15)
	(capitulos  112)
	(copias-vendidas  13000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2003-03-21")
	(metodo-distribucion  "ambos")
	(tiene-anime  FALSE)
	(valoracion  8.89)
	(restriccion-edad  5)
)
([ijiranaide-nagatoro-san] of Serializado
	(titulo  "Ijiranaide, Nagatoro-san")
	(publicado-por  [kodansha])
	(pertenece-a  [comedia]  [romance]  [slice-of-life])
	(trata-de  [escolar]  [artes])
	(tomos  17)
	(capitulos  140)
	(copias-vendidas  4000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "quincenal")
	(inicio-publicacion  "2017-11-07")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.64)
	(restriccion-edad  12)
)
([haikyuu] of Serializado
	(titulo  "Haikyuu!!")
	(publicado-por  [shueisha])
	(pertenece-a  [deportes])
	(trata-de  [escolar])
	(tomos  45)
	(capitulos  407)
	(copias-vendidas  55000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2012-02-20")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.85)
	(restriccion-edad  12)
)
([uzumaki] of Serializado
	(titulo  "Uzumaki")
	(publicado-por  [shogakukan])
	(pertenece-a  [drama]  [horror]  [sobrenatural])
	(trata-de  [gore])
	(tomos  3)
	(capitulos  19)
	(copias-vendidas  1500000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "1998-01-12")
	(metodo-distribucion  "ambos")
	(tiene-anime  FALSE)
	(valoracion  8.17)
	(restriccion-edad  16)
)
([initial-d] of Serializado
	(titulo  "Initial D")
	(publicado-por  [kodansha])
	(pertenece-a  [accion]  [drama])
	(trata-de  [carreras]  [personajes-adultos])
	(tomos  48)
	(capitulos  724)
	(copias-vendidas  55000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1995-06-26")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.23)
	(restriccion-edad  12)
)
([a-story-about-a-cat-reincarnated-in-a-different-world-where-there-are-no-cats] of Serializado
	(titulo  "A Story about a Cat Reincarnated in a Different World Where There are no Cats")
	(publicado-por  [ema-tooyama])
	(pertenece-a  [comedia]  [fantasia]  [slice-of-life])
	(trata-de  [animales]  [isekai])
	(tomos  0)
	(capitulos  17)
	(copias-vendidas  350000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2020-06-19")
	(metodo-distribucion  "digital")
	(tiene-anime  FALSE)
	(valoracion  8.8)
	(restriccion-edad  7)
)
([wangan-midnight] of Serializado
	(titulo  "Wangan Midnight")
	(publicado-por  [kodansha])
	(pertenece-a  [accion]  [drama])
	(trata-de  [carreras])
	(tomos  42)
	(capitulos  550)
	(copias-vendidas  17000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1990-06-15")
	(metodo-distribucion  "fisico")
	(tiene-anime  TRUE)
	(valoracion  7.49)
	(restriccion-edad  7)
)
([yu-yu-hakusho] of Serializado
	(titulo  "Yu Yu Hakusho")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [comedia]  [drama]  [fantasia]  [sobrenatural])
	(trata-de  [artes-marciales]  [mitologia])
	(tomos  19)
	(capitulos  176)
	(copias-vendidas  50000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1990-11-20")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.2)
	(restriccion-edad  7)
)
([black-lagoon] of Serializado
	(titulo  "Black Lagoon")
	(publicado-por  [shogakukan])
	(pertenece-a  [accion]  [drama])
	(trata-de  [personajes-adultos]  [guerra]  [delincuencia])
	(tomos  12)
	(capitulos  116)
	(copias-vendidas  8500000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2002-04-19")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.29)
	(restriccion-edad  16)
)
([shin-lupin-iii] of Serializado
	(titulo  "Shin Lupin III")
	(publicado-por  [futabasha])
	(pertenece-a  [accion]  [aventura]  [comedia]  [misterio])
	(trata-de  [personajes-adultos]  [delincuencia])
	(tomos  17)
	(capitulos  180)
	(copias-vendidas  10000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1977-06-23")
	(metodo-distribucion  "fisico")
	(tiene-anime  TRUE)
	(valoracion  7.47)
	(restriccion-edad  7)
)
([hanma-baki] of Serializado
	(titulo  "Hanma Baki")
	(publicado-por  [akita-shoten])
	(pertenece-a  [accion]  [aventura]  [deportes])
	(trata-de  [gore]  [artes-marciales])
	(tomos  37)
	(capitulos  312)
	(copias-vendidas  85000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2005-12-01")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.15)
	(restriccion-edad  18)
)
([hajime-no-ippo] of Serializado
	(titulo  "Hajime no Ippo")
	(publicado-por  [kodansha])
	(pertenece-a  [accion]  [comedia]  [drama]  [deportes])
	(trata-de  [artes-marciales])
	(tomos  139)
	(capitulos  1443)
	(copias-vendidas  100000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1989-09-27")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.72)
	(restriccion-edad  12)
)
([oliver-y-benji] of Serializado
	(titulo  "Oliver y Benji")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [deportes])
	(trata-de  [escolar])
	(tomos  37)
	(capitulos  146)
	(copias-vendidas  90000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1981-03-31")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.53)
	(restriccion-edad  5)
)
([assassination-classroom] of Serializado
	(titulo  "Assassination Classroom")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [comedia])
	(trata-de  [escolar])
	(tomos  21)
	(capitulos  187)
	(copias-vendidas  25000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2012-07-02")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.26)
	(restriccion-edad  12)
)
([tomodachi-game] of Serializado
	(titulo  "Tomodachi Game")
	(publicado-por  [kodansha])
	(pertenece-a  [psicologico]  [suspense])
	(trata-de  [battle-royale]  [juegos-de-estrategia])
	(tomos  23)
	(capitulos  103)
	(copias-vendidas  13450000)
	(dificultad-lectura  "media")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2013-12-09")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.45)
	(restriccion-edad  12)
)
([kingdom] of Serializado
	(titulo  "Kingdom")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [aventura]  [drama])
	(trata-de  [guerra]  [historico])
	(tomos  70)
	(capitulos  780)
	(copias-vendidas  100000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2006-01-26")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  9)
	(restriccion-edad  16)
)
([beastars] of Serializado
	(titulo  "Beastars")
	(publicado-por  [akita-shoten])
	(pertenece-a  [drama]  [slice-of-life]  [psicologico])
	(trata-de  [animales]  [escolar])
	(tomos  22)
	(capitulos  196)
	(copias-vendidas  7500000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2016-09-08")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.25)
	(restriccion-edad  18)
)
([mazinger-z] of Serializado
	(titulo  "Mazinger Z")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [comedia]  [drama]  [ciencia-ficcion])
	(trata-de  [robots])
	(tomos  5)
	(capitulos  33)
	(copias-vendidas  20000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1972-10-02")
	(metodo-distribucion  "fisico")
	(tiene-anime  TRUE)
	(valoracion  6.64)
	(restriccion-edad  5)
)
([dr-stone] of Serializado
	(titulo  "Dr. Stone")
	(publicado-por  [shueisha])
	(pertenece-a  [aventura]  [ciencia-ficcion])
	(trata-de  [viajes-en-el-tiempo]  [supervivencia])
	(tomos  26)
	(capitulos  233)
	(copias-vendidas  15000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2017-03-06")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.2)
	(restriccion-edad  10)
)
([bokkou] of Serializado
	(titulo  "Bokkou")
	(publicado-por  [shogakukan])
	(pertenece-a  [accion]  [drama])
	(trata-de  [guerra]  [historico])
	(tomos  11)
	(capitulos  107)
	(copias-vendidas  2000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "1992-01-25")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.7)
	(restriccion-edad  16)
)
([liar-game] of Serializado
	(titulo  "Liar Game")
	(publicado-por  [shueisha])
	(pertenece-a  [drama]  [psicologico])
	(trata-de  [battle-royale]  [juegos-de-estrategia])
	(tomos  19)
	(capitulos  203)
	(copias-vendidas  10000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2005-02-17")
	(metodo-distribucion  "ambos")
	(tiene-anime  FALSE)
	(valoracion  8.28)
	(restriccion-edad  14)
)
([mach-gogogo] of Serializado
	(titulo  "Mach Gogogo")
	(publicado-por  [shueisha])
	(pertenece-a  [accion])
	(trata-de  [carreras])
	(tomos  2)
	(capitulos  10)
	(copias-vendidas  7000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "1966-06-25")
	(metodo-distribucion  "fisico")
	(tiene-anime  TRUE)
	(valoracion  6.58)
	(restriccion-edad  5)
)
([manhole] of Serializado
	(titulo  "Manhole")
	(publicado-por  [square-enix])
	(pertenece-a  [drama]  [horror]  [misterio])
	(trata-de  [detectives]  [gore])
	(tomos  3)
	(capitulos  29)
	(copias-vendidas  9000000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "quincenal")
	(inicio-publicacion  "2004-12-03")
	(metodo-distribucion  "ambos")
	(tiene-anime  FALSE)
	(valoracion  8.95)
	(restriccion-edad  18)
)
([monster] of Serializado
	(titulo  "Monster")
	(publicado-por  [shogakukan])
	(pertenece-a  [drama]  [misterio]  [psicologico]  [suspense])
	(trata-de  [personajes-adultos])
	(tomos  18)
	(capitulos  162)
	(copias-vendidas  20000000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "1994-12-05")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  9.15)
	(restriccion-edad  18)
)
([doomsday-with-my-dog] of Serializado
	(titulo  "Doomsday with my dog")
	(publicado-por  [yuu-ishihara])
	(pertenece-a  [aventura]  [comedia]  [slice-of-life])
	(trata-de  [animales]  [postapocaliptico]  [supervivencia])
	(tomos  4)
	(capitulos  39)
	(copias-vendidas  2000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2018-03-28")
	(metodo-distribucion  "digital")
	(tiene-anime  TRUE)
	(valoracion  7.4)
	(restriccion-edad  7)
)
([planetes] of Serializado
	(titulo  "Planetes")
	(publicado-por  [kodansha])
	(pertenece-a  [ciencia-ficcion]  [drama]  [romance]  [psicologico]  [slice-of-life])
	(trata-de  [espacio-exterior]  [personajes-adultos])
	(tomos  4)
	(capitulos  27)
	(copias-vendidas  100000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "1999-01-14")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.28)
	(restriccion-edad  14)
)
([hellsing] of Serializado
	(titulo  "Hellsing")
	(publicado-por  [shonen-gahosha])
	(pertenece-a  [accion]  [horror]  [sobrenatural])
	(trata-de  [gore]  [historico]  [personajes-adultos]  [vampiros])
	(tomos  10)
	(capitulos  92)
	(copias-vendidas  4000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "1997-04-30")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.3)
	(restriccion-edad  18)
)
([sono-bisque-doll-wa-koi-wo-suru] of Serializado
	(titulo  "Sono Bisque Doll wa Koi wo Suru")
	(publicado-por  [square-enix])
	(pertenece-a  [romance]  [comedia])
	(trata-de  [artes]  [escolar]  [reconfortante])
	(tomos  12)
	(capitulos  96)
	(copias-vendidas  10000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "quincenal")
	(inicio-publicacion  "2018-01-19")
	(metodo-distribucion  "fisico")
	(tiene-anime  TRUE)
	(valoracion  8)
	(restriccion-edad  16)
)
([uchi-no-neko-ga-onnanoko-de-kawaii] of Serializado
	(titulo  "Uchi no Neko ga Onnanoko de Kawaii")
	(publicado-por  [shimahara])
	(pertenece-a  [comedia]  [slice-of-life])
	(trata-de  [animales]  [humor-absurdo]  [reconfortante])
	(tomos  3)
	(capitulos  3)
	(copias-vendidas  70000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2020-12-30")
	(metodo-distribucion  "ambos")
	(tiene-anime  FALSE)
	(valoracion  7.84)
	(restriccion-edad  3)
)
([dekihime-no-usui-hon] of Serializado
	(titulo  "Dekihime no Usui Hon")
	(publicado-por  [ajiichi])
	(pertenece-a  [romance])
	(trata-de  [escolar]  [yuri])
	(tomos  5)
	(capitulos  5)
	(copias-vendidas  20000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2022-05-05")
	(metodo-distribucion  "ambos")
	(tiene-anime  FALSE)
	(valoracion  8.12)
	(restriccion-edad  7)
)
([orange] of Serializado
	(titulo  "Orange")
	(publicado-por  [futabasha])
	(pertenece-a  [romance]  [drama])
	(trata-de  [escolar]  [triangulo-amoroso]  [viajes-en-el-tiempo])
	(tomos  7)
	(capitulos  38)
	(copias-vendidas  4700000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2012-03-13")
	(metodo-distribucion  "fisico")
	(tiene-anime  TRUE)
	(valoracion  8.3)
	(restriccion-edad  12)
)
([given] of Serializado
	(titulo  "Given")
	(publicado-por  [shinshokan])
	(pertenece-a  [drama]  [romance])
	(trata-de  [artes]  [escolar]  [musica]  [yaoi])
	(tomos  9)
	(capitulos  58)
	(copias-vendidas  500000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2013-05-30")
	(metodo-distribucion  "fisico")
	(tiene-anime  TRUE)
	(valoracion  8.53)
	(restriccion-edad  12)
)
([yoake-ni-furu] of Serializado
	(titulo  "Yoake ni Furu")
	(publicado-por  [taiyoh-tosho])
	(pertenece-a  [romance])
	(trata-de  [escolar]  [triangulo-amoroso]  [yaoi])
	(tomos  2)
	(capitulos  15)
	(copias-vendidas  80000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "quincenal")
	(inicio-publicacion  "2013-07-19")
	(metodo-distribucion  "fisico")
	(tiene-anime  FALSE)
	(valoracion  7.51)
	(restriccion-edad  12)
)
([mahou-shoujo-madoka-magica] of Serializado
	(titulo  "Mahou Shoujo Madoka Magica")
	(publicado-por  [houbunsha])
	(pertenece-a  [drama]  [fantasia]  [horror]  [sobrenatural])
	(trata-de  [escolar]  [magia]  [mahou-shoujo])
	(tomos  3)
	(capitulos  12)
	(copias-vendidas  6000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2011-02-12")
	(metodo-distribucion  "fisico")
	(tiene-anime  TRUE)
	(valoracion  8.07)
	(restriccion-edad  12)
)
([wotaku-ni-koi-wa-muzukashii] of Serializado
	(titulo  "Wotaku ni Koi wa Muzukashii")
	(publicado-por  [ichijinsha])
	(pertenece-a  [comedia]  [romance])
	(trata-de  [personajes-adultos])
	(tomos  11)
	(capitulos  105)
	(copias-vendidas  10000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2015-04-30")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.36)
	(restriccion-edad  7)
)
([gril-crush] of Serializado
	(titulo  "Gril Crush")
	(publicado-por  [shinchosha])
	(pertenece-a  [comedia]  [romance])
	(trata-de  [artes]  [idols]  [musica])
	(tomos  7)
	(capitulos  58)
	(copias-vendidas  70000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "quincenal")
	(inicio-publicacion  "2021-05-21")
	(metodo-distribucion  "digital")
	(tiene-anime  FALSE)
	(valoracion  7.12)
	(restriccion-edad  7)
)
([jujutsu-kaisen] of Serializado
	(titulo  "Jujutsu Kaisen")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [drama]  [sobrenatural])
	(trata-de  [escolar]  [mitologia]  [superpoderes])
	(tomos  24)
	(capitulos  244)
	(copias-vendidas  80000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2018-03-05")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.53)
	(restriccion-edad  12)
)
([bungou-stray-dogs] of Serializado
	(titulo  "Bungou Stray Dogs")
	(publicado-por  [kadokawa-shoten])
	(pertenece-a  [accion]  [sobrenatural]  [misterio])
	(trata-de  [personajes-adultos]  [delincuencia]  [detectives]  [superpoderes])
	(tomos  23)
	(capitulos  106)
	(copias-vendidas  10000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2012-12-04")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.52)
	(restriccion-edad  16)
)
([oshi-no-ko] of Serializado
	(titulo  "Oshi no Ko")
	(publicado-por  [shueisha])
	(pertenece-a  [drama]  [misterio]  [sobrenatural])
	(trata-de  [idols]  [musica]  [tragedia]  [venganza])
	(tomos  12)
	(capitulos  122)
	(copias-vendidas  15000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2020-04-23")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.63)
	(restriccion-edad  16)
)
([kagurabachi] of Serializado
	(titulo  "Kagurabachi")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [fantasia])
	(trata-de  [gore]  [tradicional-japones]  [venganza])
	(tomos  1)
	(capitulos  12)
	(copias-vendidas  100000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2023-09-18")
	(metodo-distribucion  "ambos")
	(tiene-anime  FALSE)
	(valoracion  7.92)
	(restriccion-edad  16)
)
([chainsaw-man] of Serializado
	(titulo  "Chainsaw Man")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [drama]  [sobrenatural]  [psicologico])
	(trata-de  [gore]  [superpoderes]  [tragedia])
	(tomos  16)
	(capitulos  150)
	(copias-vendidas  15000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2018-12-03")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.74)
	(restriccion-edad  16)
)
([banana-fish] of Serializado
	(titulo  "Banana Fish")
	(publicado-por  [shogakukan])
	(pertenece-a  [accion]  [aventura]  [drama])
	(trata-de  [delincuencia]  [tragedia])
	(tomos  19)
	(capitulos  110)
	(copias-vendidas  11000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "1985-04-13")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.61)
	(restriccion-edad  16)
)
([tokyo-ghoul] of Serializado
	(titulo  "Tokyo Ghoul")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [fantasia]  [horror]  [psicologico])
	(trata-de  [gore]  [vampiros])
	(tomos  14)
	(capitulos  144)
	(copias-vendidas  34000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2011-08-09")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.52)
	(restriccion-edad  16)
)
([kimetsu-no-yaiba] of Serializado
	(titulo  "Kimetsu no Yaiba")
	(publicado-por  [shueisha])
	(pertenece-a  [accion]  [fantasia])
	(trata-de  [historico]  [supervivencia]  [tradicional-japones])
	(tomos  23)
	(capitulos  207)
	(copias-vendidas  102000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2016-02-15")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.21)
	(restriccion-edad  12)
)

; Autores

([ajiichi] of Autor
	(nombre  "Ajiichi")
	(escribe  [dekihime-no-usui-hon])
	(ilustra  [dekihime-no-usui-hon])
)
([aka-akasaka] of Autor
	(nombre  "Aka Akasaka")
	(escribe  [kaguya-sama-wa-kokurasetai-tensai-tachi-no-renai-zunousen])
	(ilustra  [kaguya-sama-wa-kokurasetai-tensai-tachi-no-renai-zunousen])
	(escribe  [oshi-no-ko])
)
([akihito-tsukushi] of Autor
	(nombre  "Akihito Tsukushi")
	(escribe  [made-in-abyss])
	(ilustra  [made-in-abyss])
)
([akimi-yoshida] of Autor
	(nombre  "Akimi Yoshida")
	(escribe  [banana-fish])
	(ilustra  [banana-fish])
)
([boichi] of Autor
	(nombre  "Boichi")
	(ilustra  [dr-stone])
)
([daisuke-aishihara] of Autor
	(nombre  "Daisuke Aishihara")
	(escribe  [world-trigger])
	(ilustra  [world-trigger])
)
([eiichiro-oda] of Autor
	(nombre  "Eiichiro Oda")
	(escribe  [one-piece])
	(ilustra  [one-piece])
)
([ema-tooyama] of Autor
	(nombre  "Ema Tooyama")
	(escribe  [a-story-about-a-cat-reincarnated-in-a-different-world-where-there-are-no-cats])
	(ilustra  [a-story-about-a-cat-reincarnated-in-a-different-world-where-there-are-no-cats])
)
([fujita] of Autor
	(nombre  "Fujita")
	(escribe  [wotaku-ni-koi-wa-muzukashii])
	(ilustra  [wotaku-ni-koi-wa-muzukashii])
)
([gege-akutami] of Autor
	(nombre  "Gege Akutami")
	(escribe  [jujutsu-kaisen])
	(ilustra  [jujutsu-kaisen])
)
([go-nagai] of Autor
	(nombre  "Go Nagai")
	(escribe  [mazinger-z])
	(ilustra  [mazinger-z])
)
([gosho-aoyama] of Autor
	(nombre  "Gosho Aoyama")
	(escribe  [detective-conan])
	(ilustra  [detective-conan])
)
([hajime-isayama] of Autor
	(nombre  "Hajime Isayama")
	(escribe  [shingeki-no-kyojin])
	(ilustra  [shingeki-no-kyojin])
)
([hanokage] of Autor
	(nombre  "Hanokage")
	(ilustra  [mahou-shoujo-madoka-magica])
)
([haruichi-furudate] of Autor
	(nombre  "Haruichi Furudate")
	(escribe  [haikyuu])
	(ilustra  [haikyuu])
)
([harukawa35] of Autor
	(nombre  "Harukawa35")
	(ilustra  [bungou-stray-dogs])
)
([hideki-mori] of Autor
	(nombre  "Hideki Mori")
	(ilustra  [bokkou])
)
([hirohiko-araki] of Autor
	(nombre  "Hirohiko Araki")
	(escribe  [jojo-no-kimyou-na-bouken-part-5-ougon-no-kaze])
	(ilustra  [jojo-no-kimyou-na-bouken-part-5-ougon-no-kaze])
	(escribe  [jojo-no-kimyou-na-bouken-part-4-diamond-wa-kudakenai])
	(ilustra  [jojo-no-kimyou-na-bouken-part-4-diamond-wa-kudakenai])
	(escribe  [jojo-no-kimyou-na-bouken-part-3-stardust-crusaders])
	(ilustra  [jojo-no-kimyou-na-bouken-part-3-stardust-crusaders])
	(escribe  [jojo-no-kimyou-na-bouken-part-2-battle-tendency])
	(ilustra  [jojo-no-kimyou-na-bouken-part-2-battle-tendency])
	(escribe  [jojo-no-kimyou-na-bouken-part-1-phantom-blood])
	(ilustra  [jojo-no-kimyou-na-bouken-part-1-phantom-blood])
)
([ichigo-takano] of Autor
	(nombre  "Ichigo Takano")
	(escribe  [orange])
	(ilustra  [orange])
)
([junji-ito] of Autor
	(nombre  "Junji Ito")
	(escribe  [uzumaki])
	(ilustra  [uzumaki])
)
([jyoji-morikawa] of Autor
	(nombre  "Jyoji Morikawa")
	(escribe  [hajime-no-ippo])
	(ilustra  [hajime-no-ippo])
)
([kafka-asagiri] of Autor
	(nombre  "Kafka Asagiri")
	(escribe  [bungou-stray-dogs])
)
([keisuke-itagaki] of Autor
	(nombre  "Keisuke Itagaki")
	(escribe  [hanma-baki])
	(ilustra  [hanma-baki])
)
([kenichi-sakemi] of Autor
	(nombre  "Kenichi Sakemi")
	(escribe  [bokkou])
)
([kentaro-miura] of Autor
	(nombre  "Kentaro Miura")
	(escribe  [berserk])
	(ilustra  [berserk])
)
([kiyohiko-azuma] of Autor
	(nombre  "Kiyohiko Azuma")
	(escribe  [yotsuba-to])
	(ilustra  [yotsuba-to])
)
([kouta-hirano] of Autor
	(nombre  "Kouta Hirano")
	(escribe  [hellsing])
	(ilustra  [hellsing])
)
([koyoharu-gotouge] of Autor
	(nombre  "Koyoharu Gotouge")
	(escribe  [kimetsu-no-yaiba])
	(ilustra  [kimetsu-no-yaiba])
)
([magica-quartet] of Autor
	(nombre  "Magica Quartet")
	(escribe  [mahou-shoujo-madoka-magica])
)
([makoto-yukimura] of Autor
	(nombre  "Makoto Yukimura")
	(escribe  [planetes])
	(ilustra  [planetes])
)
([masashi-kishimoto] of Autor
	(nombre  "Masashi Kishimoto")
	(escribe  [naruto])
	(ilustra  [naruto])
)
([mengo-yokoyari] of Autor
	(nombre  "Mengo Yokoyari")
	(ilustra  [oshi-no-ko])
)
([michiharu-kusunoki] of Autor
	(nombre  "Michiharu Kusunoki")
	(escribe  [wangan-midnight])
	(ilustra  [wangan-midnight])
)
([midori-tayama] of Autor
	(nombre  "Midori Tayama")
	(escribe  [gril-crush])
	(ilustra  [gril-crush])
)
([mikoto-yamaguchi] of Autor
	(nombre  "Mikoto Yamaguchi")
	(escribe  [tomodachi-game])
)
([monkey-punch] of Autor
	(nombre  "Monkey Punch")
	(escribe  [shin-lupin-iii])
	(ilustra  [shin-lupin-iii])
)
([nanashi] of Autor
	(nombre  "Nanashi")
	(escribe  [ijiranaide-nagatoro-san])
	(ilustra  [ijiranaide-nagatoro-san])
)
([naoki-urasawa] of Autor
	(nombre  "Naoki Urasawa")
	(escribe  [monster])
	(ilustra  [monster])
)
([naoko-takeuchi] of Autor
	(nombre  "Naoko Takeuchi")
	(escribe  [pretty-soldier-sailor-moon])
	(ilustra  [pretty-soldier-sailor-moon])
)
([natsuki-kizu] of Autor
	(nombre  "Natsuki Kizu")
	(escribe  [given])
	(ilustra  [given])
)
([nojiko-hayakawa] of Autor
	(nombre  "Nojiko Hayakawa")
	(escribe  [yoake-ni-furu])
	(ilustra  [yoake-ni-furu])
)
([one] of Autor
	(nombre  "ONE")
	(escribe  [one-punch-man])
)
([paru-itagaki] of Autor
	(nombre  "Paru Itagaki")
	(escribe  [beastars])
	(ilustra  [beastars])
)
([rei-hiroe] of Autor
	(nombre  "Rei Hiroe")
	(escribe  [black-lagoon])
	(ilustra  [black-lagoon])
)
([riichiro-inagaki] of Autor
	(nombre  "Riichiro Inagaki")
	(escribe  [dr-stone])
)
([shimahara] of Autor
	(nombre  "Shimahara")
	(escribe  [uchi-no-neko-ga-onnanoko-de-kawaii])
	(ilustra  [uchi-no-neko-ga-onnanoko-de-kawaii])
)
([shinichi-fukuda] of Autor
	(nombre  "Shinichi Fukuda")
	(escribe  [sono-bisque-doll-wa-koi-wo-suru])
	(ilustra  [sono-bisque-doll-wa-koi-wo-suru])
)
([shinobu-kaitani] of Autor
	(nombre  "Shinobu Kaitani")
	(escribe  [liar-game])
	(ilustra  [liar-game])
)
([shuuichi-shigeno] of Autor
	(nombre  "Shuuichi Shigeno")
	(escribe  [initial-d])
	(ilustra  [initial-d])
)
([sui-ishida] of Autor
	(nombre  "Sui Ishida")
	(escribe  [tokyo-ghoul])
	(ilustra  [tokyo-ghoul])
)
([takeru-hokazono] of Autor
	(nombre  "Takeru Hokazono")
	(escribe  [kagurabachi])
	(ilustra  [kagurabachi])
)
([takeshi-obata] of Autor
	(nombre  "Takeshi Obata")
	(ilustra  [death-note])
)
([tatsuki-fujimoto] of Autor
	(nombre  "Tatsuki Fujimoto")
	(escribe  [chainsaw-man])
	(ilustra  [chainsaw-man])
)
([tatsuo-yoshida] of Autor
	(nombre  "Tatsuo Yoshida")
	(escribe  [mach-gogogo])
	(ilustra  [mach-gogogo])
)
([tetsuya-tsutsui] of Autor
	(nombre  "Tetsuya Tsutsui")
	(escribe  [manhole])
	(ilustra  [manhole])
)
([tsugumi-ohba] of Autor
	(nombre  "Tsugumi Ohba")
	(escribe  [death-note])
)
([yasuhisa-hara] of Autor
	(nombre  "Yasuhisa Hara")
	(escribe  [kingdom])
	(ilustra  [kingdom])
)
([yoichi-takahashi] of Autor
	(nombre  "Yoichi Takahashi")
	(escribe  [oliver-y-benji])
	(ilustra  [oliver-y-benji])
)
([yoshihiro-togashi] of Autor
	(nombre  "Yoshihiro Togashi")
	(escribe  [hunter-x-hunter])
	(ilustra  [hunter-x-hunter])
	(escribe  [yu-yu-hakusho])
	(ilustra  [yu-yu-hakusho])
)
([yoshio-sawai] of Autor
	(nombre  "Yoshio Sawai")
	(escribe  [bobobo-bo-bo-bobo])
	(ilustra  [bobobo-bo-bo-bobo])
)
([yusuke-murata] of Autor
	(nombre  "Yusuke Murata")
	(ilustra  [one-punch-man])
)
([yuu-ishihara] of Autor
	(nombre  "Yuu Ishihara")
	(escribe  [doomsday-with-my-dog])
	(ilustra  [doomsday-with-my-dog])
)
([yuuki-satou] of Autor
	(nombre  "Yuuki Satou")
	(ilustra  [tomodachi-game])
)
([yuusei-matsui] of Autor
	(nombre  "Yuusei Matsui")
	(escribe  [assassination-classroom])
	(ilustra  [assassination-classroom])
)

; Publicadores

([akita-shoten] of Editorial
	(nombre  "Akita Shoten")
)
([ascii-media-works] of Editorial
	(nombre  "ASCII Media Works")
)
([futabasha] of Editorial
	(nombre  "Futabasha")
)
([hakusensha] of Editorial
	(nombre  "Hakusensha")
)
([houbunsha] of Editorial
	(nombre  "Houbunsha")
)
([ichijinsha] of Editorial
	(nombre  "Ichijinsha")
)
([kadokawa-shoten] of Editorial
	(nombre  "Kadokawa Shoten")
)
([kodansha] of Editorial
	(nombre  "Kodansha")
)
([shinchosha] of Editorial
	(nombre  "Shinchosha")
)
([shinshokan] of Editorial
	(nombre  "Shinshokan")
)
([shogakukan] of Editorial
	(nombre  "Shogakukan")
)
([shonen-gahosha] of Editorial
	(nombre  "Shonen Gahosha")
)
([shueisha] of Editorial
	(nombre  "Shueisha")
)
([square-enix] of Editorial
	(nombre  "Square Enix")
)
([taiyoh-tosho] of Editorial
	(nombre  "Taiyoh Tosho")
)
([takeshobo] of Editorial
	(nombre  "Takeshobo")
)
([publ-ajiichi] of Autopublicador
	(nombre  "Ajiichi")
)
([publ-ema-tooyama] of Autopublicador
	(nombre  "Ema Tooyama")
)
([publ-shimahara] of Autopublicador
	(nombre  "Shimahara")
)
([publ-yuu-ishihara] of Autopublicador
	(nombre  "Yuu Ishihara")
)

; Generos

([accion] of Genero
	(nombre  "Accion")
)
([aventura] of Genero
	(nombre  "Aventura")
)
([ciencia-ficcion] of Genero
	(nombre  "Ciencia ficcion")
)
([comedia] of Genero
	(nombre  "Comedia")
)
([deportes] of Genero
	(nombre  "Deportes")
)
([drama] of Genero
	(nombre  "Drama")
)
([fantasia] of Genero
	(nombre  "Fantasia")
)
([horror] of Genero
	(nombre  "Horror")
)
([misterio] of Genero
	(nombre  "Misterio")
)
([psicologico] of Genero
	(nombre  "Psicologico")
)
([romance] of Genero
	(nombre  "Romance")
)
([slice-of-life] of Genero
	(nombre  "Slice of life")
)
([sobrenatural] of Genero
	(nombre  "Sobrenatural")
)
([suspense] of Genero
	(nombre  "Suspense")
)

; Temas

([animales] of Tema
	(nombre  "Animales")
)
([artes] of Tema
	(nombre  "Artes")
)
([artes-marciales] of Tema
	(nombre  "Artes marciales")
)
([battle-royale] of Tema
	(nombre  "Battle royale")
)
([carreras] of Tema
	(nombre  "Carreras")
)
([delincuencia] of Tema
	(nombre  "Delincuencia")
)
([detectives] of Tema
	(nombre  "Detectives")
)
([escolar] of Tema
	(nombre  "Escolar")
)
([espacio-exterior] of Tema
	(nombre  "Espacio exterior")
)
([familiar] of Tema
	(nombre  "Familiar")
)
([genero] of Tema
	(nombre  "Genero")
)
([gore] of Tema
	(nombre  "Gore")
)
([guerra] of Tema
	(nombre  "Guerra")
)
([harem] of Tema
	(nombre  "Harem")
)
([historico] of Tema
	(nombre  "Historico")
)
([humor-absurdo] of Tema
	(nombre  "Humor absurdo")
)
([idols] of Tema
	(nombre  "Idols")
)
([isekai] of Tema
	(nombre  "Isekai")
)
([juegos-de-estrategia] of Tema
	(nombre  "Juegos de estrategia")
)
([magia] of Tema
	(nombre  "Magia")
)
([mahou-shoujo] of Tema
	(nombre  "Mahou shoujo")
)
([mitologia] of Tema
	(nombre  "Mitologia")
)
([musica] of Tema
	(nombre  "Musica")
)
([parodia] of Tema
	(nombre  "Parodia")
)
([personajes-adultos] of Tema
	(nombre  "Personajes adultos")
)
([postapocaliptico] of Tema
	(nombre  "Postapocaliptico")
)
([reconfortante] of Tema
	(nombre  "Reconfortante")
)
([robots] of Tema
	(nombre  "Robots")
)
([superpoderes] of Tema
	(nombre  "Superpoderes")
)
([supervivencia] of Tema
	(nombre  "Supervivencia")
)
([tradicional-japones] of Tema
	(nombre  "Tradicional japones")
)
([tragedia] of Tema
	(nombre  "Tragedia")
)
([triangulo-amoroso] of Tema
	(nombre  "Triangulo amoroso")
)
([vampiros] of Tema
	(nombre  "Vampiros")
)
([venganza] of Tema
	(nombre  "Venganza")
)
([viajes-en-el-tiempo] of Tema
	(nombre  "Viajes en el tiempo")
)
([videojuegos] of Tema
	(nombre  "Videojuegos")
)
([yaoi] of Tema
	(nombre  "Yaoi")
)
([yuri] of Tema
	(nombre  "Yuri")
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
    (import preguntas-usuario ?ALL) ; no os tomeis esto como que esta bien ni aqui ni en los siguientes
    (export ?ALL)
)

; Modulo para solucionar el problema abstracto
(defmodule asociacion-heuristica
    (import abstraccion-problema ?ALL)
    (export ?ALL)
)

; Modulo para refinar la solucion (si lo llegamos a necesitar)
(defmodule refinamiento-solucion
    (import MAIN ?ALL)
    (export ?ALL)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Funciones ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; funcion que nos dan en el FAQ apartado 3.10
; recibe la pregunta que hacer al usuario y el rango de valores valido y los imprime
; lee el input y si es incorrecto vuelve a escribir la pregunta y leer hasta que
; sea correcto, entonces devuelve el valor leido
(deffunction preguntas-usuario::pregunta-numerica (?pregunta ?rangini ?rangfi)
    (format t "%s [%d, %d] " ?pregunta ?rangini ?rangfi)
    (bind ?respuesta (read))
    (while (not(and(> ?respuesta ?rangini)(< ?respuesta ?rangfi))) do
        (format t "%s [%d, %d] " ?pregunta ?rangini ?rangfi)
        (bind ?respuesta (read))
    )
    ?respuesta
)

; funcion que nos dan en el FAQ apartado 3.10
(deffunction preguntas-usuario::pregunta-enum (?pregunta $?valores-permitidos)
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

(deffunction preguntas-usuario::pregunta-enum-comentario (?pregunta ?comentario $?valores-permitidos)
    (progn$
        (?var ?valores-permitidos)
        (lowcase ?var))
    (format t "%s (%s) " ?pregunta ?comentario)
    (bind ?respuesta (read))
    (while (not (member$ (lowcase ?respuesta) ?valores-permitidos)) do
        (format t "%s (%s) " ?pregunta ?comentario)
        (bind ?respuesta (read))
    )
    ?respuesta
)

; devuelve una lista con el indice de los valores escogidos
(deffunction preguntas-usuario::pregunta-multirespuesta (?pregunta $?valores-posibles)
	(printout t ?pregunta crlf)

	; por cada valor de los valores posibles lo imprime junto a su indice
	(progn$ (?valor ?valores-posibles)
		(bind ?text (format nil "	%d %s" ?valor-index ?valor))
		(printout t ?text crlf)
	)

	(bind ?respuesta (readline))

	; la respuesta esta en un solo string asi que la separamos
    (bind ?respuesta (explode$ ?respuesta))
    (bind $?lista (create$))

	; si ha escrito un numero que no pertenece a ningun genero, pide que responda otra vez
	(while (not (progn$ (?valor ?respuesta)
					(and (integerp ?valor) (and (>= ?valor 0) (<= ?valor (length$ ?valores-posibles))))
				)
	)
	do
        (printout t "Algo ha ocurrido mal. Vuelve a escribir tu respuesta." crlf)
        (bind ?respuesta (readline))
		(bind ?respuesta (explode$ ?respuesta))
    )

	; Cuando los valores sean válidos los guardamos
	(progn$ (?valor ?respuesta)
		(if (not (member$ ?valor ?lista))
			then (bind ?lista (insert$ ?lista (+ (length$ ?lista) 1) ?valor))
		)
    )
	; si no ha respondido nada o ha respondido 0, devolvemos una lista vacia
    (if (or(member$ 0 ?lista)(= (length$ ?lista) 0)) then (bind ?lista (create$ )))
    ?lista
)

; funcion que nos dan en el FAQ apartado 3.10
(deffunction preguntas-usuario::pregunta-si-no (?pregunta)
    (format t "%s " ?pregunta)
    (bind ?respuesta (read))
    (if (or (eq (lowcase ?respuesta) si) (eq (lowcase ?respuesta) s))
        then TRUE
        else FALSE
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Templates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate preguntas-usuario::usuario
    (slot edad (type INTEGER))
    (slot ve-anime (type SYMBOL))
    (slot leeria-anime-ya-visto (type SYMBOL)
                                (default FALSE))
    (slot mangas-leidos (type SYMBOL) ; esto lo cambiaria un poco
                        (allowed-values pocos bastantes muchos))
    (slot mangas-animes-vistos (type SYMBOL) ; esto lo cambiaria un poco
                        (allowed-values pocos bastantes muchos))
    (multislot gusto-generos (type INSTANCE))
    (multislot gusto-temas (type INSTANCE))
    (slot tiempo-lectura (type INTEGER))
		(slot prefiere-acabados (type SYMBOL)
                            (allowed-values TRUE FALSE)
                            (default FALSE))
    (slot prefiere-sin-anime (type SYMBOL)
                            (allowed-values TRUE FALSE)
                            (default FALSE))
    (slot quiere-doujinshis (type SYMBOL)
                            (allowed-values TRUE FALSE)
                            (default FALSE))
    ; faltan cosas, las voy añadiendo conforme hago las preguntas
)

; Definir template para problema abstracto
(deftemplate abstraccion-problema::problema-abstracto
    (slot edad (type SYMBOL)
                (allowed-values MENOS_12 12_O_MAS 16_O_MAS 18_O_MAS))
    (slot mangas-leidos (type SYMBOL)
                        (allowed-values pocos bastantes muchos))
    (slot prefiere-acabados (type SYMBOL)
                            (allowed-values TRUE FALSE)
                            (default FALSE))
    (slot prefiere-sin-anime (type SYMBOL)
                            (allowed-values TRUE FALSE)
                            (default FALSE))
    (slot quiere-doujinshis (type SYMBOL)
                            (allowed-values TRUE FALSE)
                            (default FALSE))
    ;(slot longitud-preferida ()) no trivial
    (multislot preferencia-generos (type INSTANCE))
    (multislot preferencia-temas (type INSTANCE))
)

(deftemplate asociacion-heuristica::solucion-abstracta
    (multislot recomendables (type INSTANCE)) ;instancias de mangas
)

(deftemplate refinamiento-solucion::solucion-concreta
)

; Definir template para solucion abstracta
; (deftemplate...)

; Definir template para solucion concreta/refinada
; (deftemplate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reglas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Aqui controlaremos en cambio entre los modulos del programa

; Funcion que solo activa el modulo preguntas-usuario
; salience para que se llame la primera
(defrule MAIN::haz-preguntas-usuario
    (declare (salience 10))
    =>
    (focus preguntas-usuario)
)

(defrule MAIN::abstrae-problema
	(declare (salience 9))
	=>
	(focus abstraccion-problema)
)

(defrule MAIN::resuelve-problema
	(declare (salience 8))
	=>
	(focus asociacion-heuristica)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Preguntas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Modulo de preguntas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule preguntas-usuario::crea-usuario
    (not (usuario))
    =>
    (assert (usuario))
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

(defrule preguntas-usuario::pregunta-mangas-leidos
    ; control orden
    (edad-preguntada)
    ; control para que no se llame mas de una vez
    (not (mangas-leidos-preguntado))
    ; coge la direccion del fact usuario para modificarlo luego
    ?usr <- (usuario)
    =>
    ; modifica la edad del usuario con el resultado que devuelve la funcion pregunta-numerica
    (modify ?usr (mangas-leidos (pregunta-enum-comentario "¿Cuantos mangas has leido aproximadamente?" "pocos [0..10] bastantes [11..50] muchos [>51]" pocos bastantes muchos)))
    (assert (mangas-leidos-preguntado))
)

(defrule preguntas-usuario::pregunta-generos
    (mangas-leidos-preguntado)
    (not (generos-preguntado))
	?usr <- (usuario)
    =>
	; guarda en generos todas las instancias de la clase Genero
	(bind $?generos (find-all-instances ((?inst Genero)) TRUE))
	; crea la variable donde guardara el nombre de los generos
	(bind $?nombres-gen (create$))

	; por cada genero guarda su nombre en nombres-gen
	(progn$ (?gen ?generos)
		(bind ?nombre (send ?gen get-nombre)) ; hace get del nombre del genero y lo guarda en ?nombre
		; insert$ devuelve la lista con el nombre nuevo insertado al final, y luego hace bind de esto en la variable
		(bind $?nombres-gen(insert$ $?nombres-gen (+ (length$ $?nombres-gen) 1) ?nombre))
	)

	(bind ?respuesta (pregunta-multirespuesta "Escoge tus generos favoritos (separados por un espacio, o 0 si no tienes)" $?nombres-gen))

	(bind $?instancias (create$))
	;;;;;;;;;; aqui harias otro progn creo pero tengo sueño me voy a dormir
	(loop-for-count (?i 1 (length$ ?respuesta)) do
		(bind ?index (nth$ ?i ?respuesta))
		(bind ?gen (nth$ ?index ?generos))
		(bind $?instancias(insert$ $?instancias (+ (length$ $?instancias) 1) ?gen))
	)
	(modify ?usr (gusto-generos $?instancias))
    (assert (generos-preguntado))
)

; esta puesta para que no salga del modulo y pueda comprobar que la de generos se ha hecho bien
(defrule preguntas-usuario::pregunta-temas
    (generos-preguntado)
    (not (temas-preguntado))
	?usr <- (usuario)
    =>
	
    (assert (temas-preguntado))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Modulo de abstraccion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Control de reglas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffacts abstraccion-problema::requisitos
    (preferencia-generos-hecho FALSE)
    (preferencia-temas-hecho FALSE)
    (edad-hecho FALSE)
    (cantidad-hecho FALSE)
	(preferencia-acabados-hecho FALSE)
	(preferencia-sin-anime-hecho FALSE)
	(quiere-doujinshis-hecho FALSE)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reglas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Crea problema abstracto
(defrule abstraccion-problema::crea-problema
	(not (problema-abstracto))
    =>
    (assert (problema-abstracto))
)

; Edad
(defrule abstraccion-problema::edad-menos-12
    ?req <- (edad-hecho FALSE)
    (usuario (edad ?e))
    (< ?e 12)
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (edad MENOS_12))
    (modify ?req (edad-hecho TRUE))
)
(defrule abstraccion-problema::edad-12-mas
    ?req <- (edad-hecho FALSE)
    (usuario (edad ?e))
    (> ?e 11)
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (edad 12_O_MAS))
    (modify ?req (edad-hecho TRUE))
)
(defrule abstraccion-problema::edad-16-mas
    ?req <- (edad-hecho FALSE)
    (usuario (edad ?e))
    (> ?e 15)
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (edad 16_O_MAS))
    (modify ?req (edad-hecho TRUE))
)
(defrule abstraccion-problema::edad-18-mas
    ?req <- (edad-hecho FALSE)
    (usuario (edad ?e))
    (> ?e 17)
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (edad 18_O_MAS))
    (modify ?req (edad-hecho TRUE))
)

; Cantidad mangas leidos
(defrule abstraccion-problema::cantidad-mangas-pocos
    ?req <- (cantidad-hecho FALSE)
    (usuario (mangas-leidos ?m))
    (== ?m pocos)
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (mangas-leidos pocos))
    (modify ?req (cantidad-hecho TRUE))
)
(defrule abstraccion-problema::cantidad-mangas-bastantes
    ?req <- (cantidad-hecho FALSE)
    (usuario (mangas-leidos ?m))
    (== ?m normal)
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (mangas-leidos bastantes))
    (modify ?req (cantidad-hecho TRUE))
)
(defrule abstraccion-problema::cantidad-mangas-muchos
    ?req <- (cantidad-hecho FALSE)
    (usuario (mangas-leidos ?m))
    (== ?m muchos)
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (mangas-leidos muchos))
    (modify ?req (cantidad-hecho TRUE))
)

; Generos preferibles
(defrule abstraccion-problema::generos-preferidos
    ?req <- (preferencia-generos-hecho FALSE)
    ?usr <- (problema-abstracto (preferencia-generos $?absGen))
    (usuario (gusto-generos ?gen))
    =>
    (modify ?usr (preferencia-generos (create$ ?gen ?absGen)))
    (modify ?req (preferencia-generos-hecho TRUE))
)

; Temas preferibles
(defrule abstraccion-problema::temas-preferidos
    ?req <- (preferencia-temas-hecho FALSE)
    ?usr <- (problema-abstracto (preferencia-temas $?absTem))
    (usuario (gusto-temas ?tem))
    =>
    (modify ?usr (preferencia-temas (create$ ?tem ?absTem)))
    (modify ?req (preferencia-temas-hecho TRUE))
)

; Prefiere acabados
(defrule abstraccion-problema::preferencia-acabados
	?req <- (preferencia-acabados-hecho FALSE)
	(usuario (prefiere-acabados ?pref))
	?usr <- (problema-abstracto)
	=>
	(modify ?usr (prefiere-acabados ?pref))
	(modify ?req (preferencia-acabados-hecho TRUE))
)

; Prefiere sin anime
(defrule abstraccion-problema::preferencia-sin-anime
	?req <- (preferencia-sin-anime-hecho FALSE)
	(usuario (prefiere-sin-anime ?pref))
	?usr <- (problema-abstracto)
	=>
	(modify ?usr (prefiere-sin-anime ?pref))
	(modify ?req (preferencia-sin-anime-hecho TRUE))
)

; Quiere doujinshis
(defrule abstraccion-problema::quiere-doujinshis
	?req <- (quiere-doujinshis-hecho FALSE)
	(usuario (quiere-doujinshis ?quiere))
	?usr <- (problema-abstracto)
	=>
	(modify ?usr (quiere-doujinshis ?quiere))
	(modify ?req (quiere-doujinshis-hecho TRUE))
)


;;;;;;;;;;;;;;;;;;;;;;; Modulo de asociacion heuristica ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Control de reglas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffacts asociacion-heuristica::requisitos-asoc
    
)

; Elimina instancias que no cumplan la edad mínima
(defrule asociacion-heuristica::elimina-mas-doce
    (declare (salience 10))
    (problema-abstracto (edad MENOS_12))
    ?m <- (object (is-a Manga) (restriccion-edad ?restr))
    (> ?restr 11)
    =>
    (send ?m delete)
    (retract ?m)   
)
(defrule asociacion-heuristica::elimina-mas-dieciseis
    (declare (salience 10))
    (problema-abstracto (edad 12_O_MAS))
    ?m <- (object (is-a Manga) (restriccion-edad ?restr))
    (> ?restr 15)
    =>
    (send ?m delete)
    (retract ?m)   
)
(defrule asociacion-heuristica::elimina-mas-dieciocho
    (declare (salience 10))
    (problema-abstracto (edad 16_O_MAS))
    ?m <- (object (is-a Manga) (restriccion-edad ?restr))
    (> ?restr 17)
    =>
    (send ?m delete)
    (retract ?m)   
)

; Ejemplo tratar con instancias de clases
(defrule owo
	?m <- (object (is-a Manga) (titulo ?t) (capitulos ?c))
	(test (> ?c 1000))
	=>
	(format t "El manga %s tiene mas de 1000 capitulos" ?t)
  (printout t crlf)
)

; Add an instance of Item to the Container
;(deffunction add-instance-to-container (?container ?item)
;    (if (not (member$ ?item (slot-value ?container instances)))
;        then
;            (modify ?container (instances ?item&:(create$ (slot-value ?container instances) ?item)))
;            (printout t "Instance added successfully." crlf)
;        else
;            (printout t "Instance already exists." crlf)
;    )
;)

; Function to add a value to the multislot
;(deffunction AddValueToMultislot (?newValue)
;    (bind ?factToModify (find-fact ((?f ExampleFact)) TRUE))
;    (if ?factToModify then
;        (modify ?factToModify (exampleMultislot (create$ (get-?factToModify exampleMultislot) ?newValue)))
;    )
;)