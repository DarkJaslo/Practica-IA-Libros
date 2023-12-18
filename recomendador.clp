;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Indice simple ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Ontologia
; Instancias (¡largo!)
; Modulos y templates
; Preguntas
; Abstraccion
; Asociacion
	; Reglas red
	; Criterios generales
	; Criterios con preferencias
; Refinamiento

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
    (role abstract)
    (pattern-match non-reactive)
		(slot escrito-por
        (type INSTANCE)
        (create-accessor read-write)
				(visibility public))
    (slot ilustrado-por
        (type INSTANCE)
        (create-accessor read-write)
				(visibility public))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Instancias (hasta linea 2547) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definstances instancias
([one-piece] of Serializado
	(titulo  "One Piece")
	(publicado-por  [shueisha])
	(escrito-por  [eiichiro-oda])
	(ilustrado-por  [eiichiro-oda])
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
	(escrito-por  [yoshihiro-togashi])
	(ilustrado-por  [yoshihiro-togashi])
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
	(escrito-por  [hirohiko-araki])
	(ilustrado-por  [hirohiko-araki])
	(pertenece-a  [accion]  [aventura]  [drama]  [sobrenatural])
	(trata-de  [delincuencia]  [historico]  [superpoderes])
	(tomos  17)
	(capitulos  155)
	(copias-vendidas  9000000)
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
	(escrito-por  [hirohiko-araki])
	(ilustrado-por  [hirohiko-araki])
	(pertenece-a  [accion]  [misterio]  [sobrenatural])
	(trata-de  [escolar]  [superpoderes])
	(tomos  18)
	(capitulos  174)
	(copias-vendidas  2000000)
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
	(escrito-por  [hirohiko-araki])
	(ilustrado-por  [hirohiko-araki])
	(pertenece-a  [accion]  [aventura]  [sobrenatural]  [suspense])
	(trata-de  [superpoderes]  [historico])
	(tomos  16)
	(capitulos  152)
	(copias-vendidas  4000000)
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
	(escrito-por  [hirohiko-araki])
	(ilustrado-por  [hirohiko-araki])
	(pertenece-a  [accion]  [aventura]  [suspense])
	(trata-de  [superpoderes]  [vampiros]  [historico])
	(tomos  7)
	(capitulos  69)
	(copias-vendidas  1000000)
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
	(escrito-por  [hirohiko-araki])
	(ilustrado-por  [hirohiko-araki])
	(pertenece-a  [accion]  [aventura]  [suspense])
	(trata-de  [superpoderes]  [vampiros]  [historico])
	(tomos  5)
	(capitulos  44)
	(copias-vendidas  200000)
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
	(escrito-por  [aka-akasaka])
	(ilustrado-por  [aka-akasaka])
	(pertenece-a  [romance]  [comedia]  [psicologico])
	(trata-de  [escolar]  [reconfortante])
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
	(escrito-por  [daisuke-aishihara])
	(ilustrado-por  [daisuke-aishihara])
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
	(escrito-por  [masashi-kishimoto])
	(ilustrado-por  [masashi-kishimoto])
	(pertenece-a  [accion]  [aventura]  [fantasia]  [drama])
	(trata-de  [artes-marciales]  [tradicional-japones]  [superpoderes])
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
	(escrito-por  [hajime-isayama])
	(ilustrado-por  [hajime-isayama])
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
	(escrito-por  [tsugumi-ohba])
	(ilustrado-por  [takeshi-obata])
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
	(escrito-por  [gosho-aoyama])
	(ilustrado-por  [gosho-aoyama])
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
	(escrito-por  [akihito-tsukushi])
	(ilustrado-por  [akihito-tsukushi])
	(pertenece-a  [aventura]  [ciencia-ficcion]  [drama]  [fantasia]  [horror]  [psicologico])
	(trata-de  [gore]  [supervivencia]  [tragedia])
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
	(escrito-por  [naoko-takeuchi])
	(ilustrado-por  [naoko-takeuchi])
	(pertenece-a  [fantasia]  [romance])
	(trata-de  [magia]  [mahou-shoujo]  [mitologia])
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
	(escrito-por  [one])
	(ilustrado-por  [yusuke-murata])
	(pertenece-a  [accion]  [comedia]  [ciencia-ficcion])
	(trata-de  [humor-absurdo]  [parodia]  [personajes-adultos]  [superpoderes])
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
	(escrito-por  [kentaro-miura])
	(ilustrado-por  [kentaro-miura])
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
	(escrito-por  [yoshio-sawai])
	(ilustrado-por  [yoshio-sawai])
	(pertenece-a  [aventura]  [comedia])
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
	(restriccion-edad  12)
)
([yotsuba-to] of Serializado
	(titulo  "Yotsuba to!")
	(publicado-por  [ascii-media-works])
	(escrito-por  [kiyohiko-azuma])
	(ilustrado-por  [kiyohiko-azuma])
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
	(escrito-por  [nanashi])
	(ilustrado-por  [nanashi])
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
	(escrito-por  [haruichi-furudate])
	(ilustrado-por  [haruichi-furudate])
	(pertenece-a  [comedia]  [deportes])
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
	(escrito-por  [junji-ito])
	(ilustrado-por  [junji-ito])
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
	(escrito-por  [shuuichi-shigeno])
	(ilustrado-por  [shuuichi-shigeno])
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
	(publicado-por  [publ-ema-tooyama])
	(escrito-por  [ema-tooyama])
	(ilustrado-por  [ema-tooyama])
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
	(escrito-por  [michiharu-kusunoki])
	(ilustrado-por  [michiharu-kusunoki])
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
	(escrito-por  [yoshihiro-togashi])
	(ilustrado-por  [yoshihiro-togashi])
	(pertenece-a  [accion]  [comedia]  [fantasia]  [sobrenatural])
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
	(restriccion-edad  12)
)
([black-lagoon] of Serializado
	(titulo  "Black Lagoon")
	(publicado-por  [shogakukan])
	(escrito-por  [rei-hiroe])
	(ilustrado-por  [rei-hiroe])
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
	(escrito-por  [monkey-punch])
	(ilustrado-por  [monkey-punch])
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
	(escrito-por  [keisuke-itagaki])
	(ilustrado-por  [keisuke-itagaki])
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
	(escrito-por  [jyoji-morikawa])
	(ilustrado-por  [jyoji-morikawa])
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
	(escrito-por  [yoichi-takahashi])
	(ilustrado-por  [yoichi-takahashi])
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
	(escrito-por  [yuusei-matsui])
	(ilustrado-por  [yuusei-matsui])
	(pertenece-a  [accion]  [comedia]  [drama]  [slice-of-life])
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
	(escrito-por  [mikoto-yamaguchi])
	(ilustrado-por  [yuuki-satou])
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
	(escrito-por  [yasuhisa-hara])
	(ilustrado-por  [yasuhisa-hara])
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
	(escrito-por  [paru-itagaki])
	(ilustrado-por  [paru-itagaki])
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
	(restriccion-edad  16)
)
([mazinger-z] of Serializado
	(titulo  "Mazinger Z")
	(publicado-por  [shueisha])
	(escrito-por  [go-nagai])
	(ilustrado-por  [go-nagai])
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
	(escrito-por  [riichiro-inagaki])
	(ilustrado-por  [boichi])
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
	(escrito-por  [kenichi-sakemi])
	(ilustrado-por  [hideki-mori])
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
	(escrito-por  [shinobu-kaitani])
	(ilustrado-por  [shinobu-kaitani])
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
	(escrito-por  [tatsuo-yoshida])
	(ilustrado-por  [tatsuo-yoshida])
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
	(escrito-por  [tetsuya-tsutsui])
	(ilustrado-por  [tetsuya-tsutsui])
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
	(escrito-por  [naoki-urasawa])
	(ilustrado-por  [naoki-urasawa])
	(pertenece-a  [drama]  [misterio]  [psicologico]  [suspense])
	(trata-de  [detectives]  [personajes-adultos])
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
	(publicado-por  [publ-yuu-ishihara])
	(escrito-por  [yuu-ishihara])
	(ilustrado-por  [yuu-ishihara])
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
	(escrito-por  [makoto-yukimura])
	(ilustrado-por  [makoto-yukimura])
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
	(escrito-por  [kouta-hirano])
	(ilustrado-por  [kouta-hirano])
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
	(escrito-por  [shinichi-fukuda])
	(ilustrado-por  [shinichi-fukuda])
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
	(publicado-por  [publ-shimahara])
	(escrito-por  [shimahara])
	(ilustrado-por  [shimahara])
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
	(publicado-por  [publ-ajiichi])
	(escrito-por  [ajiichi])
	(ilustrado-por  [ajiichi])
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
	(escrito-por  [ichigo-takano])
	(ilustrado-por  [ichigo-takano])
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
	(escrito-por  [natsuki-kizu])
	(ilustrado-por  [natsuki-kizu])
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
	(escrito-por  [nojiko-hayakawa])
	(ilustrado-por  [nojiko-hayakawa])
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
	(escrito-por  [magica-quartet])
	(ilustrado-por  [hanokage])
	(pertenece-a  [drama]  [fantasia]  [psicologico]  [sobrenatural])
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
	(escrito-por  [fujita])
	(ilustrado-por  [fujita])
	(pertenece-a  [comedia]  [romance]  [slice-of-life])
	(trata-de  [personajes-adultos]  [reconfortante])
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
	(escrito-por  [midori-tayama])
	(ilustrado-por  [midori-tayama])
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
	(escrito-por  [gege-akutami])
	(ilustrado-por  [gege-akutami])
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
	(escrito-por  [kafka-asagiri])
	(ilustrado-por  [harukawa35])
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
	(escrito-por  [aka-akasaka])
	(ilustrado-por  [mengo-yokoyari])
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
	(escrito-por  [takeru-hokazono])
	(ilustrado-por  [takeru-hokazono])
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
	(escrito-por  [tatsuki-fujimoto])
	(ilustrado-por  [tatsuki-fujimoto])
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
	(escrito-por  [akimi-yoshida])
	(ilustrado-por  [akimi-yoshida])
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
	(escrito-por  [sui-ishida])
	(ilustrado-por  [sui-ishida])
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
	(escrito-por  [koyoharu-gotouge])
	(ilustrado-por  [koyoharu-gotouge])
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
([umibe-no-etranger] of One-shot
	(titulo  "Umibe no Etranger")
	(publicado-por  [shodensha])
	(escrito-por  [kanna-kii])
	(ilustrado-por  [kanna-kii])
	(pertenece-a  [romance]  [slice-of-life])
	(trata-de  [yaoi])
	(capitulos  4)
	(copias-vendidas  100000)
	(dificultad-lectura  "media")
	(inicio-publicacion  "2013-04-25")
	(metodo-distribucion  "fisico")
	(tiene-anime  FALSE)
	(valoracion  7.86)
	(restriccion-edad  12)
)
([citrus] of Serializado
	(titulo  "Citrus")
	(publicado-por  [ichijinsha])
	(escrito-por  [saburouta])
	(ilustrado-por  [saburouta])
	(pertenece-a  [romance]  [drama])
	(trata-de  [escolar]  [yuri])
	(tomos  10)
	(capitulos  50)
	(copias-vendidas  5000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2012-11-17")
	(metodo-distribucion  "fisico")
	(tiene-anime  TRUE)
	(valoracion  7.53)
	(restriccion-edad  12)
)
([kininatteru-hito-ga-otoko-ja-nakatta] of Serializado
	(titulo  "Kininatteru Hito ga Otoko ja Nakatta")
	(publicado-por  [kadokawa-shoten])
	(escrito-por  [agu])
	(ilustrado-por  [agu])
	(pertenece-a  [romance]  [slice-of-life])
	(trata-de  [artes]  [escolar]  [genero]  [musica]  [reconfortante]  [yuri])
	(tomos  5)
	(capitulos  69)
	(copias-vendidas  2000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "quincenal")
	(inicio-publicacion  "2022-04-10")
	(metodo-distribucion  "ambos")
	(tiene-anime  FALSE)
	(valoracion  8.32)
	(restriccion-edad  7)
)
([kono-subarashii-sekai-ni-shukufuku-wo] of Serializado
	(titulo  "Kono Subarashii Sekai ni Shukufuku wo!")
	(publicado-por  [fujimi-shobo])
	(escrito-por  [natsume-akatsuki])
	(ilustrado-por  [masahito-watari])
	(pertenece-a  [aventura]  [comedia]  [fantasia])
	(trata-de  [humor-absurdo]  [isekai]  [magia]  [parodia]  [reconfortante])
	(tomos  18)
	(capitulos  84)
	(copias-vendidas  10000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "semestral")
	(inicio-publicacion  "2015-04-09")
	(metodo-distribucion  "fisico")
	(tiene-anime  TRUE)
	(valoracion  7.99)
	(restriccion-edad  12)
)
([cardcaptor-sakura] of Serializado
	(titulo  "Cardcaptor Sakura")
	(publicado-por  [kodansha])
	(escrito-por  [clamp])
	(ilustrado-por  [clamp])
	(pertenece-a  [aventura]  [comedia]  [fantasia]  [romance])
	(trata-de  [escolar]  [magia]  [mahou-shoujo])
	(tomos  12)
	(capitulos  50)
	(copias-vendidas  22000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "1996-05-02")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.38)
	(restriccion-edad  7)
)
([tengen-toppa-gurren-lagann] of Serializado
	(titulo  "Tengen Toppa Gurren Lagann")
	(publicado-por  [ascii-media-works])
	(escrito-por  [kazuki-nakashima])
	(ilustrado-por  [kotarou-mori])
	(pertenece-a  [accion]  [aventura]  [ciencia-ficcion]  [comedia])
	(trata-de  [robots]  [espacio-exterior])
	(tomos  10)
	(capitulos  65)
	(copias-vendidas  8000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2007-04-27")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.95)
	(restriccion-edad  12)
)
([hirugashi-no-naku-koro-ni] of Serializado
	(titulo  "Hirugashi no Naku Koro ni")
	(publicado-por  [square-enix])
	(escrito-por  [ryukishi07])
	(ilustrado-por  [karin-suzuragi])
	(pertenece-a  [comedia]  [drama]  [horror]  [misterio]  [psicologico]  [sobrenatural]  [suspense])
	(trata-de  [tragedia])
	(tomos  2)
	(capitulos  7)
	(copias-vendidas  140000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "trimestral")
	(inicio-publicacion  "2005-03-24")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.11)
	(restriccion-edad  16)
)
([umineko-no-naku-koro-ni] of Serializado
	(titulo  "Umineko no Naku Koro ni")
	(publicado-por  [square-enix])
	(escrito-por  [ryukishi07])
	(ilustrado-por  [kei-natsumi])
	(pertenece-a  [drama]  [fantasia]  [horror]  [misterio]  [psicologico]  [sobrenatural]  [suspense])
	(trata-de  [tragedia])
	(tomos  50)
	(capitulos  258)
	(copias-vendidas  12000000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2007-12-22")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.82)
	(restriccion-edad  16)
)
([inuyasha] of Serializado
	(titulo  "InuYasha")
	(publicado-por  [shogakukan])
	(escrito-por  [rumiko-takahashi])
	(ilustrado-por  [rumiko-takahashi])
	(pertenece-a  [aventura]  [comedia]  [drama]  [fantasia]  [romance]  [sobrenatural])
	(trata-de  [historico]  [isekai]  [mitologia]  [viajes-en-el-tiempo]  [triangulo-amoroso])
	(tomos  56)
	(capitulos  558)
	(copias-vendidas  50000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1996-11-13")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.86)
	(restriccion-edad  12)
)
([elfen-lied] of Serializado
	(titulo  "Elfen Lied")
	(publicado-por  [shueisha])
	(escrito-por  [lynn-okamoto])
	(ilustrado-por  [lynn-okamoto])
	(pertenece-a  [accion]  [drama]  [horror]  [psicologico]  [romance]  [sobrenatural])
	(trata-de  [gore])
	(tomos  12)
	(capitulos  113)
	(copias-vendidas  14000000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2002-06-06")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.82)
	(restriccion-edad  18)
)
([horimiya] of Serializado
	(titulo  "Horimiya")
	(publicado-por  [gfantasy])
	(escrito-por  [hero])
	(ilustrado-por  [daisuke-hagiwara])
	(pertenece-a  [comedia]  [romance]  [slice-of-life])
	(trata-de  [escolar]  [reconfortante])
	(tomos  17)
	(capitulos  139)
	(copias-vendidas  1200000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2011-10-18")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.43)
	(restriccion-edad  12)
)
([magi] of Serializado
	(titulo  "Magi")
	(publicado-por  [shogakukan])
	(escrito-por  [shinobu-ohtaka])
	(ilustrado-por  [shinobu-ohtaka])
	(pertenece-a  [accion]  [aventura]  [comedia]  [drama]  [fantasia])
	(trata-de  [guerra]  [historico]  [magia]  [mitologia])
	(tomos  37)
	(capitulos  369)
	(copias-vendidas  25000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2009-06-03")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.27)
	(restriccion-edad  12)
)
([noragami] of Serializado
	(titulo  "Noragami")
	(publicado-por  [kodansha])
	(escrito-por  [adachitoka])
	(ilustrado-por  [adachitoka])
	(pertenece-a  [accion]  [fantasia]  [comedia]  [drama]  [sobrenatural])
	(trata-de  [mitologia])
	(tomos  26)
	(capitulos  108)
	(copias-vendidas  9200000)
	(dificultad-lectura  "media")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2010-12-06")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.39)
	(restriccion-edad  12)
)
([boku-no-hero-academia] of Serializado
	(titulo  "Boku no Hero Academia")
	(publicado-por  [shueisha])
	(escrito-por  [kouhei-horikoshi])
	(ilustrado-por  [kouhei-horikoshi])
	(pertenece-a  [accion])
	(trata-de  [escolar]  [reconfortante]  [superpoderes])
	(tomos  20)
	(capitulos  200)
	(copias-vendidas  98000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2014-07-07")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.07)
	(restriccion-edad  12)
)
([yakusoku-no-neverland] of Serializado
	(titulo  "Yakusoku no Neverland")
	(publicado-por  [shueisha])
	(escrito-por  [kaiu-shirai])
	(ilustrado-por  [posuka-demizu])
	(pertenece-a  [fantasia]  [ciencia-ficcion]  [psicologico]  [misterio]  [suspense])
	(trata-de  [supervivencia]  [postapocaliptico])
	(tomos  20)
	(capitulos  181)
	(copias-vendidas  13000000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2016-01-01")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.16)
	(restriccion-edad  16)
)
([tokyo-revengers] of Serializado
	(titulo  "Tokyo Revengers")
	(publicado-por  [shueisha])
	(escrito-por  [ken-wakui])
	(ilustrado-por  [ken-wakui])
	(pertenece-a  [accion]  [drama])
	(trata-de  [artes-marciales]  [delincuencia]  [escolar]  [tragedia]  [viajes-en-el-tiempo])
	(tomos  31)
	(capitulos  279)
	(copias-vendidas  6000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2017-01-01")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.88)
	(restriccion-edad  16)
)
([nisekoi] of Serializado
	(titulo  "Nisekoi")
	(publicado-por  [shueisha])
	(escrito-por  [naoshi-komi])
	(ilustrado-por  [naoshi-komi])
	(pertenece-a  [comedia]  [romance])
	(trata-de  [harem]  [escolar]  [triangulo-amoroso])
	(tomos  25)
	(capitulos  229)
	(copias-vendidas  13000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2011-06-23")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.72)
	(restriccion-edad  12)
)
([spy-x-family] of Serializado
	(titulo  "Spy x Family")
	(publicado-por  [shueisha])
	(escrito-por  [tatsuya-endou])
	(ilustrado-por  [tatsuya-endou])
	(pertenece-a  [accion]  [comedia]  [slice-of-life])
	(trata-de  [escolar]  [familiar]  [historico]  [reconfortante]  [detectives])
	(tomos  13)
	(capitulos  101)
	(copias-vendidas  66000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2019-03-04")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.55)
	(restriccion-edad  7)
)
([kanojo-okarishimasu] of Serializado
	(titulo  "Kanojo, Okarishimasu")
	(publicado-por  [shueisha])
	(escrito-por  [reiji-miyajima])
	(ilustrado-por  [reiji-miyajima])
	(pertenece-a  [comedia]  [romance]  [slice-of-life])
	(trata-de  [triangulo-amoroso]  [harem]  [escolar])
	(tomos  16)
	(capitulos  200)
	(copias-vendidas  1200000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2017-06-12")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  6.4)
	(restriccion-edad  16)
)
([gotoubun-no-hanayome] of Serializado
	(titulo  "Gotoubun no Hanayome")
	(publicado-por  [shueisha])
	(escrito-por  [negi-haruba])
	(ilustrado-por  [negi-haruba])
	(pertenece-a  [comedia]  [romance]  [slice-of-life])
	(trata-de  [triangulo-amoroso]  [harem]  [escolar])
	(tomos  14)
	(capitulos  122)
	(copias-vendidas  3400000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2017-08-09")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.93)
	(restriccion-edad  12)
)
([mirai-nikki] of Serializado
	(titulo  "Mirai Nikki")
	(publicado-por  [shueisha])
	(escrito-por  [sakae-esuno])
	(ilustrado-por  [sakae-esuno])
	(pertenece-a  [accion]  [sobrenatural]  [suspense]  [psicologico])
	(trata-de  [gore]  [battle-royale]  [supervivencia]  [escolar])
	(tomos  12)
	(capitulos  59)
	(copias-vendidas  120000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2006-01-10")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.66)
	(restriccion-edad  18)
)
([nana] of Serializado
	(titulo  "Nana")
	(publicado-por  [shueisha])
	(escrito-por  [ai-yazawa])
	(ilustrado-por  [ai-yazawa])
	(pertenece-a  [drama]  [romance])
	(trata-de  [personajes-adultos]  [triangulo-amoroso]  [artes]  [musica])
	(tomos  21)
	(capitulos  84)
	(copias-vendidas  6000000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "en pausa")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2000-05-26")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.79)
	(restriccion-edad  16)
)
([fullmetal-alchemist] of Serializado
	(titulo  "Fullmetal Alchemist")
	(publicado-por  [shueisha])
	(escrito-por  [hiromu-arakawa])
	(ilustrado-por  [hiromu-arakawa])
	(pertenece-a  [accion]  [aventura]  [drama]  [fantasia])
	(trata-de  [guerra]  [historico]  [magia])
	(tomos  27)
	(capitulos  116)
	(copias-vendidas  121000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "2001-07-12")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  9.03)
	(restriccion-edad  12)
)
([great-teacher-onizuka] of Serializado
	(titulo  "Great Teacher Onizuka")
	(publicado-por  [shogakukan])
	(escrito-por  [tooru-fujisawa])
	(ilustrado-por  [tooru-fujisawa])
	(pertenece-a  [accion]  [comedia]  [drama]  [slice-of-life])
	(trata-de  [delincuencia]  [escolar])
	(tomos  25)
	(capitulos  208)
	(copias-vendidas  12000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1996-02-28")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.88)
	(restriccion-edad  16)
)
([love-live-school-idol-project] of Serializado
	(titulo  "Love Live! School Idol Project")
	(publicado-por  [ascii-media-works])
	(escrito-por  [sakurako-kimino])
	(ilustrado-por  [arumi-tokita])
	(pertenece-a  [slice-of-life])
	(trata-de  [escolar]  [idols]  [musica])
	(tomos  5)
	(capitulos  43)
	(copias-vendidas  2000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en pausa")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2012-01-30")
	(metodo-distribucion  "fisico")
	(tiene-anime  TRUE)
	(valoracion  7.5)
	(restriccion-edad  7)
)
([the-idolmaster] of Serializado
	(titulo  "The iDOLMASTER")
	(publicado-por  [publ-tatsuya-takahashi])
	(escrito-por  [tatsuya-takahashi])
	(ilustrado-por  [tatsuya-takahashi])
	(pertenece-a  [comedia]  [drama])
	(trata-de  [idols]  [musica])
	(tomos  6)
	(capitulos  42)
	(copias-vendidas  100000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2012-08-27")
	(metodo-distribucion  "fisico")
	(tiene-anime  FALSE)
	(valoracion  7.6)
	(restriccion-edad  12)
)
([aikatsu] of Serializado
	(titulo  "Aikatsu!")
	(publicado-por  [publ-sunrise])
	(escrito-por  [sunrise])
	(ilustrado-por  [sunrise])
	(pertenece-a  [slice-of-life])
	(trata-de  [escolar]  [idols]  [musica])
	(tomos  3)
	(capitulos  28)
	(copias-vendidas  40000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "irregular")
	(inicio-publicacion  "2012-10-08")
	(metodo-distribucion  "ambos")
	(tiene-anime  FALSE)
	(valoracion  7.49)
	(restriccion-edad  3)
)
([sword-art-online] of Serializado
	(titulo  "Sword Art Online")
	(publicado-por  [publ-reki-kawahara])
	(escrito-por  [reki-kawahara])
	(ilustrado-por  [reki-kawahara])
	(pertenece-a  [accion]  [aventura]  [fantasia]  [romance])
	(trata-de  [isekai]  [triangulo-amoroso]  [videojuegos])
	(tomos  10)
	(capitulos  72)
	(copias-vendidas  200000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2009-04-10")
	(metodo-distribucion  "ambos")
	(tiene-anime  FALSE)
	(valoracion  7.99)
	(restriccion-edad  12)
)
([accel-world] of Serializado
	(titulo  "Accel World")
	(publicado-por  [publ-reki-kawahara])
	(escrito-por  [reki-kawahara])
	(ilustrado-por  [reki-kawahara])
	(pertenece-a  [accion]  [ciencia-ficcion]  [romance])
	(trata-de  [escolar]  [robots]  [superpoderes]  [videojuegos])
	(tomos  8)
	(capitulos  45)
	(copias-vendidas  80000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2010-04-10")
	(metodo-distribucion  "fisico")
	(tiene-anime  FALSE)
	(valoracion  7.25)
	(restriccion-edad  12)
)
([yamada-kun-to-lv999-no-koi-wo-suru] of Serializado
	(titulo  "Yamada-kun to Lv999 no Koi wo Suru")
	(publicado-por  [publ-mashiro])
	(escrito-por  [mashiro])
	(ilustrado-por  [mashiro])
	(pertenece-a  [romance])
	(trata-de  [videojuegos])
	(tomos  8)
	(capitulos  72)
	(copias-vendidas  3000000)
	(dificultad-lectura  "baja")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2019-03-07")
	(metodo-distribucion  "fisico")
	(tiene-anime  FALSE)
	(valoracion  8.15)
	(restriccion-edad  7)
)
([among-us] of One-shot
	(titulo  "Among Us")
	(publicado-por  [shogakukan])
	(escrito-por  [berabou])
	(ilustrado-por  [berabou])
	(pertenece-a  [ciencia-ficcion]  [comedia])
	(trata-de  [espacio-exterior]  [humor-absurdo])
	(capitulos  1)
	(copias-vendidas  200000)
	(dificultad-lectura  "baja")
	(inicio-publicacion  "2022-02-28")
	(metodo-distribucion  "ambos")
	(tiene-anime  FALSE)
	(valoracion  7.08)
	(restriccion-edad  5)
)
([yu-gi-oh] of Serializado
	(titulo  "Yu-Gi-Oh")
	(publicado-por  [shueisha])
	(escrito-por  [kazuki-takahashi])
	(ilustrado-por  [kazuki-takahashi])
	(pertenece-a  [accion]  [aventura]  [comedia]  [fantasia]  [misterio]  [sobrenatural])
	(trata-de  [escolar]  [historico]  [juegos-de-estrategia])
	(tomos  38)
	(capitulos  343)
	(copias-vendidas  40000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1996-09-17")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.66)
	(restriccion-edad  12)
)
([dororo] of Serializado
	(titulo  "Dororo")
	(publicado-por  [shogakukan])
	(escrito-por  [osamu-tezuka])
	(ilustrado-por  [osamu-tezuka])
	(pertenece-a  [accion]  [aventura]  [drama]  [fantasia]  [horror])
	(trata-de  [mitologia]  [tradicional-japones])
	(tomos  3)
	(capitulos  19)
	(copias-vendidas  14000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "semanal")
	(inicio-publicacion  "1967-08-09")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  7.31)
	(restriccion-edad  16)
)
([vinland-saga] of Serializado
	(titulo  "Vinland Saga")
	(publicado-por  [kodansha])
	(escrito-por  [makoto-yukimura])
	(ilustrado-por  [makoto-yukimura])
	(pertenece-a  [accion]  [aventura]  [drama])
	(trata-de  [gore]  [guerra]  [historico]  [tragedia]  [venganza])
	(tomos  27)
	(capitulos  206)
	(copias-vendidas  7000000)
	(dificultad-lectura  "alta")
	(estado-publicacion  "en publicacion")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "2005-04-13")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  9.06)
	(restriccion-edad  16)
)
([neon-genesis-evangelion] of Serializado
	(titulo  "Neon Genesis Evangelion")
	(publicado-por  [kadokawa-shoten])
	(escrito-por  [yoshiyuki-sadamoto])
	(ilustrado-por  [yoshiyuki-sadamoto])
	(pertenece-a  [accion]  [ciencia-ficcion]  [drama]  [psicologico])
	(trata-de  [robots])
	(tomos  14)
	(capitulos  97)
	(copias-vendidas  25000000)
	(dificultad-lectura  "media")
	(estado-publicacion  "acabado")
	(frecuencia-publicacion  "mensual")
	(inicio-publicacion  "1994-12-26")
	(metodo-distribucion  "ambos")
	(tiene-anime  TRUE)
	(valoracion  8.58)
	(restriccion-edad  16)
)

; Autores

([adachitoka] of Autor
	(nombre  "Adachitoka")
)
([agu] of Autor
	(nombre  "Agu")
)
([ai-yazawa] of Autor
	(nombre  "Ai Yazawa")
)
([ajiichi] of Autor
	(nombre  "Ajiichi")
)
([aka-akasaka] of Autor
	(nombre  "Aka Akasaka")
)
([akihito-tsukushi] of Autor
	(nombre  "Akihito Tsukushi")
)
([akimi-yoshida] of Autor
	(nombre  "Akimi Yoshida")
)
([arumi-tokita] of Autor
	(nombre  "Arumi Tokita")
)
([berabou] of Autor
	(nombre  "Berabou")
)
([boichi] of Autor
	(nombre  "Boichi")
)
([clamp] of Autor
	(nombre  "CLAMP")
)
([daisuke-aishihara] of Autor
	(nombre  "Daisuke Aishihara")
)
([daisuke-hagiwara] of Autor
	(nombre  "Daisuke Hagiwara")
)
([eiichiro-oda] of Autor
	(nombre  "Eiichiro Oda")
)
([ema-tooyama] of Autor
	(nombre  "Ema Tooyama")
)
([fujita] of Autor
	(nombre  "Fujita")
)
([gege-akutami] of Autor
	(nombre  "Gege Akutami")
)
([go-nagai] of Autor
	(nombre  "Go Nagai")
)
([gosho-aoyama] of Autor
	(nombre  "Gosho Aoyama")
)
([hajime-isayama] of Autor
	(nombre  "Hajime Isayama")
)
([hanokage] of Autor
	(nombre  "Hanokage")
)
([haruichi-furudate] of Autor
	(nombre  "Haruichi Furudate")
)
([harukawa35] of Autor
	(nombre  "Harukawa35")
)
([hero] of Autor
	(nombre  "HERO")
)
([hideki-mori] of Autor
	(nombre  "Hideki Mori")
)
([hirohiko-araki] of Autor
	(nombre  "Hirohiko Araki")
)
([hiromu-arakawa] of Autor
	(nombre  "Hiromu Arakawa")
)
([ichigo-takano] of Autor
	(nombre  "Ichigo Takano")
)
([junji-ito] of Autor
	(nombre  "Junji Ito")
)
([jyoji-morikawa] of Autor
	(nombre  "Jyoji Morikawa")
)
([kafka-asagiri] of Autor
	(nombre  "Kafka Asagiri")
)
([kaiu-shirai] of Autor
	(nombre  "Kaiu Shirai")
)
([kanna-kii] of Autor
	(nombre  "Kanna Kii")
)
([karin-suzuragi] of Autor
	(nombre  "Karin Suzuragi")
)
([kazuki-nakashima] of Autor
	(nombre  "Kazuki Nakashima")
)
([kazuki-takahashi] of Autor
	(nombre  "Kazuki Takahashi")
)
([kei-natsumi] of Autor
	(nombre  "Kei Natsumi")
)
([keisuke-itagaki] of Autor
	(nombre  "Keisuke Itagaki")
)
([ken-wakui] of Autor
	(nombre  "Ken Wakui")
)
([kenichi-sakemi] of Autor
	(nombre  "Kenichi Sakemi")
)
([kentaro-miura] of Autor
	(nombre  "Kentaro Miura")
)
([kiyohiko-azuma] of Autor
	(nombre  "Kiyohiko Azuma")
)
([kotarou-mori] of Autor
	(nombre  "Kotarou Mori")
)
([kouhei-horikoshi] of Autor
	(nombre  "Kouhei Horikoshi")
)
([kouta-hirano] of Autor
	(nombre  "Kouta Hirano")
)
([koyoharu-gotouge] of Autor
	(nombre  "Koyoharu Gotouge")
)
([lynn-okamoto] of Autor
	(nombre  "Lynn Okamoto")
)
([magica-quartet] of Autor
	(nombre  "Magica Quartet")
)
([makoto-yukimura] of Autor
	(nombre  "Makoto Yukimura")
)
([masahito-watari] of Autor
	(nombre  "Masahito Watari")
)
([masashi-kishimoto] of Autor
	(nombre  "Masashi Kishimoto")
)
([mashiro] of Autor
	(nombre  "Mashiro")
)
([mengo-yokoyari] of Autor
	(nombre  "Mengo Yokoyari")
)
([michiharu-kusunoki] of Autor
	(nombre  "Michiharu Kusunoki")
)
([midori-tayama] of Autor
	(nombre  "Midori Tayama")
)
([mikoto-yamaguchi] of Autor
	(nombre  "Mikoto Yamaguchi")
)
([monkey-punch] of Autor
	(nombre  "Monkey Punch")
)
([nanashi] of Autor
	(nombre  "Nanashi")
)
([naoki-urasawa] of Autor
	(nombre  "Naoki Urasawa")
)
([naoko-takeuchi] of Autor
	(nombre  "Naoko Takeuchi")
)
([naoshi-komi] of Autor
	(nombre  "Naoshi Komi")
)
([natsuki-kizu] of Autor
	(nombre  "Natsuki Kizu")
)
([natsume-akatsuki] of Autor
	(nombre  "Natsume Akatsuki")
)
([negi-haruba] of Autor
	(nombre  "Negi Haruba")
)
([nojiko-hayakawa] of Autor
	(nombre  "Nojiko Hayakawa")
)
([one] of Autor
	(nombre  "ONE")
)
([osamu-tezuka] of Autor
	(nombre  "Osamu Tezuka")
)
([paru-itagaki] of Autor
	(nombre  "Paru Itagaki")
)
([posuka-demizu] of Autor
	(nombre  "Posuka Demizu")
)
([rei-hiroe] of Autor
	(nombre  "Rei Hiroe")
)
([reiji-miyajima] of Autor
	(nombre  "Reiji Miyajima")
)
([reki-kawahara] of Autor
	(nombre  "Reki Kawahara")
)
([riichiro-inagaki] of Autor
	(nombre  "Riichiro Inagaki")
)
([rumiko-takahashi] of Autor
	(nombre  "Rumiko Takahashi")
)
([ryukishi07] of Autor
	(nombre  "Ryukishi07")
)
([saburouta] of Autor
	(nombre  "Saburouta")
)
([sakae-esuno] of Autor
	(nombre  "Sakae Esuno")
)
([sakurako-kimino] of Autor
	(nombre  "Sakurako Kimino")
)
([shimahara] of Autor
	(nombre  "Shimahara")
)
([shinichi-fukuda] of Autor
	(nombre  "Shinichi Fukuda")
)
([shinobu-kaitani] of Autor
	(nombre  "Shinobu Kaitani")
)
([shinobu-ohtaka] of Autor
	(nombre  "Shinobu Ohtaka")
)
([shuuichi-shigeno] of Autor
	(nombre  "Shuuichi Shigeno")
)
([sui-ishida] of Autor
	(nombre  "Sui Ishida")
)
([sunrise] of Autor
	(nombre  "Sunrise")
)
([takeru-hokazono] of Autor
	(nombre  "Takeru Hokazono")
)
([takeshi-obata] of Autor
	(nombre  "Takeshi Obata")
)
([tatsuki-fujimoto] of Autor
	(nombre  "Tatsuki Fujimoto")
)
([tatsuo-yoshida] of Autor
	(nombre  "Tatsuo Yoshida")
)
([tatsuya-endou] of Autor
	(nombre  "Tatsuya Endou")
)
([tatsuya-takahashi] of Autor
	(nombre  "Tatsuya Takahashi")
)
([tetsuya-tsutsui] of Autor
	(nombre  "Tetsuya Tsutsui")
)
([tooru-fujisawa] of Autor
	(nombre  "Tooru Fujisawa")
)
([tsugumi-ohba] of Autor
	(nombre  "Tsugumi Ohba")
)
([yasuhisa-hara] of Autor
	(nombre  "Yasuhisa Hara")
)
([yoichi-takahashi] of Autor
	(nombre  "Yoichi Takahashi")
)
([yoshihiro-togashi] of Autor
	(nombre  "Yoshihiro Togashi")
)
([yoshio-sawai] of Autor
	(nombre  "Yoshio Sawai")
)
([yoshiyuki-sadamoto] of Autor
	(nombre  "Yoshiyuki Sadamoto")
)
([yusuke-murata] of Autor
	(nombre  "Yusuke Murata")
)
([yuu-ishihara] of Autor
	(nombre  "Yuu Ishihara")
)
([yuuki-satou] of Autor
	(nombre  "Yuuki Satou")
)
([yuusei-matsui] of Autor
	(nombre  "Yuusei Matsui")
)

; Publicadores

([akita-shoten] of Editorial
	(nombre  "Akita Shoten")
)
([ascii-media-works] of Editorial
	(nombre  "ASCII Media Works")
)
([fujimi-shobo] of Editorial
	(nombre  "Fujimi Shobo")
)
([futabasha] of Editorial
	(nombre  "Futabasha")
)
([gfantasy] of Editorial
	(nombre  "GFantasy")
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
([shodensha] of Editorial
	(nombre  "Shodensha")
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
([publ-mashiro] of Autopublicador
	(nombre  "Mashiro")
)
([publ-reki-kawahara] of Autopublicador
	(nombre  "Reki Kawahara")
)
([publ-shimahara] of Autopublicador
	(nombre  "Shimahara")
)
([publ-sunrise] of Autopublicador
	(nombre  "Sunrise")
)
([publ-tatsuya-takahashi] of Autopublicador
	(nombre  "Tatsuya Takahashi")
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
; Número de instancias: 97

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Modulos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Modulo principal
(defmodule MAIN (export defclass ?ALL))

; Modulo obtencion datos
(defmodule preguntas-usuario
    (import MAIN defclass ?ALL)
    (export deftemplate ?ALL)
)

; Modulo para convertir al problema concreto en abstracto
(defmodule abstraccion-problema
		(import MAIN defclass ?ALL)
    (import preguntas-usuario ?ALL)
    (export deftemplate ?ALL)
)

; Modulo para solucionar el problema abstracto
(defmodule asociacion-heuristica
		(import MAIN defclass ?ALL)
    (import abstraccion-problema ?ALL)
    (export deftemplate ?ALL)
		(export defglobal ?ALL)
)

; Modulo para refinar la solucion
(defmodule refinamiento-solucion
		(import MAIN defclass ?ALL)
    (import asociacion-heuristica deftemplate ?ALL)
		(import asociacion-heuristica defglobal ?ALL)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Funciones ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; funcion que nos dan en el FAQ apartado 3.10
; recibe la pregunta que hacer al usuario y el rango de valores valido y los imprime
; lee el input y si es incorrecto vuelve a escribir la pregunta y leer hasta que
; sea correcto, entonces devuelve el valor leido
(deffunction preguntas-usuario::pregunta-numerica (?pregunta ?rangini ?rangfi)
	(format t "%s [%d, %d] " ?pregunta ?rangini ?rangfi)
	(printout t crlf)
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
	(printout t crlf)
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
	(printout t crlf)
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
		))
	do
		(printout t "Algo ha salido mal. Vuelve a escribir tu respuesta." crlf)
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
	(format t "%s (si/no)" ?pregunta)
	(printout t crlf)
	(bind ?respuesta (read))
	(if (eq (lowcase ?respuesta) si)
			then TRUE
			else FALSE
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Message handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmessage-handler Serializado print ()
	(printout t crlf)
	(format t "  %s %n" ?self:titulo)
	(bind ?escritor (send ?self:escrito-por get-nombre))
	(bind ?artista (send ?self:ilustrado-por get-nombre))
	(if (eq ?escritor ?artista) then
		(format t "  Escrito e ilustrado por %s %n" ?escritor)
		else
		(format t "  Escrito por %s %n" ?escritor)
		(format t "  Ilustrado por %s %n" ?artista)
	)
	(format t "  Publicado por %s %n" (send ?self:publicado-por get-nombre))
	(format t "  Estado de publicacion: %s %n" ?self:estado-publicacion)
	(if (or (eq ?self:estado-publicacion "en publicacion") (eq ?self:estado-publicacion "en pausa")) then
		(format t "  Frecuencia de publicacion: %s %n" ?self:frecuencia-publicacion)
	)
	(format t "  Inicio de publicacion: %s %n" ?self:inicio-publicacion)
	(format t "  Tomos: %d %n" ?self:tomos)
	(format t "  Capitulos: %d %n" ?self:capitulos)
	(format t "  Valoracion: %.2f %n" ?self:valoracion)
	(if (eq ?self:metodo-distribucion "ambos") then
		(format t "  Se puede leer en formato tanto digital como fisico %n")
		else
		(format t "  Se puede leer en formato %s %n" ?self:metodo-distribucion)
	)
	(format t "  No se recomienda su lectura a menores de %d años %n" ?self:restriccion-edad)
	; Generos
	(format t "  Generos %n")
	(bind $?generos ?self:pertenece-a)
	(progn$ (?gen ?generos)
		(bind ?nombre (send ?gen get-nombre))
		(format t "%t - %s %n" ?nombre)
	)
	; Temas
	(format t "  Temas %n")
	(bind $?temas ?self:trata-de)
	(progn$ (?tema ?temas)
		(bind ?nombre (send ?tema get-nombre))
		(format t "%t - %s %n" ?nombre)
	)
	(printout t crlf)
)

(defmessage-handler One-shot print ()
	(printout t crlf)
	(format t "  %s %n" ?self:titulo)
	(bind ?escritor (send ?self:escrito-por get-nombre))
	(bind ?artista (send ?self:ilustrado-por get-nombre))
	(if (eq ?escritor ?artista) then
		(format t "  Escrito e ilustrado por %s %n" ?escritor)
		else
		(format t "  Escrito por %s %n" ?escritor)
		(format t "  Ilustrado por %s %n" ?artista)
	)
	(format t "  Publicado por %s %n" (send ?self:publicado-por get-nombre))
	(format t "  Inicio de publicacion: %s %n" ?self:inicio-publicacion)
	(format t "  Capitulos: %d %n" ?self:capitulos)
	(format t "  Valoracion: %.2f %n" ?self:valoracion)
	(if (eq ?self:metodo-distribucion "ambos") then
		(format t "  Se puede leer en formato tanto digital como fisico %n")
		else
		(format t "  Se puede leer en formato %s %n" ?self:metodo-distribucion)
	)
	(format t "  No se recomienda su lectura a menores de %d años %n" ?self:restriccion-edad)
	; Generos
	(format t "  Generos %n")
	(bind $?generos ?self:pertenece-a)
	(progn$ (?gen ?generos)
		(bind ?nombre (send ?gen get-nombre))
		(format t "%t - %s %n" ?nombre)
	)
	; Temas
	(format t "  Temas %n")
	(bind $?temas ?self:trata-de)
	(progn$ (?tema ?temas)
		(bind ?nombre (send ?tema get-nombre))
		(format t "%t - %s %n" ?nombre)
	)
	(printout t crlf)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Templates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problema concreto
(deftemplate preguntas-usuario::usuario
    (slot edad (type INTEGER))
    (slot mangas-leidos (type SYMBOL)
                        (allowed-values pocos bastantes muchos))
    (multislot gusto-generos (type INSTANCE))
    (multislot gusto-temas (type INSTANCE))
		(slot prefiere-no-violentos (type SYMBOL)
                            		(allowed-values TRUE FALSE)
                            		(default FALSE))
		(slot prefiere-acabados (type SYMBOL)
                            (allowed-values TRUE FALSE)
                            (default FALSE))
    (slot prefiere-sin-anime (type SYMBOL)
                            (allowed-values TRUE FALSE)
                            (default FALSE))
    (slot quiere-doujinshis (type SYMBOL)
                            (allowed-values TRUE FALSE)
                            (default FALSE))
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
	(slot no-violentos 		(type SYMBOL)
							(allowed-values TRUE FALSE)
							(default TRUE))
    (multislot preferencia-generos (type INSTANCE))
    (multislot preferencia-temas (type INSTANCE))
)

; Solucion abstracta: guarda aquellos mangas que se consideran "recomendables"
(deftemplate asociacion-heuristica::solucion-abstracta
    (multislot recomendables (type INSTANCE)) ;instancias de datos-manga
)

; Estructura intermedia que guarda un manga (su titulo) y los matches en generos, temas y nomatches en ambos
(deftemplate asociacion-heuristica::datos-manga
	(slot manga (type STRING)) ; titulo de un manga (para buscar el objeto manga)
	(slot generos (type INTEGER) ; veces que un genero coincide con pref del usuario
								(default 0))
	(slot temas (type INTEGER)   ; veces que un tema coincide con pref del usuario
								(default 0))
	(slot nomatch (type INTEGER) ; veces que un tema o genero del manga no era preferido por el usuario
								(default 0))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reglas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Primera regla que se ejecuta en todo el programa
; Para establecer que la estrategia de resolucion de conflictos (orden en 
; el que se disparan reglas con misma saliencia) sea de forma aleatoria

(defrule MAIN::set-estrategia
	(declare (salience 100))
	=>
	(seed (nth$ 6 (local-time)))
	(set-strategy random)
)

; Aqui controlaremos en cambio entre los modulos del programa

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
(defrule MAIN::refina-problema
	(declare (salience 7))
	=>
	(focus refinamiento-solucion)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Preguntas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Modulo de preguntas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule preguntas-usuario::crea-usuario
    (not (usuario))
    =>
    (assert (usuario))
)

(defrule preguntas-usuario::pregunta-edad
    (not (edad-preguntada))
    ?usr <- (usuario)
    =>
    (modify ?usr (edad (pregunta-numerica "¿Cuantos años tienes?" 0 150)))
    (assert (edad-preguntada))
)

(defrule preguntas-usuario::pregunta-mangas-leidos
    (edad-preguntada)
    (not (mangas-leidos-preguntado))
    ?usr <- (usuario)
    =>
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
		(bind ?nombre (send ?gen get-nombre))
		; insert$ devuelve la lista con el nombre nuevo insertado al final, y luego hace bind de esto en la variable
		(bind $?nombres-gen(insert$ $?nombres-gen (+ (length$ $?nombres-gen) 1) ?nombre))
	)

	; devuelve los indices de los generos escogidos
	(bind ?respuesta (pregunta-multirespuesta "Escoge tus generos favoritos (separados por un espacio, o 0 si no tienes)" $?nombres-gen))

	(bind $?instancias (create$))

	; busca cada genero en ?generos y lo añade a la lista de instancias
	(progn$ (?index ?respuesta)
		(bind ?gen (nth$ ?index ?generos))
		(bind $?instancias(insert$ $?instancias (+ (length$ $?instancias) 1) ?gen))
	)

	(modify ?usr (gusto-generos $?instancias))
    (assert (generos-preguntado))
)

(defrule preguntas-usuario::pregunta-temas
    (generos-preguntado)
    (not (temas-preguntado))
	?usr <- (usuario)
    =>
	(bind $?conj-temas (find-all-instances ((?inst Tema)) TRUE))
	(bind $?conj-nombres (create$))

	(progn$ (?tema ?conj-temas)
		(bind ?nombre (send ?tema get-nombre))
		(bind $?conj-nombres(insert$ $?conj-nombres (+ (length$ $?conj-nombres) 1) ?nombre))
	)
	
	; devuelve los indices de los temas escogidos
	(bind ?respuesta (pregunta-multirespuesta "Escoge tus temas favoritos (separados por un espacio, o 0 si no tienes)" $?conj-nombres))

	(bind $?instancias (create$))

	(progn$ (?index ?respuesta)
		(bind ?tema (nth$ ?index ?conj-temas))
		(bind $?instancias(insert$ $?instancias (+ (length$ $?instancias) 1) ?tema))
	)

	(modify ?usr (gusto-temas $?instancias))
    (assert (temas-preguntado))
)

(defrule preguntas-usuario::prefiere-no-violentos
	(temas-preguntado)
	(not (prefiere-no-violentos-preguntado))
	?usr <- (usuario)
	=>
	(modify ?usr (prefiere-no-violentos (pregunta-si-no "¿Te molestan la violencia explícita o los temas desagradables?")))
	(assert (prefiere-no-violentos-preguntado))
)

(defrule preguntas-usuario::prefiere-acabados
	(prefiere-no-violentos-preguntado)
	(not (prefiere-acabados-preguntado))
	?usr <- (usuario)
	=>
	(modify ?usr (prefiere-acabados (pregunta-si-no "¿Tienes una preferencia por los mangas acabados?")))
	(assert (prefiere-acabados-preguntado))
)

(defrule preguntas-usuario::prefiere-sin-anime
	(prefiere-acabados-preguntado)
	(not (prefiere-sin-anime-preguntado))
	?usr <- (usuario)
	=>
	(modify ?usr (prefiere-sin-anime (pregunta-si-no "¿Tienes una preferencia por los mangas que no tienen anime?")))
	(assert (prefiere-sin-anime-preguntado))
)

(defrule preguntas-usuario::prefiere-doujinshis
	(prefiere-sin-anime-preguntado)
	(not (prefiere-doujinshis-preguntado))
	?usr <- (usuario)
	=>
	(modify ?usr (quiere-doujinshis (pregunta-si-no "¿Tienes una preferencia por los doujinshis (mangas autopublicados)?")))
	(assert (prefiere-doujinshis-preguntado))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Modulo de abstraccion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reglas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Crea problema abstracto
(defrule abstraccion-problema::crea-problema
	(not (problema-abstracto))
    =>
    (assert (problema-abstracto))
)

; Edad
(defrule abstraccion-problema::edad-menos-12
	(not (edad-hecho))
    (usuario (edad ?e))
    (test (< ?e 12))
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (edad MENOS_12))
    (assert (edad-hecho))
)
(defrule abstraccion-problema::edad-12-mas
	(not (edad-hecho))
    (usuario (edad ?e))
    (test (and (> ?e 11) (< ?e 16)))
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (edad 12_O_MAS))
    (assert (edad-hecho))
)
(defrule abstraccion-problema::edad-16-mas
	(not (edad-hecho))
    (usuario (edad ?e))
    (test (and (> ?e 15) (< ?e 18)))
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (edad 16_O_MAS))
    (assert (edad-hecho))
)
(defrule abstraccion-problema::edad-18-mas
	(not (edad-hecho))
    (usuario (edad ?e))
    (test (> ?e 17))
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (edad 18_O_MAS))
    (assert (edad-hecho))
)

; Cantidad mangas leidos
(defrule abstraccion-problema::cantidad-mangas-pocos
	(not (cantidad-hecho))
    (usuario (mangas-leidos ?m))
    (test (eq ?m pocos))
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (mangas-leidos pocos))
	(assert (cantidad-hecho))
)
(defrule abstraccion-problema::cantidad-mangas-bastantes
	(not (cantidad-hecho))
    (usuario (mangas-leidos ?m))
    (test (eq ?m normal))
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (mangas-leidos bastantes))
    (assert (cantidad-hecho))
)
(defrule abstraccion-problema::cantidad-mangas-muchos
	(not (cantidad-hecho))
    (usuario (mangas-leidos ?m))
    (test (eq ?m muchos))
    ?usr <- (problema-abstracto)
    =>
    (modify ?usr (mangas-leidos muchos))
    (assert (cantidad-hecho))
)

; Generos preferibles
(defrule abstraccion-problema::generos-preferidos
	(not (preferencia-generos-hecho))
    ?usr <- (problema-abstracto (preferencia-generos $?absGen))
    (usuario (gusto-generos $?gen))
    =>
    (modify ?usr (preferencia-generos (create$ ?gen ?absGen)))
	(assert (preferencia-generos-hecho))
)

; Temas preferibles
(defrule abstraccion-problema::temas-preferidos
	(not (preferencia-temas-hecho))
    ?usr <- (problema-abstracto (preferencia-temas $?absTem))
    (usuario (gusto-temas $?tem))
    =>
    (modify ?usr (preferencia-temas (create$ ?tem ?absTem)))
	(assert (preferencia-temas-hecho))
)

; Prefiere acabados
(defrule abstraccion-problema::preferencia-acabados-true
	(not (preferencia-acabados-hecho))
	(usuario (prefiere-acabados TRUE))
	?usr <- (problema-abstracto)
	=>
	(modify ?usr (prefiere-acabados TRUE))
	(assert (preferencia-acabados-hecho))
)
; No prefiere acabados
(defrule abstraccion-problema::preferencia-acabados-false
	(not (preferencia-acabados-hecho))
	(usuario (prefiere-acabados FALSE))
	?usr <- (problema-abstracto)
	=>
	(modify ?usr (prefiere-acabados FALSE))
	(assert (preferencia-acabados-hecho))
)

; Prefiere sin anime
(defrule abstraccion-problema::preferencia-sin-anime-true
	(not (preferencia-sin-anime-hecho))
	(usuario (prefiere-sin-anime TRUE))
	?usr <- (problema-abstracto)
	=>
	(modify ?usr (prefiere-sin-anime TRUE))
	(assert (preferencia-sin-anime-hecho))
)
; No prefiere sin anime
(defrule abstraccion-problema::preferencia-sin-anime-false
	(not (preferencia-sin-anime-hecho))
	(usuario (prefiere-sin-anime FALSE))
	?usr <- (problema-abstracto)
	=>
	(modify ?usr (prefiere-sin-anime FALSE))
	(assert (preferencia-sin-anime-hecho))
)

; Quiere doujinshis
(defrule abstraccion-problema::quiere-doujinshis-true
	(not (quiere-doujinshis-hecho))
	(usuario (quiere-doujinshis TRUE))
	?usr <- (problema-abstracto)
	=>
	(modify ?usr (quiere-doujinshis TRUE))
	(assert (quiere-doujinshis-hecho))
)
; No quiere doujinshis
(defrule abstraccion-problema::quiere-doujinshis-false
	(not (quiere-doujinshis-hecho))
	(usuario (quiere-doujinshis FALSE))
	?usr <- (problema-abstracto)
	=>
	(modify ?usr (quiere-doujinshis FALSE))
	(assert (quiere-doujinshis-hecho))
)
; Violencia ok
(defrule abstraccion-problema::violencia-ok
	(not (violencia-hecho))
	(usuario (prefiere-no-violentos FALSE))
	?usr <- (problema-abstracto)
	=>
	(modify ?usr (no-violentos FALSE))
	(assert (violencia-hecho))
)
; Violencia no ok
(defrule abstraccion-problema::violencia-no-ok
	(not (violencia-hecho))
	(usuario (prefiere-no-violentos TRUE))
	?usr <- (problema-abstracto)
	=>
	(modify ?usr (no-violentos TRUE))
	(assert (violencia-hecho))
)


;;;;;;;;;;;;;;;;;;;;;;; Modulo de asociacion heuristica ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Control de reglas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Sobre la valoracion
(defglobal asociacion-heuristica ?*asoc_excelente* = 8.75)
(defglobal asociacion-heuristica ?*asoc_bueno* = 7.5)
(defglobal asociacion-heuristica ?*asoc_normal* = 6.5)
(defglobal asociacion-heuristica ?*asoc_malo* = 0.0)

; Sobre la popularidad
(defglobal asociacion-heuristica ?*asoc_extr_popular* = 10000000) ;10M ventas
(defglobal asociacion-heuristica ?*asoc_popular* = 1000000) ;1M ventas
(defglobal asociacion-heuristica ?*asoc_conocido* = 100000) ;100K ventas
(defglobal asociacion-heuristica ?*asoc_desconocido* = 0)

; Crea solucion abstracta
(defrule asociacion-heuristica::crea-solucion
	(not (solucion-abstracta))
    =>
    (assert (solucion-abstracta))
)

; Elimina instancias que no cumplan la edad mínima
(defrule asociacion-heuristica::elimina-mas-doce
    (declare (salience 10))
    (problema-abstracto (edad MENOS_12))
    ?m <- (object (is-a Manga) (restriccion-edad ?restr) (titulo ?t))
    (test (> ?restr 11))
    =>
	;(format t "Manga %s fuera" ?t)
	;(printout t crlf)
    (send ?m delete)
)
(defrule asociacion-heuristica::elimina-mas-dieciseis
    (declare (salience 10))
    (problema-abstracto (edad 12_O_MAS))
    ?m <- (object (is-a Manga) (restriccion-edad ?restr) (titulo ?t))
    (test (> ?restr 15))
    =>
	;(format t "Manga %s fuera" ?t)
	;(printout t crlf)
    (send ?m delete) 
)
(defrule asociacion-heuristica::elimina-mas-dieciocho
    (declare (salience 10))
    (problema-abstracto (edad 16_O_MAS))
    ?m <- (object (is-a Manga) (restriccion-edad ?restr) (titulo ?t))
    (test (> ?restr 17))
    =>
	;(format t "Manga %s fuera" ?t)
	;(printout t crlf)
    (send ?m delete)
)

; Si el usuario no quiere, hay que quitar material en general muy violento o perturbador
(defrule asociacion-heuristica::elimina-violencia-temas-turbios
	(declare (salience 8))
	(problema-abstracto (no-violentos TRUE))
	?m <- (object (is-a Manga) (restriccion-edad ?restr) (titulo ?t) (pertenece-a $?generos) (trata-de $?temas))
	(test (or (> ?restr 17)
			  (member$ [gore] $?temas)
			  (member$ [horror] $?generos)
			  (member$ [guerra] $?temas)
			  (member$ [venganza] $?temas)
			  (member$ [postapocaliptico] $?temas)
			  (member$ [tragedia] $?temas)
			  (member$ [vampiros] $?temas)
		  )
	)
	=>
	;(format t "Manga %s fuera" ?t)
	;(printout t crlf)
    (send ?m delete)
)

(defrule asociacion-heuristica::calcula-coincidencias
	(declare (salience 5))
	?m <- (object (is-a Manga) (pertenece-a $?generos) (trata-de $?temas) (titulo ?t))
	?usr <- (problema-abstracto (preferencia-generos $?pgeneros) (preferencia-temas $?ptemas))
	=>
	(bind ?count-gen 0)
	(bind ?count-nomatch 0)
	(progn$ (?gen ?generos)
		(if (member$ ?gen ?pgeneros) then
			(bind ?count-gen (+ ?count-gen 1))
		)
		(if (not (member$ ?gen ?pgeneros)) then
			(bind ?count-nomatch (+ ?count-nomatch 1))
		)
	)
	(bind ?count-tem 0)
	(progn$ (?tem ?temas)
		(if (member$ ?tem ?ptemas) then
			(bind ?count-tem (+ ?count-tem 1))
		)
		(if (not (member$ ?tem ?ptemas)) then
			(bind ?count-nomatch (+ ?count-nomatch 1))
		)
	)
	;(format t "%s gen %d tem %d" ?t ?count-gen ?count-tem)
	;(printout t crlf)
	(assert(datos-manga (manga ?t) (generos ?count-gen) (temas ?count-tem) (nomatch ?count-nomatch)))
)

;;;;;;;;;;;;;;;;;;;;;;; Reglas red ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Se llaman si las reglas comunes no consiguen recomendar 3 mangas. Puede pasar, especialmente
; si se dan minimos generos y temas con los que trabajar

; Recomienda matches de 2 muy bien valorados
(defrule asociacion-heuristica::regla-red-2-match-excel
	(declare (salience -10))
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem))
	?m <- (object (is-a Manga) (titulo ?t) (valoracion ?val))
	?usr <- (problema-abstracto)
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (< (length$ ?rec) 3))
	(test (not (member$ ?dat $?rec)))
	(test (> (+ ?match-gen ?match-tem) 1)) ; 2 matches
	(test (> ?val ?*asoc_excelente*)) ; Excelente
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por regla red" ?t)
	(printout t crlf)
)

; Recomienda matches de 2 bien valorados
(defrule asociacion-heuristica::regla-red-2-match-bueno
	(declare (salience -15))
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem))
	?m <- (object (is-a Manga) (titulo ?t) (valoracion ?val))
	?usr <- (problema-abstracto)
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (< (length$ ?rec) 3))
	(test (not (member$ ?dat $?rec)))
	(test (> (+ ?match-gen ?match-tem) 1)) ; 2 matches
	(test (> ?val ?*asoc_bueno*)) ; Bien valorado
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por regla red" ?t)
	(printout t crlf)
)

; Recomienda matches de 2 populares
(defrule asociacion-heuristica::regla-red-2-match-popular
	(declare (salience -15))
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem))
	?m <- (object (is-a Manga) (titulo ?t) (copias-vendidas ?copias))
	?usr <- (problema-abstracto)
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (< (length$ ?rec) 3))
	(test (not (member$ ?dat $?rec)))
	(test (> (+ ?match-gen ?match-tem) 1)) ; 2 matches
	(test (> ?copias ?*asoc_popular*)) ; popular
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por regla red" ?t)
	(printout t crlf)
)

; Recomienda matches de 1 muy bien valorados
(defrule asociacion-heuristica::regla-red-1-match-excel
	(declare (salience -20))
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem))
	?m <- (object (is-a Manga) (titulo ?t) (valoracion ?val))
	?usr <- (problema-abstracto)
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (< (length$ ?rec) 3))
	(test (not (member$ ?dat $?rec)))
	(test (> (+ ?match-gen ?match-tem) 0)) ; 1 match
	(test (> ?val ?*asoc_excelente*)) ; Excelente
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por regla red" ?t)
	(printout t crlf)
)

; Recomienda matches de 1 bien valorados
(defrule asociacion-heuristica::regla-red-1-match-bueno
	(declare (salience -25))
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem))
	?m <- (object (is-a Manga) (titulo ?t) (valoracion ?val))
	?usr <- (problema-abstracto)
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (< (length$ ?rec) 3))
	(test (not (member$ ?dat $?rec)))
	(test (> (+ ?match-gen ?match-tem) 0)) ; 1 match
	(test (> ?val ?*asoc_bueno*)) ; Bien valorado
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por regla red" ?t)
	(printout t crlf)
)

; Recomienda matches de 1 populares
(defrule asociacion-heuristica::regla-red-1-match-popular
	(declare (salience -25))
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem))
	?m <- (object (is-a Manga) (titulo ?t) (copias-vendidas ?copias))
	?usr <- (problema-abstracto)
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (< (length$ ?rec) 3))
	(test (not (member$ ?dat $?rec)))
	(test (> (+ ?match-gen ?match-tem) 0)) ; 1 match
	(test (> ?copias ?*asoc_popular*)) ; popular
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por regla red" ?t)
	(printout t crlf)
)


; Recomienda matches de 1
(defrule asociacion-heuristica::regla-red-1-match
	(declare (salience -40))
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem))
	?m <- (object (is-a Manga) (titulo ?t))
	?usr <- (problema-abstracto)
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (< (length$ ?rec) 3))
	(test (not (member$ ?dat $?rec)))
	(test (> (+ ?match-gen ?match-tem) 0)) ; 1 match
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por regla red" ?t)
	(printout t crlf)
)

; Recomienda cualquier manga bien valorado
(defrule asociacion-heuristica::regla-red-bueno
	(declare (salience -45))
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem))
	?m <- (object (is-a Manga) (titulo ?t) (valoracion ?val))
	?usr <- (problema-abstracto)
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (< (length$ ?rec) 3))
	(test (> ?val ?*asoc_bueno*))
	(test (not (member$ ?dat $?rec)))
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por regla red" ?t)
	(printout t crlf)
)

; Recomienda cualquier manga
(defrule asociacion-heuristica::regla-red
	(declare (salience -50))
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem))
	?m <- (object (is-a Manga) (titulo ?t))
	?usr <- (problema-abstracto)
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (< (length$ ?rec) 3))
	(test (not (member$ ?dat $?rec)))
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por regla red" ?t)
	(printout t crlf)
)

;;;;;;;;;;;;;;;;;;;;;;; Asociacion heuristica general (sin preferencias) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Si coinciden 3 o mas generos/temas y excelente valoracion --> recomendar
(defrule asociacion-heuristica::general-1match-excelente
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?usr <- (problema-abstracto (preferencia-generos $?pgeneros) (preferencia-temas $?ptemas))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (pertenece-a $?generos) (trata-de $?temas) (titulo ?t))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (not (member$ ?dat $?rec)))
	(test (> ?val ?*asoc_excelente*))
	(test (> (+ ?match-gen ?match-tem) 2)) ; 3 matches
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por 3 match val>excelente" ?t)
	(printout t crlf)
)

;Si match 3 o más <4nomatch y buena valoracion --> recomendar
(defrule asociacion-heuristica::general-3match-bueno
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?usr <- (problema-abstracto (preferencia-generos $?pgeneros) (preferencia-temas $?ptemas))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (pertenece-a $?generos) (trata-de $?temas) (titulo ?t))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (not (member$ ?dat $?rec)))
	(test (> ?val ?*asoc_bueno*))
	(test (> (+ ?match-gen ?match-tem) 2)) ; 3 matches
	(test (< ?nmatch 4)) ; 3 nomatches
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por 3match val>bueno <4nomatch" ?t)
	(printout t crlf)
)

;Si coinciden 4 o mas géneros/temas y valoracion normal --> recomendar
(defrule asociacion-heuristica::general-4match-normal
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?usr <- (problema-abstracto (preferencia-generos $?pgeneros) (preferencia-temas $?ptemas))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (pertenece-a $?generos) (trata-de $?temas) (titulo ?t))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (not (member$ ?dat $?rec)))
	(test (> ?val ?*asoc_normal*))
	(test (> (+ ?match-gen ?match-tem) 3)) ; 4 matches
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por 4match val normal>" ?t)
	(printout t crlf)
)

;Si coinciden 6 o mas generos/temas y mala valoracion --> recomendar
(defrule asociacion-heuristica::general-6match-malo
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?usr <- (problema-abstracto (preferencia-generos $?pgeneros) (preferencia-temas $?ptemas))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (pertenece-a $?generos) (trata-de $?temas) (titulo ?t))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (not (member$ ?dat $?rec)))
	(test (> ?val ?*asoc_malo*))
	(test (> (+ ?match-gen ?match-tem) 5)) ; 6 matches
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por 6 match" ?t)
	(printout t crlf)
)

;Si la persona lee pocos mangas, el manga es popular o mas, 
;coincide en al menos 2 generos/temas, menos de 3 nomatches y valoración normal o mejor --> recomendar
(defrule asociacion-heuristica::general-lee-pocos-popular
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?usr <- (problema-abstracto (preferencia-generos $?pgeneros) (preferencia-temas $?ptemas) (mangas-leidos pocos))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (pertenece-a $?generos) (trata-de $?temas) (titulo ?t))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (not (member$ ?dat $?rec)))
	(test (> ?copias ?*asoc_popular*))
	(test (> ?val ?*asoc_normal*))
	(test (> (+ ?match-gen ?match-tem) 1)) ; 2 matches
	(test (< ?nmatch 3)) ; 2 nomatches
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por leer pocos, manga popular>, 2 match valoracion normal> <3nomatches" ?t)
	(printout t crlf)
)

;Si la persona lee bastantes mangas, el manga es conocido,
;coincide al menos 2 genero/tema, menos 4nomatch y valoracion normal o mejor --> recomendar
(defrule asociacion-heuristica::general-lee-bastantes-conocido
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?usr <- (problema-abstracto (preferencia-generos $?pgeneros) (preferencia-temas $?ptemas) (mangas-leidos bastantes))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (pertenece-a $?generos) (trata-de $?temas) (titulo ?t))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (not (member$ ?dat $?rec)))
	(test (> ?copias ?*asoc_conocido*))
	(test (> ?val ?*asoc_normal*))
	(test (> (+ ?match-gen ?match-tem) 1)) ; 2 match
	(test (< ?nmatch 4)) ; 3 nomatches
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por leer bastantes, manga conocido, 2 match bueno <4nomatch" ?t)
	(printout t crlf)
)

;Si la persona lee muchos mangas, el manga es desconocido
;coincide al menos 2 genero/tema y valoracion normal o mejor --> recomendar
(defrule asociacion-heuristica::general-lee-muchos-desconocido-2match
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?usr <- (problema-abstracto (preferencia-generos $?pgeneros) (preferencia-temas $?ptemas) (mangas-leidos muchos))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (pertenece-a $?generos) (trata-de $?temas) (titulo ?t))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (not (member$ ?dat $?rec)))
	(test (and (> ?copias ?*asoc_desconocido*) (< ?copias ?*asoc_conocido*)))
	;(test (> ?copias ?*asoc_desconocido*))
	(test (> ?val ?*asoc_normal*))
	(test (> (+ ?match-gen ?match-tem) 1)) ; 2 matches
	(test (< ?nmatch 4)) ; 3 nomatches
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por leer muchos manga desconocido 2> matches <4 matches" ?t)
	(printout t crlf)
)

;Si la persona lee muchos mangas, el manga es conocido o mas
;coincide al menos 3 genero/tema y valoracion buena o mejor --> recomendar
(defrule asociacion-heuristica::general-lee-muchos-conocido-3match
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?usr <- (problema-abstracto (preferencia-generos $?pgeneros) (preferencia-temas $?ptemas) (mangas-leidos muchos))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (pertenece-a $?generos) (trata-de $?temas) (titulo ?t))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (not (member$ ?dat $?rec)))
	(test (> ?copias ?*asoc_conocido*))
	(test (> ?val ?*asoc_bueno*))
	(test (> (+ ?match-gen ?match-tem) 2)) ; 3 matches
	(test (< ?nmatch 4)) ; 3 nomatches
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por leer muchos manga conocido 3 matches bueno <4nomatch" ?t)
	(printout t crlf)
)


; valoracion buena algun match 0 nomatch --> recomendar
(defrule asociacion-heuristica::general-excelente-0nomatch
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?m <- (object (is-a Manga) (valoracion ?val) (titulo ?t))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (not (member$ ?dat $?rec)))
	(test (> ?val ?*asoc_bueno*))
	(test (> (+ ?match-gen ?match-tem) 0)) ; algun match
	(test (eq ?nmatch 0)) ; 0 nomatches
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por valoracion excelente 1>match 0 nomatch" ?t)
	(printout t crlf)
)


;;;;;;;;;;;;;;;;;;;;;;; Asociacion heuristica con preferencias ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; User prefiere sin anime, acabados y doujinshi
; Recomienda el que cumpla las 3, no malo, 1 match y <4nomatch 
(defrule asociacion-heuristica::3-preferencias
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (titulo ?t) (escrito-por ?autor) (publicado-por ?publ) (estado-publicacion ?epubl) (tiene-anime ?anime))
	?usr <- (problema-abstracto (prefiere-acabados TRUE) (prefiere-sin-anime TRUE) (quiere-doujinshis TRUE))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (eq (class ?publ) Autopublicador)) ; comprueba doujinshi
	(test (eq ?epubl "acabado")) ; comprueba acabado
	(test (eq ?anime FALSE)) ; comprueba anime
	(test (> (+ ?match-gen ?match-tem) 0)) ; 1 match
	(test (< ?nmatch 4)) ; 3 nomatches
	(test (> ?val ?*asoc_normal*)) ; No malo
	(test (not (member$ ?dat $?rec)))
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por 3pref 1>match <4nomatch" ?t)
	(printout t crlf)
)

; User prefiere sin anime, acabados
; Recomienda bueno, 1 match, <4nomatch
(defrule asociacion-heuristica::2-pref-sin-anime-acabado
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (titulo ?t) (tiene-anime ?anime) (estado-publicacion ?epubl))
	?usr <- (problema-abstracto (prefiere-acabados TRUE) (prefiere-sin-anime TRUE))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (eq ?epubl "acabado")) ; comprueba acabado
	(test (eq ?anime FALSE)) ; comprueba anime
	(test (> (+ ?match-gen ?match-tem) 0)) ; 1 match
	(test (< ?nmatch 4)) ; 3 nomatches
	(test (> ?val ?*asoc_bueno*)) ; Bueno
	(test (not (member$ ?dat $?rec)))
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por 2pref sin anime acabado 1>match <4nomatch" ?t)
	(printout t crlf)
)

; User prefiere acabados y doujinshi
; Recomienda bueno, 1 match, <4nomatch 
(defrule asociacion-heuristica::2-pref-acabado-doujinshi
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (titulo ?t) (escrito-por ?autor) (publicado-por ?publ) (estado-publicacion ?epubl))
	?usr <- (problema-abstracto (prefiere-acabados TRUE) (quiere-doujinshis TRUE))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (eq (class ?publ) Autopublicador)) ; comprueba doujinshi
	(test (eq ?epubl "acabado")) ; comprueba acabado
	(test (> (+ ?match-gen ?match-tem) 0)) ; 1 match
	(test (< ?nmatch 4)) ; 3 nomatches
	(test (> ?val ?*asoc_bueno*)) ; Bueno
	(test (not (member$ ?dat $?rec)))
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por 2pref acabado doujinshi 1>match <4nomatch" ?t)
	(printout t crlf)
)

; User prefiere sin anime
; Recomienda el que sea bueno, 2 matches <4nomatch
(defrule asociacion-heuristica::1-pref-sin-anime-matches
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (titulo ?t) (tiene-anime ?anime))
	?usr <- (problema-abstracto (prefiere-sin-anime TRUE))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (eq ?anime FALSE)) ; comprueba anime
	(test (> (+ ?match-gen ?match-tem) 1)) ; 2 matches
	(test (< ?nmatch 4)) ; 3 nomatches
	(test (> ?val ?*asoc_bueno*)) ; Bueno
	(test (not (member$ ?dat $?rec)))
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por sin anime 2 matches <4nomatch" ?t)
	(printout t crlf)
)

; User prefiere sin anime
; Recomienda el que sea bueno, popular, 1>matches <4nomatch
(defrule asociacion-heuristica::1-pref-sin-anime-popular
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (titulo ?t) (tiene-anime ?anime))
	?usr <- (problema-abstracto (prefiere-sin-anime TRUE))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (eq ?anime FALSE)) ; comprueba anime
	(test (> (+ ?match-gen ?match-tem) 0)) ; 1 matches
	(test (< ?nmatch 4)) ; 3 nomatches
	(test (> ?val ?*asoc_bueno*)) ; Bueno
	(test (> ?copias ?*asoc_popular*)) ; Popular
	(test (not (member$ ?dat $?rec)))
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por sin anime popular 1>matches <4nomatch" ?t)
	(printout t crlf)
)

; User prefiere acabados
; Recomienda el que sea bueno, 3 matches
(defrule asociacion-heuristica::1-pref-acabado-matches
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (titulo ?t) (estado-publicacion ?epubl))
	?usr <- (problema-abstracto (prefiere-acabados TRUE))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (eq ?epubl "acabado")) ; comprueba acabado
	(test (> (+ ?match-gen ?match-tem) 2)) ; 3 matches
	(test (> ?val ?*asoc_bueno*)) ; Bueno
	(test (not (member$ ?dat $?rec)))
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por acabado bueno 3>matches" ?t)
	(printout t crlf)
)

; User prefiere acabado
; Recomienda el que sea bueno, popular, 2 matches, <4nomatches
(defrule asociacion-heuristica::1-pref-acabado-popular
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (titulo ?t) (estado-publicacion ?epubl))
	?usr <- (problema-abstracto (prefiere-acabados TRUE))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (eq ?epubl "acabado")) ; comprueba acabado
	(test (> (+ ?match-gen ?match-tem) 1)) ; 2 matches
	(test (< ?nmatch 4)) ; 3 nomatches
	(test (> ?val ?*asoc_bueno*)) ; Bueno
	(test (> ?copias ?*asoc_popular*)) ; Popular
	(test (not (member$ ?dat $?rec)))
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por acabado 2>matches popular <4nomatch" ?t)
	(printout t crlf)
)

; User prefiere doujinshi
; Recomienda el que sea bueno, 3 matches <4nomatch
(defrule asociacion-heuristica::1-pref-doujinshi-matches
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (titulo ?t) (escrito-por ?autor) (publicado-por ?publ))
	?usr <- (problema-abstracto (quiere-doujinshis TRUE))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (eq (class ?publ) Autopublicador)) ; comprueba doujinshi
	(test (> (+ ?match-gen ?match-tem) 2)) ; 3 matches
	(test (< ?nmatch 4)) ; 3 nomatches
	(test (> ?val ?*asoc_bueno*)) ; Bueno
	(test (not (member$ ?dat $?rec)))
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por doujinshi 3>matches <4nomatch" ?t)
	(printout t crlf)
)

; User prefiere doujinshi
; Recomienda el que sea bueno, popular, 2 matches <4nomatch
(defrule asociacion-heuristica::1-pref-doujinshi-popular
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem) (nomatch ?nmatch))
	?m <- (object (is-a Manga) (valoracion ?val) (copias-vendidas ?copias) (titulo ?t) (escrito-por ?autor) (publicado-por ?publ))
	?usr <- (problema-abstracto (quiere-doujinshis TRUE))
	?sol <- (solucion-abstracta (recomendables $?rec))
	(test (eq (class ?publ) Autopublicador)) ; comprueba doujinshi
	(test (> (+ ?match-gen ?match-tem) 1)) ; 2 match
	(test (< ?nmatch 4)) ; 3 nomatches
	(test (> ?val ?*asoc_bueno*)) ; Bueno
	(test (> ?copias ?*asoc_popular*)) ; Popular
	(test (not (member$ ?dat $?rec)))
	=>
	(modify ?sol (recomendables $?rec ?dat))
	(format t "El manga %s entra por doujinshi popular 2>matches  <4nomatch" ?t)
	(printout t crlf)
)

;;;;;;;;;;;;;;;;;;;;;; Modulo de refinamiento de la solucion ;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Control de reglas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;; Coincidencia generos y temas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Criterio de refinamiento, devuelve uno que coincida bien

(deftemplate refinamiento-solucion::counter
   (slot num-recomendados (type INTEGER) (default 0))
)

(defrule refinamiento-solucion::crea-solucion-concreta
  (not (counter))
  =>
	(assert (counter))
)

; En orden, se miran numeros de coincidencias. Si una regla encuentra un candidato, fin

; 5 matches
(defrule refinamiento-solucion::coincidencia-5
  (declare (salience 8))
  (not (recomendacion-coincidencia))
	(solucion-abstracta (recomendables $?rec))
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem))
	(test (member$ ?dat ?rec))
  ?m <- (object (is-a Manga) (titulo ?t))
  (test (>= (+ ?match-gen ?match-tem) 5))
	?counter <- (counter (num-recomendados ?n-rec))
  =>
	(printout t "Recomendacion por compatibilidad de generos y temas: ")
	(send ?m print)
  (send ?m delete)
  (assert (recomendacion-coincidencia))
	(modify ?counter (num-recomendados (+ ?n-rec 1)))
)

; 4 matches
(defrule refinamiento-solucion::coincidencia-4
  (declare (salience 7))
  (not (recomendacion-coincidencia))
	(solucion-abstracta (recomendables $?rec))
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem))
	(test (member$ ?dat ?rec))
  ?m <- (object (is-a Manga) (titulo ?t))
  (test (>= (+ ?match-gen ?match-tem) 4))
	?counter <- (counter (num-recomendados ?n-rec))
  =>
	(printout t "Recomendacion por compatibilidad de generos y temas: ")
	(send ?m print)
  (send ?m delete)
  (assert (recomendacion-coincidencia))
	(modify ?counter (num-recomendados (+ ?n-rec 1)))
)

; 3 matches
(defrule refinamiento-solucion::coincidencia-3
  (declare (salience 6))
  (not (recomendacion-coincidencia))
	(solucion-abstracta (recomendables $?rec))
	?dat <- (datos-manga (manga ?t) (generos ?match-gen) (temas ?match-tem))
	(test (member$ ?dat ?rec))
  ?m <- (object (is-a Manga) (titulo ?t))
  (test (>= (+ ?match-gen ?match-tem) 3))
	?counter <- (counter (num-recomendados ?n-rec))
  =>
	(printout t "Recomendacion por compatibilidad de generos y temas: ")
	(send ?m print)
  (send ?m delete)
  (assert (recomendacion-coincidencia))
	(modify ?counter (num-recomendados (+ ?n-rec 1)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Valoraciones ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Criterio de refinamiento, se toma uno excelentemente valorado, y si no lo hay, uno bien valorado

(defrule refinamiento-solucion::valoracion-excelente
	(declare (salience 10))
	(not (recomendacion-valoracion))
	(solucion-abstracta (recomendables $?rec))
	?dat <- (datos-manga (manga ?t))
	(test (member$ ?dat ?rec))
  ?m <- (object (is-a Manga) (titulo ?t) (valoracion ?v))
  (test (>= ?v ?*asoc_excelente*))
	?counter <- (counter (num-recomendados ?n-rec))
  =>
	(printout t "Recomendacion con buena valoracion: ")
	(send ?m print)
  (send ?m delete)
  (assert (recomendacion-valoracion))
	(modify ?counter (num-recomendados (+ ?n-rec 1)))
)

(defrule refinamiento-solucion::valoracion-buena
	(declare (salience 9))
	(not (recomendacion-valoracion))
	(solucion-abstracta (recomendables $?rec))
	?dat <- (datos-manga (manga ?t))
	(test (member$ ?dat ?rec))
  ?m <- (object (is-a Manga) (titulo ?t) (valoracion ?v))
  (test (>= ?v ?*asoc_bueno*))
	?counter <- (counter (num-recomendados ?n-rec))
  =>
	(printout t "Recomendacion con buena valoracion: ")
	(send ?m print)
  (send ?m delete)
  (assert (recomendacion-valoracion))
	(modify ?counter (num-recomendados (+ ?n-rec 1)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Preferencias ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Criterio de refinamiento, se toma un manga que cumpla muchas preferencias
; Actualmente son 3, si cumple 2 se prioriza a si cumple solo una

; Recomienda cualquiera que cumpla dos de las preferencias
(defrule refinamiento-solucion::2preferencias
	(declare (salience 20))
	(not (recomendacion-preferencias))
	?abs <- (problema-abstracto (prefiere-sin-anime ?anime) (prefiere-acabados ?acab) (quiere-doujinshis ?douj))
	?dat <- (datos-manga (manga ?t))
	(solucion-abstracta (recomendables $?rec))
	(test (member$ ?dat ?rec))
    ?m <- (object (is-a Manga) (titulo ?t) (tiene-anime ?ta) (estado-publicacion ?ep) (escrito-por ?autor) (publicado-por ?publ))
	(test (or
				; No tiene anime y esta acabado
				(and (and(eq ?anime TRUE) (eq ?ta FALSE)) (and(eq ?acab TRUE) (eq ?ep "acabado")))
				; Esta acabado y es un doujinshi
			  (and (and(eq ?acab TRUE) (eq ?ep "acabado")) (and(eq ?douj TRUE) (eq (class ?publ) Autopublicador)))
				; No se mira sin anime y doujinshi porque ocurre siempre
		  )
	)
	?counter <- (counter (num-recomendados ?n-rec))
    =>
	(printout t "Recomendacion que cumple preferencias: ")
	(send ?m print)
    (send ?m delete)
    (assert (recomendacion-preferencias))
		(modify ?counter (num-recomendados (+ ?n-rec 1)))
)

; Recomienda cualquiera que cumpla alguna de las preferencias
(defrule refinamiento-solucion::preferencias
	(declare (salience 10))
	(not (recomendacion-preferencias))
	?abs <- (problema-abstracto (prefiere-sin-anime ?anime) (prefiere-acabados ?acab) (quiere-doujinshis ?douj))
	?dat <- (datos-manga (manga ?t))
	(solucion-abstracta (recomendables $?rec))
	(test (member$ ?dat ?rec))
    ?m <- (object (is-a Manga) (titulo ?t) (tiene-anime ?ta) (estado-publicacion ?ep) (escrito-por ?autor) (publicado-por ?publ))
	(test (or (and(eq ?anime TRUE) (eq ?ta FALSE))
			  (and(eq ?acab TRUE) (eq ?ep "acabado"))
			  (and(eq ?douj TRUE) (eq (class ?publ) Autopublicador))
		  )
	)
	?counter <- (counter (num-recomendados ?n-rec))
    =>
	(printout t "Recomendacion que cumple preferencias: ")
	(send ?m print)
    (send ?m delete)
    (assert (recomendacion-preferencias))
		(modify ?counter (num-recomendados (+ ?n-rec 1)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; General ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Criterio general, no siempre tienen por que cumplirse los 3 criterios definidos mas arriba

; Escoge uno cualquiera
(defrule refinamiento-solucion::preferencias-no-hay
	(not (recomendacion-preferencias))
	(solucion-abstracta (recomendables $?rec))
	?dat <- (datos-manga (manga ?t))
	(test (member$ ?dat ?rec))
    ?m <- (object (is-a Manga) (titulo ?t))
		?counter <- (counter (num-recomendados ?n-rec))
    =>
	(printout t "Recomendacion: ")
	(send ?m print)
    (send ?m delete)
    (assert (recomendacion-preferencias))
		(modify ?counter (num-recomendados (+ ?n-rec 1)))
)

; Escoge uno cualquiera
(defrule refinamiento-solucion::simplemente-recomendable
	(declare (salience -10))
	(solucion-abstracta (recomendables $?rec))
	?dat <- (datos-manga (manga ?t))
	(test (member$ ?dat ?rec))
  ?m <- (object (is-a Manga) (titulo ?t))

	?counter <- (counter (num-recomendados ?n-rec))
	(test (< ?n-rec 3))
  =>
	(printout t "Recomendacion: ")
	(send ?m print)
  (send ?m delete)
	(modify ?counter (num-recomendados (+ ?n-rec 1)))
)
