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
         (inicio-publicacion  "1997-07-22")
         (metodo-distribucion  "ambos")
         (precio  "pago")
         (tiene-anime  "true")
         (valoracion  9.22)
         (genero  "Accion" "Aventura")
         (tema  "Super poderes")
         (titulo  "One Piece")
         (restriccion-edad 12)
    )

    ([shueisha] of Editorial
         (nombre  "Shueisha")
    )

)