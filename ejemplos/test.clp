;; Para cargar hechos, (reset), para ejecutar reglas, (run)

;; Hechos iniciales
(deffacts initial-facts
    (padre juan pedro)
    (hijo alberto jose)
)

;; Reglas padre-hijo hijo-padre 
(defrule padre-hijo (padre ?x  ?y) => (assert(hijo ?y ?x)))
(defrule hijo-padre (hijo ?x ?y) => (assert(padre ?y ?x)))

;; Template

(deftemplate persona
    (slot nombre)
    (slot edad)
)

(deffacts gente
    (persona (nombre "terreneitor") (edad 221))
    (persona (nombre "amogus") (edad 2))
    (padre "terreneitor" "amogus")
)