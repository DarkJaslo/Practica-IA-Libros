;; Hechos iniciales
(deffacts initial-facts
    (padre juan pedro)
    (hijo alberto jose)
)

;; Reglas padre-hijo hijo-padre 
(defrule padre-hijo (padre ?x  ?y) => (assert(hijo ?y ?x)))
(defrule hijo-padre (hijo ?x ?y) => (assert(padre ?y ?x)))