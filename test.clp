;; Facts
(assert (padre juan pedro))

;; Rules
(defrule padre-hijo
    (padre ?x ?y)
    =>
    (assert (hijo ?y ?x))
)

(defrule hijo-padre (hijo ?x ?y) => (assert (padre ?y ?x)) )