(use "plot.def")

(group "Rebel Alliance")

(label (character "Luke Skywalker") luke)

(has-member "Rebel Alliance" luke)

(character "Leia Organa")

(has-member "Rebel Alliance" "Leia Organa")

(character "C3PO")
(character "R2D2")

(owns "Leia Organa" "C3PO")
(owns "Leia Organa" "R2D2")

(allies "C3PO" "Luke Skywalker")
(allies "R2D2" "Luke Skywalker")

(new-section "Emperor's Control")

(group "Galactic Empire")

(character "Darth Vader")
(has-member "Galactic Empire" "Darth Vader")

(enemies "Luke Skywalker" "Darth Vader")
(enemies "Leia Organa" "Darth Vader")

(watching "R2D2" "Darth Vader")

(hates luke "Galactic Empire")
