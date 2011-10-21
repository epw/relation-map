(use "plot.def")

(node-type droid trapezium)
(rule uses-force-on orange solid forward)

(group "Rebel Alliance")

(label (character "Luke Skywalker") luke)

(has-member "Rebel Alliance" luke)

(character "Leia Organa")

(has-member "Rebel Alliance" "Leia Organa")

(droid "C3PO")
(droid "R2D2")

(owns "Leia Organa" "C3PO")
(owns "Leia Organa" "R2D2")

(allies "C3PO" "Luke Skywalker")
(allies "R2D2" "Luke Skywalker")

(group "Galactic Empire")

(character "Darth Vader")
(has-member "Galactic Empire" "Darth Vader")

(enemies "Luke Skywalker" "Darth Vader")
(enemies "Leia Organa" "Darth Vader")

(watching "R2D2" "Darth Vader")

(hates luke "Galactic Empire")

(new-section "Hutt Space")

(character "Jabba the Hutt")

(character "Boba Fett")

(character "Han Solo")

(allies "Han Solo" luke)

(uses-force-on "Darth Vader" luke)
