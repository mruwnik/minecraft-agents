(ns engine.far-spawn
  "Test worlds' far respawn point, shared by the fake world (its world spawn) and the world-test fixtures (:spawnpoint :far).")

(def blocks
  "Blocks east of the start a far respawn point lies: past the walk back that beats the despawn of a death's items
  (cost.value/fetch-cost, about 1300). The trial bodies' deaths that woke that far woke 2200 to 18500 blocks off."
  5000)
