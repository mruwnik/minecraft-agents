(ns engine.look-support
  "Fake-world helpers for a body that has not looked round yet.")

(defn entities-after-look
  "p that lists no entity until its first look call (a look around), then the world's."
  [p]
  (let [entities (.-entities p)
        looked? #(some (fn [call] (= "look" (.-name call))) (.-calls (.-world p)))]
    (aset p "entities" (fn [q] (if (looked?) (.call entities p q) #js [])))
    p))
