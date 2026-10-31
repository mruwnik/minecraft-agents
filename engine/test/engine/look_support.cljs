(ns engine.look-support
  "Fake-world helpers for a body that has not looked round yet.")

(defn entities-after-look
  "p that lists no entity until its first look call (a look around), then the world's."
  [p]
  (let [entities (.-entities p)
        looked? #(some (fn [call] (= "look" (.-name call))) (.-calls (.-world p)))]
    (aset p "entities" (fn [q] (if (looked?) (.call entities p q) #js [])))
    p))

(defn entities-during-look
  "p that lists entities only while its n-th look call is the latest: the view cone holds them for that one look."
  [p n]
  (let [entities (.-entities p)
        looks #(count (filter (fn [call] (= "look" (.-name call))) (.-calls (.-world p))))]
    (aset p "entities" (fn [q] (if (= n (looks)) (.call entities p q) #js [])))
    p))
