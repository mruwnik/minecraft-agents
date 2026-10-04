(ns engine.fake.pockets
  "The pockets of the fake world: :inventory is a vector of stacks {:name :count (:enchants)}, 36 slots of 64. Pure
  functions over world data, shared by the furnace, enchant and trade mechanics.")

(def slots 36)
(def stack-max 64)

(defn carried [w name]
  (transduce (comp (filter #(= name (:name %))) (map :count)) + 0 (:inventory w)))

(defn has-room?
  "Whether one more of name can go in: a stack of it is carried already, or a slot is free."
  [w name]
  (boolean (or (some #(= name (:name %)) (:inventory w)) (< (count (:inventory w)) slots))))

(defn take-from
  "Remove count of name, from the first stacks carried; emptied stacks go."
  [w name count]
  (let [[_ inv] (reduce (fn [[owed inv] stack]
                          (if (= name (:name stack))
                            (let [n (min owed (:count stack))]
                              [(- owed n) (conj inv (update stack :count - n))])
                            [owed (conj inv stack)]))
                        [count []] (:inventory w))]
    (assoc w :inventory (filterv #(pos? (:count %)) inv))))

(defn give
  "Add count of name: fill the unfilled stacks carried first, then new stacks of up to 64."
  [w name count]
  (let [[rest inv] (reduce (fn [[rest inv] stack]
                             (if (and (= name (:name stack)) (< (:count stack) stack-max))
                               (let [n (min rest (- stack-max (:count stack)))]
                                 [(- rest n) (conj inv (update stack :count + n))])
                               [rest (conj inv stack)]))
                           [count []] (:inventory w))
        fresh (map #(hash-map :name name :count (min % stack-max))
                   (take-while pos? (iterate #(- % stack-max) rest)))]
    (assoc w :inventory (into inv fresh))))

(defn distance [[ax ay az] [bx by bz]]
  (Math/sqrt (+ (* (- ax bx) (- ax bx)) (* (- ay by) (- ay by)) (* (- az bz) (- az bz)))))

(defn round2 [d] (/ (Math/round (* d 100)) 100))

(defn bad-args [message] (ex-info message {:code "bad-args"}))
