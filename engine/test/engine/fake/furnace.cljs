(ns engine.fake.furnace
  "The fake world's furnace, blast furnace and smoker as pure functions over world data
  {:blocks {pos name} :states {pos props} :self {:pos} :inventory [stack] :furnaces {pos furnace}}, with the real rates
  (a furnace takes 200 ticks an item, the other two 100), fuel that burns for its real time and one stack of input,
  fuel and output. Time passes only when (advance w ticks) says so. (furnace w args) answers [world' result], the result
  in the shape of js/furnace.mjs. Test-only; the same rules as js/fake-furnace.mjs, which stays live until the fake
  itself moves to cljs."
  (:require [engine.fake.pockets :as pockets]))

(def kinds #{"furnace" "blast_furnace" "smoker"})
(def reach 4.5)
(def slot-max 64)

(def ores {"raw_iron" "iron_ingot" "raw_gold" "gold_ingot" "raw_copper" "copper_ingot"
           "iron_ore" "iron_ingot" "gold_ore" "gold_ingot" "copper_ore" "copper_ingot"})
(def foods {"beef" "cooked_beef" "porkchop" "cooked_porkchop" "chicken" "cooked_chicken" "mutton" "cooked_mutton"
            "rabbit" "cooked_rabbit" "cod" "cooked_cod" "salmon" "cooked_salmon" "potato" "baked_potato"
            "kelp" "dried_kelp"})
(def blocks {"cobblestone" "stone" "sand" "glass" "clay_ball" "brick" "oak_log" "charcoal"})
(def recipes {"furnace" (merge ores foods blocks) "blast_furnace" ores "smoker" foods})
(def cook-ticks {"furnace" 200 "blast_furnace" 100 "smoker" 100})
;; a blast furnace and a smoker burn fuel twice as fast as they cook twice as fast: an item costs the same fuel in all three
(def burn-rate {"furnace" 1 "blast_furnace" 2 "smoker" 2})
(def fuel-ticks {"coal" 1600 "charcoal" 1600 "coal_block" 16000 "blaze_rod" 2400 "oak_planks" 300 "oak_log" 300 "stick" 100})

(def empty-furnace {:input nil :fuel nil :output nil :burn 0 :burn-total 0 :cook 0})

(defn stack-of [i] (when i {:name (:name i) :count (:count i)}))
(defn set-lit [w pos lit] (assoc-in w [:states pos :lit] lit))

(defn tick
  "One tick of vanilla's furnace: burn down, light from the fuel slot when there is something to cook, cook, finish an item."
  [kind f]
  (let [recipe (get-in recipes [kind (:name (:input f))])
        room (and recipe (or (nil? (:output f))
                             (and (= recipe (:name (:output f))) (< (:count (:output f)) slot-max))))
        f (update f :burn #(max 0 (dec %)))
        light? (and (zero? (:burn f)) room (:fuel f) (contains? fuel-ticks (:name (:fuel f))))
        f (if-not light?
            f
            (let [ticks (/ (fuel-ticks (:name (:fuel f))) (burn-rate kind))
                  left (dec (:count (:fuel f)))]
              (assoc f :burn ticks :burn-total ticks :fuel (when (pos? left) (assoc (:fuel f) :count left)))))]
    (cond
      (and (pos? (:burn f)) room)
      (let [cook (inc (:cook f))]
        (if (< cook (cook-ticks kind))
          (assoc f :cook cook)
          (let [left (dec (:count (:input f)))]
            (assoc f :cook 0
                   :output {:name recipe :count (inc (get (:output f) :count 0))}
                   :input (when (pos? left) (assoc (:input f) :count left))))))
      (not room) (assoc f :cook 0)
      :else f)))

(defn start
  "The world with its furnaces set up: the stacks a test starts them with, and every furnace block unlit."
  [w]
  (let [w (assoc w :furnaces (into {} (map (fn [[pos f]] [pos (merge empty-furnace f)])) (:furnaces w)))]
    (reduce (fn [w [pos name]] (if (kinds name) (set-lit w pos false) w)) w (:blocks w))))

(defn advance
  "Let ticks pass for every furnace; one whose block is gone is forgotten."
  [w ticks]
  (reduce (fn [w [pos f]]
            (let [kind (get-in w [:blocks pos])]
              (if-not (kinds kind)
                (update w :furnaces dissoc pos)
                (let [f' (nth (iterate #(tick kind %) f) ticks)]
                  (-> w (assoc-in [:furnaces pos] f') (set-lit pos (pos? (:burn f'))))))))
          w (:furnaces w)))

(defn slots-of [kind f]
  {:kind kind
   :input (stack-of (:input f))
   :fuel (stack-of (:fuel f))
   :output (stack-of (:output f))
   :lit (pos? (:burn f))
   :burn {:left (:burn f) :total (:burn-total f)}
   :cook {:done (:cook f) :total (cook-ticks kind)}})

(defn accepted?
  "As on the server: the input slot takes anything (a smoker holds iron and never cooks it), the fuel slot only fuel."
  [slot item]
  (or (= slot "input") (contains? fuel-ticks item)))

(defn load-slots [w pos kind f a]
  (let [wanted (for [slot ["input" "fuel"] :when (get a (keyword slot))] [slot (get a (keyword slot))])
        refusal (some (fn [[slot {:keys [item]}]]
                        (cond
                          (zero? (pockets/carried w item)) {:status "no-item" :slot slot :item item}
                          (and (get f (keyword slot)) (not= item (:name (get f (keyword slot)))))
                          {:status "busy" :slot slot :holds (stack-of (get f (keyword slot)))}))
                      wanted)]
    (if refusal
      [w refusal]
      (let [step (fn [[w f moved] [slot {:keys [item count]}]]
                   (let [k (keyword slot)
                         held (get-in f [k :count] 0)
                         ok? (accepted? slot item)
                         n (if ok? (min (or count ##Inf) (pockets/carried w item) (- slot-max held)) 0)]
                     (if (zero? n)
                       (reduced {:refused {:status "rejected" :slot slot :item item
                                           :reason (if (and (>= held slot-max) ok?) "slot-full" "not-accepted")}
                                 :world w :furnace f})
                       [(pockets/take-from w item n) (assoc f k {:name item :count (+ held n)}) (assoc moved k n)])))
            r (reduce step [w f {}] wanted)]
        (if (:refused r)
          [(assoc-in (:world r) [:furnaces pos] (:furnace r)) (:refused r)]
          (let [[w f moved] r]
            [(assoc-in w [:furnaces pos] f) (merge {:status "ok" :moved moved} (slots-of kind f))]))))))

(defn take-slots [w pos kind f a]
  (let [parts (filter #(if (= % "output") (not= false (:output a)) (get a (keyword %))) ["output" "input" "fuel"])
        [w f taken stuck] (reduce (fn [[w f taken stuck] part]
                                    (let [held (get f (keyword part))]
                                      (cond
                                        (nil? held) [w f taken stuck]
                                        (not (pockets/has-room? w (:name held))) [w f taken true]
                                        :else [(pockets/give w (:name held) (:count held)) (assoc f (keyword part) nil)
                                               (conj taken {:part part :name (:name held) :count (:count held)}) stuck])))
                                  [w f [] false] parts)]
    [(assoc-in w [:furnaces pos] f) (merge {:status (if stuck "full" "ok") :taken taken} (slots-of kind f))]))

(defn furnace
  "The furnace primitive: args {:pos [x y z] :op \"read\"|\"load\"|\"take\" :input {:item :count} :fuel {...}}."
  [w {:keys [pos op] :as a}]
  (let [block (get-in w [:blocks pos])
        distance (when pos (pockets/distance (get-in w [:self :pos]) pos))]
    (cond
      (or (nil? block) (= block "air")) [w {:status "missing"}]
      (not (kinds block)) [w {:status "cannot" :reason "not-a-furnace"}]
      (> distance reach) [w {:status "unreachable" :reason "too-far" :distance (pockets/round2 distance)}]
      :else
      (let [f (get-in w [:furnaces pos] empty-furnace)
            w (assoc-in w [:furnaces pos] f)]
        (case op
          "read" [w (merge {:status "ok"} (slots-of block f))]
          "load" (load-slots w pos block f a)
          (take-slots w pos block f a))))))
