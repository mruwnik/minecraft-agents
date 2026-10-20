(ns engine.senses-test
  "engine.senses: the sensing rules over what the primitives report raw (day and weather thresholds, line of sight from
  the raw world, the sleep preconditions), checked with a stub primitives object and a stub raw world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.fake.raw-world :as fake-raw]
            [engine.senses :as senses]
            [engine.test-util :as tu]))

(defn at [x y z] #js {:x x :y y :z z})

(defn raw-world
  "A raw world over {[x y z] :block|:fence|:pane}: state 1 blocks sight, 2 does not, -1 is not loaded (cells in unloaded)."
  [blocks & [unloaded]]
  (let [id (fn [x y z] (cond (contains? (set unloaded) [x y z]) -1 (= :block (get blocks [x y z])) 1 (contains? blocks [x y z]) 2 :else 0))]
    #js {:eye (fn [] (js/Object.assign (at 0.5 65.62 0.5) #js {:yaw (- (/ js/Math.PI 2)) :pitch 0}))
         :stateAt id
         :sightTable (fn [] (js/Uint8Array. #js [0 1 0]))
         :shapesAt (fn [x y z]
                     (case (get blocks [x y z])
                       :block #js [#js [0 0 0 1 1 1]]
                       :fence #js [#js [0.375 0 0.375 0.625 1.5 0.625]]
                       (let [b (get blocks [x y z])] (if (vector? b) (clj->js b) #js []))))}))

(defn ent [id name kind x & [extra]]
  (js/Object.assign #js {:id id :name name :kind kind :pos (at x 64 0.5) :distance (- x 0.5) :height 1.8} (clj->js extra)))

(defn stub
  "Raw primitives: self fields, the entity list entities answers, the args sleep and the body listener were given."
  [{:keys [self entities blocks unloaded names]}]
  (let [seen (atom {})
        listener (atom nil)]
    {:seen seen
     :listener listener
     :p #js {:self (fn [] (clj->js self))
             :entities (fn [a] (swap! seen assoc :entities-args a)
                           (into-array (filter #(or (nil? (.-kind a)) (= (.-kind a) (.-kind %))) entities)))
             :sleep (fn [token a] (swap! seen assoc :sleep a) (js/Promise.resolve #js {:status "sleeping"}))
             :useOn (fn [token a] (swap! seen assoc :use-on a) (js/Promise.resolve #js {:status "used"}))
             :blockAt (fn [pos] #js {:name (get names [(.-x pos) (.-y pos) (.-z pos)] "dirt")})
             :onBodyEvent (fn [l] (reset! listener l) (fn []))
             :rawWorld (raw-world blocks unloaded)}}))

(defn wrapped [spec] (let [s (stub spec)] (assoc s :w (senses/wrap (:p s) (.-rawWorld ^js (:p s))))))

(deftest day-and-weather-thresholds
  (are [t day] (= day (senses/day-at? t))
    0 true 12541 true 12542 false 18000 false 23460 false 23461 true)
  (are [rain thunder raining thundering] (= {:raining raining :thundering thundering} (senses/weather-of rain thunder))
    0 0 false false
    0.2 0 false false
    0.5 0 true false
    1 1 true true
    0.1 1 false false))

(deftest self-derives-day-and-weather
  (are [t rain thunder out] (let [s (.self ^js (:w (wrapped {:self {:timeOfDay t :rainState rain :thunderState thunder :food 20}})))]
                               (= out [(.-isDay s) (.-raining s) (.-thundering s) (.-food s) (undefined? (.-rainState s)) (undefined? (.-thunderState s))]))
    6000 0 0 [true false false 20 true true]
    14000 0.5 0.95 [false true true 20 true true]
    23500 0.15 1 [true false false 20 true true])
  (is (= "offline" (.-status (.self ^js (:w (wrapped {:self {:status "offline"}})))))))

(deftest entities-see-through-the-raw-world
  (let [es [(ent 1 "zombie" "hostile" 5.5) (ent 2 "cow" "passive" 5.5) (ent 3 "item" "item" 5.5) (ent 4 "P1" "player" 5.5 {:lyingDown true})]
        wall {[2 64 0] :block [2 65 0] :block}
        out (fn [blocks] (->> (.entities ^js (:w (wrapped {:entities es :blocks blocks})) #js {})
                              (mapv (fn [^js e] [(.-id e) (.-visible e) (.-sleeping e) (.-hittable e) (.-lyingDown e) (.-height e)]))))]
    (is (= [[1 false nil false nil nil] [3 false nil nil nil nil] [4 false false false nil nil]] (out wall))
        "behind a wall the cow is not listed, the rest read not visible, a sleeper there does not count as asleep")
    (is (= [[1 true nil true nil nil] [2 nil nil true nil nil] [3 true nil nil nil nil] [4 true true true nil nil]] (out {}))
        "in the open all are listed; hostile, item and player carry visible; the raw lyingDown and height are not passed on")))

(deftest a-sight-answer-is-kept-until-the-epoch-or-eye-moves
  (let [reads (atom 0) epoch (atom 0) eye (atom (at 0.5 65.62 0.5)) walls (atom {})
        raw (doto (raw-world {})
              (aset "eye" (fn [] @eye))
              (aset "epoch" (fn [] @epoch))
              (aset "stateAt" (fn [x y z] (swap! reads inc) (if (@walls [x y z]) 1 0))))
        mob (ent 1 "zombie" "hostile" 5.5)
        sees (fn [] (senses/can-see? raw @eye mob))
        read-count (fn [] (let [r0 @reads] (sees) (- @reads r0)))]
    (is (true? (sees)))
    (is (zero? (read-count)) "asked again with nothing moved: answered from the kept one, no cell read")
    (reset! walls {[2 64 0] true [2 65 0] true [2 66 0] true})
    (is (true? (sees)) "the epoch has not moved: still the kept answer")
    (swap! epoch inc)
    (is (false? (sees)) "a world change moves the epoch: the line is drawn again")
    (reset! walls {})
    (reset! eye (at 0.5 65.62 1.5))
    (is (true? (sees)) "the eye moved: drawn again")))

(deftest entities-hittable-follows-shapes
  (let [hostile (ent 9 "zombie" "hostile" 5.5)
        hit (fn [blocks] (.-hittable ^js (first (.entities ^js (:w (wrapped {:entities [hostile] :blocks blocks})) #js {:kind "hostile"}))))]
    (is (= false (hit {[2 64 0] :block [2 65 0] :block [2 66 0] :block})))
    (is (= true (hit {[2 64 0] :fence})))
    (is (= true (hit {})))
    (is (nil? (.-hittable ^js (first (.entities ^js (:w (wrapped {:entities [(ent 9 "zombie" "hostile" 9.5)]})) #js {})))))))

(deftest entities-max-applies-after-the-sight-filter
  (let [es (concat [(ent 1 "cow" "passive" 3.5)] (map #(ent (+ 10 %) "sheep" "passive" (+ 4.5 %)) (range 3)))
        w (:w (wrapped {:entities es :blocks {[1 64 0] :block [1 65 0] :block}}))]
    (is (= [] (vec (.entities ^js w #js {:max 2}))) "all behind the wall: nothing listed")
    (let [w (:w (wrapped {:entities es}))]
      (is (= [1 10] (mapv #(.-id ^js %) (.entities ^js w #js {:max 2})))))))

(deftest a-body-senses-nothing-passive-behind-it
  (let [es [(ent 1 "cow" "passive" 3.5) (ent 2 "cow" "passive" -2.5) (ent 3 "villager" "villager" -2.5)
            (ent 4 "item" "item" 3.5) (ent 5 "item" "item" -2.5) (ent 6 "zombie" "hostile" -2.5)]
        listed (vec (.entities ^js (:w (wrapped {:entities es})) #js {}))]
    (is (= [1 4 5 6] (mapv #(.-id ^js %) listed)) "the cow and villager behind are dropped, items and hostiles stay")
    (is (= [true false true] (mapv #(.-visible ^js %) (rest listed))) "an item behind is listed but not visible; a hostile keeps its line-only flag")
    (is (= [2 3] (mapv #(.-id ^js %) (.unseenPassives ^js (:w (wrapped {:entities es})) #js {}))) "what is behind is the unseen input of hearing")))

(deftest sleep-preconditions-come-from-the-senses
  (let [flags (fn [self entities]
                (let [{:keys [p seen] :as s} (stub {:self self :entities entities})
                      w (senses/wrap p (.-rawWorld p))]
                  (.sleep ^js w "t" #js {:pos (at 1 64 1)})
                  (js->clj (:sleep @seen) :keywordize-keys true)))]
    (is (= {:notNight true} (select-keys (flags {:timeOfDay 6000} []) [:notNight])))
    (is (not (:notNight (flags {:timeOfDay 14000} []))))
    (is (:monstersNear (flags {:timeOfDay 14000 :pos {:x 0.5 :y 64 :z 0.5}} [(ent 1 "zombie" "hostile" 5.5)])))
    (is (not (:monstersNear (flags {:timeOfDay 14000 :pos {:x 0.5 :y 64 :z 0.5}} [(ent 1 "zombie" "hostile" 12.5)]))))
    (is (not (:monstersNear (flags {:timeOfDay 14000 :pos {:x 0.5 :y 64 :z 0.5}} [(ent 1 "cow" "passive" 2.5)]))))))

(deftest use-on-line-comes-from-the-senses
  (let [no-line (fn [blocks]
                  (let [{:keys [p seen]} (stub {:self {:timeOfDay 6000} :blocks blocks})
                        w (senses/wrap p (.-rawWorld p))]
                    (.useOn ^js w "t" #js {:pos (at 3 64 0) :item "diamond_hoe"})
                    (.-noLine ^js (:use-on @seen))))
        wall {[1 64 0] :block [1 65 0] :block [2 64 0] :block [2 65 0] :block}]
    (is (= true (no-line wall)) "stone between the eye and the block: no line")
    (is (= false (no-line {})) "open air: a line")
    (is (= false (no-line {[4 64 0] :block})) "a block behind the target does not block the line")))

(deftest use-on-line-over-real-shapes
  (let [no-line (fn [target names blocks]
                  (let [{:keys [p seen]} (stub {:self {:timeOfDay 6000} :blocks blocks :names names})
                        w (senses/wrap p (.-rawWorld p))]
                    (.useOn ^js w "t" #js {:pos (apply at target)})
                    (.-noLine ^js (:use-on @seen))))
        full [[0 0 0 1 1 1]]
        leaf [[0 0 0.4 1 1 0.6]]
        gate [[0 0 0.375 1 1.5 0.625]]
        around (fn [[x y z]] (for [[dx dy dz] [[-1 0 0] [-1 1 0] [-1 -1 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1] [1 0 0]]]
                               [(+ x dx) (+ y dy) (+ z dz)]))]
    (is (= false (no-line [1 64 0] {[1 64 0] "oak_door" [1 65 0] "oak_door"} {[1 64 0] leaf [1 65 0] leaf}))
        "a door lower half: its closed upper half (the partner) never blocks")
    (is (= false (no-line [1 64 0] {} {[1 64 0] gate [1 64 1] :block [1 65 1] :block [2 64 0] :block}))
        "a gate beside a wall is clicked from the open side")
    (is (= true (no-line [3 64 0] {} (into {[3 64 0] gate} (map (fn [c] [c :block]) (around [3 64 0])))))
        "a gate walled in on every side has no line")
    (is (= true (no-line [3 64 0] {} {[2 64 0] full [2 65 0] full}))
        "glass-like full boxes between the eye and a dirt block hide it")))

(deftest weather-event-fires-when-raining-or-thundering-flips
  (let [{:keys [p listener]} (stub {:self {:timeOfDay 6000 :rainState 0 :thunderState 0}})
        w (senses/wrap p (.-rawWorld p))
        got (atom [])]
    (.onBodyEvent ^js w (fn [e] (swap! got conj (js->clj e :keywordize-keys true))))
    (doseq [[rain thunder] [[0.1 0] [0.5 0] [0.6 0] [0.6 1] [0.1 1] [0 0]]]
      (@listener #js {:kind "weather-levels" :rain rain :thunder thunder}))
    (@listener #js {:kind "chat" :from "x" :message "hi"})
    (is (= [{:kind "weather-changed" :raining true :thundering false}
            {:kind "weather-changed" :raining true :thundering true}
            {:kind "weather-changed" :raining false :thundering false}
            {:kind "chat" :from "x" :message "hi"}]
           @got))))

(deftest the-real-sight-table-decides-what-blocks-the-eye
  (let [world (fn [blocks] {:blocks blocks :yaw 270 :self {:pos [0 64 0]}})
        raw (fn [blocks] (let [w (world blocks)]
                           #js {:eye (fn [] (fake-raw/eye w))
                                :stateAt (fn [x y z] (fake-raw/state-at-slow w x y z))
                                :sightTable (fn [] @fake-raw/sight)
                                :stateInfo (fn [id] (@fake-raw/info id))
                                :shapesAt (fn [& _] #js [])}))
        cow (ent 11 "cow" "passive" 5.5 {:height 1.4})
        listed (fn [block] (let [w (senses/wrap (:p (stub {:entities [cow]})) (raw (into {} (for [x [2 3] y [64 65]] [[x y 0] block]))))]
                             (count (.entities ^js w #js {}))))]
    (is (= [1 1 1 1 1 0] (mapv listed ["oak_fence" "oak_fence_gate" "iron_bars" "glass" "red_bed" "stone"]))
        "a player sees past rails, glass and beds; a full stone hides the cow")
    (is (= [1 0] (mapv listed ["oak_trapdoor" "oak_slab"]))
        "a shut trapdoor lying at the cell's bottom is passed over; a slab as high as the line is not")))

(deftest an-entity-is-seen-when-any-point-of-its-box-is
  (let [listed (fn [e blocks] (mapv #(.-id ^js %) (.entities ^js (:w (wrapped {:entities [e] :blocks blocks})) #js {})))
        boat (js/Object.assign (ent 5 "oak_boat" "passive" 5.5 {:height 0.5625 :width 1.375}) #js {:pos (at 5.5 63 0.5)})
        cow (js/Object.assign (ent 6 "cow" "passive" 5.5 {:height 1 :width 1.375}) #js {:pos (at 5.5 64 0.5)})
        column (into {} (for [y (range 63 68)] [[4 y 0] :block]))]
    (is (= [5] (listed boat {[3 63 0] :block})) "a bank block hides the boat's middle, not its top")
    (is (= [] (listed boat {[3 63 0] :block [3 64 0] :block})) "a two-high bank hides the whole boat")
    (is (= [6] (listed cow column)) "a column hides the middle and the head; a corner of the top face shows")
    (is (= [] (listed (js/Object.assign cow #js {:width js/undefined}) column)) "no width: only the middle and the head")))
