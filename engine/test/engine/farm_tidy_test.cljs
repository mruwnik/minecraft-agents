(ns engine.farm-tidy-test
  "jobs.farm.tidy: which blocks over a plan are strays (pure), and whole runs against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]
            [jobs.farm.tidy :as tidy]
            [plan.shape :as shape]))

(def job 'jobs.farm.tidy)

(def clock (atom 1000000))

(defn block-at [p x y z] (some-> (.blockAt p #js {:x x :y y :z z}) (.-name)))
(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn dug [p] (set (map #(let [pos (.-pos (.-args %))] [(.-x pos) (.-y pos) (.-z pos)]) (calls p "dig"))))
(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn start
  "An engine over the fake world spec with the plans {id plan} and the zones (nil: never read) as its world data."
  ([spec plans] (start spec plans []))
  ([spec plans zones] (start spec plans zones registry/jobs))
  ([spec plans zones jobs]
   (let [[seen sink] (tu/capture-sink)
         p (tu/fake-on-floor spec)
         w (world/of-data plans {} zones)
         eng (core/create {:primitives p :jobs jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})
                           :world w})]
     {:eng eng :p p :seen seen :w w})))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (swap! clock + 700)
          (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async outcome
  "Run job with args as the child of a recording parent until the list is empty, at most n ticks; the child's result."
  [eng args n]
  (let [out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out))

;; ---------------------------------------------------------------- the field
;; farmland at y 63 over x 2..5, z 2..3 (a water cell at 6 63 2), wheat at y 64 over the same, a wanted torch at
;; 6 64 2, and a row at z 4 the plan wants clear.

(def field-plan
  {:id "field"
   :parts [{:id "ground" :box [[2 63 2] [5 63 3]] :want "farmland"}
           {:id "water" :cells [[6 63 2]] :want "water"}
           {:id "wheat" :box [[2 64 2] [5 64 3]] :want {:crop "wheat"}}
           {:id "light" :cells [[6 64 2]] :want "torch"}
           {:id "path" :box [[2 64 4] [5 64 4]] :want :clear}]})

(defn put [& cells] (into {} (map (fn [[x y z n]] [(str x "," y "," z) n])) cells))

(def tidy-field
  "The field with a stray of each kind: a weed, a cobblestone and a sapling in crop cells, leaves in the air above one,
  dirt in the clear row; and what is left alone: a chest and a torch standing in the field, a carrot among the
  wheat, dirt where farmland is wanted, the wanted torch, grass outside the plan."
  (merge (into {} (for [x (range 2 6) z [2 3]] [(str x ",63," z) "farmland"]))
         (into {} (for [x (range 2 6) z [2 3]] [(str x ",64," z) "wheat"]))
         (put [6 63 2 "water"] [6 64 2 "torch"]
              [2 64 2 "short_grass"] [3 64 2 "cobblestone"] [4 64 3 "oak_sapling"] [3 65 3 "oak_leaves"] [2 64 4 "dirt"]
              [4 64 2 "chest"] [4 65 2 "torch"] [5 64 2 "carrots"] [3 63 3 "dirt"] [8 64 2 "tall_grass"])))

(def strays [[2 64 2] [3 64 2] [4 64 3] [3 65 3] [2 64 4]])

(def tidy-spec {:blocks tidy-field :drops {"short_grass" "wheat_seeds" "oak_sapling" "oak_sapling"}})

;; ---------------------------------------------------------------- the rule, pure

(defn classify [want name] (tidy/classify want (when name {:name name})))

(deftest what-counts-as-a-stray
  (are [want name verdict] (= verdict (classify want name))
    {:crop "wheat"} "wheat" nil
    {:crop "wheat"} "short_grass" :dig
    {:crop "wheat"} "cobblestone" :dig
    {:crop "wheat"} "oak_sapling" :dig
    {:crop "wheat"} "oak_leaves" :dig
    {:crop "wheat"} "air" nil
    {:crop "wheat"} nil nil
    {:crop "wheat"} "carrots" :wrong
    {:crop "wheat"} "farmland" :wrong
    {:crop "melon"} "melon_stem" nil
    {:crop "melon"} "attached_melon_stem" nil
    {:crop "wheat"} "chest" [:kept :container]
    {:crop "wheat"} "red_bed" [:kept :owned]
    {:crop "wheat"} "torch" [:kept :light]
    {:crop "wheat"} "lantern" [:kept :light]
    {:crop "wheat"} "water" [:kept :fluid]
    :clear "dirt" :dig
    :clear "carrots" :dig
    :clear "air" nil
    :clear "chest" [:kept :container]
    :clear "lava" [:kept :fluid]
    :headroom "oak_leaves" :dig
    :headroom "sugar_cane" nil
    :headroom "torch" [:kept :light]
    :headroom "air" nil
    "farmland" "farmland" nil
    "farmland" "dirt" :wrong
    "water" "water" nil
    "water" "stone" :wrong
    "torch" "torch" nil
    "torch" "stone" :wrong
    [:any "water" "oak_slab"] "stone" :wrong
    [:any "water" "oak_slab"] "oak_slab" nil
    {:tree "oak"} "oak_log" nil
    {:tree "oak"} "dirt" nil))

(deftest a-stateful-want-judged-by-its-state
  (is (nil? (tidy/classify {:block "oak_fence_gate" :facing :north} {:name "oak_fence_gate" :state {:facing "north"}})))
  (is (= :wrong (tidy/classify {:block "oak_fence_gate" :facing :north} {:name "oak_fence_gate" :state {:facing "south"}})))
  (is (nil? (tidy/classify {:block "oak_fence_gate" :facing :north} {:name "oak_fence_gate"}))))

(defn expanded [plan] (:cells (shape/expand plan {})))

(deftest the-air-over-a-crop-cell-is-worked-unless-the-plan-names-it
  (let [plan {:id "p"
              :parts [{:id "a" :cells [[1 64 1] [2 64 1]] :want {:crop "wheat"}}
                      {:id "b" :cells [[2 65 1]] :want "oak_fence"}
                      {:id "c" :cells [[5 70 5]] :want "stone"}]}
        cells (tidy/work-cells (expanded plan) nil)]
    (is (= #{[1 64 1] [2 64 1] [2 65 1] [5 70 5] [1 65 1]} (set (map :pos cells))))
    (is (= :headroom (:want (first (filter #(= [1 65 1] (:pos %)) cells)))))
    (is (= #{[1 64 1] [2 64 1] [1 65 1]} (set (map :pos (tidy/work-cells (expanded plan) "a")))))))

;; ---------------------------------------------------------------- whole runs

(deftest it-digs-the-strays-and-leaves-everything-else
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start tidy-spec {"field" field-plan})
              result (await (outcome eng {:plan "field"} 200))]
          (is (= (set strays) (dug p)))
          (is (= 5 (:dug result)))
          (is (= ["air"] (distinct (map (fn [[x y z]] (block-at p x y z)) strays))))
          (is (= #{{:pos [4 64 2] :block "chest" :why :container} {:pos [4 65 2] :block "torch" :why :light}}
                 (set (:kept result))))
          (is (= #{{:pos [5 64 2] :found "carrots" :want "crop wheat"} {:pos [3 63 3] :found "dirt" :want "farmland"}}
                 (set (:wrong result))))
          (is (= [] (:refused result)))
          (is (= 1 (count (of-kind seen :tidy.done))))
          (is (= 1 (count (of-kind seen :tidy.wrong)))))))))

(deftest every-dig-goes-through-the-dig-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (start tidy-spec {"field" field-plan})]
          (await (outcome eng {:plan "field"} 200))
          (is (= 5 (count (of-kind seen :blocks.dig.done)))))))))

(deftest a-dig-child-that-declines-books-its-own-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dig (get registry/jobs 'jobs.blocks.dig)
              declining (assoc dig :check (fn [c] (ctx/wait c {:reason :not-allowed :why "claimed"}) false))
              {:keys [eng]} (start tidy-spec {"field" field-plan} [] (assoc registry/jobs 'jobs.blocks.dig declining))
              result (await (outcome eng {:plan "field" :give-up 1} 200))]
          (is (seq (:refused result)))
          (is (= #{:not-allowed} (set (map :reason (:refused result)))) (pr-str (:refused result))))))))

(deftest what-is-dug-is-picked-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start tidy-spec {"field" field-plan})
              result (await (outcome eng {:plan "field"} 200))]
          (is (= {"wheat_seeds" 1 "cobblestone" 1 "oak_sapling" 1 "oak_leaves" 1 "dirt" 1} (inv p)))
          (is (= 5 (:collected result))))))))

(deftest crops-water-and-the-wanted-torch-stand-after
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start tidy-spec {"field" field-plan})]
          (await (outcome eng {:plan "field"} 200))
          (is (= ["wheat" "wheat" "wheat" "carrots" "chest" "torch" "water" "torch" "farmland" "dirt"]
                 (mapv (fn [[x y z]] (block-at p x y z))
                       [[2 64 3] [3 64 3] [5 64 3] [5 64 2] [4 64 2] [6 64 2] [6 63 2] [4 65 2] [2 63 2] [3 63 3]])))
          (is (= "tall_grass" (block-at p 8 64 2))))))))

(deftest a-second-run-over-a-tidy-field-digs-nothing-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start tidy-spec {"field" field-plan})]
          (await (outcome eng {:plan "field"} 200))
          (let [before (count (calls p "dig"))
                again (await (outcome eng {:plan "field"} 200))]
            (is (= before (count (calls p "dig"))))
            (is (= 0 (:dug again)))
            (is (= 0 (:collected again)))
            (is (= 2 (count (of-kind seen :tidy.done))))
            (is (= "tidy of field done: nothing to dig" (:message (second (of-kind seen :tidy.done)))))))))))

(deftest a-part-limits-the-work-to-that-part
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start tidy-spec {"field" field-plan})
              result (await (outcome eng {:plan "field" :part "path"} 200))]
          (is (= #{[2 64 4]} (dug p)))
          (is (= 1 (:dug result))))))))

;; ---------------------------------------------------------------- declining

(defn declines
  "Submit tidy over tidy-spec with args and plans (zones given), tick a few times: [digs, tidy.declined reasons]."
  ([plans args] (declines plans args []))
  ([plans args zones]
   (let [{:keys [eng p seen]} (start tidy-spec plans zones)]
     (core/submit! eng (list job args) {})
     (dotimes [_ 4] (swap! clock + 700) (core/tick! eng))
     [(count (calls p "dig")) (mapv #(select-keys (:data %) [:plan :reason]) (of-kind seen :tidy.declined))])))

(deftest the-check-declines-a-plan-it-cannot-work-with-one-warn
  (are [plans args reason] (= [0 [{:plan (:plan args) :reason reason}]] (declines plans args))
    {} {:plan "nope"} "no such plan"
    {"field" field-plan} {:plan "field" :part "nothing"} "no cells"
    {"field" (assoc field-plan :parts [])} {:plan "field"} "no cells"))

(deftest a-decline-is-one-event-however-long-it-lasts
  (let [{:keys [eng seen]} (start tidy-spec {})]
    (core/submit! eng (list job {:plan "nope"}) {})
    (dotimes [_ 12] (swap! clock + 700) (core/tick! eng))
    (is (= 1 (count (of-kind seen :tidy.declined))))))

(deftest a-broken-plan-declines-naming-the-error
  (let [{:keys [eng p seen w]} (start tidy-spec {})]
    (reset! (:state w) (assoc @(:state w) :plans {"field" {:error "unreadable EDN: eof"}}))
    (core/submit! eng (list job {:plan "field"}) {})
    (dotimes [_ 3] (swap! clock + 700) (core/tick! eng))
    (is (zero? (count (calls p "dig"))))
    (is (= [{:plan "field" :reason "the plan cannot be read: unreadable EDN: eof"}]
           (mapv #(select-keys (:data %) [:plan :reason]) (of-kind seen :tidy.declined))))))

(deftest no-zone-list-declines-and-never-means-no-zones
  (is (= [0 [{:plan "field" :reason "no zone list has been read"}]]
         (declines {"field" field-plan} {:plan "field"} nil))))

(deftest a-zone-list-that-appears-lets-it-work
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p w]} (start tidy-spec {"field" field-plan} nil)]
          (core/submit! eng (list job {:plan "field"}) {})
          (dotimes [_ 3] (swap! clock + 700) (await (core/tick! eng)))
          (is (zero? (count (calls p "dig"))))
          (world/set-zones! w [])
          (await (run-until-empty eng 200))
          (is (= (set strays) (dug p))))))))

;; ---------------------------------------------------------------- access

(defn zone [name [x y z] allow] {:name name :min [x y z] :max [x y z] :owner "someone" :allow allow})

(deftest a-stray-inside-a-foreign-zone-is-dug-the-plan-is-the-permission
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start tidy-spec {"field" field-plan} [(zone "shrine" [3 64 2] #{})])
              result (await (outcome eng {:plan "field"} 200))]
          (is (= (set strays) (dug p)))
          (is (= [] (:refused result)))
          (is (= 5 (:dug result)))
          (is (empty? (of-kind seen :tidy.refused))))))))

(deftest a-zone-allowing-digging-is-no-obstacle
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start tidy-spec {"field" field-plan} [(zone "yard" [3 64 2] #{:dig})])
              result (await (outcome eng {:plan "field"} 200))]
          (is (= (set strays) (dug p)))
          (is (= [] (:refused result))))))))

(def other-plan {:id "other" :parts [{:id "o" :cells [[2 64 4]] :want "stone"}]})

(deftest a-stray-inside-another-plans-footprint-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start tidy-spec {"field" field-plan "other" other-plan})
              result (await (outcome eng {:plan "field"} 200))]
          (is (= (disj (set strays) [2 64 4]) (dug p)))
          (is (= [{:pos [2 64 4] :block "dirt" :reason :footprint :plan "other"}] (:refused result))))))))

(deftest the-access-is-checked-again-right-before-each-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p w]} (start (assoc tidy-spec :self {:pos {:x 3 :y 64 :z 3}}) {"field" field-plan})
              late-plan {:id "late" :parts [{:id "l" :cells [[3 64 2] [4 64 3] [2 64 4]] :want "stone"}]}]
          (.override (.-world p) "dig"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))]
                         (world/set-data! w {"field" field-plan "late" late-plan} {})
                         r)))
          (let [result (await (outcome eng {:plan "field"} 200))]
            (is (= 2 (count (dug p))) "the first dig of the round goes through and the other plan closes the rest")
            (is (= 3 (count (:refused result))))
            (is (= #{:footprint} (set (map :reason (:refused result)))))))))))

;; ---------------------------------------------------------------- hazards

(def hazard-plan
  {:id "h" :parts [{:id "c" :cells [[2 64 2]] :want :clear}]})

(defn hazard-world [& cells] {:blocks (apply put [2 64 2 "dirt"] cells)})

(def falling-plan
  {:id "h" :parts [{:id "c" :cells [[0 66 0]] :want :clear}]})

(defn ^:async run-hazard [spec args]
  (let [{:keys [eng p]} (start spec {"h" hazard-plan})
        result (await (outcome eng (assoc args :plan "h") 200))]
    [(count (calls p "dig")) result]))

(deftest digging-beside-water-is-accepted-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[digs result] (await (run-hazard (hazard-world [3 64 2 "water"]) {}))]
          (is (= 1 digs))
          (is (= 1 (:dug result))))))))

(deftest water-beside-is-refused-when-not-accepted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[digs result] (await (run-hazard (hazard-world [3 64 2 "water"]) {:accept #{}}))]
          (is (= 0 digs))
          (is (= 0 (:dug result)))
          (is (= [{:pos [2 64 2] :block "dirt" :reason :hazard :hazards [:fluid-adjacent]}] (:refused result))))))))

(deftest lava-beside-is-refused-by-default-and-not-by-the-water-acceptance
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[digs result] (await (run-hazard (hazard-world [3 64 2 "lava"]) {}))]
          (is (= 0 digs))
          (is (= [{:pos [2 64 2] :block "dirt" :reason :hazard :hazards [:lava-adjacent]}] (:refused result))))))))

(deftest lava-beside-is-dug-when-accepted-by-name
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[digs result] (await (run-hazard (hazard-world [3 64 2 "lava"]) {:accept #{:lava-adjacent}}))]
          (is (= 1 digs))
          (is (= 1 (:dug result))))))))

(deftest a-falling-block-over-the-body-is-refused-by-default-and-dug-when-accepted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [run (fn ^:async run-one [args]
                    (let [{:keys [eng p]} (start {:blocks (put [0 66 0 "dirt"] [0 67 0 "gravel"])} {"h" falling-plan})
                          result (await (outcome eng (assoc args :plan "h") 200))]
                      [(count (calls p "dig")) result]))
              [digs result] (await (run {}))
              [digs2 result2] (await (run {:accept #{:fluid-adjacent :falling-block}}))]
          (is (= 0 digs))
          (is (= [:falling-block] (:hazards (first (:refused result)))))
          (is (= 1 digs2))
          (is (= [] (:refused result2))))))))

(deftest a-refused-hazard-is-given-up-within-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start (hazard-world [3 64 2 "lava"]) {"h" hazard-plan})
              result (await (outcome eng {:plan "h"} 1))]
          (is (zero? (count (calls p "dig"))))
          (is (= [:hazard] (map :reason (:refused result)))))))))

(deftest the-plans-own-water-is-never-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "w" :parts [{:id "w" :cells [[2 64 2] [3 64 2]] :want [:any "water" {:block "oak_slab" :waterlogged true}]}]}
              {:keys [eng p]} (start {:blocks (put [2 64 2 "water"] [3 64 2 "stone"])} {"w" plan})
              result (await (outcome eng {:plan "w"} 100))]
          (is (zero? (count (calls p "dig"))))
          (is (= [{:pos [3 64 2] :found "stone" :want "water | oak_slab[waterlogged=true]"}] (:wrong result)))
          (is (= 0 (:dug result))))))))

;; ---------------------------------------------------------------- giving up

(deftest a-stray-that-cannot-be-walked-to-is-refused-after-three-tries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "c" :cells [[30 64 2]] :want :clear}]}
              {:keys [eng p]} (start {:blocks (put [30 64 2 "dirt"]) :unreachable ["30,64,2"]} {"far" plan})
              result (await (outcome eng {:plan "far"} 100))]
          (is (zero? (count (calls p "dig"))))
          (is (= [{:pos [30 64 2] :block "dirt" :reason :unreachable}] (:refused result))))))))

(deftest a-block-that-cannot-be-dug-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "b" :parts [{:id "c" :cells [[2 64 2] [3 64 2]] :want :clear}]}
              {:keys [eng p]} (start {:blocks (put [2 64 2 "bedrock"] [3 64 2 "dirt"])} {"b" plan})
              result (await (outcome eng {:plan "b"} 100))]
          (is (= ["bedrock" "air"] [(block-at p 2 64 2) (block-at p 3 64 2)]))
          (is (= [{:pos [2 64 2] :block "bedrock" :reason :cannot}] (:refused result)))
          (is (= 1 (:dug result))))))))

(deftest a-cell-nobody-has-loaded-is-no-stray
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "u" :parts [{:id "c" :cells [[2 64 2] [3 64 2]] :want :clear}]}
              {:keys [eng p]} (start {:blocks (put [3 64 2 "dirt"]) :unloaded ["2,64,2"]} {"u" plan})
              result (await (outcome eng {:plan "u"} 100))]
          (is (= #{[3 64 2]} (dug p)))
          (is (= [] (:refused result))))))))

;; ---------------------------------------------------------------- cut and restart

(deftest a-restart-mid-run-finishes-without-digging-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              p (tu/fake-on-floor tidy-spec)
              mk (fn []
                   (let [[seen sink] (tu/capture-sink)]
                     {:seen seen
                      :eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir dir :now #(deref clock)
                                         :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})
                                         :world (world/of-data {"field" field-plan} {} [])})}))
              a (mk)]
          (.override (.-world p) "dig"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))]
                         ((get {2 #(takeover/take! (:eng a) {:who "claude" :why "cut"})} (count (calls p "dig")) (constantly nil)))
                         r)))
          (core/submit! (:eng a) (list job {:plan "field"}) {})
          (swap! clock + 700)
          (await (core/tick! (:eng a)))
          (is (= 2 (count (calls p "dig"))) "cut after the second dig")
          (let [b (mk)]
            (await (run-until-empty (:eng b) 200))
            (is (= (set strays) (dug p)))
            (is (= (count strays) (count (calls p "dig"))) "no cell was dug twice")
            (is (= 1 (count (of-kind (:seen b) :tidy.done))))
            (is (= 5 (:dug (:data (first (of-kind (:seen b) :tidy.done))))) "the dig the cut came with is counted from the world after the restart")))))))

(deftest the-opt-out-needs-no-zone-list
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (start tidy-spec {"field" field-plan} nil)
              result (await (outcome eng {:plan "field" :ignore-zones? true} 200))]
          (is (= (count strays) (:dug result))))))))

(deftest it-looks-around-before-it-calls-the-field-tidy
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start tidy-spec {"field" field-plan})
              _ (tu/seeing-after-look p)
              result (await (outcome eng {:plan "field"} 200))]
          (is (seq (calls p "look")))
          (is (= 5 (:dug result))))))))
