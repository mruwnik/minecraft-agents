(ns engine.clear-cells-test
  "jobs.blocks.clear-cells against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.blocks-dig-test :as bd]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [jobs.blocks.clear-cells :as clear-cells]))

(defn setup [world & [shared]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world shared
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn block-at [p pos] (.-name (.blockAt p (clj->js pos))))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn dug-ys [p] (mapv #(.-y (.-pos (.-args %))) (calls p "dig")))
(defn held-items [p] (mapv #(.-item (.-args %)) (calls p "equip")))

(defn ^:async child-outcome
  "tu/child-outcome without the ending, status, reason and text the adapter adds (the contract test reads the ending)."
  [eng job args n]
  (let [r (await (tu/child-outcome eng job args n))]
    (if (map? r) (dissoc r :ending :status :reason :text) r)))

(def job 'jobs.blocks.clear-cells)
(def box {:area {:min {:x 1 :y 64 :z 1} :max {:x 2 :y 65 :z 2}}})
(def eight
  {"1,65,1" "dirt" "2,65,1" "dirt" "1,65,2" "dirt" "2,65,2" "dirt"
   "1,64,1" "stone" "2,64,1" "stone" "1,64,2" "stone" "2,64,2" "stone"})
(def one-cell {:area {:min {:x 9 :y 55 :z 1} :max {:x 9 :y 55 :z 1}}})
(def one {:area {:min {:x 1 :y 64 :z 1} :max {:x 1 :y 64 :z 1}}})

(deftest clear-cells-digs-top-layer-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks eight})
              result (await (child-outcome eng job {:area {:min (get-in box [:area :max]) :max (get-in box [:area :min])}} 40))]
          (is (= {:dug 8 :skipped {} :kept 0 :fluids {}} (dissoc result :text)))
          (is (= [65 65 65 65 64 64 64 64] (dug-ys p)))
          (is (= ["air"] (distinct (map #(block-at p %) [{:x 1 :y 65 :z 1} {:x 2 :y 64 :z 2}])))))))))

(deftest clear-cells-digs-only-the-listed-cells-of-a-cells-area
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks eight})
              result (await (child-outcome eng job {:area {:cells [[1 65 1] {:x 2 :y 64 :z 2}]}} 20))]
          (is (= 2 (:dug result)))
          (is (= ["air" "dirt" "air"] [(block-at p {:x 1 :y 65 :z 1}) (block-at p {:x 2 :y 65 :z 2}) (block-at p {:x 2 :y 64 :z 2})])))))))

(deftest clear-cells-keeps-beds-containers-fluids-and-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks {"1,65,1" "chest" "2,65,1" "red_bed" "1,65,2" "water"
                                                    "2,65,2" "glass" "1,64,1" "dirt"}})
              result (await (child-outcome eng job (assoc box :keep ["glass"]) 20))]
          (is (= {:dug 1 :skipped {} :kept 3 :fluids {"water" 1}} (dissoc result :text)))
          (is (= ["chest" "red_bed" "water" "glass" "air"]
                 (mapv #(block-at p %) [{:x 1 :y 65 :z 1} {:x 2 :y 65 :z 1} {:x 1 :y 65 :z 2} {:x 2 :y 65 :z 2} {:x 1 :y 64 :z 1}]))))))))

(deftest clear-cells-skips-bedrock-as-cannot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:blocks {"1,64,1" "bedrock"}})
              result (await (child-outcome eng job one 8))]
          (is (= {:dug 0 :skipped {{:x 1 :y 64 :z 1} :cannot} :kept 0 :fluids {}} (dissoc result :text)))
          (is (= 1 (count (kinds seen :clear-cells.skipped)))))))))

(deftest clear-cells-skips-a-failed-dig-after-two-tries-under-its-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"1,64,1" "dirt"}})]
          (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "timeout"}))
          (is (= {:dug 0 :skipped {{:x 1 :y 64 :z 1} :timeout} :kept 0 :fluids {}} (dissoc (await (child-outcome eng job one 8)) :text)))
          (is (= 2 (count (calls p "dig")))))))))

(deftest clear-cells-skips-an-unreachable-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"9,55,1" "dirt"}})
              args one-cell]
          (is (= {:dug 0 :skipped {{:x 9 :y 55 :z 1} :unreachable} :kept 0 :fluids {}} (dissoc (await (child-outcome eng job args 20)) :text)))
          (is (empty? (calls p "dig"))))))))

(deftest clear-cells-equips-the-cheapest-matching-tool-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks eight
                                      :inventory [{:name "wooden_shovel" :count 1} {:name "iron_shovel" :count 1}
                                                  {:name "stone_pickaxe" :count 1}]})]
          (await (child-outcome eng job box 20))
          (is (= ["wooden_shovel" "stone_pickaxe"] (held-items p))))))))

(deftest clear-cells-equips-nothing-without-a-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks eight})]
          (await (child-outcome eng job box 20))
          (is (empty? (calls p "equip"))))))))

(deftest clear-cells-rejects-too-big-or-missing-boxes
  (are [args] (thrown? js/Error (clear-cells/cells args))
    {:area {:min {:x 0 :y 0 :z 0} :max {:x 7 :y 7 :z 7}}}
    {:area {:min {:x 0 :y 0 :z 0}}}
    {})
  (is (= 400 (count (clear-cells/cells {:area {:min {:x 0 :y 0 :z 0} :max {:x 7 :y 4 :z 9}}})))))

(deftest clear-cells-refuses-corners-of-another-shape-at-submit-and-does-not-move
  (doseq [args [{:area {:min "9 71 9" :max [1 1 1]}} {:area {:min [1 2] :max [1 1 1]}} {:area {:min [1 "a" 3] :max [1 1 1]}}]]
    (let [{:keys [eng p]} (setup {:blocks eight})]
      (is (thrown-with-msg? js/Error #"clear-cells :area must be a map" (core/submit! eng (list job args) {})) (pr-str args))
      (is (empty? (.-calls (.-world p))) (pr-str args)))))

(deftest clear-cells-keeps-its-count-across-a-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks eight})
              world (.-world p)
              out (atom nil)
              parent (tu/recording-parent job box out #(dissoc (tu/outcome %) :status))
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (core/submit! eng '(recording-parent) {})
          (.hold world "dig")
          (let [running (core/tick! eng)]
            (loop []
              (when (empty? (calls p "dig"))
                (await (js/Promise. (fn [resolve] (js/setTimeout resolve 5))))
                (recur)))
            (core/cut! eng (core/holder eng) :test nil)
            (await running))
          (await (run-until-empty eng 40))
          (is (= {:dug 8 :skipped {} :kept 0 :fluids {}} (dissoc @out :text))))))))

(deftest clear-cells-walks-toward-a-box-in-unloaded-chunks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far {:area {:min {:x 30 :y 64 :z 30} :max {:x 31 :y 64 :z 30}}}
              {:keys [eng p]} (setup {:blocks {"30,64,30" "dirt" "31,64,30" "dirt"} :floor [-5 -5 35 35]
                                      :unloaded ["30,64,30" "31,64,30"]})]
          (await (child-outcome eng job far 8))
          (is (pos? (count (tu/walk-calls p))))
          (is (empty? (calls p "dig"))))))))

(defn act-trail
  "[[name x] ...] of the moveTo and dig calls in order."
  [p]
  (->> (.-calls (.-world p))
       (filter #(#{"steer" "dig"} (.-name %)))
       (mapv #(vector (.-name %) (.-x (or (.-pos (.-args %)) #js {}))))))

(deftest clear-cells-digs-the-others-before-the-cell-under-foot-then-steps-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [args {:area {:min {:x 1 :y 63 :z 1} :max {:x 2 :y 63 :z 1}}}
              {:keys [eng p]} (setup {:blocks {"1,63,1" "dirt" "2,63,1" "dirt" "1,62,1" "stone" "2,62,1" "stone"}
                                      :self {:pos {:x 1.5 :y 64 :z 1.5}}})
              result (await (child-outcome eng job args 20))]
          (is (= {:dug 2 :skipped {} :kept 0 :fluids {}} (dissoc result :text)))
          (is (= [["dig" 2] ["dig" 1]] (act-trail p))
              "picking up the first cell's drop walked the body off the cell under foot, so no step-off is needed"))))))

(deftest clear-cells-steps-off-a-lone-cell-under-foot-before-digging-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [args {:area {:min {:x 1 :y 63 :z 1} :max {:x 1 :y 63 :z 1}}}
              {:keys [eng p]} (setup {:blocks {"1,63,1" "dirt"}
                                      :self {:pos {:x 1.5 :y 64 :z 1.5}}})
              result (await (child-outcome eng job args 20))]
          (is (= {:dug 1 :skipped {} :kept 0 :fluids {}} (dissoc result :text)))
          (is (= ["steer" "dig"] (map first (act-trail p))) "walked off through go-to, then dug"))))))

;; ------------------------------------------------------------------ zones and footprints

(def farm-zone {:name "farm" :min [1 60 1] :max [1 70 2] :owner "Miles"})
(def pad-plan {:id "pad" :parts [{:id "p" :box [[2 64 1] [2 65 2]] :want "stone"}]})

(defn ^:async listed-after [eng n]
  (core/submit! eng (list job box) {})
  (dotimes [_ n] (await (core/tick! eng)))
  (count (:list (core/state eng))))

(deftest cells-in-a-zone-or-another-plans-footprint-are-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks eight} (ew/of-data {"pad" (assoc-in pad-plan [:parts 0 :box] [[2 64 1] [2 65 1]])}
                                                                      {} [farm-zone]))
              result (await (child-outcome eng job box 30))]
          (is (= {:dug 2 :kept 0 :fluids {}} (dissoc result :skipped)))
          (is (= {{:x 1 :y 65 :z 1} :zone {:x 1 :y 65 :z 2} :zone {:x 1 :y 64 :z 1} :zone {:x 1 :y 64 :z 2} :zone
                  {:x 2 :y 65 :z 1} :footprint {:x 2 :y 64 :z 1} :footprint}
                 (:skipped result)))
          (is (= #{{:x 2 :y 65 :z 2} {:x 2 :y 64 :z 2}} (set (map #(js->clj (.-pos (.-args %)) :keywordize-keys true) (calls p "dig")))))
          (is (= [{:zones ["farm"] :plans ["pad"] :cells 6}]
                 (map #(select-keys % [:zones :plans :cells]) (kinds seen :clear-cells.refused)))))))))

(deftest the-for-plan-footprint-is-no-obstacle
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks eight} (ew/of-data {"pad" pad-plan} {} []))
              result (await (child-outcome eng job (assoc box :for-plan "pad") 40))]
          (is (= {:dug 8 :skipped {}} (select-keys result [:dug :skipped])))
          (is (= 8 (count (calls p "dig")))))))))

(deftest a-box-wholly-refused-declines-without-a-warn-and-stays-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks eight} (ew/of-data {"pad" pad-plan} {} [farm-zone]))]
          (is (= 1 (await (listed-after eng 5))))
          (is (empty? (calls p "dig")))
          (is (empty? (kinds seen :clear-cells.declined))))))))

(deftest no-zone-list-declines-without-a-warn-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks eight} (ew/of-data {} {} nil))]
          (is (= 1 (await (listed-after eng 5))))
          (is (empty? (.-calls (.-world p))))
          (is (empty? (kinds seen :clear-cells.declined))))))))

(deftest a-zone-added-between-the-choice-and-the-dig-stops-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {} {} [])
              {:keys [eng p]} (setup {:blocks {"9,64,1" "dirt"}} w)
              args {:area {:min {:x 9 :y 64 :z 1} :max {:x 9 :y 64 :z 1}}}]
          (.override (.-world p) "steer"
                     (fn [token a impl] (ew/set-zones! w [(assoc farm-zone :min [9 64 1] :max [9 64 1])]) (impl token a)))
          (is (= {:dug 0 :skipped {{:x 9 :y 64 :z 1} :zone} :kept 0 :fluids {}} (dissoc (await (child-outcome eng job args 8)) :text)))
          (is (empty? (calls p "dig"))))))))

(deftest a-hazard-not-accepted-is-tried-twice-then-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks {"1,64,1" "dirt" "1,64,2" "water"}}
              strict (setup world)
              default (setup world)]
          (is (= {:dug 0 :skipped {{:x 1 :y 64 :z 1} :hazard} :kept 0 :fluids {}}
                 (await (child-outcome (:eng strict) job (assoc one :accept #{}) 8))))
          (is (empty? (calls (:p strict) "dig")))
          (is (= 1 (:dug (await (child-outcome (:eng default) job one 8)))))
          (is (= #{:fluid-adjacent :falling-block} (:default (:accept clear-cells/args)))))))))

(deftest a-box-in-a-zone-follows-its-owner-and-the-opt-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra dug] [["Fake" {} 8] ["fake" {} 8] ["Miles" {} 4] ["Miles" {:ignore-zones? true} 8]]]
          (let [{:keys [eng]} (setup {:blocks eight} (ew/of-data {} {} [(assoc farm-zone :owner owner)]))
                result (await (child-outcome eng job (merge box extra) 30))]
            (is (= dug (:dug result)) (pr-str [owner extra]))))))))

(deftest clear-cells-picks-up-what-it-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (merge eight {"1,63,1" "stone" "2,63,1" "stone" "1,63,2" "stone" "2,63,2" "stone"})}) ; a drop in the dug box is picked up from its floor
              result (await (child-outcome eng job box 40))
              carried (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p)))]
          (is (= 8 (:dug result)))
          (is (= {"dirt" 4 "stone" 4} (select-keys carried ["dirt" "stone"]))))))))

(def full-inventory (mapv (fn [i] {:name (str "item_" i) :count 1}) (range 36)))

(deftest clear-cells-with-a-full-inventory-yields-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks eight :inventory full-inventory})
              id (core/submit! eng (list job box) {})]
          (dotimes [_ 4] (await (core/tick! eng)))
          (is (= 1 (count (:list (core/state eng)))) "still listed, yielding")
          (is (empty? (calls p "dig"))))))))

(deftest clear-cells-gives-up-a-cell-it-reached-but-never-sees
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far {:area {:min {:x 30 :y 64 :z 30} :max {:x 31 :y 64 :z 30}}}
              {:keys [eng p]} (setup {:blocks {"30,64,30" "dirt" "31,64,30" "dirt"} :floor [-5 -5 35 35]
                                      :unloaded ["30,64,30" "31,64,30"]})
              result (await (child-outcome eng job far 8))]
          (is (= {{:x 30 :y 64 :z 30} :unloaded {:x 31 :y 64 :z 30} :unloaded} (:skipped result)) "a reached cell that stays unseen counts tries, then is skipped")
          (is (empty? (calls p "dig"))))))))

(deftest clear-cells-clears-the-whole-box-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks eight})
              out (atom :not-done)
              parent (tu/recording-parent job box out #(dissoc (tu/outcome %) :status))
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (core/submit! eng '(recording-parent) {})
          (await (core/tick! eng))
          (is (= {:dug 8 :skipped {} :kept 0 :fluids {}} (dissoc @out :text)) "one tick, one call of the child"))))))

(deftest a-dig-declined-not-loaded-waits-without-counting-a-try
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [t1 {:x 1 :y 64 :z 1}
              seen* (atom {:rs [] :skipped :none})
              parent {:round (fn recording-round [c]
                               (dotimes [_ 5]
                                 (swap! seen* update :rs conj (clear-cells/declined! c t1 {:reason :not-loaded :pos t1})))
                               (swap! seen* assoc :skipped (:skipped (ctx/mem c) {}) :cell-tries (:cell-tries (ctx/mem c) {}))
                               (ctx/end c :done nil))}
              {:keys [eng]} (setup {:blocks {"1,64,1" "dirt"}})
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (core/submit! eng '(recording-parent) {})
          (await (run-until-empty eng 3))
          (is (= [:continue :continue :continue :continue :continue] (:rs @seen*)))
          (is (= {} (:skipped @seen*)) "a briefly unloaded cell is not skipped")
          (is (= {} (:cell-tries @seen*))))))))

;; ------------------------------------------------------------------ lava a dig lays open

(deftest clear-cells-seals-hidden-lava-a-dig-lays-open-under-a-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"1,63,1" "dirt" "1,62,1" "lava" "1,61,1" "stone"} :inventory [{:name "cobblestone" :count 2}]
                                      :self {:pos {:x 3.5 :y 64 :z 1.5}}})]
          (bd/hiding p #(= [1 62 1] %))
          (let [result (await (child-outcome eng job {:area {:min {:x 1 :y 63 :z 1} :max {:x 1 :y 63 :z 1}}} 30))]
            (is (= 1 (:dug result)))
            (is (= ["air" "cobblestone"] (mapv #(block-at p %) [{:x 1 :y 63 :z 1} {:x 1 :y 62 :z 1}]))
                "the cell stays cleared and the lava under it is sealed")))))))

(deftest clear-cells-counts-the-cell-dug-when-the-lava-it-lays-open-stays-unsealed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"1,63,1" "dirt" "1,62,1" "lava" "1,61,1" "stone"}
                                      :self {:pos {:x 3.5 :y 64 :z 1.5}}})]
          (bd/hiding p #(= [1 62 1] %))
          (let [result (await (child-outcome eng job {:area {:min {:x 1 :y 63 :z 1} :max {:x 1 :y 63 :z 1}}} 30))]
            (is (= 1 (:dug result)) "the dug cell counts though the lava was not sealed")))))))

;; ------------------------------------------------------------------ the wall of a stocked pen

(def stocked-pit
  (apply dissoc (tu/box -2 60 -3 8 63 5 "dirt") (for [x (range 3) y [61 62 63] z (range 3)] (str x "," y "," z))))
(def pit-rim-row {:area {:min {:x 3 :y 63 :z 1} :max {:x 6 :y 63 :z 1}}})
(def pit-zone {:name "pit" :owner "Fake" :min [0 61 0] :max [2 63 2]})
(def pit-cow {:id 7 :name "cow" :kind "passive" :pos {:x 1.5 :y 61 :z 1.5}})

(deftest clear-cells-reads-the-pen-again-once-digging-shows-its-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks stocked-pit :entities [pit-cow] :self {:pos {:x 8.5 :y 64 :z 1.5}}}
                                     (ew/of-data {} {} [pit-zone]))
              _ (aset p "sensedAt" (fn [pos] (if (and (<= (.-x pos) 2) (not= "air" (block-at p {:x 4 :y 63 :z 1})))
                                               #js {:unknown true}
                                               (.blockAt p pos))))
              result (await (child-outcome eng job pit-rim-row 60))]
          (is (= {{:x 3 :y 63 :z 1} :own-pen} (:skipped result)) "the pit was hidden at the start; the rim cell is spared once seen")
          (is (= 3 (:dug result))))))))

(deftest clear-cells-leaves-the-wall-of-a-pen-with-animals-unless-told-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [run (fn ^:async r [entities args]
                    (let [{:keys [eng p seen]} (setup {:blocks stocked-pit :entities entities :self {:pos {:x 8.5 :y 64 :z 1.5}}}
                                                      (ew/of-data {} {} [pit-zone]))
                          result (await (child-outcome eng job (merge pit-rim-row args) 60))]
                      {:result result :p p :seen seen}))
              stocked (await (run [pit-cow] {}))
              forced (await (run [pit-cow] {:open-pens? true}))
              empty-pen (await (run [] {}))]
          (is (= {{:x 3 :y 63 :z 1} :own-pen} (:skipped (:result stocked))) "only the rim cell beside the pit is left")
          (is (= 3 (:dug (:result stocked))))
          (is (= "dirt" (block-at (:p stocked) {:x 3 :y 63 :z 1})))
          (is (= [:own-pen] (map :reason (kinds (:seen stocked) :clear-cells.refused))))
          (is (= {:dug 4 :skipped {}} (select-keys (:result forced) [:dug :skipped])))
          (is (= {:dug 4 :skipped {}} (select-keys (:result empty-pen) [:dug :skipped]))))))))

(deftest clear-cells-spares-the-pen-wall-only-up-to-its-wall-height
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (assoc stocked-pit "3,68,1" "dirt") :entities [pit-cow] :self {:pos {:x 8.5 :y 64 :z 1.5}}}
                                     (ew/of-data {} {} [pit-zone]))
              result (await (child-outcome eng job {:area {:min {:x 3 :y 68 :z 1} :max {:x 3 :y 68 :z 1}}} 60))]
          (is (= {:dug 1 :skipped {}} (select-keys result [:dug :skipped])) "a cell well above the pen is not its wall")
          (is (= "air" (block-at p {:x 3 :y 68 :z 1}))))))))
