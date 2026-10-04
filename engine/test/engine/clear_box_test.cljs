(ns engine.clear-box-test
  "jobs.build.clear-box against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.world :as ew]
            [jobs.build.clear-box :as clear-box]))

(defn setup [world & [shared]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world shared
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome [eng job args n]
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

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn block-at [p pos] (.-name (.blockAt p (clj->js pos))))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn dug-ys [p] (mapv #(.-y (.-pos (.-args %))) (calls p "dig")))
(defn held-items [p] (mapv #(.-item (.-args %)) (calls p "equip")))

(def job 'jobs.build.clear-box)
(def box {:from {:x 1 :y 64 :z 1} :to {:x 2 :y 65 :z 2}})
(def eight
  {"1,65,1" "dirt" "2,65,1" "dirt" "1,65,2" "dirt" "2,65,2" "dirt"
   "1,64,1" "stone" "2,64,1" "stone" "1,64,2" "stone" "2,64,2" "stone"})
(def one {:from {:x 1 :y 64 :z 1} :to {:x 1 :y 64 :z 1}})

(deftest clear-box-digs-top-layer-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks eight})
              result (await (child-outcome eng job {:from (:to box) :to (:from box)} 20))]
          (is (= {:dug 8 :skipped {} :kept 0 :fluids {}} result))
          (is (= [65 65 65 65 64 64 64 64] (dug-ys p)))
          (is (= ["air"] (distinct (map #(block-at p %) [{:x 1 :y 65 :z 1} {:x 2 :y 64 :z 2}])))))))))

(deftest clear-box-keeps-beds-containers-fluids-and-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks {"1,65,1" "chest" "2,65,1" "red_bed" "1,65,2" "water"
                                                    "2,65,2" "glass" "1,64,1" "dirt"}})
              result (await (child-outcome eng job (assoc box :keep ["glass"]) 20))]
          (is (= {:dug 1 :skipped {} :kept 3 :fluids {"water" 1}} result))
          (is (= ["chest" "red_bed" "water" "glass" "air"]
                 (mapv #(block-at p %) [{:x 1 :y 65 :z 1} {:x 2 :y 65 :z 1} {:x 1 :y 65 :z 2} {:x 2 :y 65 :z 2} {:x 1 :y 64 :z 1}])))
          (is (= 1 (count (kinds seen :clear-box.done)))))))))

(deftest clear-box-skips-bedrock-as-cannot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:blocks {"1,64,1" "bedrock"}})
              result (await (child-outcome eng job one 8))]
          (is (= {:dug 0 :skipped {{:x 1 :y 64 :z 1} :cannot} :kept 0 :fluids {}} result))
          (is (= 1 (count (kinds seen :clear-box.skipped)))))))))

(deftest clear-box-skips-a-refused-dig-after-two-tries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"1,64,1" "dirt"}})]
          (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "timeout"}))
          (is (= {:dug 0 :skipped {{:x 1 :y 64 :z 1} :refused} :kept 0 :fluids {}} (await (child-outcome eng job one 8))))
          (is (= 2 (count (calls p "dig")))))))))

(deftest clear-box-skips-an-unreachable-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"9,64,1" "dirt"} :unreachable ["9,64,1"]})
              args {:from {:x 9 :y 64 :z 1} :to {:x 9 :y 64 :z 1}}]
          (is (= {:dug 0 :skipped {{:x 9 :y 64 :z 1} :unreachable} :kept 0 :fluids {}} (await (child-outcome eng job args 8))))
          (is (empty? (calls p "dig"))))))))

(deftest clear-box-equips-the-matching-tool-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks eight
                                      :inventory [{:name "wooden_shovel" :count 1} {:name "iron_shovel" :count 1}
                                                  {:name "stone_pickaxe" :count 1}]})]
          (await (child-outcome eng job box 20))
          (is (= ["iron_shovel" "stone_pickaxe"] (held-items p))))))))

(deftest clear-box-equips-nothing-without-a-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks eight})]
          (await (child-outcome eng job box 20))
          (is (empty? (calls p "equip"))))))))

(deftest clear-box-rejects-too-big-or-missing-boxes
  (are [args] (thrown? js/Error (clear-box/cells args))
    {:from {:x 0 :y 0 :z 0} :to {:x 7 :y 7 :z 7}}
    {:from {:x 0 :y 0 :z 0}}
    {})
  (is (= 400 (count (clear-box/cells {:from {:x 0 :y 0 :z 0} :to {:x 7 :y 4 :z 9}})))))

(deftest clear-box-keeps-its-count-across-a-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks eight})
              world (.-world p)
              out (atom nil)
              parent {:check (constantly true)
                      :round (fn ^:async r [c]
                               (let [r (await (ctx/call-child c :kid job box))]
                                 (when (= :done r) (reset! out (ctx/child-result c :kid)))
                                 r))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (core/submit! eng '(recording-parent) {})
          (await (core/tick! eng))
          (.hold world "dig")
          (let [running (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (takeover/take! eng {:who "claude" :why "cut"})
            (await running))
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (await (run-until-empty eng 20))
          (is (= {:dug 8 :skipped {} :kept 0 :fluids {}} @out)))))))

(deftest clear-box-walks-toward-a-box-in-unloaded-chunks-then-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far {:from {:x 30 :y 64 :z 30} :to {:x 31 :y 64 :z 30}}
              {:keys [eng p]} (setup {:blocks {"30,64,30" "dirt" "31,64,30" "dirt"} :floor [-5 -5 35 35]
                                      :unloaded ["30,64,30" "31,64,30"]})
              out (atom :not-done)
              parent {:check (constantly true)
                      :round (fn ^:async r [c]
                               (let [r (await (ctx/call-child c :kid job far))]
                                 (when (= :done r) (reset! out (ctx/child-result c :kid)))
                                 r))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (core/submit! eng '(recording-parent) {})
          (await (core/tick! eng))
          (is (= :not-done @out) "unloaded cells are not an empty box")
          (is (pos? (count (tu/walk-calls p))))
          (is (empty? (calls p "dig")))
          (.clear (.. p -world -state -unloaded))
          (await (run-until-empty eng 12))
          (is (= {:dug 2 :skipped {} :kept 0 :fluids {}} @out)))))))

(defn act-trail
  "[[name x] ...] of the moveTo and dig calls in order."
  [p]
  (->> (.-calls (.-world p))
       (filter #(#{"moveTo" "dig"} (.-name %)))
       (mapv #(vector (.-name %) (.-x (or (.-pos (.-args %)) #js {}))))))

(deftest clear-box-digs-the-others-before-the-cell-under-foot-then-steps-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [args {:from {:x 1 :y 63 :z 1} :to {:x 2 :y 63 :z 1}}
              {:keys [eng p]} (setup {:blocks {"1,63,1" "dirt" "2,63,1" "dirt"}
                                      :self {:pos {:x 1.5 :y 64 :z 1.5}}})
              result (await (child-outcome eng job args 20))]
          (is (= {:dug 2 :skipped {} :kept 0 :fluids {}} result))
          (is (= [["dig" 2] ["moveTo" 0] ["dig" 1]] (act-trail p))))))))

(deftest clear-box-steps-off-a-lone-cell-under-foot-before-digging-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [args {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}}
              {:keys [eng p]} (setup {:blocks {"1,63,1" "dirt"}
                                      :self {:pos {:x 1.5 :y 64 :z 1.5}}})
              result (await (child-outcome eng job args 20))]
          (is (= {:dug 1 :skipped {} :kept 0 :fluids {}} result))
          (is (= [["moveTo" 0] ["dig" 1]] (act-trail p))))))))

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
                 (map #(select-keys % [:zones :plans :cells]) (kinds seen :clear-box.refused)))))))))

(deftest a-box-wholly-refused-declines-once-naming-zone-and-plan-and-stays-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks eight} (ew/of-data {"pad" pad-plan} {} [farm-zone]))]
          (is (= 1 (await (listed-after eng 5))))
          (is (empty? (calls p "dig")))
          (is (= [{:reason :refused :zones ["farm"] :plans ["pad"]}]
                 (map #(select-keys % [:reason :zones :plans]) (kinds seen :clear-box.declined)))))))))

(deftest no-zone-list-declines-with-one-warn-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks eight} (ew/of-data {} {} nil))]
          (is (= 1 (await (listed-after eng 5))))
          (is (empty? (.-calls (.-world p))))
          (is (= [:no-zones] (map :reason (kinds seen :clear-box.declined)))))))))

(deftest a-zone-added-between-the-choice-and-the-dig-stops-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {} {} [])
              {:keys [eng p]} (setup {:blocks {"9,64,1" "dirt"}} w)
              args {:from {:x 9 :y 64 :z 1} :to {:x 9 :y 64 :z 1}}]
          (.override (.-world p) "steer"
                     (fn [token a impl] (ew/set-zones! w [(assoc farm-zone :min [9 64 1] :max [9 64 1])]) (impl token a)))
          (is (= {:dug 0 :skipped {{:x 9 :y 64 :z 1} :zone} :kept 0 :fluids {}} (await (child-outcome eng job args 8))))
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
          (is (= #{:fluid-adjacent :falling-block} (:default (:accept clear-box/args)))))))))

(deftest a-box-in-a-zone-follows-its-owner-and-the-opt-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra dug] [["Fake" {} 8] ["fake" {} 8] ["Miles" {} 4] ["Miles" {:ignore-zones? true} 8]]]
          (let [{:keys [eng]} (setup {:blocks eight} (ew/of-data {} {} [(assoc farm-zone :owner owner)]))
                result (await (child-outcome eng job (merge box extra) 30))]
            (is (= dug (:dug result)) (pr-str [owner extra]))))))))
