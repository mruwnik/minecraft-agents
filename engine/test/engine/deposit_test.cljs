(ns engine.deposit-test
  "jobs.storage.deposit against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.triggers :as triggers]
            [jobs.lib.storage :as storage]
            [jobs.lib.world-files :as world]
            [engine.withdraw-test :as wt]
            [engine.test-util :as tu]))

(def job 'jobs.storage.deposit)

(defn setup-far
  "wt/setup with the body seeing the world, so go-to can plan the walk."
  [world]
  (let [r (wt/setup world)]
    (assoc r :p (tu/seeing-all (:p r)))))
(def chest {:x 10 :y 64 :z 0})

(deftest deposit-puts-the-item-in-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5}] :containers {"10,64,0" []}})
              result (await (tu/child-outcome eng job {:chest chest :items ["dirt"]} 8))]
          (is (= {} (wt/inv p)))
          (is (= {:gave-up false} result)))))))

(deftest deposit-gives-up-when-the-transfer-says-ok-but-moves-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5}] :containers {"10,64,0" []}})]
          (.override (.-world p) "transfer" (fn ^:async f [_ _ _] #js {:status "ok" :moved 0}))
          (let [result (await (tu/child-outcome eng job {:chest chest :items ["dirt"]} 12))]
            (is (= true (:gave-up result)))
            (is (<= (count (wt/calls p "transfer")) 3))))))))

(deftest deposit-of-everything-keeps-the-three-day-food-reserve
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5} {:name "bread" :count 20}] :containers {"10,64,0" []}})
              result (await (tu/child-outcome eng job {:chest chest} 16))]
          (is (= {"bread" 12} (wt/inv p)) "bread 12 is the 60 points; dirt and the 8 spare bread go")
          (is (= {:gave-up false} result)))))))

(deftest deposit-gives-up-when-the-known-chest-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (wt/setup {:inventory [{:name "dirt" :count 5}]})]
          (wt/know-place! eng :chest chest)
          (is (= {:gave-up true :reason "missing" :status :stopped :moved 0}
                 (await (tu/child-outcome eng job {:items ["dirt"]} 8))))
          (is (some #(= :chest_missing (:kind %)) @seen))
          (is (= [] (:list (core/state eng)))))))))

(deftest deposit-waits-with-a-reason-when-no-chest-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (wt/setup {:inventory [{:name "dirt" :count 5}]})]
          (core/submit! eng (list job {:items ["dirt"]}) {})
          (core/tick! eng)
          (is (some #(and (= :waiting (:kind %)) (= :no-chest (:reason %))) @seen)))))))

(deftest deposit-walks-and-puts-every-stack-away-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-far {:inventory [{:name "dirt" :count 5} {:name "gravel" :count 7} {:name "sand" :count 2}]
                                          :containers {"20,64,0" []}})
              result (await (tu/child-outcome eng job {:chest {:x 20 :y 64 :z 0}} 1))]
          (is (= {} (wt/inv p)))
          (is (= {:gave-up false} result))
          (is (= 3 (count (wt/calls p "transfer"))))
          (is (= 1 (count (tu/walked-to eng)))))))))

(deftest deposit-into-a-chest-that-fills-stops-with-the-count-moved
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5} {:name "gravel" :count 7}] :containers {"10,64,0" []}})
              n (atom 0)]
          (.override (.-world p) "transfer"
                     (fn ^:async f [token args impl]
                       (if (= 1 (swap! n inc))
                         (await (impl token args))
                         #js {:status "full" :moved 0})))
          (let [result (await (tu/child-outcome eng job {:chest chest :items ["dirt" "gravel"]} 1))]
            (is (= {"gravel" 7} (wt/inv p)))
            (is (= {:gave-up true :reason "full" :status :stopped :moved 5} result))))))))

(defn timed-out
  "An override of a primitive that does the work, then answers an act timeout carrying the inventory change."
  [change]
  (fn ^:async f [token args impl]
    (await (impl token args))
    #js {:status "timeout" :inventoryChange (clj->js change)}))

(deftest deposit-counts-what-moved-before-a-transfer-timed-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5}] :containers {"10,64,0" []}})]
          (.override (.-world p) "transfer" (timed-out {"dirt" -5}))
          (let [result (await (tu/child-outcome eng job {:chest chest :items ["dirt"]} 12))]
            (is (= {:gave-up false} result))
            (is (= {} (wt/inv p)))
            (is (= 1 (count (wt/calls p "transfer"))))))))))

(deftest deposit-gives-up-on-a-timeout-that-moved-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5}] :containers {"10,64,0" []}})]
          (.override (.-world p) "transfer" (fn ^:async f [_ _ _] #js {:status "timeout"}))
          (let [result (await (tu/child-outcome eng job {:chest chest :items ["dirt"]} 12))]
            (is (= true (:gave-up result)))))))))

;; ------------------------------------------------------------------ storage group: a full chest, then a new one

(defn full-at
  "Make the transfers into the chest at cell answer \"full\" with nothing moved."
  [p cell]
  (.override (.-world p) "transfer"
             (fn ^:async f [token args impl]
               (if (and (= (.-x (.-pos args)) (first cell)) (= (.-z (.-pos args)) (last cell)))
                 #js {:status "full" :moved 0}
                 (await (impl token args))))))

(defn group-cells [eng]
  (set (map (comp (juxt :x :y :z) :pos :data) (mem/entries (mem/view (:store eng)) :storage/chests))))

(defn chests-in [p]
  (set (keep (fn [[k v]] (when (= "chest" v) k)) (:blocks @(fake/state p)))))

(deftest deposit-places-a-new-chest-beside-a-full-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-far {:inventory [{:name "dirt" :count 5} {:name "chest" :count 1}]
                                          :blocks {"10,64,0" "chest"} :containers {"10,64,0" [] "11,64,0" []}})]
          (wt/know-place! eng :chest chest)
          (full-at p [10 64 0])
          (let [result (await (tu/child-outcome eng job {:items ["dirt"]} 30))
                new-cells (disj (chests-in p) [10 64 0])]
            (is (= {:gave-up false} result))
            (is (= {} (wt/inv p)))
            (is (= 1 (count new-cells)))
            (is (= 1 (js/Math.abs (apply + (map - (first new-cells) [10 64 0]))))
                "the new chest stands beside the full one")
            (is (= (conj new-cells [10 64 0]) (conj (group-cells eng) [10 64 0])) "the group remembers it")))))))

(deftest deposit-uses-the-next-chest-of-the-group-before-making-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-far {:inventory [{:name "dirt" :count 5} {:name "chest" :count 1}]
                                          :blocks {"10,64,0" "chest" "11,64,0" "chest"}
                                          :containers {"10,64,0" [] "11,64,0" []}})]
          (wt/know-place! eng :chest chest)
          (mem/write! (:store eng) :storage/chests {:pos {:x 11 :y 64 :z 0}} {:cap 16 :ttl :forever})
          (full-at p [10 64 0])
          (let [result (await (tu/child-outcome eng job {:items ["dirt"]} 30))]
            (is (= {:gave-up false} result))
            (is (= {"chest" 1} (wt/inv p)) "no chest was placed")
            (is (= #{[10 64 0] [11 64 0]} (chests-in p)))
            (is (= [{"dirt" 5}] [(into {} (map (juxt :name :count)) (get-in @(fake/state p) [:containers [11 64 0]]))]))))))))

(deftest deposit-stops-with-a-reason-when-the-chests-are-full-and-making-is-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-far {:inventory [{:name "dirt" :count 5} {:name "chest" :count 1}]
                                          :blocks {"10,64,0" "chest"} :containers {"10,64,0" []}})]
          (wt/know-place! eng :chest chest)
          (full-at p [10 64 0])
          (let [result (await (tu/child-outcome eng job {:items ["dirt"] :make-chests? false} 30))]
            (is (= {:gave-up true :reason "all-chests-full" :status :stopped :moved 0} result))
            (is (= #{[10 64 0]} (chests-in p)))))))))

(deftest deposit-stops-when-a-new-chest-cannot-be-had
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-far {:inventory [{:name "dirt" :count 5}]
                                          :blocks {"10,64,0" "chest"} :containers {"10,64,0" []}})]
          (wt/know-place! eng :chest chest)
          (full-at p [10 64 0])
          (let [result (await (tu/child-outcome eng job {:items ["dirt"]} 60))]
            (is (= true (:gave-up result)))
            (is (= :stopped (:status result)))
            (is (= "no-chest-item" (:reason result)))
            (is (= #{[10 64 0]} (chests-in p)))))))))

;; ------------------------------------------------------------------ chest making: spots, restarts, group upkeep

(def other-zone {:name "farm" :owner "Miles"})

(defn setup-zoned
  "setup-far with an engine world holding zones."
  [world zones]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (world/of-data {} {} zones)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(def one-chest-world {:inventory [{:name "dirt" :count 5} {:name "chest" :count 1}]
                      :blocks {"10,64,0" "chest"} :containers {"10,64,0" [] "9,64,0" []}})

(deftest deposit-skips-a-spot-in-anothers-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup-zoned one-chest-world [(assoc other-zone :min [11 60 0] :max [11 70 0])])
              {:keys [eng p]} env]
          (wt/know-place! eng :chest chest)
          (full-at p [10 64 0])
          (let [result (await (tu/child-outcome eng job {:items ["dirt"]} 40))
                new-cells (disj (chests-in p) [10 64 0])]
            (is (= {:gave-up false} result))
            (is (= 1 (count new-cells)))
            (is (not (contains? new-cells [11 64 0])) "the zoned cell got no chest")))))))

(deftest deposit-tries-the-next-spot-when-the-first-is-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-far one-chest-world)
              place-fn (get registry/jobs 'jobs.blocks.place)]
          (wt/know-place! eng :chest chest)
          (full-at p [10 64 0])
          ;; the first spot answers declined once (as a place check that waits would)
          (let [refused (atom #{})
                eng (assoc-in eng [:jobs 'jobs.blocks.place]
                              (assoc place-fn :check
                                     (fn [c]
                                       (let [pos (storage/cell-of (:pos (:args c)))]
                                         (if (= [11 64 0] pos)
                                           (do (swap! refused conj pos) (ctx/wait c {:reason :not-allowed :pos pos}))
                                           ((:check place-fn) c))))))
                result (await (tu/child-outcome eng job {:items ["dirt"]} 60))
                new-cells (disj (chests-in p) [10 64 0])]
            (is (= {:gave-up false} result))
            (is (= #{[11 64 0]} @refused))
            (is (= 1 (count new-cells)))
            (is (not (contains? new-cells [11 64 0])) "the next cell took the chest")))))))

(deftest deposit-stops-with-no-spot-when-every-spot-is-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-far one-chest-world)
              place-fn (get registry/jobs 'jobs.blocks.place)]
          (wt/know-place! eng :chest chest)
          (full-at p [10 64 0])
          (let [eng (assoc-in eng [:jobs 'jobs.blocks.place]
                              (assoc place-fn :check (fn [c] (ctx/wait c {:reason :not-allowed :pos (storage/cell-of (:pos (:args c)))}))))
                result (await (tu/child-outcome eng job {:items ["dirt"]} 80))]
            (is (= "no-chest-spot" (:reason result)))
            (is (= #{[10 64 0]} (chests-in p)))))))))

(deftest deposit-with-fetching-off-does-not-fetch-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-far (assoc one-chest-world :inventory [{:name "dirt" :count 5} {:name "oak_planks" :count 8}]))]
          (wt/know-place! eng :chest chest)
          (full-at p [10 64 0])
          (let [result (await (tu/child-outcome eng job {:items ["dirt"] :fetch-chests? false} 60))]
            (is (= "no-chest-item" (:reason result)))
            (is (empty? (wt/calls p "craft")) "no crafting")
            (is (= #{[10 64 0]} (chests-in p)))))))))

(deftest deposit-restarted-with-its-chest-already-placed-joins-it-to-the-group
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-far {:inventory [{:name "dirt" :count 5} {:name "chest" :count 1}]
                                          :blocks {"10,64,0" "chest" "11,64,0" "chest"}
                                          :containers {"10,64,0" [] "11,64,0" []}})
              deposit (get registry/jobs job)
              seeded (assoc deposit :round
                            (fn [c]
                              (when-not (:seeded (ctx/mem c))
                                (ctx/update-mem! c assoc :seeded true :chest-spot {:x 11 :y 64 :z 0} :full #{[10 64 0]}))
                              ((:round deposit) c)))
              eng (assoc-in eng [:jobs job] seeded)]
          (wt/know-place! eng :chest chest)
          (full-at p [10 64 0])
          (let [result (await (tu/child-outcome eng job {:items ["dirt"]} 30))]
            (is (= {:gave-up false} result))
            (is (= {"chest" 1} (wt/inv p)) "no second chest was placed")
            (is (= #{[11 64 0]} (group-cells eng)))))))))

(deftest deposit-drops-a-group-chest-that-answers-missing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-far {:inventory [{:name "dirt" :count 5}]
                                          :blocks {"10,64,0" "chest" "11,64,0" "chest"}
                                          :containers {"10,64,0" [] "11,64,0" []}})]
          (wt/know-place! eng :chest chest)
          (mem/write! (:store eng) :storage/chests {:pos {:x 11 :y 64 :z 0}} {:cap 16 :ttl :forever})
          (.override (.-world p) "transfer"
                     (fn ^:async f [token args impl]
                       (case (.-x (.-pos args))
                         10 #js {:status "full" :moved 0}
                         11 #js {:status "missing" :moved 0}
                         (await (impl token args)))))
          (await (tu/child-outcome eng job {:items ["dirt"]} 40))
          (is (= #{} (group-cells eng)) "the missing chest left the group"))))))

(deftest deposit-uses-a-chest-outside-the-group-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-far {:inventory [{:name "dirt" :count 5} {:name "chest" :count 1}]
                                          :blocks {"10,64,0" "chest" "20,64,0" "chest"}
                                          :containers {"10,64,0" [] "20,64,0" []}})]
          (wt/know-place! eng :chest chest)
          (full-at p [20 64 0])
          (let [result (await (tu/child-outcome eng job {:chest {:x 20 :y 64 :z 0} :items ["dirt"] :make-chests? true} 40))]
            (is (true? (:gave-up result)))
            (is (= #{[10 64 0] [20 64 0]} (chests-in p)) "no chest was made, none of the group used")
            (is (empty? (get-in @(fake/state p) [:containers [10 64 0]])))))))))

(defn chest-place-of [eng]
  (some-> (mem/place (mem/view (:store eng)) :chest) (select-keys [:x :y :z])))

(deftest a-deposit-into-anothers-zoned-chest-does-not-become-the-chest-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup-zoned one-chest-world [(assoc other-zone :min [10 60 0] :max [10 70 0])])
              result (await (tu/child-outcome eng job {:chest chest :items ["dirt"] :ignore-zones? true} 12))]
          (is (= {:gave-up false} result))
          (is (nil? (chest-place-of eng))))))))

(deftest a-deposit-into-an-unzoned-chest-becomes-the-chest-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup-zoned one-chest-world [])
              result (await (tu/child-outcome eng job {:chest chest :items ["dirt"] :ignore-zones? true} 12))]
          (is (= {:gave-up false} result))
          (is (= {:x 10 :y 64 :z 0} (chest-place-of eng))))))))
