(ns engine.till-test
  "jobs.farm.till against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.expr :as expr]
            [engine.fake :as fake]
            [engine.go-to-escalate-support :as pit-world]
            [engine.hostile-test :as h]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]
            [jobs.farm.till :as till]))

(defn setup [world & [shared]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (or shared (ew/of-data {} {} []))
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn block-at [p pos] (.-name (.blockAt p (clj->js pos))))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn ^:async child-outcome
  "tu/child-outcome without the ending the adapter adds, and without :status and :text when it is :done."
  [eng job args n]
  (let [r (await (tu/child-outcome eng job args n))]
    (if (map? r) (cond-> (dissoc r :ending) (= :done (:status r)) (dissoc :status :text)) r)))

(def job 'jobs.farm.till)
(def hoe [{:name "stone_hoe" :count 1}])
(def box {:from {:x 1 :y 63 :z 1} :to {:x 2 :y 63 :z 2}})
(def four {"1,63,1" "dirt" "2,63,1" "grass_block" "1,63,2" "dirt" "2,63,2" "grass_block"})

(deftest cells-of-a-box-any-corner-order
  (are [args expected] (= expected (set (till/cells args)))
    {:from {:x 0 :y 5 :z 0} :to {:x 1 :y 5 :z 1}} #{{:x 0 :y 5 :z 0} {:x 1 :y 5 :z 0} {:x 0 :y 5 :z 1} {:x 1 :y 5 :z 1}}
    {:from {:x 1 :y 5 :z 1} :to {:x 0 :y 5 :z 0}} #{{:x 0 :y 5 :z 0} {:x 1 :y 5 :z 0} {:x 0 :y 5 :z 1} {:x 1 :y 5 :z 1}}
    {:center {:x 3 :y 7 :z 3} :radius 0} #{{:x 3 :y 7 :z 3}}))

(deftest cells-of-a-radius-is-the-square-at-center-y
  (let [cs (till/cells {:center {:x 10 :y 63 :z -4} :radius 2})]
    (is (= 25 (count cs)))
    (is (= #{63} (set (map :y cs))))
    (is (= #{8 9 10 11 12} (set (map :x cs))))
    (is (= #{-6 -5 -4 -3 -2} (set (map :z cs))))))

(deftest cells-allows-256-and-rejects-more-or-nothing
  (is (= 225 (count (till/cells {:center {:x 0 :y 63 :z 0} :radius 7}))) "15x15")
  (is (= 256 (count (till/cells {:from {:x 0 :y 63 :z 0} :to {:x 15 :y 63 :z 15}}))))
  (are [args] (thrown? js/Error (till/cells args))
    {:from {:x 0 :y 63 :z 0} :to {:x 16 :y 63 :z 15}}
    {:center {:x 0 :y 63 :z 0} :radius 8}
    {}
    {:from {:x 0 :y 63 :z 0}}
    {:center {:x 0 :y 63 :z 0}}))

(deftest till-tills-a-two-by-two-box
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory hoe :blocks four})
              result (await (child-outcome eng job box 12))]
          (is (= {:tilled 4 :skipped {}} result))
          (is (= ["farmland"] (distinct (map #(block-at p %) (till/cells box)))))
          (is (= 4 (count (calls p "useOn")))))))))

(deftest each-hoe-click-is-a-blocks-use-on-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clicks (atom 0)
              use-on (get registry/jobs 'jobs.blocks.use-on)
              stub (assoc use-on :round (fn [c] (swap! clicks inc) ((:round use-on) c)))
              clock (atom 1000000)
              [_ sink] (tu/legacy-capture-sink)
              p (tu/fake {:inventory hoe :blocks four})
              eng (core/create {:primitives p :jobs (assoc registry/jobs 'jobs.blocks.use-on stub) :triggers triggers/all
                                :dir (tu/tmp-dir) :now #(deref clock) :world (ew/of-data {} {} [])
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (await (child-outcome eng job box 12))
          (is (= 4 @clicks)))))))

(deftest till-is-one-call-the-whole-box
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks four})
              result (await (child-outcome eng job box 1))]
          (is (= {:tilled 4 :skipped {}} result))
          (is (= 4 (count (calls p "useOn")))))))))

(deftest till-radius-mode-tills-the-square
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cs (till/cells {:center {:x 0 :y 63 :z 0} :radius 1})
              {:keys [eng p]} (setup {:inventory hoe :blocks (into {} (map (fn [{:keys [x y z]}] [(str x "," y "," z) "dirt"])) cs)})
              result (await (child-outcome eng job {:center {:x 0 :y 63 :z 0} :radius 1} 20))]
          (is (= 9 (:tilled result)))
          (is (= ["farmland"] (distinct (map #(block-at p %) cs)))))))))

(deftest till-leaves-farmland-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks (zipmap (keys four) (repeat "farmland"))})
              result (await (child-outcome eng job box 6))]
          (is (= {:already? true :tilled 0 :skipped {}} result) "already farmland: done at once (done?), its result the data")
          (is (empty? (calls p "useOn"))))))))

(deftest till-skips-what-is-not-tillable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:inventory hoe :blocks (assoc four "1,63,1" "stone")})
              result (await (child-outcome eng job box 12))]
          (is (= {:tilled 3 :skipped {{:x 1 :y 63 :z 1} :not-tillable}} result))
          (is (= [:debug] (distinct (map :level (kinds seen :till.skipped)))) "per-cell skips are debug: the ending lists them"))))))

(deftest till-skips-a-cell-covered-by-a-solid-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory hoe :blocks (assoc four "1,64,1" "cobblestone")})
              result (await (child-outcome eng job box 12))]
          (is (= {:tilled 3 :skipped {{:x 1 :y 63 :z 1} :covered}} result)))))))

(deftest till-digs-ground-cover-then-tills
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks (assoc four "1,64,1" "short_grass")})
              result (await (child-outcome eng job box 14))]
          (is (= {:tilled 4 :skipped {}} result))
          (is (= 1 (count (calls p "dig"))))
          (is (= "farmland" (block-at p {:x 1 :y 63 :z 1}))))))))

(deftest till-digs-a-flower-over-a-cell-then-tills
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks (assoc four "1,64,1" "poppy" "2,64,2" "sweet_berry_bush")})
              result (await (child-outcome eng job box 20))]
          (is (= {:tilled 3 :skipped {{:x 2 :y 63 :z 2} :covered}} result) "a poppy is cut; a berry bush is no cover")
          (is (= "farmland" (block-at p {:x 1 :y 63 :z 1}))))))))

(deftest till-without-a-hoe-and-with-fetch-false-the-check-is-false
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks four})]
          (core/submit! eng (list job (assoc box :ways #{:carried})) {})
          (is (nil? (core/tick! eng)) "check false: nothing runs")
          (is (empty? (calls p "useOn"))))))))

(deftest till-gives-up-on-an-unreachable-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks {"5,63,0" "dirt"} :unreachable ["5,63,0"]})
              args {:from {:x 5 :y 63 :z 0} :to {:x 5 :y 63 :z 0}}
              result (await (child-outcome eng job args 8))]
          (is (= {:status :stopped :reason :unreachable :tilled 0 :skipped {{:x 5 :y 63 :z 0} :unreachable}} (dissoc result :text)))
          (is (re-find #"1 unreachable" (:text result)))
          (is (empty? (calls p "useOn"))))))))

(deftest till-from-a-pit-gets-out-and-tills
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (merge pit-world/in-pit {:blocks (assoc pit-world/pit "8,63,0" "dirt")
                                                              :inventory (into hoe [{:name "dirt" :count 5}])}))
              args {:from {:x 8 :y 63 :z 0} :to {:x 8 :y 63 :z 0}}
              result (await (child-outcome eng job args 300))]
          (is (= {:tilled 1 :skipped {}} result))
          (is (= "farmland" (block-at p {:x 8 :y 63 :z 0}))))))))

(deftest till-skips-a-cell-the-use-refuses-after-two-tries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks {"1,63,1" "dirt"}})
              args {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}}]
          (.override (.-world p) "useOn" (fn ^:async f [_ _ _] #js {:status "unchanged" :before #js {:name "dirt"} :after #js {:name "dirt"} :consumed 0}))
          (is (= {:status :stopped :reason :refused :tilled 0 :skipped {{:x 1 :y 63 :z 1} :refused}} (dissoc (await (child-outcome eng job args 8)) :text)))
          (is (= 2 (count (calls p "useOn")))))))))

(deftest a-refused-till-is-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks {"1,63,1" "dirt"}})
              args {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}}]
          (.override (.-world p) "useOn" (fn ^:async f [_ _ _] #js {:status "unchanged" :before #js {:name "dirt"} :after #js {:name "dirt"} :consumed 0}))
          (is (= :condition-failed (get-in (await (tu/child-outcome eng job args 8)) [:ending :kind]))))))))

(deftest till-keeps-its-count-across-a-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks four})
              world (.-world p)
              out (atom nil)
              parent (tu/recording-parent job box out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (core/submit! eng '(recording-parent) {})
          (.hold world "useOn")
          (let [running (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (is (= 1 (count (calls p "useOn"))) "the first use held")
            (core/cut! eng (core/holder eng) :test nil)
            (await running))
          (await (run-until-empty eng 12))
          (is (= {:tilled 4 :skipped {}} (dissoc @out :status :text)))
          (is (= ["farmland"] (distinct (map #(block-at p %) (till/cells box))))))))))


(deftest till-tills-a-cell-the-world-turns-back-and-counts-it-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks {"1,63,1" "dirt"}})
              args {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}}
              first? (atom true)]
          (.override (.-world p) "useOn"
                     (fn ^:async f [token a impl]
                       (let [r (await (impl token a))]
                         (when @first?
                           (reset! first? false)
                           (fake/set-block! p [1 63 1] "dirt"))
                         r)))
          (is (= {:tilled 1 :skipped {}} (await (child-outcome eng job args 10))))
          (is (= 2 (count (calls p "useOn")))))))))

(deftest till-skips-a-cell-whose-cover-will-not-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks {"1,63,1" "dirt" "1,64,1" "short_grass"}})
              args {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}}]
          (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "unreachable"}))
          (is (= {:status :stopped :reason :cover-stuck :tilled 0 :skipped {{:x 1 :y 63 :z 1} :cover-stuck}} (dissoc (await (child-outcome eng job args 10)) :text)))
          (is (= 2 (count (calls p "dig"))) "each of the two tries digs again"))))))

(defn counting-ctx
  "A ctx for till/check over the fake world; reads is an atom counting blockAt calls per cell."
  [world args reads]
  (let [p (tu/fake world)
        orig (.bind (.-blockAt p) p)]
    (set! (.-blockAt p) (fn [pos] (swap! reads update (js/JSON.stringify pos) (fnil inc 0)) (orig pos)))
    {:primitives p :args args :view (constantly {}) :root "till-check" :slots [] :engine {:world (ew/of-data {} {} [])}}))

(def sixteen {:from {:x 0 :y 63 :z 0} :to {:x 15 :y 63 :z 15}})
(def sixteen-cells (till/cells sixteen))

(deftest check-stops-at-the-first-cell-that-needs-work
  (let [reads (atom {})
        c (counting-ctx {:inventory hoe :blocks (into {} (map (fn [{:keys [x y z]}] [(str x "," y "," z) "dirt"]) sixteen-cells))}
                        sixteen reads)]
    (is (nil? (till/problem c)))
    (is (<= (reduce + (vals @reads)) 1))))

(deftest check-reads-each-cell-at-most-once-when-all-is-farmland
  (let [reads (atom {})
        c (counting-ctx {:blocks (into {} (map (fn [{:keys [x y z]}] [(str x "," y "," z) "farmland"]) sixteen-cells))}
                        sixteen reads)]
    (is (nil? (till/problem c)) "nothing pending passes without a hoe")
    (is (<= (reduce + (vals @reads)) 256))
    (is (every? #(= 1 %) (vals @reads)))))

;; ------------------------------------------------------------------ zones and claims

(def zone-over-one {:name "farm" :min [1 60 1] :max [1 70 1]})

(deftest a-cell-follows-its-zone-owner-and-the-opt-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra tilled declined] [["Fake" {} 4 []] ["FAKE" {} 4 []] ["Miles" {} 3 [{:reason :refused :zones ["farm"]}]]
                                                 ["Miles" {:ignore-zones? true} 4 []]]]
          (let [{:keys [eng seen]} (setup {:inventory hoe :blocks four} (ew/of-data {} {} [(assoc zone-over-one :owner owner)]))
                result (await (child-outcome eng job (merge box extra) 20))]
            (is (= tilled (:tilled result)) (pr-str [owner extra]))
            (is (= declined
                   (mapv #(select-keys % [:reason :zones]) (kinds seen :till.declined))) "the round warns when it acts on a refusal")))))))

(deftest no-zone-list-declines-the-till
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory hoe :blocks four} (ew/of-data {} {} nil))]
          (core/submit! eng (list job box) {})
          (await (run-until-empty eng 10))
          (is (empty? (calls p "useOn")))
          (is (empty? (kinds seen :till.declined))))))))

(deftest a-box-wholly-in-anothers-zone-stays-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory hoe :blocks four}
                                          (ew/of-data {} {} [{:name "farm" :min [1 60 1] :max [2 70 2] :owner "Miles"}]))]
          (core/submit! eng (list job box) {})
          (await (run-until-empty eng 5))
          (is (= 1 (count (:list (core/state eng)))) "waits, does not end")
          (is (empty? (calls p "useOn")))
          (is (empty? (kinds seen :till.declined))))))))

(deftest a-cover-dug-in-anothers-zone-with-the-opt-out-is-recorded-for-tidying
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory hoe :blocks (assoc four "1,64,1" "short_grass")}
                                   (ew/of-data {} {} [(assoc zone-over-one :owner "Miles")]))
              result (await (child-outcome eng job (assoc box :ignore-zones? true) 16))]
          (is (= 4 (:tilled result)))
          (is (= [[1 64 1]] (mapv #(:cell (:data %)) (mem/entries (mem/view (:store eng)) :tidy)))))))))

(deftest till-spec-takes-vector-corners-and-refuses-malformed-ones
  (is (= {:from {:x 1 :y 64 :z 2} :to {:x 3 :y 64 :z 4}}
         (select-keys (:args (expr/parse registry/jobs '(jobs.farm.till {:from [1 64 2] :to [3 64 4]}))) [:from :to])))
  (is (re-find #":from must be \[x y z\]" (expr/problem registry/jobs '(jobs.farm.till {:from [1 64] :to [3 64 4]})))))

(deftest a-hoe-that-vanishes-at-the-use-counts-a-try-and-the-cell-is-skipped-as-no-hoe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks four})]
          (.override (.-world p) "useOn" (fn ^:async f [_token _args _impl] #js {:status "no-item"}))
          (let [result (await (child-outcome eng job box 40))]
            (is (= {:status :stopped :reason :no-hoe :tilled 0
                    :skipped {{:x 1 :y 63 :z 1} :no-hoe {:x 2 :y 63 :z 1} :no-hoe
                              {:x 1 :y 63 :z 2} :no-hoe {:x 2 :y 63 :z 2} :no-hoe}}
                   (dissoc result :text)))
            (is (re-find #"no hoe" (:text result)))))))))

(deftest a-hoe-lost-mid-run-with-fetch-false-skips-the-rest-as-no-hoe-without-fetching
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory hoe :blocks four})
              first? (atom true)]
          (.override (.-world p) "useOn" (fn ^:async f [t a impl]
                                           (let [r (await (impl t a))]
                                             (when @first?
                                               (reset! first? false)
                                               (swap! (fake/state p) assoc :inventory []))
                                             r)))
          (let [result (await (child-outcome eng job (assoc box :ways #{:carried}) 40))]
            (is (= 1 (:tilled result)))
            (is (= [:no-hoe :no-hoe :no-hoe] (vec (vals (:skipped result)))))
            (is (empty? (filterv #(= :need (:slot %)) (kinds seen :child_started))))))))))

(deftest till-of-only-not-tillable-cells-stops-not-tillable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks {"1,63,1" "stone" "2,63,1" "stone"}})
              args {:from {:x 1 :y 63 :z 1} :to {:x 2 :y 63 :z 1}}
              result (await (child-outcome eng job args 8))]
          (is (= :stopped (:status result)))
          (is (= :not-tillable (:reason result)))
          (is (re-find #"2 not-tillable" (:text result)))
          (is (empty? (calls p "useOn"))))))))

(deftest till-of-mixed-skips-with-nothing-tilled-stops-nothing-tilled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory hoe :blocks {"1,63,1" "stone" "5,63,1" "dirt"} :unreachable ["5,63,1"]})
              args {:from {:x 1 :y 63 :z 1} :to {:x 5 :y 63 :z 1}}
              result (await (child-outcome eng job args 8))]
          (is (= :stopped (:status result)))
          (is (= :nothing-tilled (:reason result)))
          (is (re-find #"4 not-tillable, 1 unreachable" (:text result))))))))

(deftest till-with-no-hoe-and-nothing-to-fetch-stops-with-the-failed-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks four})
              result (await (child-outcome eng job box 80))]
          (is (= :stopped (:status result)))
          (is (= :tool (:reason result)))
          (is (= 0 (:tilled result))))))))

(def hoe-chest "-2,64,3")

(defn ^:async seeing-till
  "Submit till over four dirt cells in a world with a chest holding a hoe, the body seeing through perception; n ticks."
  [extra n]
  (let [s (h/setup-seeing {:blocks (assoc four hoe-chest "chest") :containers {hoe-chest [{:name "stone_hoe" :count 1}]}} nil)]
    (perception/pass! (aget (:p s) "perception"))
    (core/submit! (:eng s) (list job (merge box {:ignore-zones? true} extra)) {})
    (await (tu/run-ticks (:eng s) (:clock s) n 700))
    s))

(deftest till-fetches-a-hoe-from-a-seen-chest-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (seeing-till {} 120))]
          (is (every? #(= "farmland" (block-at p %)) [{:x 1 :y 63 :z 1} {:x 2 :y 63 :z 1} {:x 1 :y 63 :z 2} {:x 2 :y 63 :z 2}])))))))

(deftest till-fetch-is-a-child-the-agent-sees
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (seeing-till {} 120))
              started (filterv #(= :need (:slot %)) (kinds seen :child_started))
              ended (filterv #(= :need (:slot %)) (kinds seen :child_ended))]
          (is (= 1 (count started)))
          (is (= [:done] (mapv #(get-in % [:ending :kind]) ended))))))))

(deftest till-fetch-false-waits-no-tool-and-leaves-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng]} (await (seeing-till {:ways #{:carried}} 12))]
          (is (empty? (calls p "useOn")))
          (is (= 1 (count (:list (core/state eng)))) "still waiting")
          (is (= "chest" (block-at p {:x -2 :y 64 :z 3}))))))))

(deftest bad-args-are-never-with-bad-args-and-a-why
  (are [args] (let [[kind _ data] (till/bad-args (counting-ctx {:inventory hoe} args (atom {})))]
                (and (= :never kind) (= :bad-args (:reason data)) (string? (:why data))))
    {}
    {:from {:x 0 :y 63 :z 0}}
    {:center {:x 0 :y 63 :z 0}}
    {:from {:x 0 :y 63 :z 0} :to {:x 1000000000 :y 63 :z 1000000000}}))

(deftest till-leaves-cells-in-the-footprint-of-its-own-plan-alone-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [author ["Fake" "Miles"]]
          (let [plan (shape/with-author {:id "hut" :parts [{:id "p" :box [[1 63 1] [2 63 2]] :want "stone"}]} author)
                {:keys [eng p]} (setup {:inventory hoe :blocks four} (ew/of-data {"hut" plan} {} []))]
            (await (child-outcome eng job box 4))
            (is (empty? (calls p "useOn")) author)))))))

(defn two-use-hoe
  "Models the game: a hoe on coarse or rooted dirt makes dirt, on dirt farmland."
  [p]
  (.override (.-world p) "useOn"
             (fn ^:async f [_ a _]
               (let [pos (js->clj (.-pos a) :keywordize-keys true)
                     before (block-at p pos)
                     after (if (#{"coarse_dirt" "rooted_dirt"} before) "dirt" "farmland")]
                 (fake/set-block! p [(:x pos) (:y pos) (:z pos)] after)
                 #js {:status "used" :before #js {:name before} :after #js {:name after} :consumed 0}))))

(deftest till-tills-coarse-and-rooted-dirt-with-two-hoe-uses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks (assoc four "1,63,1" "coarse_dirt" "2,63,1" "rooted_dirt")})
              _ (two-use-hoe p)
              result (await (child-outcome eng job box 20))]
          (is (= {:tilled 4 :skipped {}} result))
          (is (= 6 (count (calls p "useOn"))))
          (is (= ["farmland"] (distinct (map #(block-at p %) (till/cells box))))))))))

(deftest till-quote-counts-two-hoe-uses-for-coarse-and-rooted-dirt
  (let [{:keys [p]} (setup {:inventory hoe :blocks (assoc four "1,63,1" "coarse_dirt" "2,63,1" "rooted_dirt")})
        q (till/quote-of {:args box :primitives p})]
    (is (= {"stone_hoe" [6 6 6]} (get-in q [:uses :wear])))))
