(ns engine.recover-drops-danger-test
  "recover-drops against the mobs that killed the body: the killer is priced at the pile, and a trip cut by hostiles
  over and over is given up."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.death-test :as dt :refer [die! job recovered recovered-trip pile-drops fetching-events]]
            [engine.library-test :refer [setup]]
            [jobs.lib.ledger :as ledger]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [run-until-empty]]
            [triggers.survival.died :as died]))

(defn fetch-run
  "Die with the field-report pile and cause, run the job to its decision: the :recover-drops.decided or .fetching event."
  ([cause] (fetch-run cause {}))
  ([cause body]
  (let [pile dt/death-pile-of-the-field-report]
    (fn ^:async t []
      (let [{:keys [eng seen]} (setup (merge {:floor tu/walk-floor :entities (vec (pile-drops pile))} body))]
        (die! eng (cond-> {:pos dt/death-pos :inventory pile} cause (assoc :cause cause)))
        (core/submit! eng job {})
        (await (run-until-empty eng 20))
        {:eng eng :seen seen})))))

(defn decided-text [seen]
  (:text (first (filter #(contains? #{:recover-drops.fetching :recover-drops.decided} (:kind %)) @seen))))

(deftest a-skeleton-killer-is-priced-at-the-pile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [with-cause (await ((fetch-run "skeleton")))
              without (await ((fetch-run nil)))]
          (is (re-find #"danger [1-9][0-9.]*,.*: skeleton" (decided-text (:seen with-cause))))
          (is (re-find #"danger 0," (decided-text (:seen without)))))))))

(deftest a-damage-type-cause-adds-no-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await ((fetch-run "fall")))]
          (is (re-find #"danger 0," (decided-text (:seen r)))))))))

(def iron-kit
  {:inventory [{:name "iron_sword" :count 1}]
   :equipment {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"} :legs {:name "iron_leggings"} :feet {:name "iron_boots"}}})

(deftest an-unarmed-body-skips-a-pile-its-killer-guards
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await ((fetch-run "skeleton")))]
          (is (= :skip (:decision (recovered eng))))
          (is (re-find #"skip: .*danger [1-9][0-9.]*,.*: skeleton" (decided-text seen))))))))

(deftest a-body-with-an-iron-sword-and-armour-goes-for-the-guarded-pile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await ((fetch-run "skeleton" iron-kit)))]
          (is (re-find #"^fetch: " (decided-text seen))))))))

;; ------------------------------------------------------------ cut trips

(defn death-t [eng] (:t (mem/latest (mem/view (:store eng)) :died)))

(defn resume-run
  "A trip decided and cut before: a saved :recover-trip with `trip` merged in, `before` run against the engine (a
  :hostile entry written after the save, say), then the job's real resume through key-to-death!."
  ([trip] (resume-run trip (fn [_])))
  ([trip before]
   (fn ^:async t []
     (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities (vec (pile-drops dt/diamonds))})]
       (die! eng {:pos dt/death-pos :inventory dt/diamonds})
       (mem/write! (:store eng)  :recover-trip
                   (merge {:death-t (death-t eng) :decided {:value 1400 :cost 50} :baseline {} :phase :go} trip)
                   {:cap 1 :ttl 1000000})
       (before eng)
       (core/submit! eng job {})
       (await (run-until-empty eng 20))
       {:eng eng :seen seen}))))

(defn hostile-cut! [eng] (mem/write! (:store eng) :hostile {:name "zombie"} {:cap 5 :ttl 1000000}))

(deftest a-trip-cut-by-hostiles-twice-is-abandoned-as-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((resume-run {:cuts 1} hostile-cut!)))]
          (is (= {:decision :abandoned :reason :danger} (select-keys (recovered eng) [:decision :reason])))
          (is (false? (died/died nil (mem/view (:store eng)) {})) "the died trigger stops firing"))))))

(deftest hostile-cuts-count-while-the-trip-is-blocked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((resume-run {:cuts 1 :blocked true} hostile-cut!)))]
          (is (= {:decision :abandoned :reason :danger} (select-keys (recovered eng) [:decision :reason]))))))))

(deftest benign-cuts-are-not-hostile-cuts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((resume-run {:cuts 1})))]
          (is (= :collected (:decision (recovered eng)))))))))

(deftest a-trip-that-did-not-arrive-too-often-is-abandoned-as-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((resume-run {:blocked true :blocked-retries 3})))]
          (is (= {:decision :abandoned :reason :unreachable} (select-keys (recovered eng) [:decision :reason]))))))))

(deftest a-trip-cut-once-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((resume-run {:cuts 0} hostile-cut!)))]
          (is (= :collected (:decision (recovered eng)))))))))

(deftest a-pile-lost-to-drowning-is-priced-as-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await ((fetch-run "drowning")))]
          (is (re-find #"danger [1-9][0-9.]*,.*: drowning" (decided-text (:seen r)))))))))

(deftest a-pile-under-water-is-priced-as-water-whatever-the-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wet (into {} (for [y [64 65]] [(str (:x dt/death-pos) "," y "," (:z dt/death-pos)) "water"]))
              r (await ((fetch-run "fall" {:blocks wet})))]
          (is (re-find #"danger [1-9][0-9.]*,.*: water" (decided-text (:seen r)))))))))

(deftest a-second-run-after-a-skip-says-the-death-was-decided
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((fetch-run "skeleton")))
              id (core/submit! eng job {})]
          (await (core/tick! eng))
          (let [w (core/waiting eng id)]
            (is (= [:no-drops :skip] ((juxt :reason :decision) w)) (pr-str w))
            (is (re-find #"already decided: skip: value .*cost" (:text w)) (pr-str w))))))))

(def plug-cells
  "The two dirt cells the body placed in the mouth of a stone cell round the death point."
  [[19 64 0] [19 65 0]])

(def stone-cell
  "Stone round the death point's two interior cells, but for the mouth towards the body (west)."
  (for [x (range 19 22) z [-1 0 1] y [64 65 66]
        :when (not (and (= x 20) (= z 0) (#{64 65} y)))
        :when (not (some #{[x y z]} plug-cells))]
    [x y z]))

(deftest a-pile-inside-the-bodys-own-seal-is-dug-out-and-recovered
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile dt/death-pile-of-the-field-report
              blocks (merge (into {} (map (fn [[x y z]] [(str x "," y "," z) "stone"])) stone-cell)
                            (into {} (map (fn [[x y z]] [(str x "," y "," z) "dirt"])) plug-cells))
              {:keys [eng]} (setup {:floor tu/walk-floor :blocks blocks :entities (vec (pile-drops pile))})]
          (ledger/write! (:store eng) (mapv (fn [cell] {:cell cell :item "dirt" :before "air" :job "j9" :purpose :retreat-plug :state :placed}) plug-cells))
          (die! eng {:pos dt/death-pos :inventory pile})
          (core/submit! eng job {})
          (await (run-until-empty eng 40))
          (is (= :collected (:decision (recovered eng))) (pr-str (recovered eng))))))))

(defn sealed-run
  "The field-report pile inside the stone cell closed by the plug cells; extra ledger entries and world spec merged in."
  [extra-entries spec]
  (let [pile dt/death-pile-of-the-field-report
        blocks (merge (into {} (map (fn [[x y z]] [(str x "," y "," z) "stone"])) stone-cell)
                      (into {} (map (fn [[x y z]] [(str x "," y "," z) "dirt"])) plug-cells)
                      (:blocks spec))
        {:keys [eng seen]} (setup (merge {:floor tu/walk-floor :entities (vec (pile-drops pile))} spec {:blocks blocks}))]
    (ledger/write! (:store eng) (into (mapv (fn [cell] {:cell cell :item "dirt" :before "air" :job "j9" :purpose :retreat-plug :state :placed}) plug-cells)
                                      extra-entries))
    (die! eng {:pos dt/death-pos :inventory pile})
    (core/submit! eng job {})
    {:eng eng :seen seen}))

(deftest the-seal-cleanup-takes-back-only-blocks-near-the-pile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far [40 64 0]
              {:keys [eng]} (sealed-run [{:cell far :item "dirt" :before "air" :job "j8" :purpose :bridge :state :placed}]
                                        {:blocks {"40,64,0" "dirt"}})]
          (await (run-until-empty eng 40))
          (is (= :collected (:decision (recovered eng))) (pr-str (recovered eng)))
          (is (= [far] (mapv :cell (ledger/open-entries (mem/view (:store eng))))) "the far scaffold block is still there"))))))
