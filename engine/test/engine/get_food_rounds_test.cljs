(ns engine.get-food-rounds-test
  "get-food as one whole attempt (job-rounds design): one round eats, or gets food and eats, or stops with a reason;
  a cut round resumes from the world; jobs.lib.child/run! runs a child to its end."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.child :as child]))

(defn setup
  "step-ms: the engine's clock moves on by that much each time it is read (time passing while a round runs)."
  [world & {:keys [step-ms] :or {step-ms 0}}]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake (merge {:floor tu/walk-floor} world)))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(swap! clock + step-ms)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn call-args [p name k] (mapv #(js->clj (aget (.-args %) k) :keywordize-keys true) (calls p name)))

(defn entries [eng kind] (mapv :data (mem/entries (mem/view (:store eng)) kind)))

(defn know-source! [eng pos kind]
  (mem/write! (:store eng) :food-source {:pos pos :kind kind} {:cap 5 :ttl :forever}))

(defn food [p] (.-food (.self p)))

(defn ended [seen]
  (filterv #(and (= :job (:source %)) (#{:stopped :completed} (:kind %))) @seen))

(defn ^:async one-round!
  "Submit get-food with args and run exactly one tick."
  [eng args]
  (core/submit! eng (list 'jobs.survival.get-food args) {})
  (await (core/tick! eng)))

(def cow {:id 7 :name "cow" :kind "passive" :pos {:x 5 :y 64 :z 0} :health 20 :drops [{:name "beef" :count 1}]})

(def farm
  {:blocks {"10,64,0" "carrots" "11,64,0" "carrots" "12,64,0" "carrots" "10,63,0" "farmland"}
   :ages {"10,64,0" 7 "11,64,0" 3 "12,64,0" 7}
   :drops {:carrots "carrot"}})

;; ---------------------------------------------------------------- one round is one whole attempt

(deftest one-round-eats-all-the-carried-food-it-needs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 3} :inventory [{:name "bread" :count 5}]})]
          (await (one-round! eng {}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (= 18 (food p)))
          (is (= [:completed] (mapv :kind (ended seen)))))))))

(deftest one-round-takes-food-from-a-known-chest-and-eats-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 0}
                                           :containers {"20,64,0" [{:name "cobblestone" :count 30} {:name "bread" :count 4}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (await (one-round! eng {}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (= ["bread"] (call-args p "transfer" "item")))
          (is (= 20 (food p)))
          (is (= [:completed] (mapv :kind (ended seen)))))))))

(deftest one-round-hunts-an-animal-and-eats-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 2} :entities [cow]})]
          (await (one-round! eng {:attack-gap-ms 0}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (= [7 7 7 7] (call-args p "attack" "id")))
          (is (= ["beef"] (call-args p "eat" "item")))
          (is (= 7 (food p))))))))

(deftest an-animal-that-only-comes-into-view-when-the-body-looks-around-is-hunted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 2}})
              look (.-look p)
              added (atom false)]
          (set! (.-look p) (fn [& args]
                             (when (compare-and-set! added false true) (fake/add-entity! p cow))
                             (.apply look p (to-array args))))
          (await (one-round! eng {:attack-gap-ms 0}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (= [7 7 7 7] (call-args p "attack" "id")) "it looked around before giving up")
          (is (= 7 (food p))))))))

(deftest the-attack-child-keeps-the-gap-between-swings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [gap [0 600]]
          (let [{:keys [eng p clock]} (setup {:self {:food 2} :entities [cow]} :step-ms 5)
                swings (atom [])]
            (.override (.-world p) "attack" (fn [token args impl] (swap! swings conj @clock) (impl token args)))
            (await (one-round! eng {:attack-gap-ms gap}))
            (is (= ["beef"] (call-args p "eat" "item")) (str "gap " gap ": the cow died and was eaten"))
            (is (= 4 (count @swings)) (str "gap " gap))
            (is (every? #(>= % gap) (map - (rest @swings) @swings)) (str "gap " gap ": swings spaced by the gap " @swings))))))))

(deftest a-chest-that-cannot-be-read-is-skipped-for-this-job-but-not-forgotten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 0}
                                      :containers {"20,64,0" [{:name "bread" :count 4}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (.override (.-world p) "inspectContainer" (fn [_ _ _] #js {:status "busy"}))
          (await (one-round! eng {}))
          (is (= 1 (count (calls p "inspectContainer"))) "not asked again in this job")
          (is (= [{:pos {:x 20 :y 64 :z 0} :kind :chest}] (entries eng :food-source)) "still remembered"))))))

(deftest a-chest-seen-gone-is-forgotten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:food 0}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (await (one-round! eng {}))
          (is (= [] (entries eng :food-source))))))))

(deftest one-round-harvests-the-ripe-crops-of-a-known-farm
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc farm :self {:food 0}))]
          (know-source! eng {:x 11 :y 64 :z 0} :farm)
          (await (one-round! eng {}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (= #{{:x 10 :y 64 :z 0} {:x 12 :y 64 :z 0}} (set (call-args p "dig" "pos"))) "only the ripe ones")
          (is (= 2 (count (calls p "eat")))))))))

(deftest one-round-with-no-food-anywhere-is-stopped-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 1}})]
          (await (one-round! eng {}))
          (is (= [] (:list (core/state eng))))
          (is (= [:stopped] (mapv :kind (ended seen))) "nothing found is not completed")
          (is (re-find #"no food" (str (:text (first (ended seen))))))
          (is (= 1 (count (filterv #(= :food.none (:kind %)) @seen))))
          (is (= 1 (count (entries eng :hungry)))))))))

(deftest a-way-that-fails-is-followed-by-the-next-in-the-same-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 0} :entities [cow]
                                      :containers {"20,64,0" [{:name "cobblestone" :count 3}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (await (one-round! eng {:attack-gap-ms 0}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (= [] (entries eng :food-source)) "the empty chest is forgotten")
          (is (= ["beef"] (call-args p "eat" "item")) "then it hunted"))))))

;; ---------------------------------------------------------------- as the hungry reflex

(defn reflex-events [seen kind] (filterv #(and (= :reflex (:source %)) (= kind (:kind %))) @seen))

(deftest the-hungry-reflex-runs-one-round-and-never-continues
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[world fed?] [[{:self {:food 3} :inventory [{:name "bread" :count 5}]} true]
                              [{:self {:food 3} :entities [cow]} true]
                              [{:self {:food 3}} false]]]
          (let [{:keys [eng p seen]} (setup world :step-ms 5)]
            (core/load-scenario! eng (scenario/parse "{:register [{:trigger :hungry}]}"))
            (await (core/tick! eng))
            (is (= 1 (count (reflex-events seen :ended))) (pr-str world))
            (is (= fed? (> (food p) 3)) (pr-str world))
            (is (= [] (reflex-events seen :continued)) "a reflex job that yields is a bug (C1 turns it into :declined)")))))))

;; ---------------------------------------------------------------- cut and resume

(defn ^:async cut-at-transfer!
  "Run a get-food round until it is held at its transfer, cut it by a takeover, call (between) and release."
  [eng p between]
  (.hold (.-world p) "transfer")
  (let [running (core/tick! eng)]
    (loop [i 0]
      (when (and (empty? (calls p "transfer")) (< i 400))
        (await (js/Promise. (fn [r] (js/setTimeout r 5))))
        (recur (inc i))))
    (takeover/take! eng {:who "claude" :why "cut"})
    (await running)
    (between)
    (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})))

(deftest a-cut-round-resumes-and-finishes-from-the-world
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 0} :containers {"20,64,0" [{:name "bread" :count 4}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (cut-at-transfer! eng p (fn [])))
          (is (= 1 (count (:list (core/state eng)))) "the cut job stays listed")
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "the resumed round finished")
          (is (= 20 (food p)))
          (is (= [:completed] (mapv :kind (ended seen)))))))))

(deftest a-resumed-round-finds-the-chest-emptied-meanwhile-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 0} :containers {"20,64,0" [{:name "bread" :count 4}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (cut-at-transfer! eng p #(swap! (fake/state p) assoc-in [:containers [20 64 0]] [])))
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= [:stopped] (mapv :kind (ended seen))) "no throw, a clear stop")
          (is (= [] (entries eng :food-source)))
          (is (= 0 (food p))))))))

;; ---------------------------------------------------------------- jobs.lib.child/run!

(defn counting-child
  "A child that answers :continue n times (counted in its memory), then :done; calls counts every call."
  [n calls]
  {:check (constantly true)
   :round (fn ^:async counting-round [c]
            (swap! calls inc)
            (let [k (inc (:k (ctx/mem c) 0))]
              (ctx/update-mem! c assoc :k k)
              (if (> k n) :done :continue)))})

(defn run-parent [kid args opts out]
  {:check (constantly true)
   :round (fn ^:async parent-round [c]
            (let [r (await (child/run! c :kid kid args opts))]
              (swap! out conj [r (get-in (ctx/mem c) [:children :kid :k])])
              (if (= :done r) :done :continue)))})

(defn ^:async run-parent! [kid args opts]
  (let [{:keys [eng]} (setup {})
        out (atom [])
        eng (assoc eng :jobs (assoc (:jobs eng) 'run-parent (run-parent kid args opts out)))]
    (core/submit! eng '(run-parent) {})
    (await (core/tick! eng))
    {:eng eng :out out}))

(deftest run-calls-the-child-until-it-ends-with-a-timer-between-calls
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [paces (atom 0)
              pace child/pace!
              n (atom 0)]
          (set! child/pace! (fn [] (swap! paces inc) (pace)))
          (let [{:keys [out]} (await (run-parent! (counting-child 3 n) {} {}))]
            (set! child/pace! pace)
            (is (= [[:done nil]] @out) "one call of run! ran it to :done, its memory cleared")
            (is (= 4 @n))
            (is (= 3 @paces) "a timer between calls, never a microtask")))))))

(deftest run-yields-continue-at-its-call-cap-and-keeps-the-childs-memory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              {:keys [eng out]} (await (run-parent! (counting-child 10 n) {} {:max-calls 4}))]
          (is (= [[:continue 4]] @out))
          (await (core/tick! eng))
          (is (= [[:continue 4] [:continue 8]] @out) "the next round resumes the same child"))))))

(deftest run-clears-the-slot-when-the-args-change
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              out (atom [])
              args (atom {:a 1})
              kid (counting-child 10 n)
              {:keys [eng]} (setup {})
              eng (assoc eng :jobs (assoc (:jobs eng) 'p {:check (constantly true)
                                                          :round (fn ^:async p-round [c]
                                                                   (await (child/run! c :kid kid @args {:max-calls 2}))
                                                                   (swap! out conj (get-in (ctx/mem c) [:children :kid :k]))
                                                                   :continue)}))]
          (core/submit! eng '(p) {})
          (await (core/tick! eng))
          (reset! args {:a 2})
          (await (core/tick! eng))
          (is (= [2 2] @out) "a new target starts the child afresh"))))))
