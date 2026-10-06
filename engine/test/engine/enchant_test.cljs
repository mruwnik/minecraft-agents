(ns engine.enchant-test
  "jobs.items.enchant: the choice as plain functions, then against the fake world's enchanting tables."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.items.enchant :as enchant]))

;; ------------------------------------------------------------------ the choice

(defn offers [& levels] (vec (map-indexed (fn [i l] {:index i :level-cost l :lapis-cost (inc i) :hint nil}) levels)))
(defn pick [m] (select-keys (enchant/choose-offer (merge {:offers (offers 4 9 16) :xp 30 :lapis 9 :max-level-cost nil :slot nil :rule "best"} m))
                            [:choice :reason :need]))

(deftest the-best-rule-takes-the-dearest-offer-the-body-can-pay
  (are [m expected] (= expected (pick m))
    {} {:choice 2}
    {:xp 12} {:choice 1}
    {:xp 4} {:choice 0}
    {:lapis 2} {:choice 1}
    {:lapis 1} {:choice 0}
    {:max-level-cost 10} {:choice 1}
    {:max-level-cost 16} {:choice 2}
    {:offers (offers 3 3 3) :xp 3} {:choice 0}
    {:offers (offers 3 3 3) :xp 2} {:reason "too-few-levels" :need 3}))

(deftest the-cheapest-rule-takes-the-lowest-affordable-level-cost-and-the-lower-slot-on-a-tie
  (are [m expected] (= expected (pick (assoc m :rule "cheapest")))
    {} {:choice 0}
    {:offers (offers 9 4 4)} {:choice 1}
    {:offers (offers 9 12 16) :xp 11} {:choice 0}
    {:offers (offers 9 12 16) :xp 5} {:reason "too-few-levels" :need 9}))

(deftest a-slot-asks-for-that-offer-alone
  (are [m expected] (= expected (pick m))
    {:slot 1} {:choice 0}
    {:slot 2} {:choice 1}
    {:slot 3 :xp 15} {:reason "too-few-levels" :need 16}
    {:slot 3 :lapis 2} {:reason "no-lapis"}
    {:slot 3 :max-level-cost 10} {:reason "no-offer-within-cost"}
    {:offers (offers 4 9 0) :slot 3} {:reason "no-offer"}))

(deftest every-give-up-has-its-reason
  (are [m expected] (= expected (:reason (pick m)))
    {:offers (offers 0 0 0)} "not-enchantable"
    {:max-level-cost 3} "no-offer-within-cost"
    {:lapis 0} "no-lapis"
    {:xp 3} "too-few-levels"
    {:xp 3 :lapis 0} "no-lapis"
    {:max-level-cost 3 :lapis 0} "no-offer-within-cost"))

(deftest the-levels-needed-are-the-dearer-of-the-cost-and-the-slot-number
  (is (= {:reason "too-few-levels" :need 3} (pick {:offers (offers 1 1 1) :slot 3 :xp 2}))))

;; ------------------------------------------------------------------ against the fake world

(def clock (atom 1000000))
(def spot "1,64,0")
(def sword {:name "diamond_sword" :count 1})
(defn lapis [n] {:name "lapis_lazuli" :count n})

(defn start [world]
  (let [[seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        dir (tu/tmp-dir)
        make (fn [] (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir dir :now #(deref clock)
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}))]
    {:eng (make) :make make :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (swap! clock + 700)
          (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome
  "Run the job as the child of a recording parent until the list is empty, at most n ticks; the child's result."
  [eng job-form n]
  (let [out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid (first job-form) (second job-form)))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn ops [p] (mapv #(.-op (.-args %)) (calls p "enchant")))
(defn inv [p] (reduce (fn [m i] (update m (.-name i) (fnil + 0) (.-count i))) {} (.-inventory (.self p))))
(defn level [p] (.-level (.-experience (.self p))))
(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))

(def table-world {:blocks {spot "enchanting_table"} :inventory [sword (lapis 9)] :self {:experience {:level 30}}})

(defn ^:async enchant!
  "Run the job over world with args; [result p seen]."
  ([world args] (enchant! world args identity))
  ([world args prepare]
   (let [{:keys [eng p seen]} (start world)
         _ (prepare p)
         result (await (child-outcome eng ['jobs.items.enchant (merge {:item "diamond_sword"} args)] 40))]
     [result p seen])))

(deftest a-low-offer-needs-no-bookshelves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p seen] (await (enchant! (assoc table-world :self {:experience {:level 5}}) {}))]
          (is (= {:enchanted true :item "diamond_sword" :slot 3 :level-cost 5 :levels-spent 3 :lapis-spent 3 :xp-level 2} (dissoc r :enchants :hint)))
          (is (= [{:name "sharpness" :level 1}] (:enchants r)))
          (is (= {:enchant "sharpness" :level 1} (:hint r)) "the hint the table gave for the chosen offer")
          (is (= {"diamond_sword" 1 "lapis_lazuli" 6} (inv p)))
          (is (= 2 (level p)))
          (is (= ["offers" "enchant"] (ops p)))
          (is (= 1 (count (of-kind seen :enchant.done)))))))))

(deftest a-level-thirty-offer-with-fifteen-bookshelves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (enchant! (assoc table-world :enchantTables {spot {:shelves 15}}) {}))]
          (is (= {:enchanted true :slot 3 :level-cost 30 :levels-spent 3 :lapis-spent 3 :xp-level 27} (select-keys r [:enchanted :slot :level-cost :levels-spent :lapis-spent :xp-level])))
          (is (= [{:name "sharpness" :level 4} {:name "unbreaking" :level 3}] (:enchants r)))
          (is (= 27 (level p))))))))

(deftest the-rule-and-the-limits-choose-the-slot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc table-world :enchantTables {spot {:shelves 15}})]
          (await (tu/each-async [[{:slot 1} 1] [{:slot 2} 2] [{:choice "cheapest"} 1] [{:max-level-cost 25} 2] [{:max-level-cost 10} 1]]
                                (fn ^:async one [[args want]]
                                  (let [[r p] (await (enchant! world args))]
                                    (is (= want (:slot r)) (pr-str args))
                                    (is (= (- 30 want) (level p)) (pr-str args)))))))))))

(deftest a-table-never-seen-is-not-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (enchant! table-world {} tu/blind))]
          (is (= "no-table" (:reason r)))
          (is (empty? (ops p))))))))

(def give-up-rows
  [["no table within reach" {:blocks {"40,64,0" "enchanting_table"}} {} "no-table"]
   ["not a table" {:blocks {spot "chest"}} {:table {:x 1 :y 64 :z 0}} "not-a-table"]
   ["the table block is gone" {:blocks {}} {:table {:x 1 :y 64 :z 0}} "no-table"]
   ["item not carried" {:inventory [(lapis 9)]} {} "no-item"]
   ["already enchanted" {:inventory [(assoc sword :enchants [{:name "sharpness" :level 1}]) (lapis 9)]} {} "already-enchanted"]
   ["not enchantable" {:inventory [{:name "dirt" :count 1} (lapis 9)]} {:item "dirt"} "not-enchantable"]
   ["no lapis" {:inventory [sword]} {} "no-lapis"]
   ["too few levels" {:self {:experience {:level 1}} :enchantTables {spot {:offers [4 9 16]}}} {} "too-few-levels"]
   ["no offer within the cost limit" {:enchantTables {spot {:shelves 15}}} {:max-level-cost 5} "no-offer-within-cost"]
   ["the chosen slot has no lapis" {:inventory [sword (lapis 1)]} {:slot 3} "no-lapis"]
   ["the window never opens" {:enchantTables {spot {:busy true}}} {} "window"]])

(deftest gives-up-with-a-reason-and-spends-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (tu/each-async
                give-up-rows
                (fn ^:async one [[label spec args reason]]
                  (let [world (merge table-world spec)
                        [r p seen] (await (enchant! world args))]
                    (is (= reason (:reason r)) label)
                    (is (= false (:enchanted r)) label)
                    (is (= [0 0] [(:levels-spent r) (:lapis-spent r)]) label)
                    (is (= reason (:reason (first (of-kind seen :enchant.gave-up)))) label)
                    (is (= 1 (count (of-kind seen :enchant.gave-up))) label)
                    (is (not (some #{"enchant"} (ops p))) label)
                    (is (= (select-keys (merge {:self {:experience {:level 30}}} spec) [:self]) (select-keys {:self {:experience {:level (level p)}}} [:self])) label)))))))))

(deftest a-table-out-of-reach-is-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (enchant! (assoc table-world :blocks {"20,64,0" "enchanting_table"}) {:radius 40}))]
          (is (seq (tu/walk-calls p)))
          (is (true? (:enchanted r))))))))

(deftest the-nearest-table-in-the-radius-is-found-and-the-table-argument-wins
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc table-world :blocks {"1,64,0" "enchanting_table" "3,64,0" "enchanting_table"} :enchantTables {"3,64,0" {:shelves 15}})
              [near p] (await (enchant! world {}))
              [named _] (await (enchant! world {:table {:x 3 :y 64 :z 0}}))]
          (is (= 5 (:level-cost near)) "the nearer table has no shelves")
          (is (= 30 (:level-cost named)))
          (is (= 2 (count (calls p "enchant")))))))))

(defn override! [p f] (.override (.-world p) "enchant" f))

(deftest a-stalled-enchant-gives-up-and-says-what-was-spent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p seen] (await (enchant! table-world {}
                                          (fn [p] (override! p (fn ^:async f [token args impl]
                                                                 (if (= "enchant" (.-op args))
                                                                   #js {:status "failed" :reason "enchant-stalled" :lapisSpent 0 :levelsSpent 0}
                                                                   (await (impl token args))))))))]
          (is (= "window-stalled" (:reason r)))
          (is (= 1 (count (filter #{"enchant"} (ops p)))) "never tried again")
          (is (= "window-stalled" (:reason (first (of-kind seen :enchant.gave-up))))))))))

(deftest a-failed-enchant-that-took-the-price-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r _] (await (enchant! table-world {}
                                     (fn [p] (override! p (fn ^:async f [token args impl]
                                                            (if (= "enchant" (.-op args))
                                                              #js {:status "failed" :reason "not-confirmed" :lapisSpent 3 :levelsSpent 3}
                                                              (await (impl token args))))))))]
          (is (= {:reason "not-confirmed" :lapis-spent 3 :levels-spent 3 :enchanted false} (select-keys r [:reason :lapis-spent :levels-spent :enchanted]))))))))

(deftest a-full-inventory-gives-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r _] (await (enchant! table-world {}
                                     (fn [p] (override! p (fn ^:async f [token args impl]
                                                            (if (= "enchant" (.-op args))
                                                              #js {:status "full"}
                                                              (await (impl token args))))))))]
          (is (= "inventory-full" (:reason r))))))))

(deftest an-offer-that-changed-is-read-again-and-three-changes-give-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (enchant! table-world {}
                                     (fn [p] (override! p (fn ^:async f [token args impl]
                                                            (if (= "enchant" (.-op args))
                                                              #js {:status "cannot" :reason "offer-changed" :offers #js [1 2 3]}
                                                              (await (impl token args))))))))]
          (is (= "offer-changed" (:reason r)))
          (is (= 3 (count (filter #{"enchant"} (ops p))))))))))

(deftest a-restart-after-the-enchant-landed-reports-it-and-does-not-enchant-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc table-world :inventory [(assoc sword :enchants [{:name "sharpness" :level 2}]) (lapis 6)])
              {:keys [eng p seen]} (start world)
              id (core/submit! eng '(jobs.items.enchant {:item "diamond_sword"}) {})
              _ (mem/update-job! (:store eng) id [] #(assoc % :attempt {:slot 3 :level-cost 5 :xp 30 :lapis 9}))
              _ (await (run-until-empty eng 10))]
          (is (not (some #{"enchant"} (ops p))))
          (let [done-event (first (of-kind seen :enchant.done))]
            (is (= 3 (:slot done-event)))
            (is (true? (:resumed done-event)))))))))

(deftest the-attempt-is-written-before-the-enchant-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start (assoc table-world :self {:experience {:level 5}}))
              id (core/submit! eng '(jobs.items.enchant {:item "diamond_sword"}) {})
              held (atom nil)
              _ (override! p (fn ^:async f [token args impl]
                               (when (= "enchant" (.-op args)) (reset! held (core/job-memory eng id)))
                               (await (impl token args))))
              _ (await (run-until-empty eng 10))]
          (is (= {:slot 3 :level-cost 5 :xp 5 :lapis 9} (:attempt @held))))))))
