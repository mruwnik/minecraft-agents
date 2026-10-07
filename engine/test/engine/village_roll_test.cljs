(ns engine.village-roll-test
  "jobs.village.roll against the fake world: a villager that re-picks its offers when its workstation is broken and placed again."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.blocks-dig-test :as bd]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.registry :as registry]
            [engine.fake :as fake]
            [engine.village-trade-test :as vt]
            [engine.test-util :as tu]))

(def job 'jobs.village.roll)

(def ws-pos [2 64 0])
(def ws-key "2,64,0")

(defn offer [item] {:cost [{:item "emerald" :count 5}] :gives {:item item :count 1}})

(defn librarian
  "A villager that already claimed the workstation, with the offers."
  [offers & [extra]]
  (vt/villager offers (merge {:profession "librarian"} extra)))

(defn rolls-offers!
  "Make the fake villager behave: it loses its profession when the workstation cell is empty, and takes it again (with the
  next of scripts as its offers, none left: it stays unemployed) when the workstation stands."
  [p scripts]
  (let [left (atom scripts)
        state (fake/state p)]
    (add-watch state :roll
               (fn [_ _ _ w]
                 (let [stands? (= "lectern" (get-in w [:blocks ws-pos]))
                       v (first (filter #(= "v-1" (:uuid %)) (:entities w)))
                       i (first (keep-indexed #(when (= "v-1" (:uuid %2)) %1) (:entities w)))]
                   (cond
                     (nil? v) nil
                     (and (not stands?) (not= "unemployed" (:profession v)))
                     (swap! state update-in [:entities i] assoc :profession "unemployed" :offers nil)
                     (and stands? (= "unemployed" (:profession v)) (seq @left))
                     (let [offers (first @left)]
                       (swap! left rest)
                       (swap! state update-in [:entities i] assoc :profession "librarian" :offers offers))))))
    p))

(defn ^:async child-outcome
  "Run the job as the child of a recording parent, the clock moving 700 ms a tick, until the list is empty (at most n ticks)."
  [{:keys [eng clock]} args n]
  (let [out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i n) (seq (:list (core/state eng))))
        (swap! clock + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    @out))

(defn ^:async roll
  "Run the job on a world with a lectern at ws-pos; [result p seen]. scripts feed the villager's next offers."
  [{:keys [entities inventory blocks scripts zones keeps-job?]} args]
  (let [env (bd/setup {:entities entities :inventory inventory :zones (or zones []) :blocks (merge {ws-key "lectern" "2,63,0" "stone"} blocks)})
        {:keys [p seen]} env
        _ (when-not keeps-job? (rolls-offers! p scripts))
        result (await (child-outcome env (merge {:villager "v-1" :pos ws-pos :item "lectern" :want "enchanted_book"
                                                 :claim-s 5} args) 300))]
    [result p seen]))

(defn digs [p] (count (vt/calls p "dig")))
(defn workstation [p] (get-in @(fake/state p) [:blocks ws-pos]))

(deftest roll-check-wants-villager-pos-item-and-want
  (are [args ok] (= ok ((:check (get registry/jobs job)) {:args args}))
    {:villager "v-1" :pos [2 64 0] :item "lectern" :want "enchanted_book"} true
    {:villager nil :pos [2 64 0] :item "lectern" :want "enchanted_book"} true
    {:pos [2 64 0] :profession "librarian" :trade "enchanted_book"} true
    {:pos [2 64 0] :profession "nitwit" :want "enchanted_book"} false
    {:villager "v-1" :pos nil :item "lectern" :want "enchanted_book"} false
    {:villager "v-1" :pos [2 64 0] :item nil :want "enchanted_book"} false
    {:villager "v-1" :pos [2 64 0] :item "lectern" :want nil} false))

(deftest rolls-until-an-offer-gives-the-wanted-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (roll {:entities [(librarian [(offer "paper")])]
                                            :scripts [[(offer "paper")] [(offer "paper")] [(offer "paper") (offer "enchanted_book")]]}
                                           {}))]
          (is (= {:found true :rolls 3 :item "lectern"} (select-keys result [:found :rolls :item])))
          (is (= "enchanted_book" (get-in result [:offer :gives :item])))
          (is (= 3 (digs p)))
          (is (= "lectern" (workstation p)) "the workstation is back")
          (is (contains? (vt/kinds seen) :roll.done)))))))

(deftest the-nearest-adult-is-rolled-when-no-villager-is-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [baby (vt/villager [(offer "paper")] {:id 2 :uuid "v-0" :baby true :profession "unemployed" :pos {:x 1 :y 64 :z 1}})
              [result p] (await (roll {:entities [baby (librarian [(offer "paper")])]
                                       :scripts [[(offer "enchanted_book")]]}
                                      {:villager nil :item nil :profession "librarian" :want nil :trade "enchanted_book"}))]
          (is (= {:found true :rolls 1 :item "lectern"} (select-keys result [:found :rolls :item])))
          (is (= "lectern" (workstation p))))))))

(deftest an-offer-already-there-is-found-without-a-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "enchanted_book")])]} {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls])))
          (is (zero? (digs p)))
          (is (= "lectern" (workstation p))))))))

(deftest max-price-skips-a-dear-match
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cheap (assoc (offer "enchanted_book") :cost [{:item "emerald" :count 9}])
              [result] (await (roll {:entities [(librarian [(assoc cheap :cost [{:item "emerald" :count 30}])])]
                                     :scripts [[cheap]]}
                                    {:max-price 10}))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls]))))))))

(deftest gives-up-after-tries-with-the-workstation-restored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (roll {:entities [(librarian [(offer "paper")])]
                                            :scripts (repeat 10 [(offer "paper")])}
                                           {:tries 2}))]
          (is (= {:found false :rolls 2 :status :stopped :reason "no-match"} (select-keys result [:found :rolls :status :reason])))
          (is (= "lectern" (workstation p)))
          (is (contains? (vt/kinds seen) :roll.gave-up)))))))

(deftest a-villager-that-already-traded-is-locked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(assoc (offer "paper") :uses 1)])]} {}))]
          (is (= {:found false :status :stopped :reason "locked"} (select-keys result [:found :status :reason])))
          (is (zero? (digs p))))))))

(deftest a-baby-or-nitwit-cannot-be-rolled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[baby] (await (roll {:entities [(librarian [(offer "paper")] {:baby true})]} {}))
              [nitwit] (await (roll {:entities [(librarian [(offer "paper")] {:profession "nitwit"})]} {}))]
          (is (= "baby" (:reason baby)))
          (is (= "nitwit" (:reason nitwit))))))))

(deftest a-missing-villager-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities []} {}))]
          (is (= {:found false :status :stopped :reason "gone"} (select-keys result [:found :status :reason])))
          (is (zero? (digs p))))))))

(deftest no-claim-when-the-villager-never-takes-the-workstation
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(vt/villager nil {:profession "unemployed"})]} {}))]
          (is (= {:found false :status :stopped :reason "no-claim"} (select-keys result [:found :status :reason])))
          (is (= "lectern" (workstation p))))))))

(deftest a-restart-with-the-workstation-missing-places-it-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(vt/villager nil {:profession "unemployed"})]
                                       :inventory [{:name "lectern" :count 1}]
                                       :blocks {ws-key "air"}
                                       :scripts [[(offer "enchanted_book")]]}
                                      {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls])))
          (is (zero? (digs p)))
          (is (= "lectern" (workstation p))))))))

(deftest another-block-on-the-workstation-cell-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result] (await (roll {:entities [(librarian [(offer "paper")])] :blocks {ws-key "stone"}} {}))]
          (is (= "occupied" (:reason result))))))))

(deftest a-workstation-in-anothers-zone-is-not-broken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ p] (await (roll {:entities [(librarian [(offer "paper")])]
                                  :zones [{:name "farm" :min [2 60 0] :max [2 70 0] :owner "Miles"}]}
                                 {}))]
          (is (zero? (digs p)))
          (is (= "lectern" (workstation p))))))))

(deftest a-villager-that-keeps-its-job-after-the-break-stops-without-another-roll
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "paper")])] :keeps-job? true} {}))]
          (is (= {:found false :rolls 1 :status :stopped :reason "still-employed"}
                 (select-keys result [:found :rolls :status :reason])))
          (is (= 1 (digs p)))
          (is (= "lectern" (workstation p))))))))
