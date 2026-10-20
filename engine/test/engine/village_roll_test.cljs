(ns engine.village-roll-test
  "jobs.village.roll against the fake world: a villager that re-picks its offers when its workstation is broken and placed again."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.blocks-dig-test :as bd]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.registry :as registry]
            [engine.fake :as fake]
            [engine.look-support :as ls]
            [engine.village-trade-test :as vt]
            [engine.contract :as contract]
            [jobs.lib.look :as look]
            [engine.test-util :as tu]
            [jobs.village.roll :as roll]))

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
        parent (tu/recording-parent job args out)
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
  [{:keys [entities inventory blocks scripts zones keeps-job? self floor wrap]} args]
  (let [env (cond-> (bd/setup (cond-> {:entities entities :inventory inventory :zones (or zones [])
                                       :blocks (merge {ws-key "lectern" "2,63,0" "stone"} blocks)}
                                self (assoc :self self)
                                floor (assoc :floor floor)))
              wrap wrap)
        {:keys [p seen]} env
        _ (when-not keeps-job? (rolls-offers! p scripts))
        result (await (child-outcome env (merge {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book"
                                                 :spend {:wall-time 600}} args) 300))]
    [result p seen]))

(defn digs [p] (count (vt/calls p "dig")))
(defn workstation [p] (get-in @(fake/state p) [:blocks ws-pos]))

(deftest roll-bad-args-need-villager-pos-item-and-want
  (are [args ok] (= ok (= :ok (first (roll/bad-args {:args args}))))
    {:villager "v-1" :pos [2 64 0] :profession "librarian" :want "enchanted_book"} true
    {:villager nil :pos [2 64 0] :profession "librarian" :want "enchanted_book"} true
    {:pos [2 64 0] :profession "librarian" :want "enchanted_book"} true
    {:pos [2 64 0] :profession "nitwit" :want "enchanted_book"} false
    {:villager "v-1" :pos nil :profession "librarian" :want "enchanted_book"} false
    {:villager "v-1" :pos [2 64 0] :profession nil :want "enchanted_book"} false
    {:villager "v-1" :pos [2 64 0] :profession "librarian" :want nil} false
    {:villager "v-1" :pos [2 64 0] :profession "librarian" :want nil :enchant "mending"} true))

(deftest an-unseen-workstation-cell-asks-for-a-look-then-the-round-walks-there
  (let [args {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book"}
        world {:blocks {ws-key "lectern"}}
        cause (fn [p surveyed?]
                (with-redefs [look/surveyed? (fn ([_] surveyed?) ([_ _] surveyed?))]
                  (roll/cell-unseen {:primitives p :args args})))
        kind-reason (juxt first (comp :reason last))]
    (is (= [:look :cell-unseen] (kind-reason (cause (tu/blind (tu/fake world)) false))))
    (is (= :ok (first (cause (tu/blind (tu/fake world)) true))) "after a look the round goes to the cell")
    (is (= :ok (first (cause (tu/fake world) false))))))

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
          (is (contains? (vt/kinds seen) :completed)))))))

(deftest an-enchant-skips-books-of-other-enchantments
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [book (fn [enchant] (assoc-in (offer "enchanted_book") [:gives :enchants] [{:name enchant :level 1}]))
              [result] (await (roll {:entities [(librarian [(book "sharpness")])]
                                     :scripts [[(book "protection")] [(book "mending")]]}
                                    {:want nil :enchant "mending"}))]
          (is (= {:found true :rolls 2} (select-keys result [:found :rolls])))
          (is (= [{:name "mending" :level 1}] (get-in result [:offer :gives :enchants]))))))))

(deftest the-nearest-adult-is-rolled-when-no-villager-is-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [baby (vt/villager [(offer "paper")] {:id 2 :uuid "v-0" :baby true :profession "unemployed" :pos {:x 1 :y 64 :z 1}})
              [result p] (await (roll {:entities [baby (librarian [(offer "paper")])]
                                       :scripts [[(offer "enchanted_book")]]}
                                      {:villager nil :profession "librarian" :want "enchanted_book"}))]
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

(deftest gives-up-after-max-rolls-with-the-workstation-restored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (roll {:entities [(librarian [(offer "paper")])]
                                            :scripts (repeat 10 [(offer "paper")])}
                                           {:settings {:jobs.village.roll/max-rolls 2}}))]
          (is (= {:found false :rolls 2 :status :stopped :reason :no-match} (select-keys result [:found :rolls :status :reason])))
          (is (= "lectern" (workstation p)))
          (is (not (contains? (vt/kinds seen) :roll.gave-up))))))))

(deftest a-villager-that-already-traded-is-locked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(assoc (offer "paper") :uses 1)])]} {}))]
          (is (= {:found false :status :stopped :reason :locked} (select-keys result [:found :status :reason])))
          (is (zero? (digs p))))))))

(deftest a-missing-villager-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities []} {}))]
          (is (= {:found false :status :stopped :reason :gone} (select-keys result [:found :status :reason])))
          (is (zero? (digs p))))))))

(deftest a-villager-behind-the-body-is-looked-for-before-it-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result] (await (roll {:entities [(librarian [(offer "enchanted_book")])]
                                     :wrap #(do (ls/entities-after-look (:p %)) %)} {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls]))))))))

(deftest the-wall-time-ends-the-wait-when-the-villager-never-takes-the-workstation
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(vt/villager nil {:profession "unemployed"})]} {:spend {:wall-time 30}}))]
          (is (= {:found false :reason :wall-time} (select-keys result [:found :reason])))
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

(deftest a-workstation-in-anothers-zone-is-not-broken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ p] (await (roll {:entities [(librarian [(offer "paper")])]
                                  :zones [{:name "farm" :min [2 60 0] :max [2 70 0] :owner "Miles"}]}
                                 {}))]
          (is (zero? (digs p)))
          (is (= "lectern" (workstation p))))))))

(deftest a-villager-that-keeps-its-job-after-the-break-is-waited-for-until-the-wall-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (roll {:entities [(librarian [(offer "paper")])] :keeps-job? true} {:spend {:wall-time 30}}))]
          (is (= [:wall-time] (mapv (comp :line :cause) (filter #(= :limit_reached (:kind %)) @seen))))
          (is (= {:found false :rolls 1 :status :stopped :reason :wall-time} (select-keys result [:found :rolls :status :reason]))
              "one roll done is not the goal: the limit ends the call limit-reached, not done")
          (is (= 1 (digs p)))
          (is (= "lectern" (workstation p))))))))

(defn stub-job
  "env with job replaced by a round that ends done with result."
  [job-sym result]
  (fn [env]
    (let [j (get (:jobs (:eng env)) job-sym)]
      (assoc-in env [:eng :jobs job-sym] (assoc j :round (fn ^:async stubbed [c]
                                                           (if (= :stopped (:status result))
                                                             (ctx/end c :nothing-possible (:reason result))
                                                             (ctx/end c :done nil result))))))))

(def stopped {:status :stopped :reason :no-tool})

(deftest a-dig-child-that-stops-ends-with-its-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "paper")])] :wrap (stub-job 'jobs.blocks.dig stopped)} {}))]
          (is (= {:found false :rolls 0 :status :stopped :reason :no-tool} (select-keys result [:found :rolls :status :reason])))
          (is (= "lectern" (workstation p)) "nothing was broken"))))))

(deftest a-place-child-that-stops-ends-with-its-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (roll {:entities [(vt/villager nil {:profession "unemployed"})]
                                            :blocks {ws-key "air"}
                                            :wrap (stub-job 'jobs.blocks.place stopped)} {}))]
          (is (= {:found false :status :stopped :reason :no-tool} (select-keys result [:found :status :reason])))
          (is (= "air" (workstation p)))
          (is (not (contains? (vt/kinds seen) :roll.gave-up))))))))

(deftest a-body-on-the-workstation-cell-steps-off-before-placing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(vt/villager nil {:profession "unemployed"})]
                                       :inventory [{:name "lectern" :count 1}]
                                       :self {:pos {:x 2 :y 64 :z 0}}
                                       :blocks {ws-key "air"}
                                       :scripts [[(offer "enchanted_book")]]}
                                      {}))
              feet (.-pos (.self p))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls])))
          (is (= "lectern" (workstation p)))
          (is (not (and (= 2 (js/Math.floor (.-x feet))) (= 0 (js/Math.floor (.-z feet))))) "the body left the cell"))))))

(deftest a-body-with-nowhere-to-step-off-ends-place-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(vt/villager nil {:profession "unemployed"})]
                                       :inventory [{:name "lectern" :count 1}]
                                       :self {:pos {:x 2 :y 64 :z 0}}
                                       :floor [2 0 2 0]
                                       :blocks {ws-key "air"}}
                                      {}))]
          (is (= {:found false :status :stopped :reason :place-failed} (select-keys result [:found :status :reason])))
          (is (= "air" (workstation p))))))))

(deftest a-villager-out-of-reach-that-cannot-be-walked-to-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "paper")] {:pos {:x 20 :y 64 :z 0}})]
                                       :wrap (stub-job 'jobs.movement.go-to {:arrived false :why :no-path})}
                                      {}))]
          (is (= {:found false :rolls 0 :status :stopped :reason :unreachable} (select-keys result [:found :rolls :status :reason])))
          (is (zero? (digs p))))))))

(deftest offers-that-cannot-be-read-three-times-end-window
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (roll {:entities [(librarian [(offer "paper")] {:busy true})]} {}))]
          (is (= {:found false :status :stopped :reason :window} (select-keys result [:found :status :reason])))
          (is (zero? (digs p)))
          (is (not (contains? (vt/kinds seen) :roll.gave-up))))))))

(defn unemploys-after!
  "The villager of a world with the workstation broken loses its job lag-ms after the break (seen on the clock), then
  takes it again with offers when the workstation stands."
  [{:keys [p clock]} lag-ms offers]
  (let [state (fake/state p)
        broke (atom nil)
        step (fn [_ _ _ _]
               (let [w @state
                     stands? (= "lectern" (get-in w [:blocks ws-pos]))
                     i (first (keep-indexed #(when (= "v-1" (:uuid %2)) %1) (:entities w)))
                     v (get-in w [:entities i])]
                 (cond
                   stands? (do (reset! broke nil)
                               (when (= "unemployed" (:profession v))
                                 (swap! state update-in [:entities i] assoc :profession "librarian" :offers offers)))
                   (nil? @broke) (reset! broke @clock)
                   (and (>= (- @clock @broke) lag-ms) (not= "unemployed" (:profession v)))
                   (swap! state update-in [:entities i] assoc :profession "unemployed" :offers nil))))]
    (add-watch state :lag step)
    (add-watch clock :lag step)))

(deftest a-villager-that-loses-its-job-within-the-window-is-rolled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:entities [(librarian [(offer "paper")])] :blocks {ws-key "lectern" "2,63,0" "stone"}})
              _ (unemploys-after! env 8000 [(offer "enchanted_book")])
              result (await (child-outcome env {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book" :spend {:wall-time 600}} 300))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])))
          (is (= "lectern" (workstation (:p env)))))))))

(deftest a-villager-that-loses-its-job-late-is-still-rolled-no-clock-of-the-job-gives-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:entities [(librarian [(offer "paper")])] :blocks {ws-key "lectern" "2,63,0" "stone"}})
              _ (unemploys-after! env 30000 [(offer "enchanted_book")])
              result (await (child-outcome env {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book" :spend {:wall-time 600}} 300))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])))
          (is (= "lectern" (workstation (:p env)))))))))

(deftest a-roll-cut-after-the-break-places-the-workstation-back-in-its-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (bd/setup {:entities [(librarian [(offer "paper")])] :zones []
                                               :inventory [{:name "lectern" :count 1}]
                                               :blocks {ws-key "lectern" "2,63,0" "stone"}})
              cut (atom false)]
          (rolls-offers! p [[(offer "paper")]])
          (add-watch (fake/state p) :cut
                     (fn [_ _ _ w]
                       (when (and (not @cut) (not= "lectern" (get-in w [:blocks ws-pos])))
                         (reset! cut true)
                         (core/cancel! eng (first (:list (core/state eng)))))))
          (core/submit! eng (list job {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book" :spend {:wall-time 600}}) {})
          (loop [i 0]
            (when (and (< i 300) (seq (:list (core/state eng))))
              (swap! clock + 700)
              (await (core/tick! eng))
              (recur (inc i))))
          (is @cut "the cut came after the break")
          (is (= "lectern" (workstation p)) "the cleanup put it back"))))))
