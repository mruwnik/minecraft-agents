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
            [engine.entity-observations :as obs]
            [jobs.lib.look :as look]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.village.roll :as roll]))

(def job 'jobs.village.roll)

(def ws-pos [2 64 0])
(def ws-key "2,64,0")

(defn offer [item] {:cost [{:item "emerald" :count 5}] :gives {:item item :count 1}})

(def in-pen "Where a villager in the pen stands." {:x 3 :y 64 :z 0})

(defn librarian
  "A villager in the pen that already claimed the workstation, with the offers."
  [offers & [extra]]
  (vt/villager offers (merge {:profession "librarian" :level 1 :pos in-pen} extra)))

(defn jobless
  "A villager in the pen without a job."
  [& [extra]]
  (vt/villager nil (merge {:profession "unemployed" :level 1 :pos in-pen} extra)))

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
    (core/submit! eng (tu/order '(recording-parent {})) {})
    (loop [i 0]
      (when (and (< i n) (seq (:list (core/state eng))))
        (swap! clock + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    @out))

(def pen
  "The villager's pen at ws-pos along +x (jobs.lib.villager-pen): its cell [3 64 0] (in-pen) floored, walled, roofed,
  a top slab over the lectern, its near end [4 64 0] plugged, the lectern's sides walled; the body boxed in behind it."
  {:blocks {"3,63,0" "stone" "3,64,1" "stone" "3,65,1" "stone" "3,66,1" "stone" "3,64,-1" "stone" "3,65,-1" "stone"
            "3,66,-1" "stone" "3,66,0" "stone" "2,66,0" "stone" "2,65,0" "oak_slab" "4,64,0" "stone" "4,65,0" "stone"
            "2,64,1" "stone" "2,64,-1" "stone" "1,63,0" "stone" "1,66,0" "stone" "1,64,1" "stone" "1,65,1" "stone"
            "1,64,-1" "stone" "1,65,-1" "stone" "0,64,0" "stone" "0,65,0" "stone"
            "0,63,0" "stone" "1,63,1" "stone" "1,63,-1" "stone"}
   :states {"2,65,0" {:type "top"}}
   :self {:pos {:x 1.5 :y 64 :z 0.5}}})

(defn with-pen
  "World spec with the pen built under its blocks and the body behind the lectern (a spec's :self wins)."
  [spec]
  (-> (merge {:self (:self pen) :states (:states pen)} spec)
      (update :blocks #(merge (:blocks pen) %))))

(defn ^:async roll
  "Run the job on a world with a lectern at ws-pos, in its pen unless :pen? is false; [result p seen]. scripts feed
  the villager's next offers."
  [{:keys [entities inventory blocks scripts zones keeps-job? self floor wrap pen?] :or {pen? true}} args]
  (let [env (cond-> (bd/setup (cond-> {:entities entities :inventory inventory :zones (or zones [])
                                       :blocks (merge {ws-key "lectern" "2,63,0" "stone"} (when pen? (:blocks pen)) blocks)}
                                pen? (assoc :states (:states pen) :self (:self pen))
                                self (assoc :self self)
                                floor (assoc :floor floor)))
              wrap wrap)
        {:keys [p seen]} env
        _ (when-not keeps-job? (rolls-offers! p scripts))
        result (await (child-outcome env (merge {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book"
                                                 :spend {:wall-time 600}} args) 300))]
    [result p seen]))

(defn digs
  "The breaks of the workstation (a box dug open to walk out of is no roll)."
  [p]
  (count (filter #(let [a (.-pos (.-args %))] (= ws-pos [(.-x a) (.-y a) (.-z a)])) (vt/calls p "dig"))))
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
    {:villager "v-1" :pos [2 64 0] :profession "librarian" :want nil :enchant "mending"} true
    {:villager "v-1" :pos [2 64 0] :profession "librarian" :want nil :enchant {"mending" 1 "unbreaking" 3}} true))

(deftest an-unseen-workstation-cell-asks-for-a-look-then-the-round-walks-there
  (let [args {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book"}
        world {:blocks {ws-key "lectern"}}
        cause (fn [p surveyed?]
                (with-redefs [look/surveyed? (fn ([_] surveyed?) ([_ _] surveyed?) ([_ _ _] surveyed?))]
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

(deftest a-villager-not-shut-in-is-penned-before-the-first-break
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "paper")])] :pen? false
                                       :inventory [{:name "cobblestone" :count 20} {:name "oak_slab" :count 1} {:name "torch" :count 2}]
                                       :scripts [[(offer "enchanted_book")]]}
                                      {}))
              acts (keep #(#{"place" "dig"} (.-name %)) (.-calls (.-world p)))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])))
          (is (= ["cobblestone" "cobblestone"] (map #(get-in @(fake/state p) [:blocks %]) [[4 64 0] [4 65 0]])) "the near end is plugged")
          (is (= "top" (get-in @(fake/state p) [:states [2 65 0] :type])) "a top slab over the lectern")
          (is (= "place" (first acts)) "the pen is built before the lectern is broken"))))))

(deftest an-enchant-skips-books-of-other-enchantments
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [book (fn [enchant] (assoc-in (offer "enchanted_book") [:gives :enchants] [{:name enchant :level 1}]))
              [result _ seen] (await (roll {:entities [(librarian [(book "sharpness")])]
                                            :scripts [[(book "protection")] [(book "mending")]]}
                                           {:want nil :enchant "mending"}))]
          (is (some #(and (= :roll.offers (:kind %)) (re-find #"enchanted_book \(protection 1\)" (str (:text %)))) @seen)
              "each roll says its offers' enchantments")
          (is (= {:found true :rolls 2} (select-keys result [:found :rolls])))
          (is (= [{:name "mending" :level 1}] (get-in result [:offer :gives :enchants]))))))))

(deftest an-enchant-map-stops-on-any-book-at-its-minimum-level
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [book (fn [enchant level] (assoc-in (offer "enchanted_book") [:gives :enchants] [{:name enchant :level level}]))
              [result] (await (roll {:entities [(librarian [(book "sharpness" 5)])]
                                     :scripts [[(book "unbreaking" 2)] [(book "efficiency" 5)] [(book "unbreaking" 3)]]}
                                    {:want nil :enchant {"mending" 1 "unbreaking" 3}}))]
          (is (= {:found true :rolls 3} (select-keys result [:found :rolls])))
          (is (= [{:name "unbreaking" :level 3}] (get-in result [:offer :gives :enchants]))))))))

(deftest the-nearest-adult-is-rolled-when-no-villager-is-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [baby (vt/villager [(offer "paper")] {:id 2 :uuid "v-0" :baby true :profession "unemployed" :pos {:x 1 :y 64 :z 1}})
              farmer (vt/villager [(offer "paper")] {:id 3 :uuid "v-2" :level 1 :pos {:x 0 :y 64 :z 0.5}})
              [result p seen] (await (roll {:entities [baby farmer (librarian [(offer "paper")])]
                                            :scripts [[(offer "enchanted_book")]]}
                                           {:villager nil :profession "librarian" :want "enchanted_book"}))]
          (is (= {:found true :rolls 1 :item "lectern"} (select-keys result [:found :rolls :item])))
          (is (= ["v-1"] (map :villager (filter #(= :roll.villager (:kind %)) @seen))))
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

(deftest a-villager-whose-window-shows-xp-is-locked-without-a-dig-though-a-restock-cleared-its-uses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "paper")] {:xp 1})]} {}))]
          (is (= {:found false :status :stopped :reason :locked} (select-keys result [:found :status :reason])))
          (is (zero? (digs p))))))))

(deftest a-picked-villager-that-traded-is-passed-over-for-the-next-nearest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [traded (librarian [(offer "paper")] {:id 3 :uuid "v-2" :xp 1 :pos {:x 0.5 :y 64 :z 0.5}})
              [result p seen] (await (roll {:entities [traded (librarian [(offer "paper")])]
                                            :scripts [[(offer "enchanted_book")]]}
                                           {:villager nil}))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])))
          (is (= ["v-2" "v-1"] (map :villager (filter #(= :roll.villager (:kind %)) @seen))))
          (is (= "lectern" (workstation p))))))))

(deftest a-villager-above-level-1-is-locked-without-a-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "paper")] {:level 2})]} {}))]
          (is (= {:found false :status :stopped :reason :locked} (select-keys result [:found :status :reason])))
          (is (zero? (digs p))))))))

(deftest a-missing-villager-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities []} {}))]
          (is (= {:found false :status :stopped :reason :gone} (select-keys result [:found :status :reason])))
          (is (re-find #"out of sight, not known dead" (str (get-in result [:ending :cause :hint]))) (pr-str result))
          (is (zero? (digs p))))))))

(deftest a-villager-behind-the-body-is-looked-for-before-it-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result] (await (roll {:entities [(librarian [(offer "enchanted_book")])]
                                     :wrap #(do (ls/entities-after-look (:p %)) %)} {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls]))))))))

(deftest a-villager-out-of-view-far-from-the-workstation-is-walked-to-not-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "enchanted_book")])]
                                       :self {:pos {:x 14.5 :y 64 :z 0.5}}
                                       :wrap #(let [q (:p %)
                                                    entities (.-entities q)
                                                    walked? (fn [] (some (fn [call] (#{"goTo" "moveTo" "steer"} (.-name call))) (.-calls (.-world q))))]
                                                (aset q "entities" (fn [a] (if (walked?) (.call entities q a) #js [])))
                                                %)} {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls])))
          (is (pos? (count (vt/calls p "steer")))))))))

(deftest a-named-villager-hidden-from-the-body-beside-the-workstation-is-looked-for-from-the-cell-beside-it-not-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "enchanted_book")])]
                                       :self {:pos {:x -1.5 :y 64 :z 0.5}}
                                       :wrap #(let [q (:p %)
                                                    entities (.-entities q)
                                                    walked? (fn [] (some (fn [call] (#{"goTo" "moveTo" "steer"} (.-name call))) (.-calls (.-world q))))]
                                                (aset q "entities" (fn [a] (if (walked?) (.call entities q a) #js [])))
                                                %)} {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls])))
          (is (pos? (count (vt/calls p "steer"))) "the body stepped to a cell beside the workstation"))))))

(deftest a-named-villager-out-of-view-at-the-workstation-is-looked-for-where-it-was-last-seen-not-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result] (await (roll {:entities [(librarian [(offer "enchanted_book")] {:pos {:x 12.5 :y 64 :z 0.5}})]
                                     :wrap #(let [q (:p %)
                                                  entities (.-entities q)]
                                              (aset q "entities" (fn [a] (if (< 8 (first (:pos (fake/self q)))) (.call entities q a) #js [])))
                                              (aset q "rememberedEntity" (fn [id] (when (= "v-1" id) #js {:x 12.5 :y 64 :z 0.5})))
                                              %)} {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls]))))))))

(defn heard-until-near!
  "The villager is out of sight (entities() drops it) and heard as a heardMobs row (what a sound tells) until the body
  stands within 3 blocks of it, as one behind a house wall is seen from round the corner."
  [p]
  (let [orig (.-entities p)
        near? (fn [] (let [me (.-pos (.self p))]
                       (some (fn [^js e] (> 3 (js/Math.hypot (- (.-x (.-pos e)) (.-x me)) (- (.-z (.-pos e)) (.-z me)))))
                             (array-seq (.call orig p #js {:radius 48})))))]
    (set! (.-entities p) (fn [a] (if (near?) (.call orig p a) #js [])))
    (set! (.-heardMobs p)
          (fn [_]
            (let [me (.-pos (.self p))]
              (if (near?)
                #js []
                (into-array (for [^js e (array-seq (.call orig p #js {:radius 16}))
                                  :let [pos (.-pos e)]]
                              (clj->js (assoc (obs/rough-hearing {:x (.-x me) :y (.-y me) :z (.-z me)} {:x (.-x pos) :y (.-y pos) :z (.-z pos)})
                                              :id (.-id e) :name (.-name e) :kind "passive" :distance (.-distance e)))))))))
    p))

(deftest a-villager-heard-but-not-seen-by-day-is-walked-toward-not-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "enchanted_book")] {:pos {:x 0.5 :y 64 :z 8.5}})]
                                       :wrap #(do (heard-until-near! (:p %)) %)} {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls])) (pr-str result))
          (is (< 5 (.-z (.-pos (.self p)))) "the body walked south, toward the sound"))))))

(deftest a-villager-heard-at-night-is-waited-for-then-walked-toward-at-dawn-not-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [moved-at-night (atom false)
              [result] (await (roll {:entities [(librarian [(offer "enchanted_book")] {:pos {:x 0.5 :y 64 :z 8.5}})]
                                     :wrap (fn [{:keys [p clock] :as env}]
                                             (let [state (fake/state p)
                                                   dawn (+ @clock 60000)]
                                               (swap! state assoc :time 14000)
                                               (add-watch clock :dawn (fn [_ _ _ now] (when (and (>= now dawn) (= 14000 (:time @state)))
                                                                                         (swap! state assoc :time 1000))))
                                               (add-watch state :night-walk (fn [_ _ o w] (when (and (= 14000 (:time w)) (< 3 (get-in w [:self :pos 2] 0)))
                                                                                            (reset! moved-at-night true))))
                                               (heard-until-near! p)
                                               env))} {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls])) (pr-str result))
          (is (false? @moved-at-night) "by night the body waits at the workstation"))))))

(deftest a-villager-seen-then-out-of-view-and-unheard-is-walked-to-where-the-roll-saw-it-not-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the pen's near end shut: a boxed body waits only on a pen a villager can walk into (villager-pen/luring)
        (let [[result] (await (roll {:entities [(librarian [(offer "enchanted_book")] {:pos {:x 8.5 :y 64 :z 0.5}})]
                                     :keeps-job? true
                                     :wrap (fn [{:keys [p clock] :as env}]
                                             (let [orig (.-entities p)
                                                   hide-at (+ @clock 5000)
                                                   near? #(< 5 (.-x (.-pos (.self p))))]
                                               (aset p "entities" (fn [a] (if (or (< @clock hide-at) (near?)) (.call orig p a) #js [])))
                                               env))} {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls])) (pr-str result)))))))

(deftest a-villager-out-of-view-at-night-is-waited-for-till-day-not-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result] (await (roll {:entities [(librarian [(offer "enchanted_book")])]
                                     :wrap (fn [{:keys [p clock] :as env}]
                                             (let [state (fake/state p)
                                                   dawn (+ @clock 60000)
                                                   entities (.-entities p)]
                                               (swap! state assoc :time 14000)
                                               (add-watch clock :dawn (fn [_ _ _ now] (when (and (>= now dawn) (= 14000 (:time @state)))
                                                                                         (swap! state assoc :time 1000))))
                                               (aset p "entities" (fn [a] (if (= 14000 (:time @state)) #js [] (.call entities p a))))
                                               env))} {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls]))))))))

(deftest a-villager-seen-by-one-look-of-the-round-is-found-not-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result] (await (roll {:entities [(librarian [(offer "enchanted_book")])]
                                     :wrap #(do (ls/entities-during-look (:p %) 2) %)} {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls]))))))))

(deftest the-wall-time-ends-the-wait-when-the-villager-never-takes-the-workstation
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(jobless)]} {:spend {:wall-time 30}}))]
          (is (= {:found false :reason :wall-time} (select-keys result [:found :reason])))
          (is (= "lectern" (workstation p))))))))

(defn ticking!
  "env whose world age runs with the clock (a game tick each 50 ms) at time of day t: the game ticks villagers count by."
  [t]
  (fn [{:keys [p clock] :as env}]
    (swap! (fake/state p) assoc :time t)
    (add-watch clock :age (fn [_ _ _ ms] (swap! (fake/state p) assoc-in [:self :worldAge] (js/Math.floor (/ ms 50)))))
    env))

(defn claimed-elsewhere
  "Another villager holds the claim on the standing lectern: v-1 takes no job until the lectern is broken (the break
  drops every claim), then rolls as rolls-offers! makes it."
  [scripts]
  (fn [{:keys [p] :as env}]
    (add-watch (fake/state p) :claim
               (fn [_ s _ w]
                 (->> [w] (remove #(= "lectern" (get-in % [:blocks ws-pos])))
                      (run! (fn [_] (remove-watch s :claim) (rolls-offers! p scripts))))))
    env))

(defn walks-in-and-takes-the-job
  "v-1 walks into the open pen's cell after-ms (default 10 s) of clock after the run starts, and takes the job (offers)
  once the near end is plugged behind it with the lectern standing."
  [offers & [after-ms]]
  (fn [{:keys [p clock] :as env}]
    (let [at (+ @clock (or after-ms 10000))
          s (fake/state p)
          v1 #(first (keep-indexed (fn [i e] (when (= "v-1" (:uuid e)) i)) (:entities @s)))]
      (add-watch clock :walk-in (fn [_ _ _ ms] (->> [ms] (filter #(>= % at))
                                                    (run! (fn [_] (remove-watch clock :walk-in)
                                                            (swap! s assoc-in [:entities (v1) :pos] [3.5 64 0.5]))))))
      (add-watch s :job (fn [_ _ _ w]
                          (let [e (get-in w [:entities (v1)])]
                            (->> [e] (filter #(and (= [3.5 64 0.5] (:pos %)) (= "unemployed" (:profession %))
                                                   (= "lectern" (get-in w [:blocks ws-pos])) (= "cobblestone" (get-in w [:blocks [4 65 0]]))))
                                 (run! (fn [_] (remove-watch s :job)
                                         (swap! s update-in [:entities (v1)] assoc :profession "librarian" :offers offers)))))))
      env)))

(deftest a-roll-with-no-villager-named-rolls-the-one-that-walked-into-the-pen-not-the-one-picked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [picked (jobless {:id 2 :uuid "v-2" :pos {:x 6 :y 64 :z 3}})
              walker (jobless {:pos {:x 9 :y 64 :z 6}})
              [result p seen] (await (roll {:entities [picked walker] :keeps-job? true
                                            :blocks {"4,64,0" "air" "4,65,0" "air" "4,63,0" "stone"}
                                            :inventory [{:name "cobblestone" :count 8} {:name "torch" :count 2}]
                                            :wrap (comp (walks-in-and-takes-the-job [(offer "enchanted_book")]) (ticking! 1000)
                                                        #(do (tu/seeing-all (:p %)) %))}
                                           {:villager nil}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls])) (pr-str result))
          (is (= ["v-2" "v-1"] (map :villager (filter #(= :roll.villager (:kind %)) @seen))))
          (is (= ["cobblestone" "cobblestone"] (map #(get-in @(fake/state p) [:blocks %]) [[4 64 0] [4 65 0]]))))))))

(deftest a-villager-out-of-view-of-the-body-boxed-in-the-built-pen-is-waited-for-to-walk-in-not-looked-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(jobless {:pos {:x 60 :y 64 :z 3}})] :keeps-job? true
                                       :blocks {"4,64,0" "air" "4,65,0" "air" "4,63,0" "stone"}
                                       :inventory [{:name "cobblestone" :count 8} {:name "torch" :count 2}]
                                       :wrap (comp (walks-in-and-takes-the-job [(offer "enchanted_book")] 60000) (ticking! 1000)
                                                   #(do (tu/seeing-all (:p %)) %))}
                                      {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls])) (pr-str result))
          (is (= ["cobblestone" "cobblestone"] (map #(get-in @(fake/state p) [:blocks %]) [[4 64 0] [4 65 0]]))))))))

(deftest a-penned-villager-kept-from-its-free-workstation-by-anothers-claim-takes-it-after-one-break
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(jobless)] :keeps-job? true
                                       :wrap (comp (claimed-elsewhere [[(offer "enchanted_book")]]) (ticking! 1000))}
                                      {}))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])) (pr-str result))
          (is (= 1 (digs p)))
          (is (= "lectern" (workstation p))))))))

(deftest a-penned-villager-that-takes-no-job-ends-no-job-naming-the-other-job-sites-after-one-break
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(jobless)] :keeps-job? true
                                       :blocks {"6,64,0" "composter"} :wrap (comp (ticking! 1000) #(do (tu/seeing-all (:p %)) %))}
                                      {:settings {:jobs.lib.villager/take-job-ticks 200}}))]
          (is (= {:found false :reason :no-job :status :stopped} (select-keys result [:found :reason :status])))
          (is (= [{:name "composter" :pos {:x 6 :y 64 :z 0}}] (:sites result)))
          (is (= 1 (digs p)) "one break drops another villager's claim; a second would change nothing")
          (is (= "lectern" (workstation p))))))))

(deftest a-jobless-villager-is-waited-for-in-game-ticks-of-its-walking-hours-not-in-daytime-seconds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env ((ticking! 9500) (bd/setup (merge pen {:entities [(jobless)]
                                                         :blocks (merge {ws-key "lectern" "2,63,0" "stone"} (:blocks pen))})))
              result (await (child-outcome env {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book"
                                                :spend {:wall-time 400}} 700))
              p (:p env)]
          (is (= {:found false :reason :wall-time} (select-keys result [:found :reason]))
              "at the villagers' meeting hour (day, no walking) nothing counts toward the wait")
          (is (zero? (digs p)) "no break and place again: 300 daytime seconds used to send one"))))))



(deftest a-villager-that-took-another-job-while-the-workstation-was-out-is-not-waited-for-the-workstation-goes-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(jobless {:profession "farmer"})] :keeps-job? true
                                       :inventory [{:name "lectern" :count 1}] :blocks {ws-key "air"}}
                                      {:spend {:wall-time 30}}))]
          (is (= {:found false :status :stopped :reason :other-job} (select-keys result [:found :status :reason])))
          (is (= "lectern" (workstation p))))))))

(deftest a-named-villager-with-another-job-ends-other-job-without-a-break
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(vt/villager [(offer "paper")] {:level 1 :pos in-pen})] :keeps-job? true} {}))]
          (is (= {:found false :status :stopped :reason :other-job} (select-keys result [:found :status :reason])))
          (is (zero? (digs p))))))))

(deftest a-spare-carried-is-placed-the-moment-the-job-is-gone-and-the-drop-picked-up-after
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (roll {:entities [(librarian [(offer "paper")])] :inventory [{:name "lectern" :count 1}]
                                            :scripts [[(offer "enchanted_book")]]} {}))
              acts (keep #(when (and (= :action (:source %)) (= :started (:kind %)) (#{"dig" "place" "collect"} (:name %))) (:name %)) @seen)]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])))
          (is (= ["dig" "place" "collect"] acts) "the lectern is out no longer than the job takes to go")
          (is (= 1 (count (filter #(= "lectern" (:name %)) (:inventory @(fake/state p)))))
              "the drop is in the bag again")
          (is (= "lectern" (workstation p))))))))

(deftest a-restart-with-the-workstation-missing-places-it-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(jobless)]
                                       :inventory [{:name "lectern" :count 1}]
                                       :blocks {ws-key "air"}
                                       :scripts [[(offer "enchanted_book")]]}
                                      {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls])))
          (is (zero? (digs p)))
          (is (= "lectern" (workstation p))))))))

(deftest a-restart-with-the-workstation-missing-and-a-drop-beside-it-does-not-place-a-spare
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(jobless)
                                                  {:id 90 :name "item" :kind "item" :pos {:x 3.5 :y 64 :z 0.5} :item {:name "lectern" :count 1}}]
                                       :inventory [{:name "lectern" :count 2}] :pen? false
                                       :blocks {ws-key "air" "3,64,0" "air"}}
                                      {}))]
          (is (= {:status :stopped :reason :workstation-lost} (select-keys result [:status :reason]))
              "the roll went for the lying drop first; the old fallback of 1 placed the spare and went on")
          (is (= "lectern" (workstation p)) "the spare is put back at the end"))))))

(deftest a-workstation-in-anothers-zone-is-not-broken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ p] (await (roll {:entities [(librarian [(offer "paper")])]
                                  :zones [{:name "farm" :min [2 60 0] :max [2 70 0] :owner "Miles"}]}
                                 {}))]
          (is (zero? (digs p)))
          (is (= "lectern" (workstation p))))))))

(deftest the-calls-ways-go-to-the-place-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ways (atom [])
              wrap (fn [env]
                     (let [j (get (:jobs (:eng env)) 'jobs.blocks.place)]
                       (assoc-in env [:eng :jobs 'jobs.blocks.place]
                                 (assoc j :round (fn ^:async spy [c]
                                                   (swap! ways conj (:ways (:args c)))
                                                   (await ((:round j) c)))))))
              [result p] (await (roll {:entities [(librarian [(offer "paper")])]
                                       :scripts [[(offer "enchanted_book")]]
                                       :wrap wrap}
                                      {:ways #{:carried}}))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])))
          (is (= [#{:carried}] (distinct @ways)))
          (is (= "lectern" (workstation p))))))))

(deftest ignore-zones-breaks-and-places-in-anothers-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "paper")])]
                                       :scripts [[(offer "enchanted_book")]]
                                       :zones [{:name "farm" :min [2 60 0] :max [2 70 0] :owner "Miles"}]}
                                      {:ignore-zones? true}))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])))
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

(defn breaks-without-a-drop
  "env whose dig child clears the workstation cell and picks nothing up (the drop slid out of reach)."
  [env]
  (let [p (:p env)
        j (get (:jobs (:eng env)) 'jobs.blocks.dig)]
    (assoc-in env [:eng :jobs 'jobs.blocks.dig]
              (assoc j :round (fn ^:async stubbed [c]
                                (swap! (fake/state p) assoc-in [:blocks ws-pos] "air")
                                (ctx/end c :done nil {:dug true :collected 0}))))))

(deftest a-broken-workstation-whose-drop-is-out-of-reach-ends-workstation-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "paper")])] :wrap breaks-without-a-drop} {}))]
          (is (= {:found false :status :stopped :reason :workstation-lost} (select-keys result [:found :status :reason])))
          (is (zero? (count (vt/calls p "place"))) "no place is tried without the item")
          (is (= "air" (workstation p))))))))

(deftest a-lost-drop-is-not-covered-by-a-spare-lectern-in-the-bag
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(librarian [(offer "paper")])] :inventory [{:name "lectern" :count 1}]
                                       :scripts [[(offer "paper")] [(offer "paper")]]
                                       :wrap breaks-without-a-drop} {}))]
          (is (= {:found false :status :stopped :reason :workstation-lost :rolls 1} (select-keys result [:found :status :reason :rolls])) "the first lost drop stops the roll")
          (is (= "lectern" (workstation p)) "the spare is put back at the end: the cell stays shut"))))))

(defn eats-a-second-lectern-on-the-place
  "env whose dig child picks the drop up, and whose workstation, once placed back, takes one more lectern out of the bag:
  a loss no pickup count shows."
  [env]
  (let [state (fake/state (:p env))
        j (get (:jobs (:eng env)) 'jobs.blocks.dig)
        env (assoc-in env [:eng :jobs 'jobs.blocks.dig]
                      (assoc j :round (fn ^:async stubbed [c]
                                        (swap! state #(-> % (assoc-in [:blocks ws-pos] "air") (update :inventory conj {:name "lectern" :count 1})))
                                        (ctx/end c :done nil {:dug true :collected 1}))))]
    (add-watch state :eat
               (fn [_ _ old w]
                 (when (and (= "air" (get-in old [:blocks ws-pos])) (= "lectern" (get-in w [:blocks ws-pos])))
                   (swap! state update-in [:inventory 0 :count] dec))))
    env))

(deftest a-lectern-missing-from-the-bag-after-the-place-ends-workstation-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result _] (await (roll {:entities [(librarian [(offer "paper")])] :inventory [{:name "lectern" :count 2}]
                                       :scripts [[(offer "paper")] [(offer "paper")]]
                                       :wrap eats-a-second-lectern-on-the-place} {}))]
          (is (= {:status :stopped :reason :workstation-lost :carried 1 :expected 2}
                 (select-keys result [:status :reason :carried :expected]))
              "the roll stops at the first lectern that is gone, not after 20 digs"))))))

(deftest the-body-beside-the-workstation-steps-flush-against-it-before-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ p] (await (roll {:entities [(librarian [(offer "paper")])] :self {:pos {:x 1.5 :y 64 :z 0.5}}
                                  :wrap breaks-without-a-drop} {}))
              goals (map #(let [t (.-pos (.-args %))] [(.-x t) (.-z t)]) (vt/calls p "moveTo"))]
          (is (some #(and (< (Math/abs (- 1.7 (first %))) 0.01) (< (Math/abs (- 0.5 (second %))) 0.01)) goals)))))))

(deftest a-seen-drop-out-of-reach-is-walked-up-to-and-pressed-against
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [drop {:id 90 :name "item" :kind "item" :pos {:x 4.5 :y 64 :z 0.5} :item {:name "lectern" :count 1}}
              [_ p] (with-redefs [roll/penned? (constantly true)]
                      ;; no pen: a drop two cells beyond the workstation, as behind a training cell's back seal
                      (await (roll {:entities [(librarian [(offer "paper")]) drop] :self {:pos {:x 1.5 :y 64 :z 0.5}} :pen? false
                                    :wrap breaks-without-a-drop} {})))
              goals (map #(let [t (.-pos (.-args %))] [(.-x t) (.-z t)]) (vt/calls p "moveTo"))]
          (is (some #(and (= 4.5 (first %)) (= 0.5 (second %))) goals)
              "the body presses up against the wall nearest the drop, so a gap in it lets the pickup reach through"))))))

(deftest the-walks-to-a-lost-drop-take-the-calls-worth-and-a-root-roll-the-wanted-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [worths (atom [])
              spy (fn [env]
                    (let [j (get (:jobs (:eng env)) 'jobs.movement.go-to)]
                      (assoc-in env [:eng :jobs 'jobs.movement.go-to]
                                (assoc j :round (fn ^:async spy [c]
                                                  (when (= #{:walk} (:ways (:args c))) (swap! worths conj (ctx/worth c)))
                                                  (await ((:round j) c)))))))
              drop {:id 90 :name "item" :kind "item" :pos {:x 4.5 :y 64 :z 0.5} :item {:name "lectern" :count 1}}
              _ (await (roll {:entities [(librarian [(offer "paper")]) drop] :self {:pos {:x 1.5 :y 64 :z 0.5}}
                              :wrap (comp spy breaks-without-a-drop)} {:worth 40}))]
          (is (seq @worths))
          (is (every? #{40} @worths) "a walk to the drop is worth the call's goal, not the drop's own worth")
          (is (= (:value (cost/goal-value [{:name "enchanted_book" :count 1}])) (roll/worth-of {:args {:enchant "mending"}}))
              "a root roll is worth one of what the wanted offer gives"))))))

(defn body-cell [w]
  (let [pos (get-in w [:self :pos])]
    (mapv js/Math.floor (if (map? pos) [(:x pos) (:y pos) (:z pos)] pos))))

(defn picks-up-crouched-in-the-cell!
  "The drop lies in the villager's cell behind the workstation: the fake gives it to a body that sneaks into the emptied
  workstation cell (pickup reach covers the cell behind from there), as the game does."
  [p]
  (add-watch (fake/state p) :crouch-pickup
             (fn [_ state _ w]
               (when (and (= [2 64 0] (body-cell w)) (get-in w [:controls :sneak])
                          (not-any? #(= "lectern" (:name %)) (:inventory w)))
                 (swap! state update :inventory conj {:name "lectern" :count 1}))))
  p)

(deftest a-drop-in-the-cell-behind-is-taken-crouched-in-the-emptied-workstation-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (with-redefs [roll/penned? (constantly true)]
                           ;; no slab over the cell: the fake's body stays 1.8 high when it sneaks, so it would not fit under one
                           (await (roll {:entities [(librarian [(offer "paper")])] :blocks {"2,65,0" "air"}
                                         :scripts [[(offer "enchanted_book")]]
                                         :wrap (fn [env] (picks-up-crouched-in-the-cell! (:p env)) (breaks-without-a-drop env))} {})))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])) "the drop was taken and the lectern placed back")
          (is (= "lectern" (workstation p)))
          (is (not= [2 64 0] (body-cell @(fake/state p))) "the body sneaked back out of the cell"))))))

(def dug-cells (atom []))

(defn digs-only-what-it-is-given
  "env whose dig child clears the cell it is given and picks up nothing: the workstation's drop is out of reach."
  [env]
  (let [p (:p env)
        cell (fn [c] (let [x (:pos (:args c))] (if (map? x) [(:x x) (:y x) (:z x)] (vec x))))
        stub (fn [env job round] (assoc-in env [:eng :jobs job] (assoc (get (:jobs (:eng env)) job) :round round)))]
    (stub env 'jobs.blocks.dig
          (fn ^:async dig [c]
            (swap! dug-cells conj (cell c))
            (swap! (fake/state p) assoc-in [:blocks (cell c)] "air")
            (ctx/end c :done nil {:dug true})))))

(deftest a-drop-out-of-reach-under-a-slab-ends-workstation-lost-naming-the-drop-cell-and-digs-nothing-above
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [_ (reset! dug-cells [])
              [result p] (await (roll {:entities [(librarian [(offer "paper")])] :blocks {"2,65,0" "bamboo_slab"}
                                       :wrap digs-only-what-it-is-given} {}))]
          (is (= {:status :stopped :reason :workstation-lost} (select-keys result [:status :reason])) "no drop ever came")
          (is (= {:x 3 :y 64 :z 0} (:drop-cell result)) "the ending names the cell beyond the workstation from the dig stand")
          (is (= "bamboo_slab" (get-in @(fake/state p) [:blocks [2 65 0]])) "the slab over the cell stays: the cell is never opened")
          (is (= [[2 64 0]] @dug-cells) "only the workstation was dug"))))))

(deftest a-place-child-that-stops-ends-with-its-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (roll {:entities [(jobless)]
                                            :blocks {ws-key "air"} :inventory [{:name "lectern" :count 1}]
                                            :wrap (stub-job 'jobs.blocks.place stopped)} {}))]
          (is (= {:found false :status :stopped :reason :no-tool} (select-keys result [:found :status :reason])))
          (is (= "air" (workstation p)))
          (is (not (contains? (vt/kinds seen) :roll.gave-up))))))))

(deftest a-place-refused-for-a-mob-in-the-cell-is-waited-out-and-placed-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [refused (atom false)
              wrap (fn [env]
                     (let [j (get (:jobs (:eng env)) 'jobs.blocks.place)]
                       (assoc-in env [:eng :jobs 'jobs.blocks.place]
                                 (assoc j :round (fn ^:async refuse-once [c]
                                                   (if @refused
                                                     (await ((:round j) c))
                                                     (do (reset! refused true)
                                                         (ctx/end c :nothing-possible {:reason :failed} {:placed false :reason :failed :mobs ["villager"]}))))))))
              [result p] (await (roll {:entities [(jobless)]
                                       :inventory [{:name "lectern" :count 1}]
                                       :blocks {ws-key "air"}
                                       :scripts [[(offer "enchanted_book")]]
                                       :wrap wrap}
                                      {}))]
          (is (= {:found true :rolls 0} (select-keys result [:found :rolls])))
          (is (= "lectern" (workstation p))))))))

(deftest the-place-child-digs-nothing-in-the-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen (atom nil)
              wrap (fn [env]
                     (let [j (get (:jobs (:eng env)) 'jobs.blocks.place)]
                       (assoc-in env [:eng :jobs 'jobs.blocks.place]
                                 (assoc j :round (fn ^:async spy [c]
                                                   (reset! seen (:args c))
                                                   (await ((:round j) c)))))))]
          (await (roll {:entities [(jobless)]
                        :inventory [{:name "lectern" :count 1}]
                        :blocks {ws-key "air"}
                        :scripts [[(offer "enchanted_book")]]
                        :wrap wrap}
                       {}))
          (is (false? (:through @seen))))))))

(deftest a-body-on-the-workstation-cell-steps-off-before-placing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (roll {:entities [(jobless)]
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
        (let [[result p] (await (roll {:entities [(jobless)]
                                       :inventory [{:name "lectern" :count 1}]
                                       :self {:pos {:x 2 :y 64 :z 0}}
                                       :floor [2 0 2 0]
                                       :blocks {ws-key "air" "1,63,0" "air" "0,63,0" "air" "1,63,1" "air" "1,63,-1" "air"}}
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
        (let [env (bd/setup (with-pen {:entities [(librarian [(offer "paper")])] :blocks {ws-key "lectern" "2,63,0" "stone"}}))
              _ (unemploys-after! env 8000 [(offer "enchanted_book")])
              result (await (child-outcome env {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book" :spend {:wall-time 600}} 300))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])))
          (is (= "lectern" (workstation (:p env)))))))))

(deftest a-villager-that-loses-its-job-late-is-still-rolled-no-clock-of-the-job-gives-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup (with-pen {:entities [(librarian [(offer "paper")])] :blocks {ws-key "lectern" "2,63,0" "stone"}}))
              _ (unemploys-after! env 30000 [(offer "enchanted_book")])
              result (await (child-outcome env {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book" :spend {:wall-time 600}} 300))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])))
          (is (= "lectern" (workstation (:p env)))))))))

(defn hides-after-break!
  "The villager steps out of sight when the workstation is broken and is back ms of clock after it stands again."
  [{:keys [p clock]} ms offers]
  (let [state (fake/state p)
        away (atom nil)
        back-at (atom nil)
        step (fn [_ _ _ _]
               (let [w @state
                     stands? (= "lectern" (get-in w [:blocks ws-pos]))
                     v (first (filter #(= "v-1" (:uuid %)) (:entities w)))]
                 (cond
                   (and v (not stands?)) (do (reset! away (assoc v :profession "unemployed" :offers nil))
                                             (swap! state update :entities #(vec (remove (fn [e] (= "v-1" (:uuid e))) %))))
                   (and @away stands? (nil? @back-at)) (reset! back-at (+ @clock ms))
                   (and @away @back-at (>= @clock @back-at))
                   (let [v @away]
                     (reset! away nil)
                     (swap! state update :entities conj (assoc v :profession "librarian" :offers offers))))))]
    (add-watch state :hide step)
    (add-watch clock :hide step)))

(deftest a-villager-out-of-view-after-the-reroll-is-waited-for-not-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup (with-pen {:entities [(librarian [(offer "paper")])] :blocks {ws-key "lectern" "2,63,0" "stone"}}))
              _ (hides-after-break! env 3000 [(offer "enchanted_book")])
              result (await (child-outcome env {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book" :spend {:wall-time 600}} 300))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls]))))))))

(defn shifts-body-and-hides-villager-after-break!
  "The break moves the body 0.4 along x (the pickup of the drop); the villager is in sight only from the spot it was
  first seen from, as through a slit."
  [{:keys [p]} offers]
  (let [state (fake/state p)
        home (get-in @state [:self :pos])
        gone (atom nil)
        step (fn [_ _ _ w]
               (let [at (get-in w [:self :pos])
                     v (first (filter #(= "v-1" (:uuid %)) (:entities w)))]
                 (cond
                   (and v (not= "lectern" (get-in w [:blocks ws-pos])))
                   (do (reset! gone (assoc v :profession "unemployed" :offers nil))
                       (swap! state #(-> % (update :entities (fn [es] (vec (remove (fn [e] (= "v-1" (:uuid e))) es))))
                                         (update-in [:self :pos 0] + 0.4))))
                   (and @gone (= home at) (= "lectern" (get-in w [:blocks ws-pos])))
                   (let [g @gone]
                     (reset! gone nil)
                     (swap! state update :entities conj (assoc g :profession "librarian" :offers offers))))))]
    (add-watch state :slit step)))

(deftest a-villager-seen-only-from-where-the-body-first-stood-is-looked-for-from-there-not-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup (with-pen {:entities [(librarian [(offer "paper")])] :blocks {ws-key "lectern" "2,63,0" "stone"}}))
              _ (shifts-body-and-hides-villager-after-break! env [(offer "enchanted_book")])
              result (await (child-outcome env {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book" :spend {:wall-time 600}} 300))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls]))))))))

(deftest a-farther-view-spot-behind-a-wall-is-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup (with-pen {:entities [(librarian [(offer "paper")])] :self {:pos {:x 1.5 :y 64 :z 0.5}}
                                       :blocks {ws-key "lectern" "2,63,0" "stone" "0,64,0" "stone"}}))
              _ (hides-after-break! env 100000000 [(offer "paper")])
              _ (await (child-outcome env {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book" :spend {:wall-time 600}} 120))
              xs (map #(.-x (.-pos (.-args %))) (vt/calls (:p env) "moveTo"))]
          (is (not-any? #(< % 1) xs) (pr-str xs)))))))

(deftest a-roll-cut-after-the-break-places-the-workstation-back-in-its-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (bd/setup (with-pen {:entities [(librarian [(offer "paper")])] :zones []
                                                         :inventory [{:name "lectern" :count 1}]
                                                         :blocks {ws-key "lectern" "2,63,0" "stone"}}))
              cut (atom false)]
          (rolls-offers! p [[(offer "paper")]])
          (add-watch (fake/state p) :cut
                     (fn [_ _ _ w]
                       (when (and (not @cut) (not= "lectern" (get-in w [:blocks ws-pos])))
                         (reset! cut true)
                         (core/cancel! eng (first (:list (core/state eng)))))))
          (core/submit! eng (tu/order (list job {:villager "v-1" :pos ws-pos :profession "librarian" :want "enchanted_book" :spend {:wall-time 600}})) {})
          (loop [i 0]
            (when (and (< i 300) (seq (:list (core/state eng))))
              (swap! clock + 700)
              (await (core/tick! eng))
              (recur (inc i))))
          (is @cut "the cut came after the break")
          (is (= "lectern" (workstation p)) "the cleanup put it back"))))))

(deftest the-break-the-wait-for-the-job-to-go-and-the-place-are-one-urgent-section
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result _ seen] (await (roll {:entities [(librarian [(offer "paper")])] :scripts [[(offer "enchanted_book")]]} {}))
              marks (keep #(cond
                             (= :section (:kind %)) [(if (:open %) :open :close) (:section %) (:holds %)]
                             (and (= :action (:source %)) (= :started (:kind %)) (#{"dig" "place"} (:name %))) [(keyword (:name %))])
                          @seen)]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])))
          (is (= [[:open :roll :urgent] [:dig] [:place] [:close :roll nil]] (distinct marks))
              "no gap where a night or food reflex may cut in with the workstation out"))))))

(deftest a-roll-from-afar-walks-to-the-cell-before-the-urgent-section-and-never-walks-back-inside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (roll {:entities [(librarian [(offer "paper")])] :self {:pos {:x 10.5 :y 64 :z 0.5}}
                                            :scripts [[(offer "enchanted_book")]]} {}))
              marks (keep #(cond
                             (= :section (:kind %)) (if (:open %) :open :close)
                             (= :child_started (:kind %)) (:slot %))
                          @seen)
              inside (->> marks (drop-while #(not= :open %)) rest (take-while #(not= :close %)))]
          (is (= {:found true :rolls 1} (select-keys result [:found :rolls])))
          (is (= "lectern" (workstation p)))
          (is (some #{:to-cell :stand} (take-while #(not= :open %) marks)) "the walk to the cell (into the pen's stand) comes before the section")
          (is (not-any? #{:walk :to-stand} inside) (str "no walk while the workstation is out: " (pr-str inside))))))))
