(ns engine.walk-dangers-test
  "The walks' plans (jobs.lib.near/plan!) keep away from the dangers the body knows of (jobs.lib.threats): wide round a
  zombie for an unarmed body, nearer with a sword, straight with dangers off (a walk up to the mob it fights), and
  round a remembered :threat spot."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.path.executor :as executor]
            [engine.ready-test :as ready-test]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.flee-search :as search]
            [jobs.lib.near :as near]
            [jobs.lib.threats :as threats]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.walk.world :as wworld]))

(def zombie {:id 9 :uuid "u9" :name "zombie" :kind "hostile" :pos {:x 20.5 :y 64 :z 0.5}})

(defn nearest
  "The least distance from a step's stand point to the zombie's place."
  [steps]
  (apply min (map (fn [{:keys [px pz]}] (js/Math.hypot (- px 20.5) (- pz 0.5))) steps)))

(declare plan-steps)

(defn ^:async plan-past
  "near/plan! from [0 64 0] to [40 64 0] for a body with world spec (merged) and :threat entries remembered first, with
  dangers on or off, with the settings overrides: the plan's steps."
  [world remembered dangers & [overrides]]
  (await (settings/with-settings (or overrides {}) (fn ^:async plan [] (await (plan-steps world remembered dangers))))))

(defn ^:async plan-steps [world remembered dangers]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor (merge {:floor [-5 -20 45 20]} world))
        out (atom nil)
        parent {:round (fn ^:async planning-round [c]
                         (doseq [t remembered] (if (:death-t t) (ctx/remember! c :deaths t core/deaths-policy) (threats/remember! c t)))
                         (reset! out (await (near/plan! (assoc c :args {}) [40 64 0] 0 :never (wworld/body-policy c) [] false nil nil true dangers)))
                         (ctx/end c :done nil))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'planning-parent parent)
                          :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng (tu/order '(planning-parent)) {})
    (await (tu/tick-until-idle! eng 20))
    (:steps @out)))

(deftest an-unarmed-body-plans-wide-round-a-zombie-it-sees
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [steps (await (plan-past {:entities [zombie]} [] true))]
          (is (seq steps))
          (is (>= (nearest steps) 8) (str "nearest " (nearest steps))))))))

(deftest the-danger-cap-bounds-a-mobs-hurt-not-its-death-and-the-defaults-keep-the-plan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plain (await (plan-past {:entities [zombie]} [] true))
              default (await (plan-past {:entities [zombie]} [] true {:jobs.lib.cost.danger/max-rate 4}))
              capped (await (plan-past {:entities [zombie]} [] true {:jobs.lib.cost.danger/max-rate 0.2}))]
          (is (= (mapv (juxt :x :z) plain) (mapv (juxt :x :z) default)) "today's values as options: the same plan")
          (is (= (nearest capped) (nearest plain)) "an unarmed body's toll is the death the zombie risks, which no cap clips (Q142)"))))))

;; a zombie met costs an unarmoured body its blows (some 3 hp at 10 s an hp) even with a sword: only armour makes the
;; meeting cheap enough to pass near
(def diamond-kit {:head {:name "diamond_helmet" :count 1} :torso {:name "diamond_chestplate" :count 1}
                  :legs {:name "diamond_leggings" :count 1} :feet {:name "diamond_boots" :count 1}})

(deftest a-sword-and-armour-pass-nearer-and-dangers-off-goes-straight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [unarmed (await (plan-past {:entities [zombie]} [] true))
              armed (await (plan-past {:entities [zombie] :inventory [{:name "diamond_sword" :count 1}] :equipment diamond-kit} [] true))
              off (await (plan-past {:entities [zombie]} [] false))]
          (is (< (nearest armed) (nearest unarmed)))
          (is (< (nearest off) 1) "dangers off: straight through the zombie's place"))))))

(deftest a-remembered-threat-spot-is-kept-away-from
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [steps (await (plan-past {:entities [(assoc zombie :visible false)]} [{:mob "zombie" :id 9 :uuid "u9" :pos {:x 20.5 :y 64 :z 0.5} :ended 1}] true))]
          (is (>= (nearest steps) 8) (str "nearest " (nearest steps))))))))

(def death-at (fn [x] {:pos {:x x :y 64 :z 0.5} :cause "zombie" :death-t 990000}))

(deftest a-recent-death-is-kept-away-from-but-a-walk-to-its-spot-goes-straight-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [round (await (plan-past {} [(death-at 20.5)] true))
              to-pile (await (plan-past {} [(death-at 40.5)] true))]
          (is (>= (nearest round) 8) (str "nearest " (nearest round)))
          (is (= 40 (:x (last to-pile))) "the pile at the death spot is reached, not walled off"))))))

(deftest a-remembered-threat-spot-of-a-mob-the-world-no-longer-has-is-forgotten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [steps (await (plan-past {} [{:mob "creeper" :id 9 :uuid "u9" :pos {:x 20.5 :y 64 :z 0.5} :ended 1}] true))]
          (is (< (nearest steps) 1) (str "nearest " (nearest steps))))))))

(deftest a-kept-search-is-new-once-a-danger-moved-died-or-came
  (let [pw #js {:snapshot nil :table nil :space nil}
        with (fn [& ds] (wworld/danger-key (wworld/with-dangers pw (clj->js (vec ds)))))
        z {:mob "zombie" :x 20.5 :y 64 :z 0.5 :rate 4}]
    (is (nil? (wworld/danger-key pw)))
    (is (= (with z) (with (assoc z :x 21.2))) "a step within its 4 blocks: the same search goes on")
    (is (not= (with z) (with (assoc z :x 30.5))) "moved on")
    (is (not= (with z) (with)) "died: none left")
    (is (not= (with z) (with z (assoc z :mob "skeleton"))) "another came")
    (is (= (with z (assoc z :mob "skeleton")) (with (assoc z :mob "skeleton") z)) "the order they are met in is no change")))

(defn key-of [world]
  (let [c (ready-test/ctx-of (merge {:floor [-5 -20 45 20]} world))]
    (wworld/tolls-key (wworld/costed-world c (wworld/path-world (:primitives c)) {:dangers? true}))))

(deftest the-search-key-follows-the-body-s-weapon-and-health
  (let [unarmed (key-of {:entities [zombie]})
        armed (key-of {:entities [zombie] :inventory [{:name "diamond_sword" :count 1}]})
        hurt (key-of {:entities [zombie] :inventory [{:name "diamond_sword" :count 1}] :self {:health 6}})]
    (is (= unarmed (key-of {:entities [zombie]})) "same body, same key")
    (is (not= unarmed armed) "a weapon picked up: a new search")
    (is (not= armed hurt) "health lost: a new search")))

(defn inputs-of
  "The toll inputs a walk's search key holds (part 11) over the costed world of a body with world spec (merged)."
  [world]
  (let [c (ready-test/ctx-of (merge {:floor [-5 -20 45 20]} world))]
    (nth (wsearch/search-key c [40 64 0] 1 1 executor/policy nil (wworld/costed-world c (wworld/path-world (:primitives c)) {:dangers? true})) 11)))

;; the tolls of a sensed mob reach the search key only through its weight-free inputs: a walk's kept search must not go
;; on priced for a body that has since been hurt or disarmed
(deftest a-walk-s-search-key-holds-what-its-tolls-are-priced-by
  (let [armed {:entities [zombie] :inventory [{:name "diamond_sword" :count 1}]}
        hurt (assoc armed :self {:health 6})]
    (is (threats/toll-inputs-no-worse? (inputs-of armed) (inputs-of armed)) "same body, same threats")
    (is (threats/toll-inputs-no-worse? (inputs-of armed) (inputs-of hurt)) "health regained only cheapens")
    (is (not (threats/toll-inputs-no-worse? (inputs-of hurt) (inputs-of armed))) "a hit taken")
    (is (not (threats/toll-inputs-no-worse? (inputs-of (dissoc armed :inventory)) (inputs-of armed))) "the sword lost")))

(defn ^:async budgeted-plan
  "go-to's budgeted plan (near/plan! with a round's budget) from [0 64 0] to [40 64 0] for a body with world spec
  (merged), along seeded route seed (go-to's :route) when given (or a fn of the round's ctx giving it)."
  [world seed]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor (merge {:floor [-5 -20 45 20]} world))
        out (atom nil)
        parent {:round (fn ^:async planning-round [c]
                         (reset! out (await (near/plan! (cond-> (assoc c :args {}) seed (assoc :seed (if (fn? seed) (seed c) seed))) [40 64 0] 0 :never (wworld/body-policy c) [] false nil wsearch/round-budget true true)))
                         (ctx/end c :done nil))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'planning-parent parent)
                          :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng (tu/order '(planning-parent)) {})
    (await (tu/tick-until-idle! eng 20))
    @out))

(defn route-of
  "A found plan's route as go-to's :route: each planner step's cell and its price."
  [plan]
  (mapv (fn [^js s] {:x (.-x s) :y (.-y s) :z (.-z s) :g (.-g s)}) (array-seq (.-steps (.-path (:r plan))))))

;; a flee step hands go-to the route its search priced (8dd23380): walked as is when go-to's own prices agree, no search
(deftest a-seeded-route-go-to-prices-the-same-is-walked-without-a-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seed (route-of (await (budgeted-plan {} nil)))
              plan (await (budgeted-plan {} seed))]
          (is (:seeded plan))
          (is (<= (.-expanded ^js (:r plan)) (count seed)) "only the seed's cells were searched")
          (is (= (mapv (juxt :x :y :z) (rest seed)) (mapv (juxt :x :y :z) (rest (route-of plan))))))))))

(deftest a-seeded-route-a-new-threat-made-dearer-is-planned-afresh
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seed (route-of (await (budgeted-plan {} nil)))
              off-line (assoc zombie :pos {:x 20.5 :y 64 :z 5.5})
              plan (await (budgeted-plan {:entities [off-line]} seed))
              nearest (apply min (map (fn [{:keys [px pz]}] (js/Math.hypot (- px 20.5) (- pz 5.5))) (:steps plan)))]
          (is (not (:seeded plan)))
          (is (> nearest 5.5) (str "the fresh plan keeps further from the zombie than the seed's 5 blocks: " nearest)))))))

;; the flee search prices its route with the chasers left out; go-to tolls them (8dd23380)
(deftest a-flee-step-s-route-re-priced-with-the-chaser-tolled-is-walked-without-a-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [chaser (assoc zombie :pos {:x 3.5 :y 64 :z 2.5})
              bare (route-of (await (budgeted-plan {} nil)))
              raw (await (budgeted-plan {:entities [chaser]} bare))
              plan (await (budgeted-plan {:entities [chaser]} #(search/go-to-route % bare 0)))]
          (is (not (:seeded raw)) "the chaser-free prices are dearer than go-to's own world allows")
          (is (:seeded plan))
          (is (<= (.-expanded ^js (:r plan)) (count bare)) "no full search"))))))
