(ns engine.planner-fence-strip-test
  "A body in the sliver between a bank's fence posts and a wheat field 2 blocks below (live, Leo: a block dump of
  x -370..-363, y 64..68, z -847..-837): the way out is a drop past the posts at the strip's north end, else a drop onto
  farmland, which a walk prices (the till of the dirt it leaves)."
  (:require [cljs.test :refer [deftest is async]]
            [clojure.string :as str]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.path.fixture :as fx]
            [engine.planner-fixture :as pf :refer [near]]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.near :as near]
            [jobs.lib.walk.world :as wworld]))

(def dump
  "y 64..68 (bottom first), rows z -847..-837, columns x -370..-363; '?' unseen in the body's memory (here dirt)"
  [["??GGG???" "??GGG??G" "dddGGG?G" "fffd?G?G" "fffd?G?G" "fffdGG?G" "fffdG?G." "fffdG?d." "fffd?GG." "fffd?GG," "fffdGG.."]
   ["..,,.GG." "??...GG." "ddd...G," "wwwG?.G." "wwwdd.G." "wwwd..G." "wwwd.G.." "wwwd.Gp." "wwwG.,.." "wwwGd,.." "wwwG,,.."]
   ["......,." "........" "GGG...,." "....G.,." "...GG.,." "...G...." "..tG.,.." "...G...." "...F...." "...Fp..." "...F...."]
   ["........" "........" "FFFF...." "...F,..." "...F...." "...F...." "...F...." "...F...." "...F...." "........" "........"]
   ["........" "........" "........" "........" "........" "........" "........" "........" "........" "........" "........"]])

(def legend {"?" "dirt" "G" "grass_block" "d" "dirt" "f" "farmland" "w" "wheat" "F" "oak_fence" "," "short_grass"
             "p" "pumpkin" "t" "wall_torch"})

(def cells (fx/layers dump legend {:x -370 :y 64 :z -847}))

(def ends-shut
  "the strip's ways out past the posts at its ends filled (the cell under the north corner post, the field side of the
  south end): only drops onto farmland left"
  [[-367 66 -844 "dirt"] [-368 66 -839 "dirt"]])

(defn snapshot [more]
  (fx/fixture-snapshot {:fill [[-380 60 -850 -355 63 -815 "stone"]] :blocks (into cells more)}))

(def froms
  "where the body stood: beside the post at z -842.5, then at -839.5 after a partial walk along the fence"
  [{:x -367 :y 67 :z -843 :px -366.937 :py 67 :pz -842.546}
   {:x -367 :y 67 :z -840 :px -366.9 :py 67 :pz -839.5}])

(def gate (near -372 64 -826 1))

(def farmland (into #{} (keep (fn [[x y z n]] (when (= n "farmland") [x y z]))) cells))

(defn on-farmland? [steps] (boolean (some #(farmland [(:x %) (:y %) (:z %)]) steps)))

(deftest the-strip-is-left-by-a-drop-past-its-end-posts
  ;; drops of 1 past the fence column at an end of the strip (north under the corner post, or south); the planner took
  ;; the jump up this way, but refused the drop back down a column a fence post stands in (it was not clear): no way out
  (doseq [from froms]
    (let [r (pf/plan (snapshot []) {:from from :goal gate})]
      (is (= "found" (:status r)) (pr-str from (:reason r)))
      (is (not (on-farmland? (get-in r [:path :steps]))) (pr-str from (pf/cells r))))))

(deftest a-drop-past-a-post-is-refused-through-a-hazard
  ;; the tight drop skips the collision check, not the hazards: a cobweb or fire in both end columns (the cell the body drops into) leaves no way off
  ;; the strip (the farmland drop is unpriced here), where the open columns have one (the first test)
  (doseq [hazard ["cobweb" "fire" "soul_fire" "powder_snow"]
          from froms]
    (let [r (pf/plan (snapshot [[-367 66 -844 hazard] [-367 66 -839 hazard]]) {:from from :goal gate})]
      (is (not= "found" (:status r)) (pr-str hazard from (pf/cells r))))))

(deftest the-planner-drops-onto-farmland-only-at-a-price
  (doseq [from froms]
    (let [plan #(pf/plan (snapshot ends-shut) {:from from :goal gate} {:costs %})
          priced (plan {:trample (wworld/trample-s)})]
      (is (= "found" (:status priced)) (pr-str from))
      (is (on-farmland? (get-in priced [:path :steps])))
      (is (not= "found" (:status (plan {}))) (str "unpriced, no drop tramples: " (pr-str from))))))

(deftest the-trample-is-priced-by-the-games-chance
  ;; the game tramples with chance fall - 0.5, farmland's top being 15/16 of its cell: a drop of 2 cells falls 1.0625, so
  ;; the price is .5625 of costs.trample; one of 1 cell falls .0625, so nothing
  (doseq [[drop share] [[2 0.5625] [1 0] [3 1]]]
    (let [snap (fx/fixture-snapshot {:fill [[-5 60 -5 0 (+ 64 drop) 5 "stone"] [1 60 -5 5 64 5 "stone"]] :blocks (for [z (range -5 6)] [1 65 z "farmland"])})
          price (fn [t] (get-in (pf/plan snap {:from {:x 0 :y (+ 65 drop) :z 0} :goal (near 1 65 0 0)} {:costs {:trample t}})
                                [:path :cost :priced]))]
      (is (< (js/Math.abs (- (price 1000) (price 1e-9) (* 1000 share))) 1e-3) (pr-str drop)))))

(deftest a-walk-prices-the-trample
  ;; body-policy prices it (the till of the dirt it leaves); on the old code every drop onto farmland was refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [{:keys [px py pz]} froms]
          (let [clock (atom 1000000)
                [_ sink] (tu/legacy-capture-sink)
                p (tu/fake-on-floor {:floor [-380 -850 -355 -815]
                                     :blocks (into {} (map (fn [[x y z n]] [(str/join "," [x y z]) n])) (into cells ends-shut))
                                     :self {:pos {:x px :y py :z pz}}})
                out (atom nil)
                parent {:round (fn ^:async planning-round [c]
                                 (reset! out (await (near/plan! c [-372 64 -826] 1 :never (wworld/body-policy c) [] false nil nil true false nil nil nil)))
                                 (ctx/end c :done nil))}
                eng (core/create {:primitives p :jobs (assoc registry/jobs 'planning-parent parent)
                                  :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
            (core/submit! eng (tu/order '(planning-parent)) {})
            (await (tu/tick-until-idle! eng 20))
            (is (on-farmland? (:steps @out)) (str "from z " pz ": " (pr-str (dissoc @out :steps))))))))))
