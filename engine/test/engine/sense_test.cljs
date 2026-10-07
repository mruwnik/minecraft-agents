(ns engine.sense-test
  "jobs.lib.sense/decide! over a real perception on the fake world: a sync decision that read cells the body has not
  seen is decided again after the body looks at them, at most max-looks looks, never twice at one cell."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.lib.look :as look]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]))

(defn seeing
  "Fake primitives over world wrapped in a perception (a shorter radius), no sight pass yet: the body knows only what a
  glance at its current view shows."
  [world]
  (let [p (tu/fake-on-floor world)]
    (.setOwner p "t1")
    (perception/wrap p (perception/create (fake-raw/create p) {:radius 16 :ray-deg 2}))))

(defn stub-ctx
  "A round's ctx over p: job memory in an atom, acts straight to the primitives, events into a vector."
  [p]
  (let [m (atom {})
        events (atom [])]
    {:primitives p :root "j1" :slots [] :mem m :events events
     :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data @m}]}}})
     :act (fn [k args] (.call (aget p (name k)) p "t1" args))
     :update-mem (fn [f args] (apply swap! m f args))
     :emit (fn [kind level fields] (swap! events conj (assoc fields :kind kind :level level)))}))

(defn looks [p] (count (filter #(= "look" (.-name %)) (.-calls (.-world p)))))

(defn look-points
  "The points the body looked at, in order, as [x y z]."
  [p]
  (->> (.-calls (.-world p)) (filter #(= "look" (.-name %))) (mapv #(let [q (.. % -args -pos)] [(.-x q) (.-y q) (.-z q)]))))

(def feet {:x 0 :y 64 :z 0})

(deftest flat-ground-unseen-round-the-feet-is-looked-at-and-planned-3-deep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}}})
              c (stub-ctx p)]
          (is (nil? (dig-cells/dig-plan p feet)) "the ground beside the feet is unseen: no plan without a look")
          (is (= {:roof {:x 0 :y 63 :z 0} :depth 3}
                 (await (sense/decide! c ::dig-plan #(dig-cells/dig-plan p feet)))))
          (is (= [[-1 64 0]] (:cells (first (filter #(= :sense.looked (:kind %)) @(:events c))))) "one look, at the unseen cell beside the feet")
          (is (= 1 (count (filter #(= :sense.looked (:kind %)) @(:events c)))) "one event per call that looked"))))))

(def rock
  "A solid stone cube x 4..8, y 62..66, z -2..2: its inside never shows."
  (tu/box 4 62 -2 8 66 2 "stone"))

(def inside (for [x [5 6 7] y [63 64 65] z [-1 0 1]] {:x x :y y :z z}))

(deftest occluded-cells-cost-at-most-max-looks-and-are-never-looked-at-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
              c (stub-ctx p)
              all-seen #(= (count inside) (count (keep (fn [cell] (u/seen-name p cell)) inside)))]
          (is (false? (await (sense/decide! c ::rock all-seen))))
          (is (= (sense/max-looks) (looks p)) "the bound")
          (dotimes [_ 4] (await (sense/decide! c ::rock all-seen)))
          (is (= (count inside) (looks p)) "each hidden cell looked at once, then no more looks"))))))

(deftest an-area-query-with-nothing-near-surveys-once-and-finds-the-block-behind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :yaw 0 :blocks {"0,64,-5" "iron_ore"}})
              c (stub-ctx p)
              ore #(first (look/seen-blocks p {:names ["iron_ore"] :radius 16}))]
          (is (nil? (ore)) "behind the body: not seen")
          (is (= "iron_ore" (:name (await (sense/decide! c ::ore ore)))))
          (is (= [[0.5 65.62 -3.5]] (look-points p)) "one survey, stopped at its first look: level, north, at the ore")
          (let [before (looks p)]
            (is (nil? (await (sense/decide! c ::gold #(first (look/seen-blocks p {:names ["gold_ore"] :radius 16}))))))
            (is (= before (looks p)) "surveyed from this cell already: no second survey")))))))

(deftest an-entity-query-stops-at-the-first-look-that-shows-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}}})
              c (stub-ctx p)
              cow #js {:id 7 :name "cow" :kind "passive" :pos #js {:x 0 :y 64 :z -5} :visible true}
              raw-entities (aget p "entities")
              _ (aset p "entities" (fn [opts] (if (>= (looks p) 3) #js [cow] (.call raw-entities p opts))))
              find-cow #(first (filter (fn [e] (= "cow" (:name e))) (look/seen-entities p {:radius 16})))]
          (is (= 7 (:id (await (sense/decide! c ::cow find-cow)))))
          (is (= 3 (looks p)) "no look after the one that showed it"))))))

(deftest a-promise-throws-a-positive-takes-no-look-an-unloaded-cell-is-never-looked-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :unloaded #{"3,63,0"}})
              c (stub-ctx p)]
          (is (= :thrown (try (await (sense/decide! c ::p #(js/Promise.resolve 1))) (catch :default _ :thrown))))
          (is (= 1 (await (sense/decide! c ::one (constantly 1)))))
          (is (nil? (await (sense/decide! c ::unloaded #(u/seen-name p {:x 3 :y 63 :z 0})))))
          (is (zero? (looks p))))))))

(deftest a-positive-that-guessed-is-checked-after-a-look-with-verify-guesses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks {"0,64,-3" "sand"}})
              c (stub-ctx p)
              floor #(u/block-name-or p {:x 0 :y 64 :z -3} "stone")]
          (is (= "stone" (await (sense/decide! c ::guess floor))) "without verify-guesses the guess stands")
          (is (zero? (looks p)))
          (is (= "sand" (await (sense/decide! c ::guess floor {:verify-guesses true}))))
          (is (= 1 (looks p))))))))

(deftest a-survey-hit-that-guessed-is-checked-after-a-look-with-verify-guesses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :yaw 0 :blocks (merge rock {"0,64,-5" "iron_ore"})})
              c (stub-ctx p)
              hidden {:x 6 :y 64 :z 0}
              ore-and-guess #(when-let [o (first (look/seen-blocks p {:names ["iron_ore"] :radius 16}))]
                               [(:name o) (u/block-name-or p hidden "air")])]
          (is (= ["iron_ore" "air"] (await (sense/decide! c ::ore ore-and-guess {:verify-guesses true}))))
          (is (= [[6 64 0]] (:cells (first (filter #(= :sense.looked (:kind %)) @(:events c)))))
              "the survey found the ore; the guessed cell is looked at after it"))))))
