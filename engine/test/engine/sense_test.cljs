(ns engine.sense-test
  "jobs.lib.sense/decide! over a real perception on the fake world: a sync decision that read cells the body has not
  seen is decided again after the body looks at them, as many looks as the call pays, never twice at one cell."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.clock :as clock]
            [jobs.lib.cost.look :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.reach.room :as reach-room]
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
    {:primitives p :worth (constantly ##Inf) :root "j1" :slots [] :mem m :events events
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

(defn all-seen [p] (= (count inside) (count (keep #(u/seen-name p %) inside))))

(deftest occluded-cells-cost-the-looks-the-chance-of-a-turn-pays-and-are-never-looked-at-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (settings/with-settings {::cost/turn-prior {:a 1 :b 9}}
          (fn ^:async f []
            (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
                  c (assoc (stub-ctx p) :worth (constantly (* 19.5 (cost/look-s 1))))]
              (is (false? (await (sense/decide! c ::rock #(all-seen p)))))
              (is (= 10 (looks p)) "a look turns it at 1/(10 + i) after i fruitless ones: worth 19.5 looks pays ten")
              (dotimes [_ 4] (await (sense/decide! c ::rock #(all-seen p))))
              (is (= 10 (looks p)) "the cells looked at from here count as fruitless looks: no later call pays one")))))))))

(deftest a-call-at-inf-worth-looks-at-most-max-looked-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (settings/with-settings {::sense/max-looked 5}
          (fn ^:async f []
            (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
                  c (stub-ctx p)]
              (await (sense/decide! c ::rock #(all-seen p)))
              (is (= 5 (looks p)) "arrive pays every look; the memory of looked cells bounds the call")))))))))

(deftest a-costly-decision-is-made-again-only-after-looks-as-long-as-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
              c (stub-ctx p)
              ms (atom 0)
              runs (atom 0)
              decide-ms (* 1000 (cost/look-s 3.5))]
          (with-redefs [clock/now (fn [] @ms)]
            (await (sense/decide! c ::rock #(do (swap! runs inc) (swap! ms + decide-ms) (all-seen p)))))
          (is (= (count inside) (looks p)) "arrive: every hidden cell is looked at")
          (is (= 8 @runs) "a decision that takes 3.5 looks' seconds is made again every 4 looks: 1 + ceil(27 / 4), not 28"))))))

(deftest a-call-worth-every-look-looks-at-each-hidden-cell-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
              c (stub-ctx p)]
          (await (sense/decide! c ::rock #(all-seen p)))
          (dotimes [_ 4] (await (sense/decide! c ::rock #(all-seen p))))
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
            (is (nil? (await (sense/decide! c ::iron-again #(first (remove (constantly true) (look/seen-blocks p {:names ["iron_ore"] :radius 16})))))))
            (is (= before (looks p)) "surveyed for iron from this cell already: no second survey")
            (is (nil? (await (sense/decide! c ::gold #(first (look/seen-blocks p {:names ["gold_ore"] :radius 16}))))))
            (is (< before (looks p)) "a look for iron is no look for gold: it surveys again")))))))

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

(deftest look-round-marks-every-cell-it-was-given
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
              c (stub-ctx p)
              cells (mapv (fn [{:keys [x y z]}] [x y z]) inside)]
          (await (sense/look-round! c cells))
          (is (<= (count cells) (looks p)) "a call worth any seconds looks at every cell")
          (is (= (set cells) (sense/round-looked c)) "one round per :look: the cells asked for count as looked, so the check asks no second"))))))

(deftest look-round-looks-only-at-the-cells-the-call-pays-for-and-counts-only-those
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (settings/with-settings {::cost/turn-prior {:a 1 :b 9}}
          (fn ^:async f []
            (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
                  asked (atom [])
                  c (assoc (stub-ctx p) :worth (constantly (* 19.5 (cost/look-s 1))))
                  cells (mapv (fn [{:keys [x y z]}] [x y z]) inside)]
              (with-redefs [sense/look-at! (fn ^:async [_ cell & _] (swap! asked conj cell))
                            look/look-around! (fn ^:async ([_ _ _]) ([_ _]) ([_]))
                            reach-room/unseen-sides (constantly [])]
                (await (sense/look-round! c cells))
                (is (= 10 (count @asked)) "ten of the 27 cells pay, each at its own falling chance")
                (is (= (set @asked) (sense/round-looked c)) "only the paid cells count looked; the rest is asked for again")
                (await (sense/look-round! c cells))
                (is (= 10 (count @asked)) "the ten looks taken from here leave no paid look for a second round"))))))))))

(deftest look-round-stops-only-at-a-room-wall-a-mob-can-pass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[near far stops?] [[{} {} true]
                                   [{"2,64,0" "snow"} {} true]
                                   [{"2,64,0" "stone"} {"-6,64,0" "stone" "-6,65,0" "stone"} false]
                                   [{"2,64,0" "oak_leaves"} {"-6,64,0" "stone" "-6,65,0" "stone"} false]
                                   [{"2,65,0" "stone"} {"-6,64,0" "stone" "-6,65,0" "stone"} false]
                                   [{"2,64,0" "glass" "2,65,0" "glass"} {"-6,64,0" "stone" "-6,65,0" "stone"} false]
                                   [{"2,64,0" "stone"} {} true]]]
          (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks (merge near far)})
                c (stub-ctx p)
                cells [[2 64 0] [-6 64 0]]
                passes (atom 0)]
            (with-redefs [reach-room/unseen-sides (constantly cells)
                          look/look-around! (fn ^:async ([_ _ _] (swap! passes inc)) ([_ _] (swap! passes inc)) ([_] (swap! passes inc)))]
              (await (sense/look-round! c cells)))
            (is (= (if stops? 0 1) @passes) (str "the round's own look pass follows only when no wall was open, walls " near far))
            (is (= (set cells) (sense/round-looked c)) "the walls left unlooked count as looked")))))))

(deftest look-round-skips-cells-asked-for-from-this-spot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
              c (stub-ctx p)
              cells (mapv (fn [{:keys [x y z]}] [x y z]) inside)]
          (await (sense/look-round! c cells))
          (let [asked (atom [])]
            (with-redefs [sense/look-at! (fn ^:async [_ cell & _] (swap! asked conj cell))]
              (await (sense/look-round! c cells)))
            (is (empty? @asked) "a second round on the same cells from the same spot looks at none")))))))

