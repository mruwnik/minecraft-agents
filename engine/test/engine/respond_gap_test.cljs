(ns engine.respond-gap-test
  "respond-to-hostile calls block-arrow-gap for a ranged mob with a line of fire when the body holds cover."
  (:require [cljs.test :refer [deftest is async]]
            [engine.block-arrow-gap-test :as g]
            [engine.core :as core]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.lib.danger :as danger-q]
            [jobs.survival.respond-to-hostile :as respond]))

(def pickaxe {:name "iron_pickaxe" :count 1})
(def cobble8 {:name "cobblestone" :count 8})
(def zombie {:id 5 :name "zombie" :kind "hostile" :visible true :pos {:x 0.5 :y 64 :z 6.5}})

(def wide-ground
  "Ground to flee over: a retreat runs until it is out of line."
  (g/blocks "dirt" [-40 40] [60 63] [-40 40]))

(defn hiding-seen? [{:keys [seen]}]
  (boolean (some #(and (= :holding (:kind %)) (= "hiding" (some-> (:reason %) name))) @seen)))

(defn ^:async hiding!
  "Resolves :hiding once the body holds hid in a refuge (a sealed body holds for as long as a danger stays, by design), or
  :stop once stop is set."
  [s stop]
  (loop []
    (cond
      @stop :stop
      (hiding-seen? s) :hiding
      :else (do (await (js/Promise. (fn [resolve] (js/setTimeout resolve 10)))) (recur)))))

(def tick-timeout-ms
  "Wall time one tick may take before the test fails: a stuck tick would hang it (a bound against a hang, not a speed check)."
  20000)

(defn ^:async respond!
  "Run respond-to-hostile over the gap test's doorway cell with the given spec until the job ends or the body hides
  (a hold that is then cancelled): up to six ticks, each given tick-timeout-ms to end or hide, else the test fails; the setup map."
  [spec]
  (let [{:keys [eng] :as s} (g/setup (:zones spec []) (merge {:entities [g/pit-skeleton] :blocks (merge wide-ground g/pit g/shell) :act-ms 1000} (dissoc spec :zones)))]
    (core/submit! eng '(jobs.survival.respond-to-hostile) {})
    (loop [i 0]
      (when (and (< i 6) (not (hiding-seen? s)))
        (let [stop (atom false)
              timer (atom nil)
              r (try (await (js/Promise.race [(core/tick! eng) (hiding! s stop)
                                              (js/Promise. (fn [_ reject]
                                                             (reset! timer (js/setTimeout #(reject (js/Error. "respond-to-hostile tick did not return")) tick-timeout-ms))))]))
                     (finally (js/clearTimeout @timer)))]
          (reset! stop true)
          (when (= :hiding r) (core/cancel! eng "j1"))
          (recur (inc i)))))
    s))

(defn decisions
  "What respond-to-hostile decided at the first danger it logged, once per encounter."
  [{:keys [eng]}]
  (mapv (comp :decision :data) (mem/entries (mem/view (:store eng)) :hostile)))

(defn calls
  "The slots respond-to-hostile called as children, in order (a child's own children not counted)."
  [{:keys [seen]}]
  (mapv :slot (filter #(and (= :child_started (:kind %)) (= 2 (count (:chain %)))) @seen)))

(defn placed?
  "Whether the block-arrow-gap child ended done."
  [{:keys [seen]}]
  (some #(and (= :child_ended (:kind %)) (= :gap (:slot %)) (= :done (get-in % [:ending :kind]))) @seen))

(deftest cover-blocks-and-a-pickaxe-stop-the-arrows-by-placing-not-walking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (respond! {:inventory [cobble8 pickaxe]}))]
          (is (placed? s))
          (is (= 7 (g/cobble p)) "one block ended the line of fire")
          (is (empty? (g/shot-at p))))))))

(deftest cover-not-yet-seen-is-looked-for-before-the-body-flees
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (respond! {:inventory [cobble8 pickaxe] :seeing tu/seeing-after-look}))]
          (is (placed? s) "the walls behind the body are seen after one look around, then the doorway is stopped")
          (is (= 7 (g/cobble p))))))))

(defn wanted?
  "gap-wanted? for the body of a doorway cell setup with the spec, the dangers it sees."
  [spec tried?]
  (let [{:keys [p]} (g/setup [] (merge {:entities [g/pit-skeleton] :blocks (merge g/ground g/pit g/shell)} spec))
        hs (danger-q/dangers p 16 {:ranged-radius 16} {})]
    (respond/gap-wanted? {:primitives p :view (fn [] {:data mem/empty-data :now 1})} hs tried?)))

(deftest the-gap-is-wanted-only-with-cover-blocks-a-digging-tool-and-ranged-mobs-alone
  (is (wanted? {:inventory [cobble8 pickaxe]} false))
  (is (not (wanted? {:inventory [cobble8]} false)) "no digging tool: a fill could shut the body in")
  (is (not (wanted? {:inventory [pickaxe]} false)) "no blocks")
  (is (not (wanted? {:inventory [cobble8 pickaxe]} true)) "tried this call")
  (is (not (wanted? {:inventory [cobble8 pickaxe] :entities [g/pit-skeleton zombie]} false)) "a melee mob is near")
  (is (not (wanted? {:inventory [cobble8 pickaxe] :blocks g/ground
                     :entities [(assoc g/skeleton :pos {:x 0.5 :y 64 :z 8.5})]} false))
      "open ground: no roof"))

(def canopy
  "A roof over the body with open sides all round: an overhang in open ground."
  {"0,66,0" "oak_planks"})

(deftest a-roof-with-open-sides-is-no-cover
  (is (not (wanted? {:inventory [cobble8 pickaxe] :blocks (merge g/ground g/pit canopy)} false))))

(defn ^:async no-gap-run
  "Respond with the spec: no gap block is placed, the calls are the expected slots and the usual response is a flight
  that hides sealed in (hold why hiding) or ends out of line. Over the dirt ground the pit dug there (respond-to-hostile
  cover-answers), or with blocks carried the walls (two placements open at plug-way/::open-place-s each), is priced under the walk out of a lone skeleton's range (the walk pays the flight's catch at the bow
  mobs' prior, cost.caught), by hand too: the pit is the flight's first step."
  [spec slots hides?]
  (let [s (await (respond! spec))]
    (is (not (placed? s)))
    (is (= slots (remove #{:pause :hide} (calls s))) "holds not counted: how many depends on real time")
    (is (= [:flee] (decisions s)) "a lone skeleton with no weapon: flee")
    (is (= hides? (hiding-seen? s)))
    s))

(deftest no-pickaxe-no-gap-placed-and-the-body-digs-in
  (async done
    (tu/run-async done
      (fn ^:async t [] (await (no-gap-run {:inventory [cobble8]} [:plug] true))))))

(deftest no-blocks-no-gap-placed-and-the-body-digs-in
  (async done
    (tu/run-async done
      (fn ^:async t [] (await (no-gap-run {:inventory [pickaxe]} [:plug-dig] true))))))

(deftest a-melee-mob-as-well-no-gap-placed-and-the-body-flees-and-hides
  (async done
    (tu/run-async done
      (fn ^:async t [] (await (no-gap-run {:inventory [cobble8 pickaxe] :entities [g/pit-skeleton zombie]} [:plug] true))))))

(deftest open-ground-no-gap-placed-and-the-body-digs-in
  (async done
    (tu/run-async done
      (fn ^:async t [] (await (no-gap-run {:inventory [cobble8 pickaxe] :blocks (merge wide-ground g/pit canopy)} [:plug-dig] true))))))

(deftest a-gap-that-fails-refused-falls-back-to-the-usual-response
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (no-gap-run {:inventory [cobble8 pickaxe]
                                                 :zones [{:name "keep" :owner "Miles" :min [-4 60 -4] :max [4 70 4]}]}
                                                [:gap :gap :pocket :step] false))]
          (is (some #(and (= :child_ended (:kind %)) (= :gap (:slot %)) (= :refused (:reason %))) @seen) "the gap job ran and placed nothing"))))))
