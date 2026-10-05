(ns engine.dig-in-leave-test
  "jobs.survival.dig-in/leave!: from every shelter dig-in builds (a walled cell, a 3-deep pit on flat ground, a 2-deep
  pit beside a block, a pit in a slope, a roofed shaft) the body gets out by day to a standable open cell, and a go-to
  then arrives; at night, or with a hostile by the shelter, it stays shut. Also dig-in's :shelter entry and its
  dig-in.sealed event (BaseMiner #39: a shelter dug open at night was closed again with nothing the agent could see)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.survival.dig-in :as dig-in]))

(def night 14000)
(def dawn 0)

(defn ground
  "Dirt at y 63 over stone 58..62, x -14..30, z -6..6: flat ground with its surface feet height 64."
  []
  (into {} (for [x (range -14 31) z (range -6 7) y (range 58 64)]
             [(str x "," y "," z) (if (= y 63) "dirt" "stone")])))

(defn leaver
  "A caller job, as the night-shelter job is by day: dig-in/leave! every round until it hands a result over."
  [out toward]
  {:check (constantly true)
   :round (fn ^:async leaver-round [c]
            (let [r (await (dig-in/leave! c {:toward toward}))]
              (if (map? r) (do (reset! out r) :done) :continue)))})

(defn setup
  ([] (setup {}))
  ([spec]
   (let [clock (atom 1000000)
         [seen sink] (tu/legacy-capture-sink)
         out (atom nil)
         p (tu/fake (merge {:offlineScale 0.0001 :blocks (ground) :time night
                            :drops {"stone" "cobblestone"}
                            :inventory [{:name "iron_pickaxe" :count 1}]}
                           spec))
         eng (core/create {:primitives p :jobs (assoc registry/jobs 'leaver (leaver out {:x 10 :y 64 :z 0}))
                           :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     {:eng eng :p p :seen seen :clock clock :out out})))

(defn ^:async run-until-empty [{:keys [eng clock]} n]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i)))))

(defn feet [p] (let [pos (.-pos (.self p))] (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])))
(defn block-at [p [x y z]] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn air? [p cell] (= "air" (block-at p cell)))
(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn emitted [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn entries [eng kind] (mapv :data (mem/entries (mem/view (:store eng)) kind)))
(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 2 :y 64 :z 0}})

(defn standable-open?
  "Feet and head cells open, a solid floor."
  [p [x y z]]
  (and (air? p [x y z]) (air? p [x (inc y) z]) (not (air? p [x (dec y) z]))))

(defn ^:async dig-in! [{:keys [eng] :as s}]
  (core/submit! eng '(jobs.survival.dig-in) {})
  (await (run-until-empty s 20)))

(defn ^:async leave-at-dawn! [{:keys [eng p] :as s}]
  (.setTime (.-world p) dawn)
  (core/submit! eng '(leaver) {})
  (await (run-until-empty s 200)))

(defn ^:async go-to! [{:keys [eng] :as s} goal]
  (core/submit! eng (list 'jobs.movement.go-to {:pos goal}) {})
  (await (run-until-empty s 300)))

(defn slope
  "The ground one block higher (dirt at 64) for x >= 1: a step up the hill east of x 0."
  []
  (into (ground) (for [x (range 1 31) z (range -6 7)] [(str x ",64," z) "dirt"])))

(defn shaft
  "The ground with a 1x1 shaft at x 0 z 0 dug from the surface down to feet height 60."
  []
  (reduce dissoc (ground) ["0,60,0" "0,61,0" "0,62,0" "0,63,0"]))

(def dirt-kit [{:name "iron_pickaxe" :count 1} {:name "dirt" :count 32}])

(deftest dig-in-records-where-the-pit-was-dug-from
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup)]
          (await (dig-in! s))
          (is (= [0 61 0] (feet p)) "three deep")
          (is (= {:x 0 :y 64 :z 0} (:start (last (entries eng :shelter))))))))))

(deftest by-day-the-body-leaves-every-shelter-shape-for-a-standable-open-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label spec inside min-y]
                [["a 3-deep pit on flat ground" {} [0 61 0] 64]
                 ["a 2-deep pit beside a block" {:blocks (assoc (ground) "1,64,0" "stone")} [0 62 0] 64]
                 ["a pit in a slope" {:blocks (slope)} [0 62 0] 64]
                 ["a walled cell" {:inventory dirt-kit} [0 64 0] 64]
                 ["a roofed 1x1 shaft" {:blocks (shaft) :self {:pos {:x 0.5 :y 60 :z 0.5}} :inventory dirt-kit} [0 60 0] 64]]]
          (let [{:keys [p out seen] :as s} (setup spec)]
            (await (dig-in! s))
            (is (= inside (feet p)) (str label ": shut in"))
            (await (leave-at-dawn! s))
            (is (= [:done :out] ((juxt :status :reason) @out)) label)
            (is (<= min-y (second (feet p))) (str label ": up and out"))
            (is (not= inside (feet p)) (str label ": left the shelter cell"))
            (is (standable-open? p (feet p)) (str label ": stands in an open cell"))
            (is (= 1 (count (emitted seen :dig-in.left))) label)))))))

(deftest out-of-the-pit-a-go-to-ten-blocks-away-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out seen] :as s} (setup)]
          (await (dig-in! s))
          (await (leave-at-dawn! s))
          (is (= :east (:heading @out)) "the stair heads toward the goal")
          (await (go-to! s {:x 10 :y 64 :z 0}))
          (is (= [] (emitted seen :unreachable)))
          (is (<= 9 (first (feet p)) 11) "arrived beside the target")
          (is (= 64 (second (feet p))) "on the surface"))))))

(deftest at-night-with-a-zombie-by-the-pit-the-shelter-stays-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out] :as s} (setup)]
          (await (dig-in! s))
          (fake/add-entity! p zombie)
          (let [digs (count (calls p "dig"))]
            (core/submit! eng '(leaver) {})
            (await (run-until-empty s 20))
            (is (= [:stopped :unsafe :night] ((juxt :status :reason :why) @out)))
            (is (= digs (count (calls p "dig"))) "nothing dug")
            (is (= "dirt" (block-at p [0 63 0])) "the roof stays")
            (is (= [0 61 0] (feet p)))))))))

(deftest at-dawn-a-zombie-by-the-pit-still-keeps-it-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (setup)]
          (await (dig-in! s))
          (fake/add-entity! p zombie)
          (let [digs (count (calls p "dig"))]
            (await (leave-at-dawn! s))
            (is (= [:unsafe :hostile-near] ((juxt :reason :why) @out)))
            (is (= digs (count (calls p "dig"))))
            (is (= "dirt" (block-at p [0 63 0])))))))))

(deftest away-from-the-shelter-leave-is-out-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (setup {:time dawn})]
          (await (leave-at-dawn! s))
          (is (= [:done :out] ((juxt :status :reason) @out)))
          (is (= [] (calls p "dig"))))))))

(deftest a-shelter-dug-open-at-night-is-sealed-again-says-so-and-is-still-left-by-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen out] :as s} (setup)]
          (await (dig-in! s))
          (is (= [[false nil]] (mapv (juxt :resealed :attention) (emitted seen :dig-in.sealed))) "the first seal is reported")
          (fake/remove-block! p [0 63 0])
          (await (dig-in! s))
          (is (not (air? p [0 63 0])) "the roof is back")
          (is (= [[false nil] [true :notice]] (mapv (juxt :resealed :attention) (emitted seen :dig-in.sealed))))
          (is (re-find #"again" (:text (last (emitted seen :dig-in.sealed)))))
          (is (= {:x 0 :y 64 :z 0} (:start (last (entries eng :shelter)))) "the new entry keeps the pit's start")
          (await (leave-at-dawn! s))
          (is (= :out (:reason @out)))
          (is (= 64 (second (feet p))) "out by day"))))))

(deftest dig-in-at-the-bottom-of-a-shaft-records-its-top-as-the-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as s} (setup {:blocks (shaft) :self {:pos {:x 0.5 :y 60 :z 0.5}} :inventory dirt-kit})]
          (await (dig-in! s))
          (is (= {:pos {:x 0 :y 60 :z 0} :roof {:x 0 :y 62 :z 0} :start {:x 0 :y 64 :z 0} :state :built}
                 (last (entries eng :shelter)))))))))
