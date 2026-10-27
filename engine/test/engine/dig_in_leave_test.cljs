(ns engine.dig-in-leave-test
  "jobs.lib.dig-in-leave/leave!: from every shelter dig-in builds (a walled cell, a 3-deep pit on flat ground, a 2-deep
  pit beside a block, a pit in a slope, a roofed shaft) the body gets out by day to a standable open cell, and a go-to
  then arrives; at night it stays shut, by day it leaves whatever hostiles are around (the hostile reflex deals with a
  real danger). Also dig-in's :shelter entry and its
  dig-in.sealed event (BaseMiner #39: a shelter dug open at night was closed again with nothing the agent could see)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [jobs.lib.ledger :as ledger]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]
            [jobs.lib.dig-in-leave :as dig-leave]))

(def night 14000)
(def dawn 0)

(defn ground
  "Dirt at y 63 over stone 58..62, x -14..30, z -6..6: flat ground with its surface feet height 64."
  []
  (into {} (for [x (range -14 31) z (range -6 7) y (range 58 64)]
             [(str x "," y "," z) (if (= y 63) "dirt" "stone")])))

(defn leaver
  "A caller job, as the night-shelter job is by day: dig-leave/leave! every round until it hands a result over."
  [out toward]
  {:defaults {:checks {:safe? '(always) :fit? '(always)}}
   :round (fn ^:async leaver-round [c]
            (let [r (await (dig-leave/leave! c (merge {:toward toward} (:leave-args (:args c)))))]
              (if (map? r) (do (reset! out r) (ctx/end c :done nil)) :continue)))})

(defn again
  "A caller job that leaves twice; between the leaves a second shelter is sealed (one log wall at 3 64 0)."
  [p runs]
  {:round (fn ^:async again-round [c]
            (let [r (await (dig-leave/leave! c))]
              (cond
                (not (map? r)) :continue
                (< 1 (swap! runs inc)) :done
                :else (do (fake/set-block! p [3 64 0] "oak_log")
                          (ctx/remember! c :shelter {:pos {:x 3 :y 64 :z 1} :state :built
                                                     :take-back [{:cell {:x 3 :y 64 :z 0} :item "oak_log"}]}
                                         {:cap 10 :ttl 1200000})
                          :continue))))})

(defn setup
  ([] (setup {}))
  ([spec]
   (let [clock (atom 1000000)
         [seen sink] (tu/legacy-capture-sink)
         out (atom nil)
         runs (atom 0)
         p (tu/fake (merge {:offlineScale 0.0001 :blocks (ground) :time night
                            :drops {"stone" "cobblestone"}
                            :inventory [{:name "iron_pickaxe" :count 1}]}
                           spec))
         eng (core/create (cond-> {:primitives p :jobs (assoc registry/jobs 'leaver (leaver out {:x 10 :y 64 :z 0}) 'again (again p runs))
                           :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                     (:zones spec) (assoc :world (world/of-data (:plans spec {}) {} (:zones spec)))))]
     {:eng eng :p p :seen seen :clock clock :out out :runs runs :ways (:ways spec)})))

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

(defn ^:async dig-in! [{:keys [eng ways] :as s}]
  (core/submit! eng (list 'jobs.survival.shut-in {:ways (or ways #{:room :walls :pit}) :for :night}) {})
  (await (run-until-empty s 20)))

(defn ^:async leave-at-dawn!
  ([s] (leave-at-dawn! s {}))
  ([{:keys [eng p] :as s} leave-args]
   (.setTime (.-world p) dawn)
   (core/submit! eng (list 'leaver {:leave-args leave-args}) {})
   (await (run-until-empty s 200))))

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
                 ["a walled cell" {:inventory dirt-kit :ways #{:walls}} [0 64 0] 64]
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

(deftest by-day-a-pit-dug-in-anothers-zone-is-left-by-a-last-resort-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out seen] :as s} (setup {:zones [{:name "farm" :min [-20 55 -20] :max [20 70 20] :owner "Miles"}]})]
          (await (dig-in! s))
          (is (< (second (feet p)) 64) "in the pit")
          (await (leave-at-dawn! s))
          (is (= [:done :out] ((juxt :status :reason) @out)))
          (is (empty? (emitted seen :dig-in.trapped)))
          (is (<= 64 (second (feet p))) "out of the pit"))))))

(deftest by-day-a-pit-with-no-pickaxe-is-dug-out-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup {:inventory [{:name "cobblestone" :count 20}]})]
          (await (dig-in! s))
          (is (< (second (feet p)) 64) "in the pit")
          (await (leave-at-dawn! s))
          (is (zero? (count (emitted seen :dig-in.trapped))) "not trapped")
          (is (<= 64 (second (feet p))) "out of the pit"))))))

(deftest a-way-up-is-dug-with-a-worn-last-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (setup {:blocks (shaft) :self {:pos {:x 0.5 :y 60 :z 0.5}}
                                            :ways #{:walls}
                                            :inventory [{:name "stone_pickaxe" :count 1 :durability 3} {:name "dirt" :count 32}]})]
          (await (dig-in! s))
          (is (= 60 (second (feet p))) "shut in at the bottom")
          (await (leave-at-dawn! s))
          (is (= [:done :out] ((juxt :status :reason) @out)) "stair not stopped :tool-spent")
          (is (<= 64 (second (feet p)))))))))

(deftest out-of-the-pit-a-go-to-ten-blocks-away-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out seen] :as s} (setup)]
          (await (dig-in! s))
          (await (leave-at-dawn! s))
          (is (= :east (:heading @out)) "the stair heads toward the goal")
          (await (go-to! s {:x 10 :y 64 :z 0}))
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

(defn pocket
  "The ground with a closed 1x1x2 air pocket in the stone at x 5 z 0 (feet 60, head 61): nothing connects it to the
  outside."
  []
  (reduce dissoc (ground) ["5,60,0" "5,61,0"]))

(deftest by-day-leave-digs-out-whatever-hostiles-are-around
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label spec mob] [["a zombie by the pit, with a way to the exit" {} zombie]
                                  ["a zombie in a closed pocket of stone 5 blocks away" {:blocks (pocket)}
                                   (assoc zombie :pos {:x 5 :y 60 :z 0})]]]
          (let [{:keys [p out seen] :as s} (setup spec)]
            (await (dig-in! s))
            (fake/add-entity! p mob)
            (await (leave-at-dawn! s))
            (is (= [:done :out] ((juxt :status :reason) @out)) label)
            (is (= 64 (second (feet p))) (str label ": up and out"))
            (is (= [] (emitted seen :dig-in.staying)) (str label ": never unsafe"))))))))

(deftest a-climb-child-that-waits-out-a-hostile-is-waited-for-not-counted-a-failed-heading
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (setup {})
              orig ctx/call
              asked (atom 0)
              hostile-near {:kind :condition-failed :cause {:reason :hostile-near :text "a hostile is near"}}]
          (await (dig-in! s))
          (set! ctx/call (fn [c slot job & more] (if (= 'jobs.access.stair job) (do (swap! asked inc) (js/Promise.resolve hostile-near)) (apply orig c slot job more))))
          (await (leave-at-dawn! s))
          (set! ctx/call orig)
          (is (< 1 @asked) "the climb child was asked again, not given up on")
          (is (nil? @out) "leave! is still waiting, no heading was given up")
          (is (< (second (feet p)) 64) "still in the pit"))))))

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
          (is (= [[false nil]] (mapv (juxt :resealed :attention) (emitted seen :shut-in.sealed))) "the first seal is reported")
          (fake/remove-block! p [0 63 0])
          (await (dig-in! (assoc s :ways #{:walls})))
          (is (not (air? p [0 63 0])) "the roof is back")
          (is (= [[false nil] [true :notice]] (mapv (juxt :resealed :attention) (emitted seen :shut-in.sealed))))
          (is (re-find #"again" (:text (last (emitted seen :shut-in.sealed)))))
          (is (= {:x 0 :y 64 :z 0} (:start (last (entries eng :shelter)))) "the new entry keeps the pit's start")
          (await (leave-at-dawn! s))
          (is (= :out (:reason @out)))
          (is (= 64 (second (feet p))) "out by day"))))))

(deftest a-roof-that-is-gone-again-after-every-seal-fails-the-site-instead-of-sealing-for-ever
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:inventory [{:name "iron_pickaxe" :count 1} {:name "cobblestone" :count 20}]})
              walls (assoc s :ways #{:walls})]
          (await (dig-in! s))
          (dotimes [_ 3]
            (fake/remove-block! p [0 63 0])
            (await (dig-in! walls)))
          (is (= [:walls] (mapv :way (entries eng :shut-in-failed))) "the third reseal marks the site failed")
          (is (= [:seal-lost] (mapv :reason (entries eng :shut-in-failed))))
          (fake/remove-block! p [0 63 0])
          (await (dig-in! walls))
          (is (air? p [0 63 0]) "the next call does not seal the barred site again"))))))

(deftest dig-in-at-the-bottom-of-a-shaft-records-its-top-as-the-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as s} (setup {:blocks (shaft) :self {:pos {:x 0.5 :y 60 :z 0.5}} :inventory dirt-kit})]
          (await (dig-in! s))
          (is (= {:pos {:x 0 :y 60 :z 0} :roof {:x 0 :y 62 :z 0} :start {:x 0 :y 64 :z 0} :state :built}
                 (last (entries eng :shelter)))))))))

(deftest a-door-dug-open-in-a-pit-shelter-is-followed-by-the-climb-not-by-a-step-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out] :as s} (setup {:blocks (dissoc (ground) "0,62,0" "0,63,0") :self {:pos {:x 0.5 :y 62 :z 0.5}} :inventory dirt-kit})]
          (mem/write! (:store eng) :shelter
                      {:pos {:x 0 :y 62 :z 0} :state :built :start {:x 0 :y 64 :z 0}
                       :door [{:x 1 :y 62 :z 0} {:x 1 :y 63 :z 0}]}
                      {:cap 10 :ttl 1200000})
          (await (leave-at-dawn! s))
          (is (= [:done :out] ((juxt :status :reason) @out)) "leave! ends, it does not step in and out of the door")
          (is (<= 64 (second (feet p))) "up out of the pit"))))))

(deftest a-roofed-room-with-no-door-or-start-is-left-through-the-wall-at-the-floor-not-by-a-stair-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [room (into {} (for [[x z] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str x "," y "," z) "stone"]))
              {:keys [eng p out] :as s} (setup {:blocks (assoc (merge (ground) room) "0,66,0" "stone") :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory dirt-kit})]
          (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state :built} {:cap 10 :ttl 1200000})
          (await (leave-at-dawn! s))
          (is (= [:done :out] ((juxt :status :reason) @out)))
          (is (= 64 (second (feet p))) "out at ground level")
          (is (empty? (calls p "place")) "no stair step was laid"))))))

(defn have [p item]
  (->> (array-seq (.-inventory (.self p))) (filter #(= item (.-name %))) (map #(.-count %)) (reduce + 0)))

(def walled-logs
  "Two log wall cells at x 3 and a stone one, with a :shelter entry saying the logs were placed (the stone is not a log any more)."
  (merge (ground) {"3,64,0" "oak_log" "3,65,0" "oak_log" "4,64,0" "stone"}))

(defn remember-walls! [eng]
  (mem/write! (:store eng) :shelter
              {:pos {:x 3 :y 64 :z 1} :state :built
               :take-back [{:cell {:x 3 :y 64 :z 0} :item "oak_log"} {:cell {:x 3 :y 65 :z 0} :item "oak_log"}
                           {:cell {:x 4 :y 64 :z 0} :item "oak_log"}]}
              {:cap 10 :ttl 1200000}))

(deftest by-day-the-body-takes-back-the-logs-the-shelter-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out] :as s} (setup {:blocks walled-logs})]
          (remember-walls! eng)
          (await (leave-at-dawn! s))
          (is (= [:done :out] ((juxt :status :reason) @out)))
          (is (air? p [3 64 0]))
          (is (air? p [3 65 0]))
          (is (= "stone" (block-at p [4 64 0])) "a cell that is no longer the placed block is left")
          (is (= 2 (have p "oak_log")) "the logs are picked up"))))))

(defn remember-cells! [eng cells]
  (mem/write! (:store eng) :shelter
              {:pos {:x 3 :y 64 :z 1} :state :built
               :take-back (mapv (fn [[cell item]] {:cell (zipmap [:x :y :z] cell) :item item}) cells)}
              {:cap 10 :ttl 1200000}))

(deftest take-back-leaves-the-block-under-the-feet
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup)
              [x y z] (feet p)]
          (remember-cells! eng [[[x (dec y) z] "dirt"]])
          (await (leave-at-dawn! s))
          (is (= "dirt" (block-at p [x (dec y) z])) "the floor stays")
          (is (= [:floor] (mapv :reason (emitted seen :dig-in.take-back-skipped)))))))))

(deftest take-back-leaves-a-block-beside-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:blocks (assoc walled-logs "3,64,1" "lava")})]
          (remember-cells! eng [[[3 64 0] "oak_log"]])
          (await (leave-at-dawn! s))
          (is (= "oak_log" (block-at p [3 64 0])))
          (is (= [:hazard] (mapv :reason (emitted seen :dig-in.take-back-skipped)))))))))

(deftest take-back-leaves-a-block-in-anothers-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:blocks walled-logs
                                                 :zones [{:name "farm" :min [3 60 -1] :max [3 70 1] :owner "Miles"}]})]
          (remember-walls! eng)
          (await (leave-at-dawn! s))
          (is (= "oak_log" (block-at p [3 64 0])))
          (is (= "oak_log" (block-at p [3 65 0])))
          (is (every? #{:zone :refused} (mapv :reason (emitted seen :dig-in.take-back-skipped))))
          (is (seq (emitted seen :dig-in.take-back-skipped))))))))

(deftest take-back-without-a-harvesting-tool-leaves-the-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:blocks (assoc (ground) "3,64,0" "cobblestone") :inventory []})]
          (remember-cells! eng [[[3 64 0] "cobblestone"]])
          (await (leave-at-dawn! s))
          (is (= "cobblestone" (block-at p [3 64 0])) "a dig by hand would lose it")
          (is (= [:no-tool] (mapv :reason (emitted seen :dig-in.take-back-skipped)))))))))

(deftest take-back-false-leaves-the-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out] :as s} (setup {:blocks walled-logs})]
          (remember-walls! eng)
          (await (leave-at-dawn! s {:take-back? false}))
          (is (= [:done :out] ((juxt :status :reason) @out)))
          (is (= "oak_log" (block-at p [3 64 0])))
          (is (zero? (have p "oak_log"))))))))

(deftest a-new-shelter-gets-a-new-take-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p runs] :as s} (setup {:blocks walled-logs})]
          (remember-walls! eng)
          (.setTime (.-world p) dawn)
          (core/submit! eng '(again) {})
          (await (run-until-empty s 300))
          (is (= 2 @runs))
          (is (= "air" (block-at p [3 64 0])) "the second shelter's block comes back in the same job"))))))

(deftest dig-in-records-its-dear-walls-and-leave-takes-them-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:inventory [{:name "iron_pickaxe" :count 1} {:name "oak_log" :count 20}]
                                            :blocks (assoc (ground) "0,66,0" "stone")})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
          (await (run-until-empty s 40))
          (let [entry (last (entries eng :shelter))
                placed (frequencies (map #(.-item (.-args %)) (calls p "place")))]
            (is (pos? (get placed "oak_log" 0)) "walled in with logs")
            (is (= (get placed "oak_log") (count (:take-back entry))) (pr-str (:take-back entry)))
            (is (every? #(= "oak_log" (:item %)) (:take-back entry))))
          (let [before (have p "oak_log")]
            (await (leave-at-dawn! s))
            (is (< before (have p "oak_log")) "the logs are back in the pack")))))))

(deftest dig-in-takes-cobblestone-walls-back-by-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:inventory [{:name "iron_pickaxe" :count 1} {:name "cobblestone" :count 20}]
                                            :blocks (assoc (ground) "0,66,0" "stone")})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
          (await (run-until-empty s 40))
          (let [entry (last (entries eng :shelter))]
            (is (seq (:take-back entry)) "cobblestone walls are listed for take-back")
            (is (every? #(= "cobblestone" (:item %)) (:take-back entry))))
          (let [before (have p "cobblestone")]
            (await (leave-at-dawn! s))
            (is (< before (have p "cobblestone")) "the cobblestone is back in the pack")))))))

(deftest dig-in-records-no-cheap-block-for-take-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as s} (setup {:inventory [{:name "iron_pickaxe" :count 1} {:name "dirt" :count 20}]
                                          :blocks (assoc (ground) "0,66,0" "stone")})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
          (await (run-until-empty s 40))
          (is (nil? (:take-back (last (entries eng :shelter))))))))))

(deftest dig-in-takes-back-even-its-cheap-walls-beside-a-pen-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [want ["oak_fence_gate" "oak_door"]]
        (let [pen {:id "pen" :parts [{:id "gate" :cells [[2 64 0]] :want want}]}
              {:keys [eng p] :as s} (setup {:inventory [{:name "iron_pickaxe" :count 1} {:name "dirt" :count 20}]
                                            :blocks (assoc (ground) "0,66,0" "stone")
                                            :zones [] :plans {"pen" pen}})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
          (await (run-until-empty s 40))
          (let [take-back (:take-back (last (entries eng :shelter)))
                near-gate? (fn [{:keys [x y z]}] (<= (js/Math.hypot (- x 2) (- y 64) (- z 0)) 2))]
            (is (seq take-back) "dirt walls beside the gate are listed")
            (is (every? #(near-gate? (:cell %)) take-back) "and only those")
            (is (< (count take-back) (count (calls p "place")))))
          (await (leave-at-dawn! s))
          (is (= "air" (block-at p [1 64 0])) (str "the seal beside the " want " is gone"))))))))

(deftest dig-in-writes-its-placed-blocks-to-the-scaffold-ledger-where-a-default-cleanup-leaves-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:inventory [{:name "iron_pickaxe" :count 1} {:name "dirt" :count 20}]
                                            :blocks (assoc (ground) "0,66,0" "stone") :zones []})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
          (await (run-until-empty s 40))
          (let [view (mem/view (:store eng))
                l (ledger/open-entries view)
                n (count (calls p "place"))]
            (is (seq l))
            (is (= #{:shelter} (set (map :purpose l))))
            (is (= n (count l)) "one entry per placed block")
            (is (not-any? #(air? p (:cell %)) l))
            (is (empty? (ledger/select l view :orphans)) "the default cleanup does not dig a shelter")
            (is (= (count l) (count (ledger/select l view :all))))))))))

(deftest dig-in-takes-back-even-its-cheap-walls-beside-the-respawn-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:inventory [{:name "iron_pickaxe" :count 1} {:name "dirt" :count 20}]
                                            :blocks (assoc (ground) "0,66,0" "stone")})]
          (mem/write! (:store eng) :spawn-bed {:pos {:x 2 :y 64 :z 0}} {:cap 1 :ttl :forever})
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
          (await (run-until-empty s 40))
          (let [take-back (:take-back (last (entries eng :shelter)))]
            (is (seq take-back) "dirt walls within reach of the bed are listed")
            (is (< (count take-back) (count (calls p "place"))) "and only those"))
          (await (leave-at-dawn! s))
          (is (= "air" (block-at p [1 64 0])) "no seal is left on the bed's respawn cells"))))))
