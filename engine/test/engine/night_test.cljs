(ns engine.night-test
  "The one night handler: the :night trigger and jobs.survival.night. A bed first (seen, remembered within :bed-radius
  under any roof, or carried, also in the open), then a sleeper on the server (the action bar's sleep count, or a
  sleeping player in sight): logged out in short stints until morning, then roofed or buried: the queue runs, else
  dig in."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.job-api :as job-api]
            [engine.events :as events]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.shelter-test :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.shelter :as sh]
            [jobs.lib.targets :as targets]
            [jobs.survival.night :as night]))

(def night st/night)
(def noon st/noon)
(def roof {"0,66,0" "stone"})

(defn holds?
  "The :night condition in world after seed! wrote body memory; :later moves the clock on after seed!."
  [world seed! & {:keys [later] :or {later 0}}]
  (let [{:keys [eng clock]} (st/setup {})]
    (seed! eng)
    (swap! clock + later)
    (boolean ((:when (get triggers/all :night)) (tu/fake world) (mem/view (:store eng)) {}))))

(defn write! [eng kind data] (mem/write! (:store eng) kind data nil))

(defn status! [sleeping needed] (fn [eng] (write! eng :sleep-status {:sleeping sleeping :needed needed})))

;; ------------------------------------------------------------------ trigger

(deftest night-is-the-only-night-trigger
  (is (some? (get triggers/all :night)))
  (is (= '(jobs.survival.night) (:job (get triggers/all :night))))
  (is (every? nil? (map triggers/all [:night-unsafe :player-sleeping-nearby :shut-in-by-day]))))

(deftest night-holds-for-a-remembered-bed-within-the-radius-under-any-roof
  (let [world {:time night :blocks (merge roof {"20,64,0" "red_bed" "60,64,0" "red_bed"})}]
    (is (true? (holds? world #(st/know-bed! % {:x 20 :y 64 :z 0}))) "20 blocks away, roofed")
    (is (false? (holds? world #(st/know-bed! % {:x 60 :y 64 :z 0}))) "beyond 48")
    (is (false? (holds? (assoc world :states {"20,64,0" {:occupied true}}) #(st/know-bed! % {:x 20 :y 64 :z 0})))
        "an occupied bed is not one to sleep in")
    (is (false? (holds? world (fn [eng] (st/know-bed! eng {:x 20 :y 64 :z 0})
                                (write! eng :bed-unreachable {:pos {:x 20 :y 64 :z 0}}))))
        "given up on")
    (is (false? (holds? {:time night :blocks roof} (fn [_]))) "roofed, no bed, nobody asleep")))

(deftest night-holds-for-a-sleeper-the-action-bar-counts-tonight
  (let [world {:time night :blocks roof}]
    (is (true? (holds? world (status! 1 2))))
    (is (true? (holds? world (fn [eng] (write! eng :sleep-status {:skipping true})))))
    (is (false? (holds? world (fn [eng] ((status! 1 2) eng) ((status! 0 2) eng)))) "the sleeper woke")
    (is (false? (holds? world (status! 1 2) :later (* 50 (- 14000 12542) 2))) "a count from before dusk")
    (is (false? (holds? world (fn [eng] ((status! 1 2) eng)
                                (mem/write! (:store eng) :log-out {:ms 0 :status "unsupported"} nil))))
        "the server does not let it log out")
    (is (false? (holds? (assoc world :time noon) (status! 1 2))) "by day")
    (is (false? (holds? (assoc-in world [:self :isSleeping] true) (status! 1 2))) "asleep itself")))

(deftest after-a-log-out-for-a-sleeper-no-count-since-the-return-still-counts-as-asleep
  (let [world {:time night :blocks roof}
        out-and-back (fn [eng] ((status! 1 2) eng)
                       (mem/write! (:store eng) :log-out {:ms 30000 :status "ok"} nil))]
    (is (true? (holds? world (fn [eng] (out-and-back eng) (write! eng :online {})))) "no news since the return")
    (is (false? (holds? world (fn [eng] (out-and-back eng) (write! eng :online {}) ((status! 0 1) eng))))
        "a count of none since the return")))

(def sleeper {:id 5 :name "Alex" :kind "player" :sleeping true :pos {:x 4 :y 64 :z 0}})

(deftest night-holds-for-a-sleeper-in-sight-not-one-behind-a-wall
  (let [world {:time night :blocks roof :entities [sleeper]}]
    (is (true? (holds? world (fn [_]))))
    (is (false? (holds? (update world :blocks merge {"2,64,0" "stone" "2,65,0" "stone"}) (fn [_]))) "behind a wall")))

(deftest night-holds-by-day-for-a-bed-it-put-down-outside-its-zone
  (let [world {:time noon :blocks {"3,64,0" "red_bed"}}
        placed (fn [eng] (write! eng :bed-placed {:pos {:x 3 :y 64 :z 0}}))]
    (is (true? (holds? world placed)))
    (is (false? (holds? {:time noon} placed)) "the bed is gone")
    (is (false? (holds? world (fn [_]))) "a bed it did not put down")))

(deftest a-muted-night-never-fires
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/mute! eng :night nil)
          (await (st/tick-n eng 4))
          (is (= [] (st/calls p "place")))
          (is (empty? (:instances (core/state eng)))))))))

;; ------------------------------------------------------------------ job: beds

(deftest the-night-walks-to-a-remembered-bed-under-a-roof-and-sleeps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks (merge roof {"20,64,0" "red_bed"})})]
          (st/know-bed! eng {:x 20 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 20))
          (is (= [{:x 20 :y 64 :z 0}] (mapv st/arg-pos (st/calls p "sleep")))))))))

(deftest the-night-leaves-an-occupied-remembered-bed-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks (merge roof {"20,64,0" "red_bed"})
                                         :states {"20,64,0" {:occupied true}}})
              eng (update eng :triggers assoc :always-night (assoc st/always-shelter :name :always-night :job '(jobs.survival.night)))]
          (st/know-bed! eng {:x 20 :y 64 :z 0})
          (core/register-reflex! eng {:trigger :always-night})
          (await (st/tick-n eng 4))
          (is (= [] (st/calls p "sleep")))
          (is (= [] (tu/walk-calls p)) "roofed with nothing else to do: done without a step"))))))

(deftest sleep-declines-an-occupied-remembered-bed
  (let [{:keys [eng]} (st/setup {:time night :blocks {"6,64,0" "red_bed"} :states {"6,64,0" {:occupied true}}})]
    (st/know-bed! eng {:x 6 :y 64 :z 0})
    (core/submit! eng '(jobs.survival.sleep) {})
    (is (nil? (core/tick! eng)))))

(deftest sleep-says-why-it-waits
  (let [reason (fn [world know?]
                 (let [{:keys [eng]} (st/setup world)]
                   (when know? (st/know-bed! eng {:x 6 :y 64 :z 0}))
                   (let [id (core/submit! eng '(jobs.survival.sleep) {})]
                     (core/tick! eng)
                     (:reason (:waiting (job-api/summary eng id))))))]
    (is (= :bed-occupied (reason {:time night :blocks {"6,64,0" "red_bed"} :states {"6,64,0" {:occupied true}}} true)))
    (is (= :no-bed (reason {:time night} false)))
    (is (= :not-night (reason {:time noon :blocks {"6,64,0" "red_bed"}} true)))))

(deftest the-night-puts-a-carried-bed-down-in-the-open-sleeps-and-picks-it-up-by-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory [{:name "red_bed" :count 1}] :skipNight false})
              mid (atom nil)]
          (st/on-wait! p (fn [n] (case n
                                1 (reset! mid {:sleeps (mapv st/arg-pos (st/calls p "sleep")) :placed (st/entries eng :bed-placed)})
                                2 (do (fake/swap-self! p assoc :isSleeping false) (.setTime (.-world p) noon))
                                nil)))
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (let [foot (st/arg-pos (first (st/calls p "place")))]
            (is (some? foot) "put down beside the body, no roof needed")
            (is (= [foot] (:sleeps @mid)))
            (is (= [{:pos foot}] (:placed @mid)) "outside any own zone: remembered for the morning")
            (is (= [foot] (mapv st/arg-pos (st/calls p "dig"))) "picked up by day, in the same round")
            (is (= [] (st/entries eng :bed-placed)))
            (is (empty? (st/calls p "moveTo")) "the drop is walked to by go-to, not a direct move")
            (is (= [] (:list (core/state eng))))))))))

;; ------------------------------------------------------------------ job: sleepers

(defn offline-why [p] (mapv #(.-why (.-args %)) (st/calls p "offline")))

(defn on-return!
  "Each return from offline moves the engine clock on by the time away and calls (f n), n the return's count."
  [p clock f]
  (let [n (atom 0)]
    (.override (.-world p) "offline"
               (fn ^:async g [token a impl]
                 (let [r (await (impl token a))]
                   (swap! clock + (.-ms a))
                   (.emit (.-world p) #js {:kind "online" :pos #js {:x 0 :y 64 :z 0}})
                   (f (swap! n inc))
                   r)))))

(defn sleep-count! [p sleeping needed]
  (.emit (.-world p) #js {:kind "sleep-status" :sleeping sleeping :needed needed}))

(deftest a-sleeper-on-the-server-logs-the-body-out-for-a-short-stint
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (on-return! p clock (fn [_] (.setTime (.-world p) noon)))
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 10))
          (is (= [30000] (st/ms-of-offline p)))
          (is (= ["logged-out-for-sleeping-player"] (offline-why p)))
          (is (= [] (st/calls p "place")) "not dug in")
          (is (= [] (:list (core/state eng))) "ended at morning"))))))

(deftest the-body-stays-out-while-someone-sleeps-and-the-news-is-unknown-until-morning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (on-return! p clock (fn [n] (case n
                                        1 (sleep-count! p 1 2)
                                        2 nil
                                        (.setTime (.-world p) noon))))
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 30))
          (is (= 3 (count (st/calls p "offline"))) "out again after a count of one and after no news; back by day")
          (is (= [] (st/calls p "place"))))))))

(deftest once-nobody-sleeps-the-body-shelters-instead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (on-return! p clock (fn [_] (sleep-count! p 0 1)))
          (sleep-count! p 1 2)
          (st/dawn-after! p 2)
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 12))
          (is (= 1 (count (st/calls p "offline"))))
          (is (pos? (count (st/calls p "place"))) "dug in"))))))

(deftest a-bed-comes-before-a-sleeper
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks {"6,64,0" "red_bed"}})]
          (st/know-bed! eng {:x 6 :y 64 :z 0})
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 8))
          (is (= 1 (count (st/calls p "sleep"))))
          (is (= [] (st/calls p "offline"))))))))

(deftest a-log-out-stint-ends-at-morning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time 23000})]
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.log-out) {})
          (await (tu/run-until-empty eng 3))
          (is (= [25050] (st/ms-of-offline p))))))))

;; ------------------------------------------------------------------ one whole attempt per night

(defn fired [seen] (count (filterv #(= [:reflex :fired :night] [(:source %) (:kind %) (:reflex %)]) @seen)))

(def wide-ground
  "Dirt from y 60 to 63 over x -14..14, z -2..2: pit sites a relocation can reach."
  (into {} (for [x (range -14 15) z (range -2 3) y (range 60 64)] [(str x "," y "," z) "dirt"])))

(deftest the-night-is-one-round-that-holds-until-morning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})
              mid (atom nil)]
          (st/dawn-after! p 4 (fn [n] (when (= n 2)
                                     (reset! mid {:holding (some-> (core/holder eng) :id (->> (core/holding eng)) :reason)
                                                  :notified (st/notified seen)
                                                  :places (count (st/calls p "place"))}))))
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (await (core/tick! eng))
          (is (= {:holding :sheltered :notified 0 :places 10} @mid) "walled in, a declared hold, the queue waits")
          (is (= 1 (fired seen)))
          (is (not= {:x 0 :y 64 :z 0} (st/pos-of p)) "one round: by its end it is day and the body stepped out")
          (await (st/tick-n eng 4))
          (is (= 1 (fired seen)) "not fired again")
          (is (= 1 (st/notified seen)) "then the queued job ran"))))))

(deftest at-night-a-respawned-body-shelters-before-it-recovers-its-drops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})
              fired-first (fn [] (some #(when (= :fired (:kind %)) (:reflex %)) @seen))]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night} {:trigger :died}]}"))
          (write! eng :died {:pos {:x 40 :y 64 :z 0}})
          (swap! clock + 1000)
          (write! eng :respawned {})
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= :night (fired-first)) "the night handler goes first: no walk to the drops in the dark"))))))

(deftest a-site-whose-roof-fails-is-left-for-another-site
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks wide-ground})]
          (st/refuse-cell! p [0 63 0])
          (st/dawn-after! p 3)
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (let [roofs (filterv #(= 63 (:y %)) (mapv st/arg-pos (st/calls p "place")))]
            (is (= {:x 0 :y 63 :z 0} (first roofs)) "the first pit's roof was refused")
            (is (<= 9 (js/Math.hypot (:x (last roofs)) (:z (last roofs))))
                "a second pit 9+ blocks away was roofed (8.06 away, dig-in's futile check from the body declines it)"))
          (is (= [[:roof-failed]] (mapv #(mapv :reason (:sites %)) (st/emitted seen :shelter.relocated)))
              "moved once, after the failed site, with dig-in's stop reason")
          (is (= [] (st/entries eng :night-site)) "tonight's failed sites are forgotten at morning")
          (is (empty? (st/emitted seen :shelter.exposed))))))))

(deftest a-failed-site-is-not-dug-again-after-a-cut-however-long-the-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (st/setup {:time night :blocks (into {} (for [x (range -4 5) z (range -4 5) y (range 54 64)] [(str x "," y "," z) "dirt"]))})]
          (st/refuse-placing! p)
          ;; the 2nd wait (an exposed hold) is cut by a higher reflex 11 minutes on: the night was lengthened
          ;; with time set, so dig-in's own 10-minute futile entry has expired
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (st/on-wait! p (fn [n]
                        (case n
                          2 (do (swap! clock + 660000) (core/cut! eng (core/holder eng) :test nil))
                          8 (.setTime (.-world p) noon)
                          nil)))
          (await (st/tick-n eng 40))
          (swap! clock + 11000)
          (await (st/tick-n eng 40))
          (is (= [63 62 61] (keep #(when (= [0 0] [(:x %) (:z %)]) (:y %)) (mapv st/arg-pos (st/calls p "dig"))))
              "one pit; the second firing does not dig deeper (the other digs are the stair out by day)")
          (is (= 2 (fired seen)) "fired again after the cut"))))))

(deftest with-no-site-left-the-night-holds-exposed-then-stops-at-morning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks st/ground :inventory [{:name "iron_pickaxe" :count 1}]})
              mid (atom nil)]
          (st/refuse-placing! p)
          (st/dawn-after! p 4 (fn [n] (when (= n 2) (reset! mid (:reason (core/holding eng "j1"))))))
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (is (= :exposed @mid) "a declared hold")
          (is (= 1 (count (st/emitted seen :shelter.exposed))))
          (let [[s & more] (st/emitted seen :stopped)]
            (is (empty? more))
            (is (= :exposed (:reason s)) "a night with no shelter is no success")
            (is (= [:roof-failed] (take 1 (mapv :reason (:sites s)))) "the first site's reason is dig-in's")
          (is (= 4 (count (:sites s))) "max-sites tried, then exposed"))
          (is (= [] (:list (core/state eng)))))))))

(deftest dig-in-is-one-call-and-stops-with-the-site-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (core/tick! eng))
          (is (= 10 (count (st/calls p "place"))) "walled and roofed in one call")
          (is (= [] (:list (core/state eng)))))
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks st/floor})]
          (st/refuse-placing! p)
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (core/tick! eng))
          (is (= 3 (count (st/calls p "dig"))))
          (is (= [:roof-failed] (mapv :reason (st/emitted seen :stopped))))
          (is (= [] (:list (core/state eng)))))))))

;; ------------------------------------------------------------------ review follow-ups

(deftest a-roofed-end-forgets-tonights-failed-sites
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks st/floor})
              call-child ctx/call-child]
          ;; the dig-in leaves the body roofed some other way (a passer-by built over it), the night then ends :roofed
          (set! ctx/call-child (fn ^:async f [c k sym a]
                                 (swap! (.-state (.-world p)) assoc-in [:blocks [0 66 0]] "stone")
                                 :declined))
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (set! ctx/call-child call-child)
          (is (= [] (st/entries eng :night-site)) "no entry outlives the night that wrote it"))))))

(deftest a-dig-in-that-yields-is-not-a-failed-site
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})
              calls (atom 0)
              seen-sites (atom nil)
              call-child ctx/call-child]
          (set! ctx/call-child (fn ^:async f [c k sym a]
                                 (let [n (swap! calls inc)]
                                   (when (= 2 n)
                                     (reset! seen-sites (mapv :reason (st/entries eng :night-site)))
                                     (.setTime (.-world p) st/noon))
                                   (if (= :dig-in k) :continue (await (call-child c k sym a))))))
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (set! ctx/call-child call-child)
          (is (= [:interrupted] @seen-sites) "the yield left only the in-flight mark, and the next call resumed the dig-in"))))))

(deftest a-body-held-exposed-tries-to-dig-in-again-later
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (st/setup {:time night :blocks st/floor})
              tries (atom 0)
              call-child ctx/call-child]
          (st/refuse-placing! p)
          (st/on-wait! p (fn [n] (if (< n 10) (swap! clock + 660000) (.setTime (.-world p) st/noon))))
          (set! ctx/call-child (fn ^:async f [c k sym a]
                                 (when (= :dig-in k) (swap! tries inc))
                                 (await (call-child c k sym a))))
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (set! ctx/call-child call-child)
          (is (= (inc night/max-retries) @tries) "one dig-in, then one more per retry of the exposed hold"))))))

(deftest an-exposed-hold-retries-only-from-futile-radius-plus-one-away
  (let [due? (fn [feet]
               (with-redefs [ctx/mem (constantly {:exposed {:pos {:x 0 :y 0 :z 0} :at 0 :retries 0}})
                             ctx/now (constantly 1000)
                             sh/feet (constantly feet)]
                 (night/retry-due? {:primitives nil})))]
    (is (not (due? {:x 8 :y 0 :z 1})) "8.06 away is still inside dig-in's futile check")
    (is (due? {:x 9 :y 0 :z 0}))))

(deftest a-pit-site-is-judged-by-the-surface-a-player-sees
  (let [p (tu/fake {:blocks (into {} (for [x (range -2 3) z (range -2 3)] [(str x ",63," z) "dirt"]))})]
    (is (night/pit-site? p {:x 1 :y 64 :z 1}) "one layer of dirt shows nothing against a pit; dig-in finds out")
    (is (not (night/pit-site? p {:x 9 :y 64 :z 9})) "no ground to stand on")))

(deftest a-pit-site-needs-ground-the-carried-tools-dig
  (let [stone (into {} (for [x (range -2 3) z (range -2 3)] [(str x ",63," z) "stone"]))
        site {:x 1 :y 64 :z 1}]
    (is (not (night/pit-site? (tu/fake {:blocks stone}) site)) "no pickaxe: stone cannot be dug")
    (is (night/pit-site? (tu/fake {:blocks stone :inventory [{:name "iron_pickaxe" :count 1}]}) site))))

;; ------------------------------------------------------------------ a niche in a hillside

(def hillside
  "Dirt ground over x -2..8, z -2..2 with a stone hill (x 4..8, y 64..66) on it: no pit site 9+ from a failed one, a niche in the hill."
  (merge (into {} (for [x (range -2 9) z (range -2 3) y (range 60 64)] [(str x "," y "," z) "dirt"]))
         (into {} (for [x (range 4 9) z (range -2 3) y (range 64 67)] [(str x "," y "," z) "stone"]))))

(deftest with-no-pit-site-left-the-night-digs-a-niche-into-the-hill
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks hillside :floor [-2 -2 8 2] :inventory [{:name "iron_pickaxe" :count 1}]})
              mid (atom nil)]
          (st/refuse-cell! p [0 63 0])
          (st/dawn-after! p 4 (fn [n] (when (= n 2) (reset! mid {:at (st/pos-of p) :reason (:reason (core/holding eng "j1"))}))))
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (is (= {:at {:x 5 :y 64 :z -1} :reason :sheltered} @mid) "shut in at the end of the niche (the stair out of the failed pit went north), a declared hold")
          (is (= #{{:x 4 :y 64 :z -1} {:x 4 :y 65 :z -1}}
                 (set (filterv #(= 4 (:x %)) (mapv st/arg-pos (st/calls p "place")))))
              "the opening, feet and head, plugged")
          (is (empty? (st/emitted seen :shelter.exposed)))
          (is (empty? (st/emitted seen :stopped)) "the night ended done"))))))

;; ------------------------------------------------------------------ job: a roofed place close by

(def home-roof {"10,66,0" "stone"})
(def lit-world {:time night :inventory st/dirt-stack :blocks (merge wide-ground home-roof) :light-default [15 4]})

(defn know-home! [eng pos] (mem/write! (:store eng) :home {:pos pos} mem/place-policy))

(defn night-at-home
  "Run the night job (with args) in world with :home at pos, the body seeing through its perception: {:eng :p}."
  ([world pos] (night-at-home world pos {}))
  ([world pos args]
  (let [[seen sink] (tu/legacy-capture-sink)
        raw-p (tu/fake (merge {:offlineScale 0.0001 :floor tu/walk-floor} (dissoc world :light-default :light)))
        _ (swap! (fake/state raw-p) merge (select-keys world [:light-default :light]))
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})
        p (perception/wrap raw-p per)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now (constantly 1000000)
                          :events (events/make {:body "Fake" :sinks [sink] :now (constantly 1000000)})})]
    (aset p "seenBlockAt" (fn [pos] #js {:name (.-name (.blockAt raw-p pos)) :pos pos :age-ms 0}))
    (doseq [yaw [0 90 180 270]] ; what the body has seen by now (reads are sensed: felt, in view or remembered)
      (swap! (fake/state raw-p) assoc :yaw yaw)
      (perception/pass! per))
    (swap! (fake/state raw-p) assoc :yaw 0)
    (st/dawn-after! p st/default-dawn)
    (know-home! eng pos)
    (core/submit! eng (list 'jobs.survival.night args) {})
    (js/Promise.resolve {:eng eng :p p :seen seen}))))

(deftest a-lit-route-to-a-close-roofed-home-is-walked-instead-of-digging-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home lit-world {:x 10 :y 64 :z 0}))]
          (await (tu/run-until-empty eng 40))
          (is (= [] (st/calls p "place")) "no pit dug")
          (is (= {:x 10 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z]))))))))

(deftest an-unlit-route-is-dug-in-where-the-body-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (assoc lit-world :light-default [15 0]) {:x 10 :y 64 :z 0}))]
          (await (core/tick! eng))
          (is (seq (st/calls p "place")) "dark route: dug in")
          (is (< (:x (st/pos-of p)) 5)))))))

(deftest a-home-beyond-the-walk-radius-is-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (assoc lit-world :blocks (assoc wide-ground "40,66,0" "stone")) {:x 40 :y 64 :z 0}))]
          (await (core/tick! eng))
          (is (seq (st/calls p "place")))
          (is (< (:x (st/pos-of p)) 5)))))))

(deftest a-home-with-no-seen-roof-is-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (assoc lit-world :blocks wide-ground) {:x 10 :y 64 :z 0}))]
          (await (core/tick! eng))
          (is (seq (st/calls p "place")))
          (is (< (:x (st/pos-of p)) 5)))))))

(deftest a-route-the-body-has-not-seen-is-not-lit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home lit-world {:x 10 :y 64 :z 0}))
              at (.-seenBlockAt p)]
          (aset p "seenBlockAt" (fn [pos] (let [b (.call at p pos)] (if (and (>= (.-x pos) 3) (not= "stone" (.-name b))) #js {:unknown true :pos pos} b))))
          (await (core/tick! eng))
          (is (seq (st/calls p "place")) "loaded light of unseen cells is not read: dug in")
          (is (< (:x (st/pos-of p)) 5)))))))

(defn ^:async with-child-log
  "Run (f), a promise, with ctx/call-child logging [key sym args] of every child call; the log. (on-call c k sym a) may
  answer a result to stand for the child."
  ([f] (with-child-log f (fn [_ _ _ _] nil)))
  ([f on-call]
   (let [log (atom [])
         orig ctx/call-child]
     (set! ctx/call-child (fn ^:async g [c k sym a]
                            (swap! log conj [k sym a])
                            (let [r (on-call c k sym a)]
                              (if (some? r) r (await (orig c k sym a))))))
     (await (f))
     (set! ctx/call-child orig)
     @log)))

(def walled-home
  (into home-roof (for [x [9 10 11] y [64 65] z [-1 0 1] :when (not= [x z] [10 0])] [(str x "," y "," z) "stone"])))

(deftest a-roof-walk-that-does-not-arrive-digs-in-and-is-not-retried-tonight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (update lit-world :blocks merge walled-home) {:x 10 :y 64 :z 0}))]
          (let [log (await (with-child-log #(tu/run-until-empty eng 60)))]
            (is (= 1 (count (filter #(= :roof-walk (first %)) log))) "one walk attempted, never repeated"))
          (is (seq (st/calls p "place")) "the night dug in after the walk failed")
          (is (< (:x (st/pos-of p)) 9) "and did not walk to the home again"))))))

(deftest a-roof-walk-that-arrives-but-is-still-not-roofed-is-not-repeated
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home lit-world {:x 10 :y 64 :z 0}))
              beside {:x 10 :y 64 :z 1}
              log (await (with-child-log #(tu/run-until-empty eng 60)
                           (fn [c k sym a] (when (:place a) (ctx/call-child c k sym (-> a (dissoc :place) (assoc :pos beside)))))))]
          (is (= 1 (count (filter #(and (= :roof-walk (first %)) (:place (nth % 2))) log))) "walked once, arrived unroofed, then dug in")
          (is (seq (st/calls p "place")) "the night dug in"))))))

;; ------------------------------------------------------------------ flee somewhere safer

(def dark-world (assoc lit-world :light-default [15 0]))

(defn flee-home-night
  "night-at-home in the dark with a home at x 10 beyond :walk-radius 5, no placing, nothing but the home to see: the
  dig-in fails everywhere, so the night has to flee to the roof. {:eng :p}."
  [world args]
  (js/Promise.resolve
   (.then (night-at-home world {:x 10 :y 64 :z 0} (merge {:walk-radius 5} args))
          (fn [{:keys [p] :as r}]
            (st/refuse-placing! p)
            (aset p "seenBlocks" (fn [_] #js []))
            r))))

(deftest the-night-never-fetches-a-tool-for-its-niche
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks hillside :floor [-2 -2 8 2]})
              _ (st/refuse-cell! p [0 63 0])
              _ (st/dawn-after! p 4)
              _ (core/submit! eng '(jobs.survival.night) {})
              log (await (with-child-log #(core/tick! eng)))
              niche (first (filter #(= :niche (first %)) log))]
          (is (= {:fetch false} (nth niche 2)) "the niche child is told not to fetch")
          (is (empty? (filter #(= 'jobs.items.get-tool (second %)) log)) "no tool fetched in the dark"))))))

(deftest with-nothing-to-dig-the-night-flees-to-a-roofed-home-beyond-the-walk-radius-by-an-unlit-route
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {}))]
          (await (tu/run-until-empty eng 80))
          (is (= {:x 10 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z])) "walked to the roofed home")
          (is (empty? (st/emitted seen :shelter.exposed)))
          (let [fled (st/emitted seen :shelter.fled)]
            (is (= [:roofed-place] (mapv :target fled)))))))))

(deftest a-night-that-fled-ends-with-its-flee-count
  (with-redefs [ctx/mem (constantly {:flees 2})]
    (is (= 2 (night/fled nil))))
  (with-redefs [ctx/mem (constantly {})]
    (is (nil? (night/fled nil)))))

(deftest with-flee-radius-0-the-night-holds-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {:flee-radius 0}))]
          (st/dawn-after! p 4)
          (await (tu/run-until-empty eng 80))
          (is (empty? (st/emitted seen :shelter.fled)))
          (is (= 1 (count (st/emitted seen :shelter.exposed)))))))))

(deftest a-flee-that-cannot-arrive-is-reported-and-the-night-holds-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee-home-night (update dark-world :blocks merge walled-home) {}))]
          (st/dawn-after! p 6)
          (let [log (await (with-child-log #(tu/run-until-empty eng 80)))]
            (is (= 1 (count (filter #(= :flee (first %)) log))) "one walk, never the same target twice")
            (is (= {:place :home :range 0} (nth (first (filter #(= :flee (first %)) log)) 2))))
          (is (= 1 (count (st/emitted seen :shelter.flee_failed))))
          (is (= 1 (count (st/emitted seen :shelter.exposed))))
          (is (= [[:exposed 1]] (mapv (juxt :reason :fled) (st/emitted seen :stopped)))))))))

(deftest a-flee-cut-mid-walk-resumes-the-same-target-from-where-the-body-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {}))
              n (atom 0)
              log (await (with-child-log #(tu/run-until-empty eng 80)
                           (fn [_ k _ _] (when (and (= :flee k) (= 1 (swap! n inc)))
                                           (st/teleport! p 4 64 0)
                                           :continue))))
              flees (filter #(= :flee (first %)) log)]
          (is (<= 2 (count flees)) "walked again after the cut")
          (is (apply = (map #(nth % 2) flees)) "the same target"))))))

(def stone-and-dirt-patch
  (merge (into {} (for [x (range 28 33) z (range -2 3)] [(str x ",63," z) "dirt"]))))

(deftest with-nothing-to-dig-here-the-night-flees-to-seen-ground-the-carried-tools-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :inventory st/dirt-stack :blocks stone-and-dirt-patch})]
          (tu/seeing-all p)
          (.override (.-world p) "place"
                     (fn ^:async f [token a impl]
                       (if (< (.-x (.-pos a)) 20)
                         #js {:status "no-support"}
                         (await (impl token a)))))
          (st/dawn-after! p 12)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 120))
          (is (= [:ground] (mapv :target (st/emitted seen :shelter.fled))))
          (is (some #(<= 27 (:x (st/arg-pos %))) (st/calls p "place")) "the pit's roof is placed at the patch"))))))

(defn ^:async flee-with
  "The home flee night (a roofed home beyond the walk radius) with targets/nearest! answering by answer-of (a fn of the
  targets it is given); [log seen calls]: the child log, the events and the targets each call was given."
  [answer-of]
  (let [calls (atom [])
        real targets/nearest!]
    (set! targets/nearest! (fn ([c ts r] (targets/nearest! c ts r nil))
                               ([_ ts _ _] (swap! calls conj ts) (js/Promise.resolve (answer-of ts)))))
    (try
      (let [{:keys [eng seen]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {}))]
        [(await (with-child-log #(tu/run-until-empty eng 80))) seen @calls])
      (finally (set! targets/nearest! real)))))

(deftest the-flee-walks-to-the-target-go-to-costs-cheapest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[log seen calls] (await (flee-with (fn [_] {:status :found :index 0 :cost 7})))]
          (is (seq (filter #(= :flee (first %)) log)))
          (is (= [7] (mapv :cost (st/emitted seen :shelter.fled))) "the planner's cost is reported")
          (is (every? #(and (:x %) (:z %)) (first calls)) "the candidates are given as cells"))))))

(deftest a-flee-search-still-going-walks-nowhere-until-it-answers
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              [log seen calls] (await (flee-with (fn [_] (if (< (swap! n inc) 4) {:status :searching} {:status :found :index 0 :cost 3}))))]
          (is (= 4 (count calls)) "asked again after each :searching")
          (is (= 1 (count (filter #(= :flee (first %)) log))) "walks once, after the answer")
          (is (= [3] (mapv :cost (st/emitted seen :shelter.fled)))))))))

(deftest with-no-reachable-target-the-night-holds-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[log seen] (await (flee-with (fn [_] {:status :none :reason :exhausted :proved true})))]
          (is (empty? (filter #(= :flee (first %)) log)))
          (is (= 1 (count (st/emitted seen :shelter.exposed)))))))))

;; ------------------------------------------------------------------ caves and overhangs

(def cave-blocks
  "An overhang at x 22..26, z -2..2: a stone floor, a roof at y 66, a back wall at x 26."
  (merge (into {} (for [x (range 22 27) z (range -2 3)] [(str x ",63," z) "stone"]))
         (into {} (for [x (range 22 27) z (range -2 3)] [(str x ",66," z) "stone"]))
         (into {} (for [y [64 65] z (range -2 3)] [(str "26," y "," z) "stone"]))))

(defn cave-night
  "A night at the origin where nothing can be placed west of x 20 (no pit there), over cave-blocks; {:eng :p :seen}."
  [inventory]
  (let [{:keys [p] :as r} (st/setup {:time night :inventory inventory :blocks cave-blocks})]
    (tu/seeing-all p)
    (.override (.-world p) "place"
               (fn ^:async f [token a impl]
                 (if (< (.-x (.-pos a)) 20)
                   #js {:status "no-support"}
                   (await (impl token a)))))
    (st/dawn-after! p 12)
    (core/submit! (:eng r) '(jobs.survival.night) {})
    r))

(deftest with-blocks-to-close-the-open-sides-the-night-flees-to-an-overhang-and-walls-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (cave-night [{:name "dirt" :count 8}])
              log (await (with-child-log #(tu/run-until-empty eng 160)))
              enclose (first (filter #(and (= 'jobs.survival.dig-in (second %)) (contains? (nth % 2) :enclose)) log))]
          (is (= [:cave] (mapv :target (st/emitted seen :shelter.fled))))
          (is (= true (:enclose (nth enclose 2))))
          (is (= 1 (count (st/emitted seen :dig-in.sealed))))
          (is (some? enclose) "walls it in, never a pit")
          (is (every? #(<= 21 (:x (st/arg-pos %))) (st/calls p "place")) "placed only at the cave"))))))

(deftest with-too-few-blocks-the-night-ignores-the-overhang
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (cave-night [{:name "dirt" :count 3}])]
          (await (tu/run-until-empty eng 160))
          (is (empty? (st/emitted seen :shelter.fled))))))))

(deftest an-overhang-whose-roof-the-body-has-not-seen-is-no-flee-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (cave-night [{:name "dirt" :count 8}])
              all (.-seenBlocks p)]
          (aset p "seenBlocks" (fn [q] (.filter (all q) (fn [b] (< (.-y (.-pos b)) 66)))))
          (await (tu/run-until-empty eng 160))
          (is (empty? (st/emitted seen :shelter.fled))))))))
