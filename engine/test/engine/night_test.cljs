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
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.shelter-test :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
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
          (await (st/run-until-empty eng 20))
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
          (await (st/run-until-empty eng 10))
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
          (await (st/run-until-empty eng 30))
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
          (await (st/run-until-empty eng 8))
          (is (= 1 (count (st/calls p "sleep"))))
          (is (= [] (st/calls p "offline"))))))))

(deftest a-log-out-stint-ends-at-morning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time 23000})]
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.log-out) {})
          (await (st/run-until-empty eng 3))
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
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks st/ground})
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

(deftest a-pit-site-is-judged-by-the-surface-a-player-sees
  (let [p (tu/fake {:blocks (into {} (for [x (range -2 3) z (range -2 3)] [(str x ",63," z) "dirt"]))})]
    (is (night/pit-site? p {:x 1 :y 64 :z 1}) "one layer of dirt shows nothing against a pit; dig-in finds out")
    (is (not (night/pit-site? p {:x 9 :y 64 :z 9})) "no ground to stand on")))
