(ns engine.go-to-quote-test
  "go-to's quote against the fake world: the planner's route to the goal, asked as ./bin/quote asks it, walks nowhere."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.core :as core]
            [engine.event-api :as event-api]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [jobs.lib.world-files :as world-files]
            ["fs" :as fs]
            ["path" :as path]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]
            [jobs.lib.near :as near]
            [jobs.lib.cost.planner :as cost-planner]
            [jobs.movement.go-to.preview :as pv]
            [jobs.movement.go-to.tuning :refer [quote-unseen-factor]]))

(defn ^:async quote-reply
  "eng's answer to GET /quote for go-to with args (what ./bin/quote prints)."
  [eng args]
  (await (event-api/quote-view eng "jobs.movement.go-to" (pr-str args))))

(declare preview*)

(defn ^:async preview
  "preview* with the :settings of args {setting-key value} laid over the settings (a quote runs outside the call's :settings scope)."
  [blocks args & [opts]]
  (await (settings/with-settings (or (:settings args) {}) (fn ^:async run [] (await (preview* blocks (dissoc args :settings) opts))))))

(defn ^:async preview*
  "Ask go-to's quote over the fake world (body at 0 64 1); {:out its :route :quote :reply the /quote answer :p primitives}.
  opts: :places {name pos} written to memory, :markers shared markers, :no-path-world true, :states block states."
  [blocks args & [{:keys [places markers no-path-world states]}]]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake (cond-> {:blocks blocks :self {:pos {:x 0 :y 64 :z 1}}} states (assoc :states states)))
        dir (tu/tmp-dir)
        _ (when markers (fs/writeFileSync (path/join dir "places.json") (js/JSON.stringify (clj->js markers))))
        _ (when no-path-world (set! (.-pathWorld p) nil))
        w (when markers (world-files/open {:plans-dir (path/join dir "plans") :blueprint-dir (path/join dir "bps")
                                           :zones-file (path/join dir "zones.edn") :emit (fn [_])}))
        eng (core/create (cond-> {:primitives p :jobs registry/jobs
                                  :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                           w (assoc :world w)))]
    (doseq [[k pos] places] (mem/write! (:store eng) k {:pos pos} mem/place-policy))
    (let [reply (await (quote-reply eng args))]
      {:out (get-in reply [:quote :route]) :quote (:quote reply) :reply reply :p p})))

(defn pos-of [p] (let [q (.-pos (.self p))] [(.-x q) (.-y q) (.-z q)]))

(deftest a-found-route-is-quoted-and-not-walked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out quote p]} (await (preview (floor 0 0 11 2) {:pos [10 64 1] :range 0}))
              [least mean most] (get-in quote [:uses :job-time])]
          (is (true? (:found out)))
          (is (= 10 (js/Math.round (:length out))))
          (is (pos? (:seconds out)))
          (is (= (:seconds out) least) "the least time is the planner's")
          (is (<= least mean most))
          (is (js/isFinite most))
          (is (= [10 64 1] (:moves-to quote)))
          (is (string? (:summary out)))
          (is (pos? (:steps out)))
          (is (= [10 64 1] (last (:waypoints out))))
          (is (= [0 64 1] (pos-of p))))))))

(deftest a-feet-cell-over-a-short-floor-is-planned-to-the-blocks-own-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [n ["farmland" "dirt_path"]]
          (let [{:keys [out]} (await (preview (assoc (floor 0 0 11 2) "10,63,1" n) {:pos [10 64 1] :range 0}))]
            (is (true? (:found out)) (str n " " (pr-str out)))
            (is (= [10 63 1] (last (:waypoints out))) n)))))))

(deftest a-goal-with-no-way-says-why-and-its-time-has-no-bound
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out quote]} (await (preview (merge (floor 0 0 3 2) (floor 8 0 11 2)) {:pos [10 64 1] :range 0}))]
          (is (false? (:found out)))
          (is (= :partial (:reason out)) "the island is out of reach: the plan only gets nearer")
          (is (number? (:near out)))
          (is (= ##Inf (nth (get-in quote [:uses :job-time]) 2))))))))

(deftest a-walk-only-call-with-no-way-quotes-no-time-at-all
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shut-in (dissoc (merge (floor -1 -1 11 3) (box -1 64 0 1 66 2 "stone")) "0,64,1" "0,65,1")
              walk (:quote (await (preview shut-in {:pos [10 64 1] :range 0 :ways #{:walk}})))
              any (:quote (await (preview shut-in {:pos [10 64 1] :range 0})))]
          (is (false? (:found (:route walk))))
          (is (nil? (:partial (:route walk))) (pr-str (:route walk)))
          (is (= ##Inf (first (get-in walk [:uses :job-time]))) "walking alone never gets out")
          (is (js/isFinite (first (get-in any [:uses :job-time]))) "digging out may"))))))

(deftest a-partial-route-keeps-a-least-and-has-no-most
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [quote]} (await (preview (merge (floor 0 0 3 2) (floor 8 0 11 2)) {:pos [10 64 1] :range 0 :ways #{:walk}}))
              [least mean most] (get-in quote [:uses :job-time])]
          (is (js/isFinite least) "the goal may lie past what is loaded")
          (is (< least mean))
          (is (= ##Inf most)))))))

(deftest a-partial-routes-rest-counts-the-blocks-past-its-end-once
  (let [r {:found false :reason :partial :near 30 :goal [14 64 0]
           :partial {:seconds 5 :waypoints [[0 64 0] [10 64 0]]}}
        [least mean] (get-in (pv/quote-of-route r false) [:uses :job-time])]
    (is (= 5 least))
    (is (< (js/Math.abs (- mean (+ 5 (* 4 cost-planner/walk-floor (quote-unseen-factor))))) 1e-9)
        "four blocks are left past the route's end, not the 30 of the body")))

(deftest a-search-still-going-on-is-a-straight-line-estimate
  (let [r {:found false :reason :searching :near 30 :goal [30 64 0]}
        q (pv/quote-of-route r false)
        [least mean most] (get-in q [:uses :job-time])]
    (is (= 0 least))
    (is (< (js/Math.abs (- mean (* 30 cost-planner/walk-floor (quote-unseen-factor)))) 1e-9))
    (is (= ##Inf most))
    (is (true? (:estimate q)))))

(deftest drop-cost-false-takes-no-two-block-drop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge (floor 0 0 3 2) (floor 61 4 0 6 2))
              free (:out (await (preview world {:pos [5 62 1] :range 0})))
              none (:out (await (preview world {:pos [5 62 1] :range 0 :settings {:jobs.lib.cost.planner/drop-cost false}})))]
          (is (true? (:found free)))
          (is (= 1 (get-in free [:moves :drop])))
          (is (false? (:found none))))))))

(deftest tolls-send-the-route-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (floor 0 0 11 2)
              cheap (:out (await (preview world {:pos [10 64 1] :range 0})))
              dear (:out (await (preview world {:pos [10 64 1] :range 0
                                                :tolls (vec (for [x (range 2 9)] {:x x :y 64 :z 1 :factor 20}))})))]
          (is (= 1 (count (:waypoints cheap))))
          (is (> (count (:waypoints dear)) 1)))))))

(deftest bad-shaped-args-are-refused-as-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args re] [[{:pos [5 64 1] :tolls [{:x 1}]} #":tolls must be a list of \{:x :y :z :factor\}"]]]
          (let [{:keys [reply]} (await (preview (floor 0 0 11 2) args))]
            (is (= :bad-args (:reason reply)) (pr-str args))
            (is (re-find re (str (:message reply))) (pr-str reply))))))))

(deftest every-refusal-carries-found-false-the-reason-and-no-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (floor 0 0 11 2)
              refused (fn ^:async refused [args opts] (await (preview world args opts)))
              pos (await (refused {:pos [1 -9999 1]} nil))
              place (await (refused {:place :mine} nil))
              sensing (await (refused {:pos [5 64 1]} {:no-path-world true}))]
          (doseq [[{:keys [out quote]} reason] [[pos :bad-pos] [place :unknown-place] [sensing :unsupported]]]
            (is (false? (:found out)) (str reason))
            (is (= reason (:reason out)))
            (is (string? (:text out)) (str reason))
            (is (= [##Inf ##Inf ##Inf] (get-in quote [:uses :job-time])) (str reason))))))))

(deftest a-place-is-quoted-from-memory-and-from-a-shared-marker
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (floor 0 0 11 2)
              own (await (preview world {:place :hut :range 0} {:places {:hut {:x 5 :y 64 :z 1}}}))
              marker (await (preview world {:place :hut :range 0} {:markers [{:name "hut" :kind "base" :x 10 :y 64 :z 1}]}))
              both (await (preview world {:place :hut :range 0} {:places {:hut {:x 5 :y 64 :z 1}}
                                                                 :markers [{:name "hut" :kind "base" :x 10 :y 64 :z 1}]}))]
          (is (= [5 64 1] (last (:waypoints (:out own)))))
          (is (= [10 64 1] (last (:waypoints (:out marker)))))
          (is (= [5 64 1] (last (:waypoints (:out both)))) "the body's own place wins")
          (is (= [0 64 1] (pos-of (:p marker)))))))))

(deftest a-partial-plan-says-partial-and-hands-over-how-far-it-gets
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (preview (floor 0 0 6 2) {:pos [30 64 1] :range 0}))]
          (is (false? (:found out)))
          (is (= :partial (:reason out)))
          (is (pos? (:steps (:partial out))))
          (is (= 30 (:near out)) "the body's distance to the goal")
          (is (< (:length (:partial out)) 7))
          (is (= [0 64 1] (pos-of p))))))))

(deftest a-far-goal-spends-one-round-of-search-and-is-quoted-as-an-estimate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out quote]} (await (preview (floor 0 0 80 80) {:pos [400 64 40] :range 0}))]
          (is (false? (:found out)))
          (is (= :searching (:reason out)) "the node budget of one round, not a clock, ends it")
          (is (true? (:estimate quote)) "priced as a straight line until the search ends"))))))

(defn pond
  "Land at y 63 from x 0 to 20 and z -10 to 10, with a pond of water over x 5-15 and z -8 to 8 (stone beneath at y 61)."
  []
  (merge (floor 0 -10 20 10) (box 0 61 -10 20 61 10 "stone") (box 5 62 -8 15 63 8 "water")))

(deftest a-swim-price-flips-the-route-across-a-pond
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [route (fn ^:async route [costs] (:out (await (preview (pond) (cond-> {:pos [20 64 1] :range 0} costs (assoc :settings {:jobs.lib.cost.planner/costs costs}))))))
              dear (await (route {:swim-h 5}))
              default (await (route nil))
              cheap (await (route {:swim-h 0.25}))]
          (is (every? :found [dear default cheap]))
          (is (nil? (re-find #"swims" (:summary dear))) "dear water: the way round on land")
          (is (nil? (re-find #"swims" (:summary default))) "the default price: land is quicker too")
          (is (re-find #"swims" (:summary cheap)) "cheap water: straight across")
          (is (< (:seconds cheap) (:seconds default))))))))

(deftest a-route-reports-the-hp-its-drowning-costs
  (let [steps [{:x 0 :y 64 :z 0 :move :walk} {:x 1 :y 64 :z 0 :move :swim}]
        path (fn [drown] #js {:seconds 2 :summary "" :cost #js {:seconds 2 :drown drown}})]
    (is (= 4.5 (:drown (pv/route steps (path 4.5)))))
    (is (not (contains? (pv/route steps (path 0)) :drown)))))

(deftest unknown-names-a-dark-drop-only-for-a-reason-go-to-would-give-up-on
  (let [dark {:x 1 :y 62 :z 0}
        report (fn [plan]
                 (with-redefs [near/with-sight (fn [result _ _] (assoc result :dark-best dark))
                               pv/u-dist (fn [_ _] 5)]
                   (pv/report nil plan {:x 9 :y 61 :z 0} nil true)))]
    (are [plan reason] (= reason (:reason (report plan)))
      {:status "none" :r #js {:reason nil}} :unseen-floor-dark
      {:status "none" :r #js {:reason nil :damageRefused true}} nil
      {:beyond {:kind :jump :at [1 2 3]}} :abilities)))

(defn ^:async dark-of
  "The quote's :dark for a walk to [10 64 1] over a floor, in a world seen through the body's perception: light is the
  [sky block] of every cell, `lit` {[x y z] [sky block]} overrides some; seen? false skips the look passes."
  [{:keys [time light lit seen?]}]
  (let [raw-p (tu/fake {:blocks (floor 0 0 11 2) :time time :offlineScale 0.0001 :self {:pos {:x 0 :y 64 :z 1}}})
        _ (swap! (fake/state raw-p) merge {:light-default light :light lit})
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000) :rand (constantly 0.5)})
        p (perception/wrap raw-p per)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now (constantly 1000000)
                          :events (events/make {:body "Fake" :sinks [(second (tu/legacy-capture-sink))] :now (constantly 1000000)})})]
    (when seen?
      (aset p "seenBlockAt" (fn [pos] #js {:name (.-name (.blockAt raw-p pos)) :pos pos :age-ms 0}))
      (doseq [yaw [0 90 180 270]]
        (swap! (fake/state raw-p) assoc :yaw yaw)
        (perception/pass! per)))
    (get-in (await (quote-reply eng {:pos [10 64 1] :range 0 :unknown false})) [:quote :route :dark])))

(def night-ms 14000)

(deftest the-quote-counts-seen-dark-cells-and-unseen-ones-only-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lit (await (dark-of {:time night-ms :light [15 4] :seen? true}))
              dark (await (dark-of {:time night-ms :light [15 0] :seen? true}))]
          (is (< lit 2) "seen and lit: at most the cell under the body, which its own sight does not show")
          (is (= 11 dark) "seen dark: every cell of the 11-step route"))
        (is (= 11 (await (dark-of {:time night-ms :light [15 4] :seen? false}))) "unseen at night reads dark")
        (is (zero? (await (dark-of {:time 1000 :light [15 0] :seen? false}))) "unseen by day costs like a lit one")))))

(def gate-blocks
  (assoc (merge (floor -2 -3 40 3) (box 5 64 -6 5 64 6 "oak_fence")) "5,64,0" "oak_fence_gate"))

(deftest doors-never-quotes-a-shut-gate-as-a-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [opts {:states {"5,64,0" {:open false :facing "east"}}}
              shut (await (preview gate-blocks {:pos [10 64 1] :range 0} opts))
              never (await (preview gate-blocks {:pos [10 64 1] :range 0 :doors :never} opts))]
          (is (true? (:found (:out shut))))
          (is (= [[5 64 0]] (:doors (:out shut))))
          (is (false? (:found (:out never)))))))))
