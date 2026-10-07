(ns engine.blocks-dig-test
  "jobs.blocks.dig against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.takeover :as takeover]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]
            [plan.shape :as shape]))

(defn setup
  "An engine over a fake world on the stone walk floor; world-spec :zones (default []) is the zone list."
  [world-spec]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor (dissoc world-spec :zones :plans))
        w (world/of-data (get world-spec :plans {}) {} (get world-spec :zones []))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world w
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async child-outcome
  "Run job with args as the child of a recording parent until the list is empty, at most n ticks; the child's result."
  [eng job args n]
  (let [out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           (if (= :declined r) :done r)))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out))

(defn ^:async waiting-after
  "Submit spec top level and run n ticks: why it waits (core/waiting), nil when it does not."
  [{:keys [eng clock]} spec n]
  (let [id (core/submit! eng spec {})]
    (dotimes [_ n]
      (swap! clock + 700)
      (await (core/tick! eng)))
    (core/waiting eng id)))

(defn ^:async ended-in-one-tick?
  "Submit spec as a top-level job and run one tick: whether the job is over (one call is one whole attempt)."
  [{:keys [eng clock]} spec]
  (core/submit! eng spec {})
  (swap! clock + 700)
  (await (core/tick! eng))
  (empty? (:list (core/state eng))))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn block-at [p pos] (.-name (.blockAt p (clj->js pos))))
(defn carried [p item] (reduce + 0 (keep #(when (= item (.-name %)) (.-count %)) (array-seq (.-inventory (.self p))))))

(def job 'jobs.blocks.dig)
(def at {:x 2 :y 64 :z 0})
(def body {:pos {:x 0 :y 64 :z 0}})
(def pick [{:name "wooden_pickaxe" :count 1}])

(deftest a-block-in-reach-is-dug-with-the-right-tool-and-its-drop-picked-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"2,64,0" "stone"} :drops {"stone" "cobblestone"}
                                           :inventory (into [{:name "oak_log" :count 1}] pick)})
              result (await (child-outcome eng job {:pos [2 64 0]} 10))]
          (is (= {:dug true :pos at :block "stone" :reason :dug :collected 1} result))
          (is (= "air" (block-at p at)))
          (is (= ["wooden_pickaxe"] (mapv #(.-item (.-args %)) (calls p "equip"))))
          (is (= 1 (carried p "cobblestone")))
          (is (= 1 (count (filter #(= :blocks.dig.done (:kind %)) @seen)))))))))

(deftest a-block-out-of-reach-is-walked-to-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"12,64,0" "dirt"}})
              result (await (child-outcome eng job {:pos {:x 12 :y 64 :z 0}} 20))]
          (is (= {:dug true :block "dirt" :collected 1} (select-keys result [:dug :block :collected])))
          (is (= "air" (block-at p {:x 12 :y 64 :z 0})))
          (is (= 1 (carried p "dirt"))))))))

(deftest air-is-already-clear-and-nothing-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body})
              result (await (child-outcome eng job {:pos at} 5))]
          (is (= {:dug false :reason :already-clear} (select-keys result [:dug :reason])))
          (is (empty? (calls p "dig"))))))))

(deftest without-a-harvesting-tool-it-waits-no-tool-naming-the-cheapest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "stone"}})]
          (is (= {:reason :no-tool :needs "wooden_pickaxe" :block "stone"} (await (waiting-after env (list job {:pos at :fetch false}) 3))))
          (is (empty? (calls (:p env) "dig"))))))))

(deftest need-drop-false-digs-without-the-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "stone"}})
              result (await (child-outcome eng job {:pos at :need-drop false :collect false} 5))]
          (is (= {:dug true :collected 0} (select-keys result [:dug :collected])))
          (is (= "air" (block-at p at))))))))

(def farm-zone {:name "farm" :min [2 60 0] :max [2 70 0] :owner "Miles"})

(deftest a-cell-in-anothers-zone-waits-not-allowed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "dirt"} :zones [farm-zone]})]
          (is (= {:reason :not-allowed :pos at :by :zone :zone "farm" :owner "Miles"}
                 (await (waiting-after env (list job {:pos at}) 3))))
          (is (empty? (calls (:p env) "dig"))))))))

(defn zone-on-first-step
  "env whose world gains farm-zone (over x 11..13) when the first walk step is taken; nil world zones before."
  [env]
  (let [w (:world (:eng env))]
    (.override (.-world (:p env)) "steer"
               (fn [token args impl]
                 (world/set-zones! w [(assoc farm-zone :min [11 60 -1] :max [13 70 1])])
                 (impl token args)))
    env))

(deftest a-zone-appearing-during-the-walk-declines-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (zone-on-first-step (setup {:self body :blocks {"12,64,0" "dirt"}}))
              r (await (waiting-after env (list job {:pos [12 64 0]}) 2))]
          (is (= {:reason :not-allowed :by :zone :zone "farm"} (select-keys r [:reason :by :zone])))
          (is (empty? (calls (:p env) "dig")))
          (is (= "dirt" (block-at (:p env) {:x 12 :y 64 :z 0}))))))))

(deftest no-zone-list-waits-not-allowed-no-zones
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "dirt"} :zones nil})]
          (is (= {:reason :not-allowed :pos at :by :no-zones} (await (waiting-after env (list job {:pos at}) 3)))))))))

(deftest ignore-zones-digs-anothers-block-and-records-it-for-tidying
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "dirt"} :zones [farm-zone]})
              result (await (child-outcome eng job {:pos at :ignore-zones? true} 10))]
          (is (:dug result))
          (is (= [{:cell [2 64 0] :action :dig :was "dirt" :now "air" :zone "farm"}]
                 (mapv #(select-keys (:data %) [:cell :action :was :now :zone]) (mem/entries (mem/view (:store eng)) :tidy))))
          (is (= "air" (block-at p at))))))))

(def full-inventory (mapv (fn [i] {:name (str "item_" i) :count 1}) (range 36)))

(deftest a-full-inventory-waits-unless-the-drop-is-not-wanted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "dirt"} :inventory full-inventory})]
          (is (= {:reason :inventory-full :pos at} (await (waiting-after env (list job {:pos at}) 3)))))
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "dirt"} :inventory full-inventory})]
          (is (:dug (await (child-outcome eng job {:pos at :collect false} 5))))
          (is (= "air" (block-at p at))))))))

(deftest a-same-named-item-lying-nearby-is-not-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lying {:id 99 :name "item" :kind "item" :pos {:x 1 :y 64 :z 0} :item {:name "cobblestone" :count 5}}
              {:keys [eng p]} (setup {:self body :blocks {"2,64,0" "stone"} :drops {"stone" "cobblestone"}
                                      :entities [lying] :inventory pick})
              result (await (child-outcome eng job {:pos [2 64 0]} 10))]
          (is (= {:dug true :collected 1} (select-keys result [:dug :collected])))
          (is (= 1 (carried p "cobblestone")))
          (is (= [99] (mapv #(.-id %) (array-seq (.entities p #js {:kind "item"}))))))))))

(deftest the-drop-of-stone-is-cobblestone-for-the-room-check
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stone-full (conj (vec (butlast full-inventory)) {:name "stone" :count 10})
              env (setup {:self body :blocks {"2,64,0" "stone"} :inventory (into stone-full pick)})]
          (is (= {:reason :inventory-full :pos at} (await (waiting-after env (list job {:pos at}) 3)))))))))

(deftest a-carried-stack-of-the-real-drop-is-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "stone"} :drops {"stone" "cobblestone"}
                                      :inventory (into (conj (vec (drop-last 2 full-inventory)) {:name "cobblestone" :count 10}) pick)})
              result (await (child-outcome eng job {:pos at} 10))]
          (is (= {:dug true :collected 1} (select-keys result [:dug :collected])))
          (is (= 11 (carried p "cobblestone"))))))))

(deftest a-block-with-no-drop-needs-no-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "oak_leaves"} :drops {"oak_leaves" []}
                                      :inventory full-inventory})
              result (await (child-outcome eng job {:pos at} 10))]
          (is (= {:dug true :collected 0} (select-keys result [:dug :collected])))
          (is (= "air" (block-at p at))))))))

(deftest a-hazard-not-accepted-waits-hazard
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "dirt" "3,64,0" "water"}})]
          (is (= {:reason :hazard :pos at :hazards [:fluid-adjacent]} (await (waiting-after env (list job {:pos at}) 3)))))
        (let [{:keys [eng]} (setup {:self body :blocks {"2,64,0" "dirt" "3,64,0" "water"}})]
          (is (:dug (await (child-outcome eng job {:pos at :accept #{:fluid-adjacent}} 5)))))))))

(deftest on-fluid-fail-ends-at-once-with-a-hint-instead-of-waiting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "dirt" "3,64,0" "water"}})
              r (await (child-outcome eng job {:pos at :on-fluid :fail} 5))]
          (is (= {:dug false :reason :fluid-adjacent :hazards [:fluid-adjacent]} (select-keys r [:dug :reason :hazards])))
          (is (string? (:hint r)))
          (is (empty? (calls p "dig"))))
        (let [{:keys [eng]} (setup {:self body :blocks {"2,64,0" "dirt" "3,64,0" "water"}})]
          (is (:dug (await (child-outcome eng job {:pos at :on-fluid :fail :accept #{:fluid-adjacent}} 5)))))
        (let [{:keys [eng]} (setup {:self body :blocks {"2,64,0" "dirt"}})]
          (is (:dug (await (child-outcome eng job {:pos at :on-fluid :fail} 5)))))))))

(deftest a-block-the-walk-cannot-reach-waits-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"3,55,0" "dirt"}})
              r (await (waiting-after env (list job {:pos [3 55 0]}) 15))]
          (is (= {:reason :unreachable :pos {:x 3 :y 55 :z 0}} (select-keys r [:reason :pos])))
          (is (keyword? (:why r)))
          (is (empty? (calls (:p env) "dig"))))))))

(defn failing-walks
  "env with jobs.movement.go-to replaced by a walk that never arrives (why :no-path); walks counts its calls."
  [env walks]
  (let [go-to (get (:jobs (:eng env)) 'jobs.movement.go-to)
        stub (fn ^:async failing-walk [c]
               (swap! walks inc)
               (ctx/result! c {:arrived false :why :no-path})
               :done)]
    (assoc-in env [:eng :jobs 'jobs.movement.go-to] (assoc go-to :round stub))))

(deftest a-walk-that-fails-twice-in-one-call-waits-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)
              env (failing-walks (setup {:self body :blocks {"12,64,0" "dirt"}}) walks)
              r (await (waiting-after env (list job {:pos [12 64 0]}) 2))]
          (is (= 2 @walks) "one more try, then it declines")
          (is (= {:reason :unreachable :why :no-path} (select-keys r [:reason :why]))))))))

(defn after-walks
  "env with jobs.movement.go-to replaced by the real one followed by (f p): the world changes during the walk."
  [env f]
  (let [go-to (get (:jobs (:eng env)) 'jobs.movement.go-to)
        round (:round go-to)
        wrapped (fn ^:async walk-then-change [c]
                  (let [r (await (round c))]
                    (f (:p env))
                    r))]
    (assoc-in env [:eng :jobs 'jobs.movement.go-to] (assoc go-to :round wrapped))))

(deftest lava-appearing-beside-the-block-during-the-walk-declines-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (after-walks (setup {:self body :blocks {"12,64,0" "dirt"}})
                               #(fake/set-block! % [12 65 0] "lava"))
              r (await (waiting-after env (list job {:pos [12 64 0]}) 2))]
          (is (= {:reason :hazard :pos {:x 12 :y 64 :z 0}} (select-keys r [:reason :pos])))
          (is (some #{:lava-adjacent} (:hazards r)))
          (is (empty? (calls (:p env) "dig")) "walked, then refused: nothing dug"))))))

(defn ^:async cut-at-act-then-resume
  "Run spec as a top-level job; hold the primitive act, cut the tick while it is held, release, and run on until the
  list is empty (at most 40 ticks)."
  [{:keys [eng p]} spec act]
  (core/submit! eng spec {})
  (.hold (.-world p) act)
  (let [running (core/tick! eng)]
    (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
    (takeover/take! eng {:who "claude" :why "cut"})
    (await running))
  (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
  (await (run-until-empty eng 40)))

(deftest a-cut-at-the-dig-is-resumed-and-the-block-dug-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "dirt"}})
              left (await (cut-at-act-then-resume env (list job {:pos [2 64 0]}) "dig"))]
          (is (< left 40) "the job ended")
          (is (= 2 (count (calls (:p env) "dig"))) "the cut dig and the resumed one")
          (is (empty? (:list (core/state (:eng env)))))
          (is (= "air" (block-at (:p env) at)))
          (is (= 1 (carried (:p env) "dirt")) "the drop was picked up once"))))))

(deftest a-far-block-is-walked-to-and-dug-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"12,64,0" "dirt"}})]
          (is (await (ended-in-one-tick? env (list job {:pos [12 64 0]}))))
          (is (= "air" (block-at (:p env) {:x 12 :y 64 :z 0})))
          (is (= 1 (carried (:p env) "dirt"))))))))

(deftest bad-args-end-the-job-with-a-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self body})
              result (await (child-outcome eng job {} 3))]
          (is (= {:dug false :reason :bad-args} (select-keys result [:dug :reason])))
          (is (= 1 (count (filter #(= :blocks.dig.declined (:kind %)) @seen)))))))))

(def hut {:id "hut" :parts [{:id "w" :cells [[2 64 0]] :want "stone"}]})

(deftest a-cell-of-the-bodys-own-plan-is-dug-another-bodys-plan-refuses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [own (setup {:self body :blocks {"2,64,0" "dirt"} :plans {"hut" (shape/with-author hut "Fake")}})
              other (setup {:self body :blocks {"2,64,0" "dirt"} :plans {"hut" (shape/with-author hut "Miles")}})]
          (is (= {:dug true :reason :dug} (select-keys (await (child-outcome (:eng own) job {:pos [2 64 0]} 10)) [:dug :reason])))
          (is (= {:reason :not-allowed :pos at :by :footprint :plan "hut"}
                 (select-keys (await (waiting-after other (list job {:pos at}) 3)) [:reason :pos :by :plan])))
          (is (empty? (calls (:p other) "dig"))))))))

;; ------------------------------------------------------------------ the block under the feet

(defn hiding
  "p whose body has not seen the cells (hidden? [x y z]) while the cell over each is not air: the floor under the
  block stood on is out of sight."
  [p hidden?]
  (aset p "sensedAt" (fn [pos]
                       (let [b (.blockAt p pos)
                             over (.blockAt p #js {:x (.-x pos) :y (inc (.-y pos)) :z (.-z pos)})]
                         (if (and (hidden? [(.-x pos) (.-y pos) (.-z pos)]) (not= "air" (some-> over .-name)))
                           #js {:unknown true}
                           b))))
  p)

(defn digging-from
  "Record in stands the body's feet column [x z] at each dig."
  [p stands]
  (let [st (fake/state p)]
    (.override (.-world p) "dig" (fn [token a impl]
                                   (let [[x _ z] (get-in @st [:self :pos])]
                                     (swap! stands conj [(js/Math.floor x) (js/Math.floor z)])
                                     (impl token a))))))

(def under-feet {:x 0 :y 63 :z 0})

(deftest the-block-under-the-feet-is-dug-from-beside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "stone"}})
              stands (atom [])]
          (hiding p #(= [0 62 0] %))
          (digging-from p stands)
          (let [result (await (child-outcome eng job {:pos under-feet} 20))]
            (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])))
            (is (= "air" (block-at p under-feet)))
            (is (= 1 (count @stands)))
            (is (not= [0 0] (first @stands)) "the dig is made from beside the column, never from on top")))))))

(deftest with-no-cell-beside-to-stand-on-the-block-under-the-feet-waits-hazard
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :floor [0 0 0 0] :blocks {"0,63,0" "dirt" "0,62,0" "stone"}})]
          (hiding (:p env) #(= [0 62 0] %))
          (is (= {:reason :hazard :pos under-feet :hazards [:under-feet] :why :no-side-stand}
                 (await (waiting-after env (list job {:pos under-feet}) 3))))
          (is (empty? (calls (:p env) "dig")) "a pillar top is never dug from on top"))))))

(deftest a-cell-beside-on-an-unseen-floor-is-never-stood-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :floor [0 0 1 0] :blocks {"0,63,0" "dirt" "0,62,0" "stone"}})]
          (aset (:p env) "sensedAt" (let [p (:p env)]
                                      (fn [pos] (if (#{[1 63 0] [0 62 0]} [(.-x pos) (.-y pos) (.-z pos)])
                                                  #js {:unknown true}
                                                  (.blockAt p pos)))))
          (is (= :no-side-stand (:why (await (waiting-after env (list job {:pos under-feet}) 3)))))
          (is (empty? (calls (:p env) "dig")))
          (is (= [0 64 0] (vec (map js/Math.floor (:pos (fake/self (:p env)))))) "it does not step onto the unseen floor"))))))

;; ------------------------------------------------------------------ the drop in the dug hole

(defn heights
  "Record in ys the body's feet y at every change of the fake world."
  [p ys]
  (add-watch (fake/state p) ::heights (fn [_ _ _ w] (swap! ys conj (second (get-in w [:self :pos]))))))

(defn left-drops [seen] (filterv #(= :collect-drops.left (:kind %)) @seen))

(deftest a-drop-in-a-hole-over-lava-is-left-and-the-body-never-steps-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "lava"}})
              ys (atom [])]
          (heights p ys)
          (let [result (await (child-outcome eng job {:pos under-feet :accept #{:lava-adjacent} :on-lava :leave} 20))]
            (is (= {:dug true :reason :dug :collected 0} (select-keys result [:dug :reason :collected])))
            (is (every? #(>= % 64) @ys) "the body never steps into the hole")
            (is (= [{:pos under-feet :reason :unsafe-floor}] (mapv #(select-keys % [:pos :reason]) (left-drops seen))))
            (is (zero? (carried p "dirt")) "the drop is left")))))))

(deftest a-drop-over-a-cave-opening-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "air" "0,61,0" "air"}})
              ys (atom [])]
          (heights p ys)
          (let [result (await (child-outcome eng job {:pos under-feet} 20))]
            (is (= {:dug true :collected 0} (select-keys result [:dug :collected])))
            (is (every? #(>= % 64) @ys) "no walk down into the cave")
            (is (= [:unsafe-floor] (mapv :reason (left-drops seen))))))))))

(deftest a-drop-on-a-seen-stone-floor-is-picked-up-from-inside-the-hole
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "stone"}})]
          (hiding p #(= [0 62 0] %))
          (let [result (await (child-outcome eng job {:pos under-feet} 20))]
            (is (= {:dug true :collected 1} (select-keys result [:dug :collected])))
            (is (= 1 (carried p "dirt")))
            (is (= 63 (second (get-in @(fake/state p) [:self :pos]))) "it stepped into the hole for the drop")
            (is (empty? (left-drops seen)))))))))

(deftest the-no-side-stand-wait-ends-once-a-cell-beside-can-be-stood-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :floor [0 0 0 0] :blocks {"0,63,0" "dirt" "0,62,0" "stone"}})
              p (:p env)]
          (hiding p #(= [0 62 0] %))
          (is (= :no-side-stand (:why (await (waiting-after env (list job {:pos under-feet}) 3)))))
          (swap! (fake/state p) fake/put-block [1 63 0] "stone")
          (swap! (:clock env) + 700)
          (await (core/tick! (:eng env)))
          (is (= "air" (block-at p under-feet)) "the body stepped beside and dug")
          (is (= 1 (count (calls p "dig")))))))))

;; ------------------------------------------------------------------ lava the dig lays open

(def lava-under {:x 0 :y 62 :z 0})
(def cobble [{:name "cobblestone" :count 2}])

(defn sealed-events [seen] (filterv #(= :blocks.dig.sealed (:kind %)) @seen))

(deftest hidden-lava-under-the-dug-cell-is-sealed-and-the-dug-cell-stays-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "lava" "0,61,0" "stone"} :inventory cobble})
              ys (atom [])]
          (hiding p #(= [0 62 0] %))
          (heights p ys)
          (let [result (await (child-outcome eng job {:pos under-feet} 30))]
            (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])))
            (is (= "air" (block-at p under-feet)) "the dug cell is not refilled")
            (is (= "cobblestone" (block-at p lava-under)) "the lava under it is sealed")
            (is (= [{:cell [0 62 0]}] (mapv #(select-keys % [:cell]) (sealed-events seen))))
            (is (every? #(>= % 63) @ys) "the body never went down to the lava")))))))

(deftest with-no-block-to-seal-with-the-dig-stops-lava-unsealed-and-the-body-steps-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "lava"}})]
          (hiding p #(= [0 62 0] %))
          (let [result (await (child-outcome eng job {:pos under-feet} 30))
                [x _ z] (get-in @(fake/state p) [:self :pos])]
            (is (= {:status :stopped :dug true :reason :lava-unsealed :cell [0 62 0] :place :need :stepped-away true}
                   (select-keys result [:status :dug :reason :cell :place :stepped-away])))
            (is (= "lava" (block-at p lava-under)))
            (is (>= (max (js/Math.abs (js/Math.floor x)) (js/Math.abs (js/Math.floor z))) 2)
                "the body stands off the cells beside the open lava")
            (is (re-find #"lava at \[0 62 0\] not sealed: no block to seal with"
                         (:text (last (filterv #(= :blocks.dig.done (:kind %)) @seen)))))))))))

(deftest seen-lava-beside-the-block-is-a-lava-adjacent-hazard-not-fluid-adjacent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "dirt" "2,64,1" "lava"}})]
          (is (= {:reason :hazard :pos at :hazards [:lava-adjacent]}
                 (await (waiting-after env (list job {:pos [2 64 0] :accept #{:fluid-adjacent}}) 2))))
          (is (empty? (calls (:p env) "dig"))))))))

(deftest seen-lava-taken-with-lava-adjacent-is-sealed-after-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "dirt" "2,64,1" "lava" "2,63,1" "stone"} :inventory cobble})
              result (await (child-outcome eng job {:pos [2 64 0] :accept #{:lava-adjacent}} 30))]
          (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])))
          (is (= ["air" "cobblestone"] (mapv #(block-at p %) [at {:x 2 :y 64 :z 1}]))))))))

(deftest on-lava-leave-leaves-the-lava-to-the-caller
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "lava" "0,61,0" "stone"} :inventory cobble})]
          (hiding p #(= [0 62 0] %))
          (let [result (await (child-outcome eng job {:pos under-feet :on-lava :leave} 30))]
            (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])))
            (is (= "lava" (block-at p lava-under)))
            (is (empty? (calls p "place")))))))))

(deftest after-a-failed-step-off-walk-a-new-cell-beside-ends-the-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)
              env (failing-walks (setup {:self body :floor [1 0 1 0] :blocks {"0,63,0" "dirt" "0,62,0" "stone"}}) walks)
              p (:p env)]
          (hiding p #(= [0 62 0] %))
          (is (= {:why :no-side-stand :walk :no-path} (select-keys (await (waiting-after env (list job {:pos under-feet}) 3)) [:why :walk])))
          (is (= 1 @walks) "the wait holds while nothing beside has changed")
          (swap! (fake/state p) fake/put-block [-1 63 0] "stone")
          (swap! (:clock env) + 700)
          (await (core/tick! (:eng env)))
          (is (= 2 @walks) "a cell beside that came free is tried"))))))
