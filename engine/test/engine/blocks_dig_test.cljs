(ns engine.blocks-dig-test
  "jobs.blocks.dig against the fake world."
  (:require [engine.child-ends :as ce]
            [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.scenario :as scenario]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.blocks.dig :as dig]
            [jobs.lib.blocks :as b]
            [jobs.lib.lava :as lava]
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

(defn ^:async ending-cause
  "The cause of the ending of job with args run as a child, at most n ticks."
  [eng job args n]
  (:cause (:ending (await (tu/child-outcome eng job args n)))))

(defn wait-cause "The wait a can-start? status gives: its data without the text." [w] (dissoc w :text))

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
(defn block-at [p pos] (some-> (.blockAt (tu/oracle p) (clj->js pos)) .-name))
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
              result (await (tu/child-outcome eng job {:pos [2 64 0]} 10))]
          (is (= {:dug true :pos at :block "stone" :reason :dug :collected 1 :now "air" :status :done} (dissoc result :text)))
          (is (re-find #"the cell is now clear" (:text result)) "a dig says what the cell holds after it")
          (is (= "air" (block-at p at)))
          (is (= ["wooden_pickaxe"] (mapv #(.-item (.-args %)) (calls p "equip"))))
          (is (= 1 (carried p "cobblestone")))
          (is (= 1 (count (ce/ends seen :dug)))))))))

(deftest a-hurt-body-digs-unless-the-caller-passes-a-health-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self (assoc body :health 4) :blocks {"2,64,0" "stone"} :drops {"stone" "cobblestone"} :inventory pick}
              {:keys [eng p]} (setup world)
              dug (await (tu/child-outcome eng job {:pos [2 64 0]} 10))
              {eng2 :eng p2 :p} (setup world)
              _ (await (tu/child-outcome eng2 job {:pos [2 64 0] :floor {:health 6}} 10))]
          (is (= :dug (:reason dug)))
          (is (= "air" (block-at p at)))
          (is (= "stone" (block-at p2 at))))))))

(deftest a-call-given-use-list-picks-the-tool-the-default-would-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "stone"}
                                      :inventory [{:name "iron_pickaxe" :count 1} {:name "diamond_pickaxe" :count 1}]})]
          (await (tu/child-outcome eng job {:pos [2 64 0] :use {:tools ["diamond_pickaxe"]}} 10))
          (is (= ["diamond_pickaxe"] (mapv #(.-item (.-args %)) (calls p "equip")))))))))

(deftest leaves-are-never-dug-with-a-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "oak_leaves"} :inventory pick})
              result (await (tu/child-outcome eng job {:pos [2 64 0]} 10))]
          (is (:dug result) (pr-str result))
          (is (empty? (calls p "equip")) "a pickaxe digs leaves no faster and wears out"))))))

(deftest a-held-pickaxe-is-put-away-for-dirt-when-no-shovel-is-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self (assoc body :held "stone_pickaxe") :blocks {"2,64,0" "dirt"}
                                      :inventory [{:name "stone_pickaxe" :count 1}]})
              result (await (tu/child-outcome eng job {:pos [2 64 0]} 10))]
          (is (:dug result) (pr-str result))
          (is (nil? (.-held (.self p))) "the dirt is dug by hand"))))))

(deftest a-block-out-of-reach-is-walked-to-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"12,64,0" "dirt"}})
              result (await (tu/child-outcome eng job {:pos {:x 12 :y 64 :z 0}} 20))]
          (is (= {:dug true :block "dirt" :collected 1} (select-keys result [:dug :block :collected])))
          (is (= "air" (block-at p {:x 12 :y 64 :z 0})))
          (is (= 1 (carried p "dirt"))))))))

(deftest walk-false-declines-a-block-out-of-reach-without-a-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"12,64,0" "dirt"}})
              r (await (ending-cause (:eng env) job {:pos [12 64 0] :walk false} 6))]
          (is (= {:reason :unreachable :why :no-walk} (select-keys r [:reason :why])) (pr-str r))
          (is (empty? (calls (:p env) "dig")))
          (is (< (.-x (.-pos (.self (:p env)))) 1) "the body stood still"))))))

(deftest air-and-an-invisible-light-block-are-already-clear-and-nothing-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [block ["air" "light"]]
          (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" block}})
                result (await (tu/child-outcome eng job {:pos at} 5))]
            (is (= {:status :done :already? true} (select-keys result [:status :already?])) (str block ": the engine ends it before a round"))
            (is (empty? (calls p "dig")))))))))

(deftest without-a-harvesting-tool-it-waits-no-tool-naming-the-cheapest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "stone"}})]
          (is (= {:reason :no-tool :needs "wooden_pickaxe" :block "stone"}
                 (dissoc (wait-cause (await (waiting-after env (list job {:pos at :ways #{:carried}}) 3))) :hint)))
          (is (empty? (calls (:p env) "dig"))))))))

(defn ^:async unseen-from-waits-no-tool
  [unseen-from]
  (let [env (setup {:self body :blocks {"2,64,0" "stone"}})
        p (:p env)
        shown (atom false)]
    (aset p "sensedAt" (fn [pos] (if (and (>= (.-x pos) unseen-from) (not @shown)) #js {:unknown true} (.blockAt p pos))))
    (.override (.-world p) "look" (fn [token a impl] (reset! shown true) (impl token a)))
    (is (= :no-tool (:reason (wait-cause (await (waiting-after env (list job {:pos at :ways #{:carried}}) 6))))))
    (is (empty? (calls p "dig")))))

(deftest an-unseen-block-or-face-is-looked-at-before-the-call-starts-so-a-listed-call-waits-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (unseen-from-waits-no-tool 1))
        (await (unseen-from-waits-no-tool 3))))))

(deftest the-no-tool-wait-says-need-drop-false-digs-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "stone"}})]
          (is (re-find #":need-drop false" (:hint (wait-cause (await (waiting-after env (list job {:pos at :ways #{:carried}}) 3)))))))))))

(deftest need-drop-false-digs-without-the-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"2,64,0" "stone"}})
              result (await (tu/child-outcome eng job {:pos at :need-drop false :collect false} 5))]
          (is (= {:dug true :collected 0} (select-keys result [:dug :collected])))
          (is (= 1 (count (filter #(= :blocks.dig.by-hand (:kind %)) @seen))) "a hand dig says so, so it is seen working")
          (is (= "air" (block-at p at))))))))

(def farm-zone {:name "farm" :min [2 60 0] :max [2 70 0] :owner "Miles"})

(deftest a-cell-in-anothers-zone-waits-not-allowed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "dirt"} :zones [farm-zone]})]
          (is (= {:reason :not-allowed :pos at :by :zone :zone "farm" :owner "Miles"}
                 (wait-cause (await (waiting-after env (list job {:pos at}) 3)))))
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
              r (await (ending-cause (:eng env) job {:pos [12 64 0]} 6))]
          (is (= {:reason :not-allowed :by :zone :zone "farm"} (select-keys r [:reason :by :zone])))
          (is (empty? (calls (:p env) "dig")))
          (is (= "dirt" (block-at (:p env) {:x 12 :y 64 :z 0}))))))))

(deftest no-zone-list-waits-not-allowed-no-zones
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "dirt"} :zones nil})]
          (is (= {:reason :not-allowed :pos at :by :no-zones} (wait-cause (await (waiting-after env (list job {:pos at}) 3))))))))))

(deftest ignore-zones-digs-anothers-block-and-records-it-for-tidying
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "dirt"} :zones [farm-zone]})
              result (await (tu/child-outcome eng job {:pos at :ignore-zones? true} 10))]
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
          (is (= {:reason :inventory-full :pos at} (wait-cause (await (waiting-after env (list job {:pos at}) 3))))))
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "dirt"} :inventory full-inventory})]
          (is (:dug (await (tu/child-outcome eng job {:pos at :collect false} 5))))
          (is (= "air" (block-at p at))))))))

(deftest a-same-named-item-lying-nearby-is-not-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lying {:id 99 :name "item" :kind "item" :pos {:x 1 :y 64 :z 0} :item {:name "cobblestone" :count 5}}
              {:keys [eng p]} (setup {:self body :blocks {"2,64,0" "stone"} :drops {"stone" "cobblestone"}
                                      :entities [lying] :inventory pick})
              result (await (tu/child-outcome eng job {:pos [2 64 0]} 10))]
          (is (= {:dug true :collected 1} (select-keys result [:dug :collected])))
          (is (= 1 (carried p "cobblestone")))
          (is (= [99] (mapv #(.-id %) (array-seq (.entities p #js {:kind "item"}))))))))))

(deftest the-drop-of-stone-is-cobblestone-for-the-room-check
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stone-full (conj (vec (butlast full-inventory)) {:name "stone" :count 10})
              env (setup {:self body :blocks {"2,64,0" "stone"} :inventory (into stone-full pick)})]
          (is (= {:reason :inventory-full :pos at} (wait-cause (await (waiting-after env (list job {:pos at}) 3))))))))))

(defn items-hidden-until-moved!
  "p whose entities() shows no item while the body stands where it began: a drop beyond leaves is out of sight until it walks round."
  [p]
  (let [entities (.bind (.-entities p) p)
        start (let [q (.-pos (.self p))] [(.-x q) (.-z q)])]
    (aset p "entities"
          (fn [opts]
            (let [q (.-pos (.self p))]
              (if (= start [(.-x q) (.-z q)])
                (.filter (entities opts) (fn [e] (not= "item" (.-kind e))))
                (entities opts)))))))

(deftest a-drop-out-of-sight-after-a-dig-is-looked-for-from-the-cells-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "oak_log"} :drops {"oak_log" "oak_log"}})]
          (items-hidden-until-moved! p)
          (let [result (await (tu/child-outcome eng job {:pos at} 60))]
            (is (= {:dug true :collected 1} (select-keys result [:dug :collected])) (pr-str result))
            (is (= 1 (carried p "oak_log")))))))))

(defn picks-up-during-dig!
  "p whose dig act ends with the dug block's drop already in the inventory (an adjacent drop flies to the body)."
  [p]
  (.override (.-world p) "dig"
             (fn ^:async dig-and-pick [token args impl]
               (let [r (await (impl token args))]
                 (doseq [d (array-seq (or (.-drops r) #js []))]
                   (swap! (.-state (.-world p)) #(-> % (update :entities (fn [es] (filterv (fn [e] (not= (.-id d) (:id e))) es)))
                                                      (fake/give (.-name d) 1))))
                 r))))

(deftest a-drop-picked-up-during-the-dig-is-not-searched-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "oak_log"} :drops {"oak_log" "oak_log"}})]
          (picks-up-during-dig! p)
          (let [result (await (tu/child-outcome eng job {:pos at} 60))]
            (is (= 1 (carried p "oak_log")))
            (is (= {:dug true} (select-keys result [:dug])) (pr-str result))
            (is (zero? (count (calls p "steer"))) (pr-str (map #(.-name %) (.-calls (.-world p)))))))))))

(deftest drops-left-with-no-room-left-are-reported-as-inventory-full
  (are [room? expected] (= expected (dig/mark-full [{:reason :unseen :ids [3]}] room?))
    true [{:reason :unseen :ids [3]}]
    false [{:reason :inventory-full :ids [3]}]))

(deftest a-carried-stack-of-the-real-drop-is-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "stone"} :drops {"stone" "cobblestone"}
                                      :inventory (into (conj (vec (drop-last 2 full-inventory)) {:name "cobblestone" :count 10}) pick)})
              result (await (tu/child-outcome eng job {:pos at} 10))]
          (is (= {:dug true :collected 1} (select-keys result [:dug :collected])))
          (is (= 11 (carried p "cobblestone"))))))))

(deftest a-block-with-no-drop-needs-no-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "oak_leaves"} :drops {"oak_leaves" []}
                                      :inventory full-inventory})
              result (await (tu/child-outcome eng job {:pos at} 10))]
          (is (= {:dug true :collected 0} (select-keys result [:dug :collected])))
          (is (= "air" (block-at p at))))))))

(deftest a-hazard-not-accepted-waits-hazard
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "dirt" "3,64,0" "water"}})]
          (is (= {:reason :hazard :pos at :hazards [:fluid-adjacent]} (wait-cause (await (waiting-after env (list job {:pos at}) 3))))))
        (let [{:keys [eng]} (setup {:self body :blocks {"2,64,0" "dirt" "3,64,0" "water"}})]
          (is (:dug (await (tu/child-outcome eng job {:pos at :accept #{:fluid-adjacent}} 5)))))))))

(deftest on-fluid-fail-ends-at-once-with-a-hint-instead-of-waiting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "dirt" "3,64,0" "water"}})
              r (await (tu/child-outcome eng job {:pos at :on-fluid :fail} 5))]
          (is (= {:dug false :reason :fluid-adjacent :hazards [:fluid-adjacent]} (select-keys r [:dug :reason :hazards])))
          (is (string? (:hint r)))
          (is (empty? (calls p "dig"))))
        (let [{:keys [eng]} (setup {:self body :blocks {"2,64,0" "dirt" "3,64,0" "water"}})]
          (is (:dug (await (tu/child-outcome eng job {:pos at :on-fluid :fail :accept #{:fluid-adjacent}} 5)))))
        (let [{:keys [eng]} (setup {:self body :blocks {"2,64,0" "dirt"}})]
          (is (:dug (await (tu/child-outcome eng job {:pos at :on-fluid :fail} 5)))))))))

(deftest a-block-the-walk-cannot-reach-waits-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"3,55,0" "dirt"}})
              e (:ending (await (tu/child-outcome (:eng env) job {:pos [3 55 0]} 15)))
              r (:cause e)]
          (is (= :nothing-possible (:kind e)))
          (is (= {:reason :unreachable :pos {:x 3 :y 55 :z 0}} (select-keys r [:reason :pos])))
          (is (keyword? (:why r)))
          (is (empty? (calls (:p env) "dig"))))))))

;; Stone y 60-64 (feet y 65 on top), a pit at x 3 z 1 two deep (feet y 63) with a step out at x 2. The dirt at (5 65 0)
;; is within eye reach of the pit floor, but the surface stone is between the eye and every face of it (the farm case:
;; a body in a hole under a field dug crops through the farmland and never saw the drops).
(def pit-blocks
  (-> (tu/box -2 60 -3 12 64 3 "stone")
      (dissoc "3,63,1" "3,64,1" "2,64,1")
      (assoc "5,65,0" "dirt")))

(def in-pit {:pos {:x 3.5 :y 63 :z 1.5}})

(deftest a-block-hidden-from-where-the-body-stands-is-dug-from-where-it-sees-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self in-pit :blocks pit-blocks})
              result (await (tu/child-outcome eng job {:pos [5 65 0]} 10))
              y (.-y (.-pos (.self p)))]
          (is (= {:dug true :collected 1} (select-keys result [:dug :collected])) (pr-str result))
          (is (>= y 64) (str "walked out of the pit first, feet y " y)))))))

(deftest a-block-no-stand-sees-is-dug-through-the-stone-in-the-line
  ;; the dirt is buried under the surface: no stand within the walk's range sees it, so the seen stone in the line is dug first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self in-pit :inventory [{:name "iron_pickaxe" :count 1}] :blocks (assoc pit-blocks "5,62,0" "dirt")})
              _ (tu/seeing-all (:p env))
              result (await (tu/child-outcome (:eng env) job {:pos [5 62 0]} 40))]
          (is (= {:dug true} (select-keys result [:dug])) (pr-str result))
          (is (< 1 (count (calls (:p env) "dig")))))))))

(deftest a-dig-act-that-times-out-is-a-failed-attempt-not-the-time-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "dirt"}})
              _ (.override (.-world p) "dig" (fn [_ _ _] (js/Promise.resolve #js {:status "timeout"})))
              e (:ending (await (tu/child-outcome eng job {:pos [2 64 0]} 10)))]
          (is (= [:nothing-possible :timeout] ((juxt :kind :cause) e))))))))

(defn failing-walks
  "env with jobs.movement.go-to replaced by a walk that never arrives (why :no-path); walks counts its calls."
  [env walks]
  (let [go-to (get (:jobs (:eng env)) 'jobs.movement.go-to)
        stub (fn ^:async failing-walk [c]
               (swap! walks inc)
               (ctx/end c :done nil {:arrived false :why :no-path}))]
    (assoc-in env [:eng :jobs 'jobs.movement.go-to] (assoc go-to :round stub))))

(deftest a-walk-that-fails-twice-in-one-call-waits-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)
              env (failing-walks (setup {:self body :blocks {"12,64,0" "dirt"}}) walks)
              e (:ending (await (tu/child-outcome (:eng env) job {:pos [12 64 0]} 6)))]
          (is (= 2 @walks) "one more try, then it ends")
          (is (= :nothing-possible (:kind e)) "the walk ran: no route is not a fact to wait on")
          (is (= {:reason :unreachable :why :no-path} (select-keys (:cause e) [:reason :why]))))))))

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
              r (await (ending-cause (:eng env) job {:pos [12 64 0]} 6))]
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
    (core/cut! eng (core/holder eng) :test nil)
    (await running))
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

(deftest bad-args-end-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body})
              e (:ending (await (tu/child-outcome (:eng env) job {} 4)))]
          (is (= :impossible (:kind e)))
          (is (= :bad-args (:reason (:cause e))))
          (is (string? (:text (:cause e)))))))))

(def hut {:id "hut" :parts [{:id "w" :cells [[2 64 0]] :want "stone"}]})

(deftest a-cell-of-the-bodys-own-plan-is-dug-another-bodys-plan-refuses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [own (setup {:self body :blocks {"2,64,0" "dirt"} :plans {"hut" (shape/with-author hut "Fake")}})
              other (setup {:self body :blocks {"2,64,0" "dirt"} :plans {"hut" (shape/with-author hut "Miles")}})]
          (is (= {:dug true :reason :dug} (select-keys (await (tu/child-outcome (:eng own) job {:pos [2 64 0]} 10)) [:dug :reason])))
          (is (= {:reason :not-allowed :pos at :by :footprint :plan "hut"}
                 (select-keys (wait-cause (await (waiting-after other (list job {:pos at}) 3))) [:reason :pos :by :plan])))
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
(def seal-fetch-off {:jobs.lib.blocks/seal-fetch false})

(deftest a-dug-cell-the-body-has-not-seen-since-is-not-told-as-a-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "stone"} :drops {"stone" "cobblestone"} :inventory pick})
              _ (aset p "sensedAt" (fn [pos] (if (= 2 (.-x pos)) #js {:unknown true} (.blockAt p pos))))
              result (await (tu/child-outcome eng job {:pos [2 64 0]} 10))]
          (is (not (contains? result :now)))
          (is (re-find #"was not seen since" (:text result))))))))

(deftest the-block-under-the-feet-is-dug-from-beside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "stone"}})
              stands (atom [])]
          (hiding p #(= [0 62 0] %))
          (digging-from p stands)
          (let [result (await (tu/child-outcome eng job {:pos under-feet} 20))]
            (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])))
            (is (= "air" (block-at p under-feet)))
            (is (= 1 (count @stands)))
            (is (not= [0 0] (first @stands)) "the dig is made from beside the column, never from on top")))))))

(def ring {:id "ring" :parts [{:id "w" :cells (vec (for [x [-1 0 1] y [63 64 65] z [-1 0 1] :when (not= [x z] [0 0])] [x y z])) :want "stone"}]})

(deftest the-body-steps-aside-onto-a-cell-of-its-own-plan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "stone"} :plans {"ring" (shape/with-author ring "Fake")}})
              stands (atom [])]
          (hiding p #(= [0 62 0] %))
          (digging-from p stands)
          (let [result (await (tu/child-outcome eng job {:pos under-feet :for-plan "ring"} 20))]
            (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])))
            (is (not= [0 0] (first @stands)))))))))

(deftest a-dig-under-the-feet-says-what-it-read-of-the-floor-before-it-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "stone"}})]
          (hiding p #(= [0 62 0] %))
          (await (tu/child-outcome eng job {:pos under-feet} 20))
          (let [e (first (filterv #(= :blocks.dig.under-feet (:kind %)) @seen))]
            (is (= {:feet [0 64 0] :cell [0 63 0] :decision :step-aside :floor-read "stone" :hazards [:under-feet]}
                   (select-keys e [:feet :cell :decision :floor-seen :floor-read :hazards])))))))))

(deftest a-dig-from-beside-says-where-the-body-stepped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "stone"}})
              stands (atom [])]
          (hiding p #(= [0 62 0] %))
          (digging-from p stands)
          (let [result (await (tu/child-outcome eng job {:pos under-feet} 20))
                [sx sz] (first @stands)
                text (:text result)]
            (is (= [sx 64 sz] (:stepped-aside result)) "the cell the body stood on to dig is in the result")
            (is (re-find #"stepped aside to \[-?\d+ 64 -?\d+\]" (str text)) "the done text says the body moved")))))))

(deftest with-no-cell-beside-to-stand-on-the-block-under-the-feet-waits-hazard
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :floor [0 0 0 0] :blocks {"0,63,0" "dirt" "0,62,0" "stone"}})]
          (hiding (:p env) #(= [0 62 0] %))
          (is (= {:reason :hazard :pos under-feet :hazards [:under-feet] :why :no-side-stand}
                 (await (ending-cause (:eng env) job {:pos under-feet} 10))))
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
          (is (= :no-side-stand (:why (await (ending-cause (:eng env) job {:pos under-feet} 10)))))
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
          (let [result (await (tu/child-outcome eng job {:pos under-feet :accept #{:lava-adjacent} :on-lava :leave} 20))]
            (is (= {:dug true :reason :dug :collected 0} (select-keys result [:dug :reason :collected])))
            (is (every? #(>= % 64) @ys) "the body never steps into the hole")
            (is (= [{:pos under-feet :reason :unsafe-floor}] (mapv #(select-keys % [:pos :reason]) (left-drops seen))))
            (is (zero? (carried p "dirt")) "the drop is left")))))))

(deftest the-loose-items-reflex-does-not-walk-into-the-hole-over-a-magma-floor-after-the-dig-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/seeing-all (tu/fake-on-floor {:self body :inventory [{:name "wooden_pickaxe" :count 1}] :blocks {"0,63,0" "stone" "0,62,0" "magma_block"}}))
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                :backoff false :world (world/of-data {} {} [])
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
              ys (atom [])]
          (heights p ys)
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :loose-items}]}"))
          (core/submit! eng (list job {:pos under-feet}) {})
          (dotimes [_ 60]
            (swap! clock + 700)
            (await (core/tick! eng)))
          (is (empty? (calls p "collect")) "no collect for the drop in the magma hole")
          (is (every? #(>= % 64) @ys) "the body never steps into the hole")
          (is (zero? (carried p "cobblestone"))))))))

(deftest a-drop-over-a-cave-opening-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "air" "0,61,0" "air"}})
              ys (atom [])]
          (heights p ys)
          (let [result (await (tu/child-outcome eng job {:pos under-feet} 20))]
            (is (= {:dug true :collected 0} (select-keys result [:dug :collected])))
            (is (every? #(>= % 64) @ys) "no walk down into the cave")
            (is (= [:unsafe-floor] (mapv :reason (left-drops seen))))))))))

(deftest a-drop-on-a-seen-stone-floor-is-picked-up-from-inside-the-hole
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "stone"}})]
          (hiding p #(= [0 62 0] %))
          (let [result (await (tu/child-outcome eng job {:pos under-feet} 20))]
            (is (= {:dug true :collected 1} (select-keys result [:dug :collected])))
            (is (= 1 (carried p "dirt")))
            (is (= 63 (second (get-in @(fake/state p) [:self :pos]))) "it stepped into the hole for the drop")
            (is (= [0 63 0] (:stepped-down result)) "the walk down is told")
            (is (re-find #"stepped down to \[0 63 0\]" (:text result)))
            (is (empty? (left-drops seen)))))))))

(deftest a-new-call-digs-once-a-cell-beside-can-be-stood-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :floor [0 0 0 0] :blocks {"0,63,0" "dirt" "0,62,0" "stone"}})
              p (:p env)]
          (hiding p #(= [0 62 0] %))
          (is (= :no-side-stand (:why (await (ending-cause (:eng env) job {:pos under-feet} 10)))))
          (swap! (fake/state p) fake/put-block [1 63 0] "stone")
          (is (:dug (await (tu/child-outcome (:eng env) job {:pos under-feet} 10))))
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
          (let [result (await (tu/child-outcome eng job {:pos under-feet} 30))]
            (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])))
            (is (= "air" (block-at p under-feet)) "the dug cell is not refilled")
            (is (= "cobblestone" (block-at p lava-under)) "the lava under it is sealed")
            (is (= [{:cell [0 62 0]}] (mapv #(select-keys % [:cell]) (sealed-events seen))))
            (is (every? #(>= % 63) @ys) "the body never went down to the lava")))))))

(deftest lava-under-the-dug-cell-shows-only-when-looked-at-and-is-then-sealed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "lava" "0,61,0" "stone"} :inventory cobble})
              shown (atom false)]
          (aset p "sensedAt" (fn [pos] (if (and (= [0 62 0] [(.-x pos) (.-y pos) (.-z pos)]) (not @shown))
                                         #js {:unknown true}
                                         (.blockAt p pos))))
          (.override (.-world p) "look" (fn [token a impl]
                                          (when (<= 62 (.. a -pos -y) 63) (reset! shown true))
                                          (impl token a)))
          (let [result (await (tu/child-outcome eng job {:pos under-feet} 40))]
            (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])))
            (is (= "cobblestone" (block-at p lava-under)) "the lava the dig laid open was looked for and sealed")
            (is (= [{:cell [0 62 0]}] (mapv #(select-keys % [:cell]) (sealed-events seen))))))))))

(deftest lava-beside-the-dug-cell-shows-only-when-looked-at-on-its-face-and-is-then-sealed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [side {:x 1 :y 63 :z 0}
              {:keys [eng p seen]} (setup {:self body :blocks {"0,63,0" "dirt" "1,63,0" "lava" "0,62,0" "stone" "1,62,0" "stone"} :inventory cobble})
              shown (atom false)]
          (aset p "sensedAt" (fn [pos] (if (and (= [1 63 0] [(.-x pos) (.-y pos) (.-z pos)]) (not @shown))
                                         #js {:unknown true}
                                         (.blockAt p pos))))
          (.override (.-world p) "look" (fn [token a impl]
                                          (when (< (js/Math.abs (- 1.05 (.. a -pos -x))) 0.1) (reset! shown true))
                                          (impl token a)))
          (let [result (await (tu/child-outcome eng job {:pos under-feet :accept #{:lava-adjacent}} 40))]
            (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])))
            (is (= "cobblestone" (block-at p side)) "the lava beside the dug cell was looked for and sealed")
            (is (= [{:cell [1 63 0]}] (mapv #(select-keys % [:cell]) (sealed-events seen))))))))))

(deftest with-no-block-to-seal-with-the-dig-stops-lava-unsealed-and-the-body-steps-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "lava"}})]
          (hiding p #(= [0 62 0] %))
          (let [result (await (tu/child-outcome eng job {:pos under-feet} 30))
                [x _ z] (get-in @(fake/state p) [:self :pos])]
            (is (= {:status :stopped :dug true :reason :lava-unsealed :cell [0 62 0] :place :need :stepped-away true}
                   (select-keys result [:status :dug :reason :cell :place :stepped-away])))
            (is (= "lava" (block-at p lava-under)))
            (is (>= (max (js/Math.abs (js/Math.floor x)) (js/Math.abs (js/Math.floor z))) 2)
                "the body stands off the cells beside the open lava")
            (is (re-find #"lava at \[0 62 0\] not sealed: no block to seal with"
                         (:text result)))))))))

(defn setup-seeing
  "setup with the body's sight (engine.perception) over the fake world, one sight pass done: a chest in view is a seen one."
  [world-spec]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake-on-floor world-spec)
        p (perception/wrap raw (perception/create (fake-raw/create raw) {:radius 16 :ray-deg 2 :now #(deref clock)}))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (world/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (perception/pass! (aget p "perception"))
    {:eng eng :p p :seen seen :clock clock}))

(deftest with-no-block-carried-the-seal-fetches-one-from-a-seen-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-seeing {:self body :blocks {"2,64,0" "dirt" "2,64,1" "lava" "2,63,1" "stone" "-2,64,3" "chest"}
                                             :containers {"-2,64,3" [{:name "cobblestone" :count 5}]}})
              result (await (tu/child-outcome eng job {:pos [2 64 0] :accept #{:lava-adjacent} :ways #{:carried}} 60))]
          (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])))
          (is (= "cobblestone" (block-at p {:x 2 :y 64 :z 1})) "the lava is sealed with a fetched block"))))))

(deftest a-missing-tool-is-fetched-from-a-seen-chest-then-the-block-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-seeing {:self body :blocks {"2,64,0" "stone" "-2,64,3" "chest"}
                                             :containers {"-2,64,3" [{:name "wooden_pickaxe" :count 1}]}})
              result (await (tu/child-outcome eng job {:pos [2 64 0]} 80))]
          (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])) (pr-str result))
          (is (= "air" (block-at p at))))))))

(deftest a-use-list-without-a-harvesting-tool-ends-no-tool-without-fetching
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-seeing {:self body :blocks {"2,64,0" "iron_ore" "-2,64,3" "chest"}
                                             :containers {"-2,64,3" [{:name "stone_pickaxe" :count 1}]}})
              e (:ending (await (tu/child-outcome eng job {:pos [2 64 0] :use {:tools ["wooden_pickaxe"]}} 80)))]
          (is (= {:kind :condition-failed :reason :no-tool} (assoc (select-keys e [:kind]) :reason (:reason (:cause e)))) (pr-str e))
          (is (nil? (:why (:cause e))) "no fetch was tried")
          (is (= "iron_ore" (block-at p at))))))))

(deftest a-missing-tool-of-the-use-list-is-the-one-fetched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-seeing {:self body :blocks {"2,64,0" "stone" "-2,64,3" "chest"}
                                             :containers {"-2,64,3" [{:name "wooden_pickaxe" :count 1}]}})
              result (await (tu/child-outcome eng job {:pos [2 64 0] :use {:tools ["wooden_pickaxe"]}} 80))]
          (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])) (pr-str result))
          (is (= "air" (block-at p at))))))))

(deftest a-tool-no-fetch-finds-ends-condition-failed-no-tool-with-the-fetch-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-seeing {:self body :blocks {"2,64,0" "stone"}})
              e (:ending (await (tu/child-outcome eng job {:pos [2 64 0]} 80)))]
          (is (= {:kind :condition-failed :reason :no-tool :block "stone"}
                 (assoc (select-keys e [:kind]) :reason (:reason (:cause e)) :block (:block (:cause e)))) (pr-str e))
          (is (some? (:why (:cause e))) "the fetch's cause is kept")
          (is (= "stone" (block-at p at))))))))

(deftest endings-follow-the-contract
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [:pos] dig/target))
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "bedrock"} :inventory pick})
              e (:ending (await (tu/child-outcome eng job {:pos [2 64 0] :need-drop false} 10)))]
          (is (= {:kind :condition-failed :cause :cannot} (select-keys e [:kind :cause])) (pr-str e))
          (is (empty? (calls p "dig")) "seen bedrock is refused without a dig action"))
        (let [{:keys [eng]} (setup {:self body :blocks {"2,64,0" "dirt"}})
              e (:ending (await (tu/child-outcome eng job {:pos [2 64 0]} 10)))]
          (is (nil? e) "a done call has no stop ending"))))))

(deftest seal-fetch-false-seals-only-with-a-carried-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "lava" "-2,64,3" "chest"}
                                      :containers {"-2,64,3" [{:name "cobblestone" :count 5}]}})]
          (hiding p #(= [0 62 0] %))
          (let [result (await (settings/with-settings seal-fetch-off #(tu/child-outcome eng job {:pos under-feet} 60)))]
            (is (= {:reason :lava-unsealed :place :need} (select-keys result [:reason :place])))
            (is (= "lava" (block-at p lava-under)))))))))

(deftest step-away-tries-the-next-cell-when-the-first-walk-fails
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)
              env (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "lava"}})
              go-to (get (:jobs (:eng env)) 'jobs.movement.go-to)
              round (:round go-to)
              first-fails (fn ^:async first-walk-fails [c]
                            (if (and (= "air" (block-at (:p env) under-feet)) (= 1 (swap! walks inc)))
                              (ctx/end c :done nil {:arrived false :why :no-path})
                              (await (round c))))
              env (assoc-in env [:eng :jobs 'jobs.movement.go-to] (assoc go-to :round first-fails))]
          (hiding (:p env) #(= [0 62 0] %))
          (let [result (await (settings/with-settings seal-fetch-off #(tu/child-outcome (:eng env) job {:pos under-feet} 40)))]
            (is (= {:reason :lava-unsealed :stepped-away true} (select-keys result [:reason :stepped-away])))
            (is (= 2 @walks) "the first walk after the dig failed, the next cell was walked to")))))))

(deftest step-away-ends-when-a-failed-walk-left-the-body-clear-of-the-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)
              env (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "lava"}})
              go-to (get (:jobs (:eng env)) 'jobs.movement.go-to)
              round (:round go-to)
              moves-then-fails (fn ^:async moves-then-fails [c]
                                 (if (= "air" (block-at (:p env) under-feet))
                                   (do (swap! walks inc)
                                       (swap! (fake/state (:p env)) assoc-in [:self :pos] [3 64 0])
                                       (ctx/end c :done nil {:arrived false :why :no-path}))
                                   (await (round c))))
              env (assoc-in env [:eng :jobs 'jobs.movement.go-to] (assoc go-to :round moves-then-fails))]
          (hiding (:p env) #(= [0 62 0] %))
          (let [result (await (settings/with-settings seal-fetch-off #(tu/child-outcome (:eng env) job {:pos under-feet} 40)))]
            (is (= {:reason :lava-unsealed :stepped-away true} (select-keys result [:reason :stepped-away])))
            (is (= 1 @walks) "the body stands clear after the first walk: no second walk")))))))

(deftest seen-lava-beside-the-block-is-a-lava-adjacent-hazard-not-fluid-adjacent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup {:self body :blocks {"2,64,0" "dirt" "2,64,1" "lava"}})]
          (is (= {:reason :hazard :pos at :hazards [:lava-adjacent]}
                 (wait-cause (await (waiting-after env (list job {:pos [2 64 0] :accept #{:fluid-adjacent}}) 2)))))
          (is (empty? (calls (:p env) "dig"))))))))

(deftest seen-lava-taken-with-lava-adjacent-is-sealed-after-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"2,64,0" "dirt" "2,64,1" "lava" "2,63,1" "stone"} :inventory cobble})
              result (await (tu/child-outcome eng job {:pos [2 64 0] :accept #{:lava-adjacent}} 30))]
          (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])))
          (is (= ["air" "cobblestone"] (mapv #(block-at p %) [at {:x 2 :y 64 :z 1}]))))))))

(deftest on-lava-leave-leaves-the-lava-to-the-caller
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self body :blocks {"0,63,0" "dirt" "0,62,0" "lava" "0,61,0" "stone"} :inventory cobble})]
          (hiding p #(= [0 62 0] %))
          (let [result (await (tu/child-outcome eng job {:pos under-feet :on-lava :leave} 30))]
            (is (= {:dug true :reason :dug} (select-keys result [:dug :reason])))
            (is (= "lava" (block-at p lava-under)))
            (is (empty? (calls p "place")))))))))

(deftest after-a-failed-step-off-walk-a-new-cell-beside-is-tried-by-a-new-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)
              env (failing-walks (setup {:self body :floor [1 0 1 0] :blocks {"0,63,0" "dirt" "0,62,0" "stone"}}) walks)
              p (:p env)]
          (hiding p #(= [0 62 0] %))
          (is (= {:why :no-side-stand :walk :no-path} (select-keys (await (ending-cause (:eng env) job {:pos under-feet} 10)) [:why :walk])))
          (is (= 1 @walks))
          (swap! (fake/state p) fake/put-block [-1 63 0] "stone")
          (await (ending-cause (:eng env) job {:pos under-feet} 10))
          (is (= 2 @walks) "a cell beside that came free is tried"))))))

(defn failing-step-offs
  "env whose go-to walks fail (why :no-path) once the cell under the feet is dug; escalations holds whether each failed
  walk may escalate (its :ways more than :walk and :clear-seal)."
  [env escalations]
  (let [go-to (get (:jobs (:eng env)) 'jobs.movement.go-to)
        round (:round go-to)
        stub (fn ^:async failing-step-off [c]
               (if (= "air" (block-at (:p env) under-feet))
                 (do (swap! escalations conj (not (contains? #{#{:walk} #{:walk :clear-seal}} (:ways (:args c)))))
                     (ctx/end c :done nil {:arrived false :why :no-path}))
                 (await (round c))))]
    (assoc-in env [:eng :jobs 'jobs.movement.go-to] (assoc go-to :round stub))))

(defn ^:async step-off-escalations
  "The :escalate of each failed step-off walk after an unsealed dig under the feet on floor [x0 z0 x1 z1], and the result."
  [floor]
  (let [escalations (atom [])
        env (failing-step-offs (setup (cond-> {:self body :blocks {"0,63,0" "dirt" "0,62,0" "lava"}} floor (assoc :floor floor)))
                               escalations)]
    (hiding (:p env) #(= [0 62 0] %))
    (let [result (await (settings/with-settings seal-fetch-off #(tu/child-outcome (:eng env) job {:pos under-feet} 60)))]
      [@escalations result])))

(deftest with-one-cell-to-step-off-to-its-walk-escalates-and-the-failure-is-kept
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[escalations result] (await (step-off-escalations [1 0 2 0]))]
          (is (= [true] escalations) "the only cell is the last walk: it escalates")
          (is (= {:reason :lava-unsealed :stepped-away false :unreachable :all-walks-failed :walk :no-path}
                 (select-keys result [:reason :stepped-away :unreachable :walk]))))))))

(deftest with-two-cells-to-step-off-to-the-second-walk-escalates
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[escalations result] (await (step-off-escalations [1 0 3 0]))]
          (is (= [false true] escalations))
          (is (= :all-walks-failed (:unreachable result))))))))

(deftest step-away-walks-at-most-three-cells-the-last-escalated
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[escalations result] (await (step-off-escalations nil))]
          (is (= [false false true] escalations))
          (is (= {:stepped-away false :unreachable :all-walks-failed} (select-keys result [:stepped-away :unreachable]))))))))

(deftest with-no-cell-to-step-off-to-the-result-says-no-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[escalations result] (await (step-off-escalations [1 0 1 0]))]
          (is (= [] escalations))
          (is (= {:stepped-away false :unreachable :no-cell} (select-keys result [:stepped-away :unreachable]))))))))

;; ------------------------------------------------------------------ the cleanup of a cut dig

(def own-drop {:id 100 :name "item" :kind "item" :pos {:x 1 :y 64 :z 0} :item {:name "dirt" :count 1}})
(def other-drop {:id 101 :name "item" :kind "item" :pos {:x -1 :y 64 :z 0} :item {:name "dirt" :count 1}})

(defn ^:async cleanup-of-a-cut-dig
  "dig's cleanup run (by a probe job's round, memory as a dig cut right after it dug under-feet) over a world with lava
  under the dug cell, the dug block's own drop and another drop lying; [the cleanup's result p]."
  [inventory]
  (let [{:keys [eng p]} (setup {:self body :blocks {"0,63,0" "air" "0,62,0" "lava" "0,61,0" "stone"} :inventory inventory
                                :entities [own-drop other-drop]})
        out (atom nil)
        probe {:round (fn ^:async probe [c]
                        (if-not (:for (ctx/mem c))
                          (do (ctx/update-mem! c assoc :for (:pos (b/parse (:args c))) :dug {:block "dirt" :ids [100] :want 1}) :continue)
                          (let [r (await (dig/cleanup c nil))]
                            (if (= :continue r) :continue (do (reset! out r) (ctx/end c :done nil))))))}]
    (hiding p #(= [0 62 0] %))
    (core/submit! (assoc eng :jobs (assoc (:jobs eng) 'dig-cleanup-probe probe))
                  (list 'dig-cleanup-probe {:pos under-feet :on-lava :seal :collect true}) {})
    (await (run-until-empty (assoc eng :jobs (assoc (:jobs eng) 'dig-cleanup-probe probe)) 30))
    [@out p]))

(deftest the-cleanup-of-a-cut-dig-seals-the-lava-and-collects-only-its-own-drop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (cleanup-of-a-cut-dig cobble))]
          (is (= :done (:kind r)) (pr-str r))
          (is (= "cobblestone" (block-at p lava-under)) "the lava is sealed")
          (is (= 1 (carried p "dirt")) "its own drop is picked up")
          (is (= [101] (mapv #(.-id %) (array-seq (.entities p #js {:kind "item"})))) "the other drop lies on"))))))

(deftest the-cleanup-of-a-cut-dig-reports-a-seal-it-could-not-make
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (cleanup-of-a-cut-dig []))]
          (is (= {:kind :nothing-possible :cause :lava-unsealed} (select-keys r [:kind :cause])) (pr-str r))
          (is (= "lava" (block-at p lava-under))))))))

(deftest a-side-wall-hidden-from-the-spot-a-shaft-was-dug-from-is-seen-from-the-far-edge
  (let [mass (into {} (for [x (range -3 4) y (range 55 62) z (range -3 4)
                            :when (not (and (zero? x) (zero? z) (>= y 59)))]
                        [(str x "," y "," z) "stone"]))
        p (tu/fake {:self {:pos {:x 3.5 :y 62 :z 1.5}} :blocks mass})
        far {:x 3.5 :y 62 :z 1.5}
        stance (lava/peek-stance p far (constantly true) [0 59 0] [[0 0 1]])]
    (is (and (zero? (:x stance)) (neg? (:z stance))) "over the column on the side away from the wall, where the line runs down the shaft")
    (is (nil? (lava/peek-stance p {:x 0.5 :y 62 :z -0.5} (constantly true) [0 59 0] [[0 0 1]])) "none better than where the body already sees")))

(deftest a-peek-stance-is-never-a-cell-the-body-has-not-seen
  (let [mass (into {} (for [x (range -3 4) y (range 55 62) z (range -3 4)
                            :when (not (and (zero? x) (zero? z) (>= y 59)))]
                        [(str x "," y "," z) "stone"]))
        p (tu/fake {:self {:pos {:x 3.5 :y 62 :z 1.5}} :blocks mass})
        sensed (.-sensedAt p)
        stance #(lava/peek-stance p {:x 3.5 :y 62 :z 1.5} (constantly true) [0 59 0] [[0 0 1]])
        before (stance)]
    (aset p "sensedAt" (fn [pos] (if (and (= (.-x pos) (:x before)) (= (.-z pos) (:z before)) (>= (.-y pos) (:y before)))
                                   #js {:unknown true}
                                   (sensed pos))))
    (is (some? before))
    (is (not= (select-keys before [:x :z]) (select-keys (stance) [:x :z])) "the stance whose feet and head were never seen is passed over")))
