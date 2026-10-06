(ns engine.blocks-dig-test
  "jobs.blocks.dig against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
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

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

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
          (is (= {:reason :no-tool :needs "wooden_pickaxe" :block "stone"} (await (waiting-after env (list job {:pos at}) 3))))
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
              result (await (child-outcome eng job {:pos "here"} 3))]
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
