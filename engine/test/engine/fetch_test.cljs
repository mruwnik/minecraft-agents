(ns engine.fetch-test
  "The :fetch option (jobs.lib.fetch), jobs.items.obtain and jobs.items.get-tool against
  the fake world, slice F1: carried and chest sources only, chests the body has seen."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [jobs.items.obtain.craft :as obtain-craft]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [jobs.lib.crafting-table :as crafting-table]
            [jobs.items.obtain :as obtain]
            [jobs.items.obtain.base :as base]
            [jobs.lib.child :as child]
            [jobs.items.obtain.gather :as gather]
            [jobs.items.obtain.plan :as plan]
            [jobs.items.obtain.seen :as seen]
            [jobs.items.obtain.table :as table]
            [jobs.lib.need :as need]
            [jobs.lib.stock :as stock]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(def sight-opts {:radius 16 :ray-deg 2})

(defn start
  "An engine over a fake world on an andesite floor, seeing through perception (one sight pass first, ray jitter fixed unless sight gives :rand), with zones."
  ([world zones] (start world zones sight-opts))
  ([world zones sight] (start world zones sight {}))
  ([world zones sight plans]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake-on-floor (assoc world :floor-block "andesite"))
        p (perception/wrap raw (perception/create (fake-raw/create raw) (merge {:rand (constantly 0.5)} sight {:now #(deref clock)})))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock) :world (ew/of-data plans {} zones)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (perception/pass! (aget p "perception"))
    {:eng eng :p p :seen seen :clock clock})))

(defn ^:async run-ticks
  [{:keys [eng clock]} n]
  (await (tu/run-ticks eng clock n 700)))

(defn calls [{:keys [p]} name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn inv [{:keys [p]}] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn block-at [{:keys [p]} x y z] (some-> (.blockAt p (tu/pos x y z)) .-name))
(defn chest-items [{:keys [p]} pos] (get-in @(fake/state p) [:containers pos]))
(defn listed [{:keys [eng]}] (:list (core/state eng)))

(def stone-at "2,64,3")
(def chest-at "-2,64,3")
(def pickaxe-stock {chest-at [{:name "wooden_pickaxe" :count 1}]})

(defn world
  "Stone to dig south-east of the body, a chest south-west of it holding containers: both where the body faces (south),
  so its first sight pass sees them."
  [containers]
  {:self {:pos {:x 0 :y 64 :z 0}}
   :blocks {stone-at "stone" chest-at "chest" "4,63,3" "podzol"}
   :containers containers
   :drops {"stone" "cobblestone"}})

(def own-zone {:name "home" :min [-6 60 -6] :max [6 70 6] :owner "Fake"})
(def foreign-zone {:name "vault" :min [-3 60 2] :max [-1 70 4] :owner "Miles"})

(defn equip-spec [extra] (list 'jobs.items.equip (merge {:item "wooden_pickaxe"} extra)))


(defn stopped-reasons [s] (mapv :reason (events-of s :stopped)))

(defn no-source-events
  "The :no-source endings of kind (:stopped: a call that looked around; :waiting: one that could not, no chests allowed)."
  [s kind]
  (filterv #(= :no-source (:reason %)) (events-of s kind)))

(defn no-source-why [s kind] (some #(or (:why %) (get-in % [:cause :why])) (no-source-events s kind)))

(defn dig-spec [extra] (list 'jobs.blocks.dig (merge {:pos [2 64 3]} extra)))

(defn transfers [s] (calls s "transfer"))
(defn inspects [s] (calls s "inspectContainer"))

;; ------------------------------------------------------------------ the option

(deftest a-need-above-64-is-fetched-whole
  (let [o need/built-in]
    (is (= {:item "cobblestone" :count 100 :explore false} (:args (need/plan-for {:reason :need :item "cobblestone" :count 100} o))) "no 64 cap on one fetch")
    (is (= {:any-of ["dirt"] :count 100 :explore false} (:args (need/plan-for {:reason :need :any-of ["dirt"] :count 100} o))))))

(deftest a-wait-maps-to-a-fetch-only-for-wanted-kinds
  (let [o need/built-in]
    (is (= 'jobs.items.get-tool (:job (need/plan-for {:reason :no-tool :needs "wooden_pickaxe" :block "stone"} o))))
    (is (= {:block "stone" :explore false} (:args (need/plan-for {:reason :no-tool :needs "wooden_pickaxe" :block "stone"} o))))
    (is (= {:item "oak_planks" :count 1 :explore false} (:args (need/plan-for {:reason :need :item "oak_planks" :pos {}} o))))
    (is (= {:any-of ["dirt" "cobblestone"] :count 1 :explore false} (:args (need/plan-for {:reason :need :any-of ["dirt" "cobblestone"]} o))))
    (is (nil? (need/plan-for {:reason :no-tool :block "stone"} (assoc o :what #{:item}))))
    (is (nil? (need/plan-for {:reason :inventory-full :pos {}} o)))
    (is (nil? (need/plan-for {:reason :not-allowed :pos {}} o)))))

;; ------------------------------------------------------------------ dig with :fetch, the chest source

(deftest dig-fetches-a-pickaxe-from-an-own-seen-chest-then-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (core/submit! (:eng s) (dig-spec {}) {})
          (await (run-ticks s 40))
          (is (empty? (listed s)) "the dig ended")
          (is (= "air" (block-at s 2 64 3)))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (= 1 (get (inv s) "cobblestone")))
          (is (= 1 (count (transfers s))))
          (is (empty? (filter #(= "wooden_pickaxe" (:name %)) (chest-items s [-2 64 3]))) "the chest lost its pickaxe")
          (is (= 1 (count (transfers s)))))))))

;; the default call (no :fetch) fetches: dig, place and stair
(deftest dig-by-default-fetches-a-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (core/submit! (:eng s) (dig-spec {}) {})
          (await (run-ticks s 40))
          (is (= "air" (block-at s 2 64 3)))
          (is (= 1 (get (inv s) "wooden_pickaxe"))))))))

(deftest place-by-default-fetches-the-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "cobblestone" :count 5}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.blocks.place {:pos [1 64 1] :item "cobblestone"}) {})
          (await (run-ticks s 40))
          (is (= "cobblestone" (block-at s 1 64 1)))
          (is (= 1 (count (filter #(= :need (:slot %)) (events-of s :child_started))))))))))

(deftest stair-by-default-fetches-a-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:self {:pos {:x 0 :y 64 :z 0}}
                        :blocks {"0,61,1" "andesite" "0,62,1" "andesite" "0,63,1" "stone" "0,64,1" "stone" "0,65,1" "stone"
                                 "-3,64,2" "chest"}
                        :containers {"-3,64,2" [{:name "wooden_pickaxe" :count 1}]}
                        :drops {"stone" "cobblestone"}}
                       [own-zone])]
          (core/submit! (:eng s) (list 'jobs.access.stair {:dir :down :heading :south :steps 1}) {})
          (await (run-ticks s 60))
          (is (= "air" (block-at s 0 64 1)))
          (is (= 1 (count (filter #(= :need (:slot %)) (events-of s :child_started))))))))))

(deftest dig-and-place-fetch-and-act-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (core/submit! (:eng s) (dig-spec {}) {})
          (swap! (:clock s) + 700)
          (await (core/tick! (:eng s)))
          (is (empty? (listed s)))
          (is (= "air" (block-at s 2 64 3))))
        (let [s (start (world {chest-at [{:name "cobblestone" :count 5}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.blocks.place {:pos [1 64 1] :item "cobblestone"}) {})
          (swap! (:clock s) + 700)
          (await (core/tick! (:eng s)))
          (is (empty? (listed s)))
          (is (= "cobblestone" (block-at s 1 64 1))))))))

(deftest dig-without-fetch-waits-no-tool-and-touches-no-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (core/submit! (:eng s) (dig-spec {:ways #{:carried}}) {})
          (await (run-ticks s 10))
          (is (= [:no-tool] (distinct (mapv :reason (events-of s :waiting)))))
          (is (empty? (stopped-reasons s)))
          (is (empty? (inspects s)))
          (is (empty? (transfers s)))
          (is (= "stone" (block-at s 2 64 3))))))))

(deftest a-chest-in-another-zone-is-not-used-and-the-dig-ends-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [foreign-zone])]
          (core/submit! (:eng s) (dig-spec {}) {})
          (await (run-ticks s 15))
          (is (= [:no-tool] (stopped-reasons s)))
          (is (empty? (inspects s)))
          (is (empty? (transfers s))))))))

(deftest a-chest-in-another-zone-that-allows-take-is-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [(assoc foreign-zone :allow #{:take})])]
          (core/submit! (:eng s) (dig-spec {}) {})
          (await (run-ticks s 40))
          (is (= "air" (block-at s 2 64 3)))
          (is (= 1 (count (transfers s)))))))))

(deftest an-open-unzoned-seen-chest-is-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [])]
          (core/submit! (:eng s) (dig-spec {}) {})
          (await (run-ticks s 40))
          (is (= "air" (block-at s 2 64 3)))
          (is (= 1 (get (inv s) "wooden_pickaxe"))))))))

(def walled-chest
  "The chest west of the body shut in stone on every side: there, but never seen."
  {"-3,64,3" "stone" "-1,64,3" "stone" "-2,64,4" "stone" "-2,64,2" "stone" "-2,65,3" "stone" "-2,63,3" "stone"})

(deftest a-chest-never-seen-is-not-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (world pickaxe-stock) :blocks merge walled-chest) [own-zone])]
          (core/submit! (:eng s) (dig-spec {}) {})
          (await (run-ticks s 15))
          (is (empty? (inspects s)))
          (is (empty? (transfers s)))
          (is (= [:no-tool] (stopped-reasons s))))))))

;; ------------------------------------------------------------------ place with :fetch

(deftest place-fetches-the-block-from-a-seen-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "cobblestone" :count 5}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.blocks.place {:pos [1 64 1] :item "cobblestone"}) {})
          (await (run-ticks s 40))
          (is (= "cobblestone" (block-at s 1 64 1)))
          (is (empty? (listed s))))))))

;; ------------------------------------------------------------------ stair with :fetch

(deftest stair-fetches-a-pickaxe-then-cuts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:self {:pos {:x 0 :y 64 :z 0}}
                        :blocks {"0,61,1" "andesite" "0,62,1" "andesite" "0,63,1" "stone" "0,64,1" "stone" "0,65,1" "stone"
                                 "-3,64,2" "chest"}
                        :containers {"-3,64,2" [{:name "wooden_pickaxe" :count 1}]}
                        :drops {"stone" "cobblestone"}}
                       [own-zone])
              id (core/submit! (:eng s) (list 'jobs.access.stair {:dir :down :heading :south :steps 1}) {})]
          (await (run-ticks s 60))
          (is (= 1 (get (inv s) "wooden_pickaxe")) (pr-str (core/waiting (:eng s) id)))
          (is (= "air" (block-at s 0 64 1)))
          (is (= 1 (count (filter #(= :need (:slot %)) (events-of s :child_started)))))
          (is (empty? (events-of s :stopped)) "the stair went on from where it began")
          (is (empty? (listed s)))
          (let [me (:pos (fake/self (:p s)))]
            (is (= [0 0] [(js/Math.floor (:x me)) (js/Math.floor (:z me))]) "back on the stair line at its origin")))))))

(deftest stair-fetches-a-pickaxe-lost-mid-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:self {:pos {:x 0 :y 64 :z 0}}
                        :inventory [{:name "stone_pickaxe" :count 1}]
                        :blocks {"0,61,1" "andesite" "0,62,1" "andesite" "0,63,1" "stone" "0,64,1" "stone" "0,65,1" "stone"
                                 "0,60,2" "stone" "0,61,2" "stone" "0,62,2" "stone" "0,63,2" "stone" "0,64,2" "stone"
                                 "-3,64,2" "chest"}
                        :containers {"-3,64,2" [{:name "wooden_pickaxe" :count 1}]}
                        :drops {"stone" "cobblestone"}}
                       [own-zone])
              n (atom 0)
              world (.-world (:p s))
              _ (.override world "dig"
                           (fn ^:async f [token a impl]
                             (let [r (await (impl token a))]
                               (when (= 1 (swap! n inc))
                                 (swap! (.. world -state) assoc :inventory []))
                               r)))
              id (core/submit! (:eng s) (list 'jobs.access.stair {:dir :down :heading :south :steps 2}) {})]
          (await (run-ticks s 80))
          (is (= 1 (get (inv s) "wooden_pickaxe")) (pr-str (core/waiting (:eng s) id)))
          (is (empty? (events-of s :stopped)) "no stop for the lost pickaxe")
          (is (= 1 (count (filter #(= :need (:slot %)) (events-of s :child_started))))))))))

(defn ^:async with-waiting-fetch-child
  "Run (f counter) with the :need child of every job answering :continue (a child waiting on the world); counter
  counts its rounds."
  [f]
  (let [orig ctx/call
        n (atom 0)]
    (set! ctx/call (fn [c slot job args]
                     (if (= :need slot)
                       (do (swap! n inc) (js/Promise.resolve :continue))
                       (orig c slot job args))))
    (try (await (f n))
         (finally (set! ctx/call orig)))))

(deftest a-waiting-fetch-child-makes-the-stair-yield-and-the-fetch-resumes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:self {:pos {:x 0 :y 64 :z 0}}
                        :blocks {"0,61,1" "andesite" "0,62,1" "andesite" "0,63,1" "stone" "0,64,1" "stone" "0,65,1" "stone"
                                 "-3,64,2" "chest"}
                        :containers {"-3,64,2" [{:name "wooden_pickaxe" :count 1}]}
                        :drops {"stone" "cobblestone"}}
                       [own-zone])
              id (core/submit! (:eng s) (list 'jobs.access.stair {:dir :down :heading :south :steps 1}) {})]
          (await (with-waiting-fetch-child
                   (fn ^:async waiting-round [n]
                     (await (run-ticks s 1))
                     (is (= 1 @n) "one fetch round per scheduler round, no busy loop")
                     (is (= 1 (count (listed s))) "the job yielded and is still there"))))
          (await (run-ticks s 60))
          (is (= 1 (get (inv s) "wooden_pickaxe")) (pr-str (core/waiting (:eng s) id))))))))

(deftest a-failed-fetch-clears-the-return-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:self {:pos {:x 0 :y 64 :z 0}}
                        :blocks {"0,61,1" "andesite" "0,62,1" "andesite" "0,63,1" "stone" "0,64,1" "stone" "0,65,1" "stone"
                                 "-3,64,2" "chest"}
                        :containers {"-3,64,2" [{:name "dirt" :count 1}]}
                        :drops {"stone" "cobblestone"}}
                       [own-zone])
              id (core/submit! (:eng s) (list 'jobs.access.stair {:dir :down :heading :south :steps 1}) {})]
          (await (run-ticks s 40))
          (is (nil? (:need/return (core/job-memory (:eng s) id))) (pr-str (core/job-memory (:eng s) id))))))))

(deftest a-fetch-child-that-ends-done-without-solving-the-need-is-not-asked-again-the-stair-ends-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:self {:pos {:x 0 :y 64 :z 0}}
                        :blocks {"0,61,1" "andesite" "0,62,1" "andesite" "0,63,1" "stone" "0,64,1" "stone" "0,65,1" "stone"}}
                       [own-zone])
              n (atom 0)
              orig ctx/call]
          (set! ctx/call (fn [c slot job args]
                           (if (= :need slot)
                             (do (swap! n inc) (js/Promise.resolve (contract/ending :done nil nil)))
                             (orig c slot job args))))
          (try
            (core/submit! (:eng s) (list 'jobs.access.stair {:dir :down :heading :south :steps 1}) {})
            (await (run-ticks s 6))
            (finally (set! ctx/call orig)))
          (is (< @n 5) (str "tool fetches asked " @n)))))))

(deftest stair-without-fetch-waits-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start {:self {:pos {:x 0 :y 64 :z 0}}
                        :blocks {"0,61,1" "andesite" "0,62,1" "andesite" "0,63,1" "stone" "0,64,1" "stone" "0,65,1" "stone"
                                 "-3,64,2" "chest"}
                        :containers {"-3,64,2" [{:name "wooden_pickaxe" :count 1}]}
                        :drops {"stone" "cobblestone"}}
                       [own-zone])
              id (core/submit! (:eng s) (list 'jobs.access.stair {:dir :down :heading :south :steps 1 :ways #{:carried}}) {})]
          (await (run-ticks s 10))
          (is (= :no-tool (:reason (core/waiting (:eng s) id))))
          (is (empty? (inspects s))))))))

;; ------------------------------------------------------------------ the fetch jobs themselves

(deftest a-chest-in-view-just-now-is-read-live
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at []}) [own-zone])
              chests #(mapv stock/cell-of (stock/seen-chests {:primitives (:p s)}))]
          (is (= [[-2 64 3]] (chests)))
          (fake/remove-block! (:p s) [-2 64 3])
          (is (empty? (chests)) "in view just now: the live block says it is gone"))))))

(deftest a-remembered-chest-a-minute-old-is-not-checked-against-the-live-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at []}) [own-zone])
              chests #(mapv stock/cell-of (stock/seen-chests {:primitives (:p s)}))]
          (is (= [[-2 64 3]] (chests)))
          (swap! (:clock s) + 60000)
          (fake/remove-block! (:p s) [-2 64 3])
          (is (= [[-2 64 3]] (chests))))))))

(deftest obtain-with-no-seen-chest-fails-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (world {}) (update :blocks dissoc chest-at) (assoc :inventory [{:name "dirt" :count 2}])) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "dirt" :count 3}) {})
          (await (run-ticks s 5))
          (is (empty? (inspects s)) "no chest seen: no look either")
          (is (= 1 (count (no-source-events s :stopped))))
          (is (re-find #"no seen chest that may hold dirt" (no-source-why s :stopped)))
          (is (re-find #"no recipe makes dirt" (no-source-why s :stopped))))))))

(deftest obtain-short-after-emptying-a-chest-says-the-chest-was-taken-from
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (world {chest-at [{:name "dirt" :count 1}]}) (assoc :inventory [{:name "dirt" :count 2}])) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "dirt" :count 5}) {})
          (await (run-ticks s 30))
          (is (= 3 (get (inv s) "dirt")) "the chest's dirt is carried")
          (is (re-find #"1 chest\(s\) already looked in or taken from hold no more dirt \(3 carried\)" (no-source-why s :stopped)))
          (is (not (re-find #"no seen chest" (no-source-why s :stopped)))))))))

(deftest obtain-books-a-chest-that-would-not-open-as-blocked-not-empty
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (world {chest-at []}) (assoc :inventory [{:name "dirt" :count 2}])) [own-zone])]
          (.override (.-world (:p s)) "inspectContainer" (fn [_ _ _] #js {:status "timeout"}))
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "dirt" :count 3 :ways #{:chest}}) {})
          (await (run-ticks s 5))
          (is (= 2 (count (inspects s))) "a timeout is asked once more, then the chest is given up")
          (is (re-find #"could not be opened.*timeout" (no-source-why s :stopped)))
          (is (re-find #"-2 64 3" (no-source-why s :stopped))))))))

(deftest obtain-opens-a-chest-whose-first-look-timed-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "oak_log" :count 2}]}) [own-zone])
              n (atom 0)]
          (.override (.-world (:p s)) "inspectContainer"
                     (fn [token a impl] (if (= 1 (swap! n inc)) #js {:status "timeout"} (impl token a))))
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_log" :count 2 :ways #{:chest}}) {})
          (await (run-ticks s 12))
          (is (= 2 (get (inv s) "oak_log"))))))))

(defn shut-under-a-block
  "inspectContainer times out while a block sits on the chest, as the game's chest lid does."
  [s]
  (.override (.-world (:p s)) "inspectContainer"
             (fn [token a impl]
               (if (= "dirt" (block-at s -2 65 3)) #js {:status "timeout"} (impl token a)))))

(deftest obtain-digs-the-block-off-a-chest-that-would-not-open-and-opens-it-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (world {chest-at [{:name "oak_log" :count 2}]}) (assoc-in [:blocks "-2,65,3"] "dirt")) [own-zone])]
          (shut-under-a-block s)
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_log" :count 2 :ways #{:chest}}) {})
          (await (run-ticks s 12))
          (is (not= "dirt" (block-at s -2 65 3)) "the block above was dug")
          (is (= 2 (get (inv s) "oak_log")))
          (is (empty? (listed s))))))))

(deftest obtain-leaves-a-block-it-may-not-dig-and-books-the-chest-blocked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [roof {:name "roof" :min [-2 65 3] :max [-2 65 3] :owner "Miles"}
              s (start (-> (world {chest-at [{:name "oak_log" :count 2}]}) (assoc-in [:blocks "-2,65,3"] "dirt")) [own-zone roof])]
          (shut-under-a-block s)
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_log" :count 2 :ways #{:chest}}) {})
          (await (run-ticks s 12))
          (is (= "dirt" (block-at s -2 65 3)))
          (is (re-find #"chest at -2 64 3 could not be opened" (no-source-why s :stopped))))))))

(deftest obtain-cobblestone-wait-names-the-missing-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (world {}) (update :blocks dissoc chest-at)) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "cobblestone" :count 3 :explore false}) {})
          (await (run-ticks s 5))
          (is (re-find #"pickaxe" (no-source-why s :stopped))))))))

(deftest obtain-craft-wait-names-the-short-ingredient
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (world {}) (update :blocks dissoc chest-at) (assoc :inventory [{:name "stick" :count 1}])) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "wooden_pickaxe" :ways #{:craft :smelt}}) {})
          (await (run-ticks s 5))
          (is (re-find #"needs 2 stick, have 1" (no-source-why s :waiting))))))))

(deftest obtain-walks-to-a-remembered-chest-however-far
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far-at "40,64,3"
              s (start (-> (world {far-at [{:name "stone_pickaxe" :count 1}]}) (update :blocks #(-> % (dissoc chest-at) (assoc far-at "chest")))) [own-zone])]
          (mem/write! (:store (:eng s)) stock/stock-kind {:pos [40 64 3] :items {"stone_pickaxe" 1}} stock/stock-policy)
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "stone_pickaxe"}) {})
          (await (run-ticks s 120))
          (is (= 1 (get (inv s) "stone_pickaxe"))))))))

(deftest obtain-any-of-takes-the-first-name-the-chest-holds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "stone_pickaxe" :count 1} {:name "iron_pickaxe" :count 1}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:any-of ["wooden_pickaxe" "stone_pickaxe" "iron_pickaxe"]}) {})
          (await (run-ticks s 30))
          (is (empty? (listed s)))
          (is (= {"stone_pickaxe" 1} (select-keys (inv s) ["stone_pickaxe" "iron_pickaxe"]))))))))

(deftest obtain-looks-around-for-a-chest-behind-the-body-before-it-crafts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (world {}) (update :blocks dissoc chest-at) (assoc-in [:blocks "-2,64,-3"] "chest")
                           (assoc :containers {"-2,64,-3" [{:name "oak_planks" :count 8}]}
                                  :inventory [{:name "oak_log" :count 2}]))
                       [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_planks" :count 4}) {})
          (await (run-ticks s 30))
          (is (seq (calls s "look")))
          (is (empty? (calls s "craft")) "the chest behind the body is used, nothing is crafted")
          (is (= 4 (get (inv s) "oak_planks")))
          (is (= 2 (get (inv s) "oak_log"))))))))

(deftest obtain-passes-its-for-plan-to-the-craft-children
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [orig ctx/call
              seen (atom [])
              s (start (-> (world {}) (update :blocks dissoc chest-at) (assoc :inventory [{:name "oak_log" :count 2}])) [own-zone])]
          (set! ctx/call (fn [c slot j args]
                           (when (= 'jobs.items.craft j) (swap! seen conj args))
                           (orig c slot j args)))
          (try
            (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_planks" :count 4 :for-plan "house"}) {})
            (await (run-ticks s 30))
            (finally (set! ctx/call orig)))
          (is (seq @seen))
          (is (every? #(= "house" (:for-plan %)) @seen) (pr-str @seen)))))))

(deftest get-tool-for-a-hand-block-is-not-needed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.get-tool {:block "dirt"}) {})
          (await (run-ticks s 5))
          (is (empty? (listed s)))
          (is (empty? (inspects s))))))))

(deftest get-tool-fails-no-source-when-nothing-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (world {}) :blocks dissoc chest-at) [own-zone])
              id (core/submit! (:eng s) (list 'jobs.items.get-tool {:block "stone" :explore false}) {})]
          (await (run-ticks s 5))
          (is (= 1 (count (no-source-events s :stopped))))
          (is (empty? (inspects s))))))))

(deftest stock-is-remembered-after-a-withdraw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "dirt" :count 5}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.storage.withdraw {:chest [-2 64 3] :items {"dirt" 2}}) {})
          (await (run-ticks s 20))
          (is (= {"dirt" 3} (get (stock/stock-of (mem/view (:store (:eng s)))) [-2 64 3]))))))))

;; ------------------------------------------------------------------ the craft source

(defn bare
  "A world with no chest and no stone, the body carrying inv."
  [inv]
  (-> (world {}) (update :blocks dissoc chest-at stone-at) (assoc :inventory inv)))

(defn ^:async get-tool! [s args n]
  (let [id (core/submit! (:eng s) (list 'jobs.items.get-tool args) {})]
    (await (run-ticks s n))
    id))

(defn ^:async one-tick! [s spec]
  (core/submit! (:eng s) spec {})
  (await (run-ticks s 1)))

(defn tables [s]
  (filterv (fn [[_ n]] (= "crafting_table" n)) (:blocks @(fake/state (:p s)))))

(deftest get-tool-passes-its-for-plan-to-the-craft-children
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [orig ctx/call
              seen (atom [])
              s (start (bare [{:name "oak_log" :count 12}]) [own-zone])]
          (set! ctx/call (fn [c slot j args]
                           (when (= 'jobs.items.craft j) (swap! seen conj args))
                           (orig c slot j args)))
          (try
            (await (get-tool! s {:kind "pickaxe" :for-plan "house"} 60))
            (finally (set! ctx/call orig)))
          (is (seq @seen) "not rejected as bad-args")
          (is (every? #(= "house" (:for-plan %)) @seen) (pr-str @seen)))))))

(def own-house
  {"house" {:id "house" :metadata {:by "Fake"}
            :parts [{:id "all" :cells (vec (for [x (range -3 4) y [64 65] z (range -3 4)] [x y z])) :want {:block "stone"}}]}})

(deftest get-tool-for-a-plan-digs-up-the-table-it-put-in-the-plans-own-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare [{:name "oak_log" :count 12}]) [own-zone] sight-opts own-house)]
          (core/submit! (:eng s) (list 'jobs.items.get-tool {:kind "pickaxe" :for-plan "house"}) {})
          (await (run-ticks s 80))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (empty? (tables s)) "the table is not left in a cell the build wants"))))))

(deftest get-tool-crafts-a-wooden-pickaxe-from-logs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare [{:name "oak_log" :count 12}]) [own-zone])]
          (await (get-tool! s {:kind "pickaxe"} 60))
          (is (empty? (listed s)))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (= 1 (count (tables s))) "one table was put down")
          (is (nil? (get (inv s) "crafting_table"))))))))

(deftest dig-with-fetch-and-only-logs-crafts-the-pickaxe-then-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (world {}) (update :blocks dissoc chest-at) (assoc :inventory [{:name "oak_log" :count 12}])) [own-zone])]
          (core/submit! (:eng s) (dig-spec {}) {})
          (await (run-ticks s 80))
          (is (empty? (listed s)) "the dig ended")
          (is (= "air" (block-at s 2 64 3)))
          (is (= 1 (get (inv s) "wooden_pickaxe"))))))))

(deftest get-tool-crafts-from-sticks-and-planks-and-uses-a-seen-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (bare [{:name "oak_planks" :count 3} {:name "stick" :count 2}])
                           (assoc-in [:blocks "1,64,2"] "crafting_table")) [own-zone])]
          (await (get-tool! s {:item "wooden_pickaxe"} 60))
          (is (empty? (listed s)))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (= 1 (count (tables s))) "the seen table, no second one"))))))

(deftest get-tool-from-sticks-and-planks-without-a-table-puts-one-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare [{:name "oak_planks" :count 7} {:name "stick" :count 2}]) [own-zone])]
          (await (get-tool! s {:kind "pickaxe"} 60))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (= 1 (count (tables s)))))))))

(deftest get-tool-looks-around-for-a-chest-behind-the-body-when-nothing-else-is-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (bare []) (assoc-in [:blocks "-2,64,-3"] "chest")
                           (assoc :containers {"-2,64,-3" [{:name "wooden_pickaxe" :count 1}]}))
                       [own-zone])
              id (await (get-tool! s {:kind "pickaxe"} 30))]
          (is (nil? (core/waiting (:eng s) id)) "the check did not wait before a first look")
          (is (= 1 (get (inv s) "wooden_pickaxe")) "the pickaxe came out of the chest behind the body"))))))

(deftest get-tool-with-nothing-usable-fails-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare [{:name "dirt" :count 5}]) [own-zone])
              id (await (get-tool! s {:kind "pickaxe" :explore false} 6))]
          (is (= 1 (count (no-source-events s :stopped))))
          (is (empty? (calls s "craft"))))))))

(deftest a-stone-pickaxe-without-cobblestone-fails-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare [{:name "wooden_pickaxe" :count 1} {:name "stick" :count 2} {:name "oak_log" :count 3}]) [own-zone])
              id (await (get-tool! s {:item "stone_pickaxe" :explore false} 6))]
          (is (= 1 (count (no-source-events s :stopped)))))))))

(deftest a-stone-pickaxe-whose-gather-finds-no-stone-ends-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (world {}) (update :blocks dissoc chest-at stone-at)
                           (assoc :inventory [{:name "wooden_pickaxe" :count 1} {:name "stick" :count 2} {:name "oak_log" :count 3}]))
                       [own-zone])
              n (atom 0)
              orig ctx/call]
          (set! ctx/call (fn [c slot job args]
                           (if (= :gather-cobblestone slot)
                             (do (swap! n inc) (js/Promise.resolve {:kind :nothing-possible :cause :no-stone-found}))
                             (orig c slot job args))))
          (try
            (let [id (await (get-tool! s {:item "stone_pickaxe"} 40))
                  st (last (events-of s :stopped))]
              (is (= :no-source (:reason st)) (pr-str st (core/waiting (:eng s) id)))
              (is (= 3 @n)))
            (finally (set! ctx/call orig))))))))

(deftest crafting-is-off-when-how-leaves-it-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare [{:name "oak_log" :count 12}]) [own-zone])
              id (await (get-tool! s {:kind "pickaxe" :ways #{:chest}} 6))]
          (is (= 1 (count (no-source-events s :stopped))))
          (is (empty? (calls s "craft"))))))))

(deftest a-craft-that-does-nothing-three-times-ends-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare [{:name "oak_log" :count 12}]) [own-zone])]
          (swap! (fake/state (:p s)) assoc-in [:recipes "oak_planks"] {:count 4 :needs {"oak_log" 1000}})
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_planks" :count 4}) {})
          (await (run-ticks s 30))
          (is (empty? (listed s)))
          (is (= :no-source (:reason (first (events-of s :stopped))))))))))

(defn far-table-world [inv]
  (-> (bare inv)
      (assoc-in [:blocks "1,64,8"] "crafting_table")
      (assoc :unreachable (vec (for [x (range -2 5) y (range 62 68) z (range 5 12)] (str x "," y "," z))))))

(deftest get-tool-puts-down-a-carried-table-when-the-seen-one-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (far-table-world [{:name "oak_planks" :count 3} {:name "stick" :count 2} {:name "crafting_table" :count 1}]) [own-zone])]
          (await (get-tool! s {:kind "pickaxe"} 80))
          (is (empty? (listed s)))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (nil? (get (inv s) "crafting_table")) "the carried table was put down"))))))

(def pocket
  "Stone round the body's two cells (x -1..1, z -1..1, y 64..65), over its head and under it: no free cell beside it."
  (into {"0,66,0" "stone"}
        (for [x [-1 0 1] z [-1 0 1] y [63 64 65] :when (or (= 63 y) (not (and (zero? x) (zero? z))))]
          [(str x "," y "," z) "stone"])))

(deftest get-tool-in-a-stone-pocket-digs-a-table-spot-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (bare [{:name "oak_planks" :count 3} {:name "stick" :count 2} {:name "crafting_table" :count 1}])
                               :blocks merge pocket) [own-zone])]
          (await (get-tool! s {:kind "pickaxe"} 80))
          (is (empty? (listed s)))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (nil? (get (inv s) "crafting_table")) "a stone cell beside was dug by hand and the table put in it")
          (is (= 1 (count (tables s)))))))))

(def stair-pocket
  "Stone round the body (x -1..1, z -1..1, y 63..66) but the 1-wide stair cells behind it, one level up: air at (-1,65..66,0)."
  (into {}
        (for [x [-1 0 1] z [-1 0 1] y [63 64 65 66]
              :when (not (or (and (zero? x) (zero? z) (#{64 65} y))
                             (and (= -1 x) (zero? z) (#{65 66} y))))]
          [(str x "," y "," z) "stone"])))

(deftest get-tool-in-a-stair-puts-the-table-in-the-cell-behind-one-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (bare [{:name "oak_planks" :count 3} {:name "stick" :count 2} {:name "crafting_table" :count 1}])
                               :blocks merge stair-pocket) [own-zone])]
          (await (get-tool! s {:kind "pickaxe"} 80))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (= [[-1 65 0]] (map first (tables s))) "the table went in the stair cell behind"))))))

(deftest get-tool-puts-the-table-outside-the-bodys-box
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc-in (bare [{:name "oak_planks" :count 3} {:name "stick" :count 2} {:name "crafting_table" :count 1}])
                                 [:self :pos] {:x 0.71 :y 64 :z 0.5}) [own-zone])]
          (await (get-tool! s {:kind "pickaxe"} 80))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (every? (fn [[[x]]] (not= 1 x)) (tables s))
              "no table in column x=1, which the body's box (0.41..1.01) touches"))))))

(deftest get-tool-makes-a-table-when-the-seen-one-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (far-table-world [{:name "oak_planks" :count 7} {:name "stick" :count 2}]) [own-zone])]
          (await (get-tool! s {:kind "pickaxe"} 80))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (= 2 (count (tables s)))))))))

(deftest get-tool-stops-table-unreachable-when-it-cannot-put-one-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (far-table-world [{:name "oak_planks" :count 3} {:name "stick" :count 2}]) [own-zone])
              id (await (get-tool! s {:kind "pickaxe"} 80))
              reason (or (:reason (core/waiting (:eng s) id)) (:reason (first (events-of s :stopped))))]
          (is (= :table-unreachable reason))
          (is (nil? (get (inv s) "wooden_pickaxe"))))))))

(deftest a-later-call-uses-the-table-once-it-is-reachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (far-table-world [{:name "oak_planks" :count 3} {:name "stick" :count 2}]) [own-zone])]
          (await (one-tick! s (list 'jobs.items.get-tool {:kind "pickaxe"})))
          (is (nil? (get (inv s) "wooden_pickaxe")))
          (swap! (fake/state (:p s)) assoc :unreachable #{})
          (await (one-tick! s (list 'jobs.items.get-tool {:kind "pickaxe"})))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (= 1 (count (tables s)))))))))

(deftest fertilize-fetches-bone-meal-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc-in (world {chest-at [{:name "bone_meal" :count 4}]}) [:blocks "3,63,1"] "grass_block") [own-zone])]
          (core/submit! (:eng s) (list 'jobs.farm.fertilize {:grass true :max 1}) {})
          (await (run-ticks s 40))
          (is (empty? (listed s)) "the job ended")
          (is (= 1 (count (calls s "useOn"))))
          (is (= 1 (count (filter #(= :need (:slot %)) (events-of s :child_started)))) "the bone meal fetch is a child the agent sees")
          (is (= [:done] (mapv #(get-in % [:ending :kind]) (filter #(= :need (:slot %)) (events-of s :child_ended))))))))))

(deftest fertilize-fetches-bone-meal-from-an-own-seen-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc-in (world {chest-at [{:name "bone_meal" :count 4}]}) [:blocks "3,63,1"] "grass_block") [own-zone])]
          (core/submit! (:eng s) (list 'jobs.farm.fertilize {:grass true :max 1}) {})
          (await (run-ticks s 40))
          (is (empty? (listed s)) "the job ended")
          (is (= 1 (count (calls s "useOn")))))))))

(def sapling-spot {:x 4 :y 64 :z 3})

(deftest plant-sapling-fetches-a-sapling-from-an-own-seen-chest-then-plants
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "oak_sapling" :count 2}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.forestry.plant-sapling {:at sapling-spot :species "oak"}) {})
          (await (run-ticks s 40))
          (is (empty? (listed s)) "the job ended")
          (is (= "oak_sapling" (block-at s 4 64 3))))))))

(def oak-plan {"forest" {:id "forest" :parts [{:id "a" :cells [[3 64 0]] :want {:tree "oak"}}]}})

(defn forest-world [] (assoc-in (world {chest-at [{:name "oak_sapling" :count 2}]}) [:blocks "3,63,0"] "dirt"))

(deftest forestry-maintain-fetches-the-sapling-for-a-bare-planned-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (forest-world) [own-zone] sight-opts oak-plan)]
          (core/submit! (:eng s) (list 'jobs.forestry.maintain {:plan "forest"}) {})
          (await (run-ticks s 60))
          (is (= "oak_sapling" (block-at s 3 64 0)))
          (is (= 1 (count (filter #(= :plant (:slot %)) (events-of s :child_started))))))))))

(deftest forestry-maintain-without-fetch-waits-no-sapling-and-touches-no-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (forest-world) [own-zone] sight-opts oak-plan)]
          (core/submit! (:eng s) (list 'jobs.forestry.maintain {:plan "forest" :ways #{:carried}}) {})
          (await (run-ticks s 10))
          (is (= 1 (count (listed s))) "still queued")
          (is (empty? (transfers s)))
          (is (= "air" (block-at s 3 64 0)) "nothing planted")
          (is (= [:no-sapling] (distinct (mapv :reason (events-of s :waiting))))))))))

(deftest plant-sapling-without-fetch-waits-no-sapling-and-touches-no-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "oak_sapling" :count 2}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.forestry.plant-sapling {:at sapling-spot :species "oak" :ways #{:carried}}) {})
          (await (run-ticks s 10))
          (is (= 1 (count (listed s))) "still queued")
          (is (empty? (transfers s)))
          (is (= [:no-sapling] (mapv :reason (events-of s :waiting)))))))))

(deftest plant-sapling-with-nothing-to-fetch-ends-need-with-the-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.forestry.plant-sapling {:at sapling-spot :species "oak"}) {})
          (await (run-ticks s 20))
          (is (empty? (listed s)) "the job ended")
          (is (= :need (:reason (last (events-of s :stopped))))))))))

(deftest plant-sapling-fetches-a-mangrove-propagule-for-the-species
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "mangrove_propagule" :count 2}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.forestry.plant-sapling {:at sapling-spot :species "mangrove"}) {})
          (await (run-ticks s 40))
          (is (empty? (listed s)) "the job ended")
          (is (= "mangrove_propagule" (block-at s 4 64 3))))))))

(defn plan-ops
  "The step ops craft-plan makes for a pickaxe from carried logs, with mem as the job memory and no table in sight."
  [mem]
  (let [s (start (-> (world {}) (assoc :inventory [{:name "oak_planks" :count 12} {:name "stick" :count 4}])) [own-zone])]
    (with-redefs [ctx/mem (constantly mem)
                  ctx/frozen-call (constantly nil)
                  crafting-table/nearest (constantly nil)]
      (mapv :op (:steps (plan/craft-plan {:primitives (:p s)} ["wooden_pickaxe"] 1))))))

(deftest obtain-plan-uses-the-remembered-table
  (is (= [:craft] (plan-ops {:table [1 64 0]}))))

(deftest obtain-plan-crafts-a-new-table-once-the-table-is-unreachable
  (is (= [:craft :craft] (plan-ops {:table [1 64 0] :craft {:table-unreachable true}}))))

(deftest get-tool-for-an-unknown-block-is-bad-args-not-a-hand-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (await (get-tool! s {:block "not_a_block"} 5))
          (is (= [:bad-args] (stopped-reasons s)))
          (is (empty? (inspects s))))))))

;; ------------------------------------------------------------------ the gather source

(def tree-blocks
  (merge (into {} (for [y (range 64 68)] [(str "0," y ",4") "oak_log"]))
         {"0,68,4" "oak_leaves" "1,67,4" "oak_leaves"}))

(deftest obtain-gathers-logs-then-crafts-the-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (bare []) :blocks merge tree-blocks {"1,64,2" "crafting_table"}) [own-zone])]
          (await (get-tool! s {:kind "pickaxe"} 120))
          (is (empty? (listed s)))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (seq (calls s "dig")) "a log was felled"))))))

(deftest an-obtain-whose-pickaxe-child-is-cooking-yields-instead-of-failing-the-mine
  ;; the pickaxe is smelting (a copper pickaxe's ingots): the child yields :continue, the mine source is not written off as no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {}) [own-zone])
              id (core/submit! (:eng s) (list 'jobs.items.obtain {:item "cobblestone" :count 3}) {})
              call ctx/call]
          (set! ctx/call (fn [c slot job args] (if (= :pickaxe slot) (js/Promise.resolve :continue) (call c slot job args))))
          (try (await (run-ticks s 20))
               (finally (set! ctx/call call)))
          (is (empty? (events-of s :stopped)) (pr-str (map :why (events-of s :stopped))))
          (is (seq (listed s)) "the obtain is still running"))))))

(def small-explore
  "A search that walks a few short legs: the whole default range of a search that finds nothing is minutes of walking."
  {:max-distance 12 :max-legs 2 :spend {:job-time 300}})

(def small-explore-settings
  {:jobs.items.obtain.base/explore small-explore :jobs.items.obtain.base/explore-far small-explore})

(deftest obtain-oak-planks-fells-no-birch
  ;; a birch log makes no oak planks: felling birch for them gathered hundreds of logs live
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [birch (into {} (for [y (range 64 68)] [(str "4," y ",9") "birch_log"]))
              s (start (update (bare []) :blocks merge birch {"4,68,9" "birch_leaves"}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_planks" :count 4}) {})
          (await (settings/with-settings small-explore-settings #(run-ticks s 60)))
          (is (empty? (calls s "dig")))
          (is (nil? (get (inv s) "birch_log")))
          (is (some #(re-find #"gather what a craft lacks: oak_log" (str (:why %))) (concat (events-of s :waiting) (events-of s :stopped)))
              (pr-str (map :why (concat (events-of s :waiting) (events-of s :stopped))))))))))

(deftest obtain-wooden-pickaxe-fells-a-birch-any-planks-do
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [birch (into {} (for [y (range 64 68)] [(str "4," y ",9") "birch_log"]))
              s (start (update (bare []) :blocks merge birch {"4,68,9" "birch_leaves"}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "wooden_pickaxe"}) {})
          (await (run-ticks s 60))
          (is (pos? (get (inv s) "birch_log" 0)) (pr-str (inv s))))))))

(deftest obtain-gathers-the-cobblestone-a-craft-lacks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (world {}) :blocks merge (into {} (for [x [0 1] z [5 6]] [(str x ",64," z) "stone"]))) :inventory [{:name "wooden_pickaxe" :count 1} {:name "oak_planks" :count 4} {:name "stick" :count 4} {:name "crafting_table" :count 1}]) [])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "stone_pickaxe"}) {})
          (await (run-ticks s 80))
          (is (empty? (listed s)))
          (is (= 1 (get (inv s) "stone_pickaxe"))))))))

(deftest obtain-stone-pickaxe-from-planks-with-no-pickaxe-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (world {}) :blocks merge (into {} (for [x [0 1] z [5 6]] [(str x ",64," z) "stone"]))) :inventory [{:name "oak_planks" :count 12} {:name "crafting_table" :count 1}]) [])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "stone_pickaxe"}) {})
          (await (run-ticks s 160))
          (is (empty? (listed s)))
          (is (= 1 (get (inv s) "stone_pickaxe"))))))))

(deftest obtain-gather-fails-no-source-when-nothing-is-seen-to-gather
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare []) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "wooden_pickaxe" :explore false}) {})
          (await (run-ticks s 6))
          (is (= 1 (count (no-source-events s :stopped))))
          (is (re-find #"nothing seen to gather" (no-source-why s :stopped)))
          (is (re-find #"pass :explore true to search for it" (no-source-why s :stopped))))))))

(deftest obtain-gather-is-off-when-how-leaves-it-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (bare []) :blocks merge tree-blocks) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "wooden_pickaxe" :ways #{:craft :smelt :chest}}) {})
          (await (run-ticks s 10))
          (is (empty? (calls s "dig"))))))))

(def leaf-blocks
  {"0,66,4" "oak_leaves" "1,66,4" "oak_leaves" "0,65,4" "oak_leaves" "1,65,4" "oak_leaves"})

(deftest obtain-gathers-a-sapling-from-leaves-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (bare []) :blocks merge leaf-blocks) :drops {"oak_leaves" "oak_sapling"}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_sapling"}) {})
          (await (run-ticks s 80))
          (is (empty? (listed s)))
          (is (pos? (get (inv s) "oak_sapling" 0)))
          (is (seq (calls s "dig")) "a leaf was broken"))))))

(deftest plant-sapling-without-a-chest-breaks-seen-leaves-for-the-sapling-then-plants
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ground {"0,63,4" "andesite" "1,63,4" "andesite"}
              s (start (assoc (update (bare []) :blocks merge ground leaf-blocks) :drops {"oak_leaves" "oak_sapling"}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.forestry.plant-sapling {:at sapling-spot :species "oak"}) {})
          (await (run-ticks s 120))
          (is (empty? (listed s)) "the job ended")
          (is (seq (calls s "dig")) "a leaf was broken")
          (is (= "oak_sapling" (block-at s 4 64 3))))))))

(deftest obtain-sapling-fails-no-source-without-leaves-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare []) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_sapling"}) {})
          (await (run-ticks s 6))
          (is (= 1 (count (no-source-events s :stopped))))
          (is (empty? (calls s "dig"))))))))

(deftest obtain-sapling-gather-is-off-when-how-leaves-it-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (bare []) :blocks merge leaf-blocks) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_sapling" :ways #{:chest :craft :smelt}}) {})
          (await (run-ticks s 10))
          (is (empty? (calls s "dig"))))))))

(deftest obtain-gather-needs-every-raw-item-seen-before-it-starts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (bare []) :blocks merge tree-blocks) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "stone_pickaxe" :explore false}) {})
          (await (run-ticks s 10))
          (is (= 1 (count (no-source-events s :stopped))))
          (is (empty? (calls s "dig")) "no tree felled for a chain that then lacks stone")
          (let [why (no-source-why s :stopped)]
            (is (re-find #"cobblestone \(from stone\)" why))
            (is (not (re-find #"log" why)) "the log that is seen is not named")))))))

(deftest obtain-gathers-coal-from-a-deepslate-ore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (world {}) :blocks merge {"1,64,5" "deepslate_coal_ore" "2,64,5" "deepslate_coal_ore"})
                              :drops {"deepslate_coal_ore" "coal"}
                              :inventory [{:name "wooden_pickaxe" :count 1} {:name "stick" :count 4}]) [])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "torch"}) {})
          (await (run-ticks s 80))
          (is (empty? (listed s)))
          (is (pos? (get (inv s) "torch" 0))))))))

(deftest obtain-cobblestone-with-only-logs-crafts-a-wooden-pickaxe-and-mines-seen-stone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (world {}) :blocks merge (into {} (for [x [0 1] z [5 6]] [(str x ",64," z) "stone"])))
                              :inventory [{:name "oak_log" :count 4}]) [])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "cobblestone" :count 3}) {})
          (await (run-ticks s 160))
          (is (empty? (listed s)) (pr-str (map :why (events-of s :waiting))))
          (is (= 1 (get (inv s) "wooden_pickaxe")) (pr-str (inv s)))
          (is (<= 3 (get (inv s) "cobblestone" 0))))))))

(deftest obtain-cobblestone-unseen-with-explore-and-only-logs-crafts-the-pickaxe-then-runs-the-mine
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (bare [{:name "oak_log" :count 4}]) :floor [-30 -30 40 30]) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "cobblestone" :count 3 :explore true}) {})
          (await (run-ticks s 60))
          (is (= 1 (get (inv s) "wooden_pickaxe")) (pr-str (inv s)))
          (is (empty? (events-of s :obtain.exploring)) "no surface search")
          (is (some #(re-find #"^mine\." (name (:kind %))) @(:seen s)) "the mine child ran"))))))

(deftest obtain-digs-dirt-from-seen-grass-when-nothing-else-has-it
  ;; a pillar fetch (any-of dirt/cobblestone) on grassland: the dirt under seen grass is dug by hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (world {}) :blocks merge {"1,63,5" "grass_block" "2,63,5" "grass_block" "3,63,5" "grass_block"
                                               "1,62,5" "stone" "2,62,5" "stone" "3,62,5" "stone"})
                              :drops {"grass_block" "dirt"}
                              :inventory []) [])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:any-of ["dirt" "cobblestone"] :count 2}) {})
          (await (run-ticks s 80))
          (is (empty? (events-of s :waiting)))
          (is (<= 2 (get (inv s) "dirt" 0))))))))

(deftest obtain-gather-that-brings-in-nothing-stops-no-source-with-tried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (bare []) :blocks merge tree-blocks) :unreachable (vec (keys tree-blocks))) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "wooden_pickaxe" :explore false}) {})
          (await (run-ticks s 200))
          (is (empty? (listed s)))
          (is (nil? (get (inv s) "wooden_pickaxe")))
          (let [st (last (events-of s :stopped))]
            (is (= :no-source (:reason st)))))))))

(deftest obtain-explores-when-the-trees-it-sees-cannot-be-reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (bare []) :blocks merge tree-blocks) :unreachable (vec (keys tree-blocks))) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "wooden_pickaxe" :explore true}) {})
          (await (settings/with-settings small-explore-settings #(run-ticks s 120)))
          (is (seq (events-of s :obtain.exploring)) "it walked to look for another tree")
          (is (<= (count (events-of s :obtain.exploring)) 6) "bounded by the explore rounds")
          (is (= :no-source (:reason (last (events-of s :stopped))))))))))

(deftest obtain-sapling-leaf-breaking-is-bounded
  (let [need (table/gather-need {"oak_sapling" 1})]
    (is (= (base/sapling-leaf-limit) (:jobs.gather.get-seeds/dry-digs (:settings (:args need)))) "digs per child run")
    (is (= 1 (:max-runs need)) "one fruitless run ends the source")
    (is (nil? (:max-runs (table/gather-need {"cobblestone" 1}))) "other sources use the default bound")))

(deftest each-gather-source-has-its-own-child-slot
  ;; a child slot's memory outlives its job while it runs: mine started over harvest-wood's :phase ended :count with nothing and a half tunnel
  (let [slot #(seen/gather-slot (table/gather-need %))]
    (is (not= (slot {"oak_log" 2}) (slot {"cobblestone" 1})))
    (is (not= (slot {"cobblestone" 1}) (slot {"coal" 1})))
    (is (= (slot {"cobblestone" 1}) (slot {"cobblestone" 3})) "the same source keeps its slot between rounds")))

(deftest a-declined-child-is-not-a-fruitless-run
  (let [sapling (table/gather-need {"oak_sapling" 1})]
    (is (= 1 (gather/fruitless-limit sapling {:kind :nothing-possible :cause :no-leaves :data {:broken 0}})))
    (is (= (child/max-fruitless) (gather/fruitless-limit sapling {:kind :condition-failed :cause :no-source})) "a child that could not start broke no leaves")))

(deftest gather-mines-fetch-a-pickaxe-without-gathering
  (is (= table/mine-fetch (get-in (table/gather-need {"cobblestone" 1}) [:args :ways])))
  (is (= table/mine-fetch (get-in (table/gather-need {"coal" 1}) [:args :ways]))))

(deftest gather-needs-mine-the-block-that-drops-the-material
  (is (= #{"stone"} (:seen (table/gather-need {"cobblestone" 1}))))
  (let [n (table/gather-need {"cobbled_deepslate" 2})]
    (is (= #{"deepslate"} (:seen n)))
    (is (= {:block "deepslate" :item "cobbled_deepslate" :count 2 :ways table/mine-fetch :radius (base/seen-radius)} (:args n)))))

(deftest the-stone-materials-are-those-of-the-bodys-version
  (is (= #{"deepslate"} (table/material-blocks "26.1" "cobbled_deepslate")))
  (is (nil? (table/material-blocks "1.16.5" "cobbled_deepslate")) "no deepslate in that version")
  (is (nil? (:seen (table/gather-need {"cobbled_deepslate" 1} "1.16.5")))))

(deftest obtain-mines-seen-deepslate-for-a-stone-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (bare [{:name "wooden_pickaxe" :count 1} {:name "oak_planks" :count 4} {:name "stick" :count 4} {:name "crafting_table" :count 1}])
                                    :blocks merge (into {} (for [x [0 1] z [5 6]] [(str x ",64," z) "deepslate"])))
                              :drops {"deepslate" "cobbled_deepslate"}
                              :recipes {"stone_pickaxe" {:count 1 :needs {"cobbled_deepslate" 3 "stick" 2} :table true}}) [])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "stone_pickaxe"}) {})
          (await (run-ticks s 80))
          (is (= 1 (get (inv s) "stone_pickaxe"))))))))

(deftest obtain-sapling-does-not-break-leaves-in-another-owners-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [leaves {"-2,65,3" "oak_leaves" "-2,66,3" "oak_leaves"}
              s (start (assoc (update (bare []) :blocks merge leaves) :drops {"oak_leaves" "oak_sapling"}) [own-zone foreign-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_sapling"}) {})
          (await (settings/with-settings small-explore-settings #(run-ticks s 60)))
          (is (empty? (calls s "dig")))
          (is (nil? (get (inv s) "oak_sapling"))))))))

(deftest obtain-gather-passes-harvest-wood-the-logs-to-carry-in-total
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen-args (atom [])
              s (start (update (bare [{:name "oak_log" :count 1}]) :blocks merge tree-blocks {"1,64,2" "crafting_table"}) [own-zone])
              eng (assoc-in (:eng s) [:jobs 'jobs.forestry.harvest-wood :round]
                            (fn ^:async f [c] (swap! seen-args conj (:args c)) :done))]
          (core/submit! eng (list 'jobs.items.obtain {:item "wooden_pickaxe"}) {})
          (await (run-ticks (assoc s :eng eng) 12))
          (is (= 2 (:count (first @seen-args))) "one log carried, one more needed: harvest-wood's :count is logs carried in all"))))))

(deftest get-tool-crafts-from-logs-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare [{:name "oak_log" :count 12}]) [own-zone])]
          (await (one-tick! s (list 'jobs.items.get-tool {:kind "pickaxe"})))
          (is (empty? (listed s)))
          (is (= 1 (get (inv s) "wooden_pickaxe"))))))))

(deftest get-tool-takes-from-a-chest-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (await (one-tick! s (list 'jobs.items.get-tool {:kind "pickaxe"})))
          (is (empty? (listed s)))
          (is (= 1 (get (inv s) "wooden_pickaxe"))))))))

;; ------------------------------------------------------------------ exploring for what is not seen

(defn ^:async with-explore-stub
  "Run (f s calls) with the :explore child stubbed: each call is recorded in the calls atom and ends at once with
  result res; (on-call s) runs first (e.g. puts trees in view)."
  [s res on-call f]
  (let [orig ctx/call
        seen (atom [])
        ending (if (= :found (:reason res)) {:kind :done :data res} {:kind :nothing-possible :cause (:why res) :data res})]
    (set! ctx/call (fn [c slot job args]
                     (if (= :explore slot)
                       (do (swap! seen conj {:job job :args args}) (on-call s) (js/Promise.resolve ending))
                       (orig c slot job args))))
    (try
      (await (f seen))
      (finally (set! ctx/call orig)))))

(deftest get-tool-with-nothing-seen-explores-for-logs-instead-of-waiting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare []) [own-zone])]
          (await (with-explore-stub s {:reason :not-found :why :distance :found []} (fn [_])
                   (fn ^:async body [seen]
                     (let [id (await (get-tool! s {:item "stone_pickaxe"} 20))
                           call (first @seen)]
                       (is (= 'jobs.explore.search (:job call)))
                       (is (some #{"oak_log"} (:target (:args call))) (pr-str call))
                       (is (not-any? #(= :no-source (:reason %)) (events-of s :waiting)) "no wait: the explore child solves it")
                       (let [ev (events-of s :obtain.exploring)]
                         (is (= 6 (count ev)) "one notice per search, one search per explore round")
                         (is (= :notice (:attention (first ev))) (pr-str ev))
                         (is (some #{"oak_log"} (:for (first ev))))
                         (is (= 64 (:max-distance (first ev))))
                         (is (vector? (:from (first ev)))))
                       (is (nil? (core/waiting (:eng s) id))))))))))))

(deftest get-tool-whose-explore-finds-nothing-ends-no-source-saying-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare []) [own-zone])]
          (await (with-explore-stub s {:reason :not-found :why :distance :found []} (fn [_])
                   (fn ^:async body [seen]
                     (await (get-tool! s {:item "stone_pickaxe"} 30))
                     (let [st (last (events-of s :stopped))]
                       (is (= 6 (count @seen)) "the explore rounds of one material")
                       (is (empty? (listed s)))
                       (is (= :no-source (:reason st)) (pr-str st))
                       (is (re-find #"searched" (pr-str (:tried st))) (pr-str st)))))))))))

(deftest a-gather-round-that-brought-a-log-starts-the-fruitless-count-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [trees {"1,68,4" "oak_leaves" "2,67,4" "oak_leaves" "1,64,4" "oak_log" "1,65,4" "oak_log" "1,66,4" "oak_log" "1,67,4" "oak_log"}
              s (start (update (world {}) :blocks merge trees) [own-zone])
              orig ctx/call
              calls (atom 0)]
          (set! ctx/call (fn [c slot job args]
                           (if-not (= 'jobs.forestry.harvest-wood job)
                             (orig c slot job args)
                             (do (when (= 3 (swap! calls inc)) (swap! (fake/state (:p s)) fake/give "oak_log" 1))
                                 (js/Promise.resolve {:kind :done})))))
          (try
            (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_log" :count 20 :explore false}) {})
            (await (run-ticks s 40))
            (finally (set! ctx/call orig)))
          (is (= 6 @calls) "two empty rounds, one with a log, then three empty ones end it"))))))

(def tree-far
  "A tree 24 blocks east of the body, past the 16 blocks within which a seen log counts."
  (merge (into {} (for [y (range 64 68)] [(str "24," y ",2") "oak_log"]))
         {"24,63,2" "dirt" "24,68,2" "oak_leaves" "25,67,2" "oak_leaves" "23,67,2" "oak_leaves"}))

(deftest get-tool-whose-explore-finds-a-tree-fells-it-and-crafts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (bare []) :blocks merge tree-far {"1,64,2" "crafting_table"}) [own-zone])
              walk! (fn [s] (swap! (fake/state (:p s)) #(-> % (assoc-in [:self :pos] [20 64 2]) (fake/look-at {:pos [24 67 2]})))
                      (perception/pass! (aget (:p s) "perception")))]
          (await (with-explore-stub s {:reason :found :found [{:what "oak_log" :pos [24 64 2]}]} walk!
                   (fn ^:async body [seen]
                     (await (get-tool! s {:kind "pickaxe" :ways #{:craft :gather}} 160))
                     (is (= 1 (count @seen)) "no log within 16: one search")
                     (is (empty? (listed s)))
                     (is (= 1 (get (inv s) "wooden_pickaxe")) (pr-str (inv s) (last (events-of s :stopped))))))))))))

(deftest get-tool-with-explore-off-fails-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare []) [own-zone])
              id (await (get-tool! s {:item "stone_pickaxe" :explore false} 6))]
          (is (= 1 (count (no-source-events s :stopped)))))))))

(deftest a-stone-pickaxe-with-no-pickaxe-gets-a-wooden-one-first-from-seen-logs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (world {}) (update :blocks dissoc chest-at)
                           (update :blocks merge (into {} (for [y (range 64 68)] [(str "4," y ",2") "oak_log"])) {"4,63,2" "dirt"} {"4,68,2" "oak_leaves" "5,67,2" "oak_leaves"}
                                   (into {} (for [x [0 1] z [5 6]] [(str x ",64," z) "stone"])))
                           (assoc-in [:blocks "1,64,2"] "crafting_table"))
                       [own-zone])]
          (await (get-tool! s {:item "stone_pickaxe" :explore false} 300))
          (is (empty? (listed s)) (pr-str (last (events-of s :stopped))))
          (is (= 1 (get (inv s) "stone_pickaxe")) (pr-str (inv s) (last (events-of s :stopped)))))))))

(deftest explore-is-a-fetch-limit-passed-to-the-fetch-jobs
  (let [o (assoc need/built-in :explore true)]
    (is (= {:item "oak_log" :count 2 :explore true} (:args (need/plan-for {:reason :need :item "oak_log" :count 2} o))))
    (is (= {:block "stone" :explore true} (:args (need/plan-for {:reason :no-tool :block "stone"} o))))))

(deftest obtain-craft-keeps-the-short-detail-of-a-need-stop
  (is (= {:short {"stick" 2} :alternatives {"oak_log" ["birch_log"]}}
         (obtain-craft/stop-why {:kind :condition-failed :cause {:reason :need :short {"stick" 2}}
                                 :data {:made 0 :short {"stick" 2} :alternatives {"oak_log" ["birch_log"]}}})))
  (is (= :no-table (obtain-craft/stop-why {:kind :condition-failed :cause {:reason :no-table}}))))

(deftest fetch-limits-become-the-childs-call-keys
  (is (= {:ways #{:craft :smelt :gather} :spend {:job-time 120}} (need/child-limits 'jobs.items.obtain {:how #{:craft :gather} :minutes 2})))
  (is (= {:ways #{:chest :gather}} (need/child-limits 'jobs.items.get-tool {:how #{:chest :gather}})))
  (is (= {} (need/child-limits 'jobs.items.get-tool {}))))

(deftest get-tool-looks-around-and-takes-the-chest-behind-it-before-crafting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [behind "0,64,-6"
              s (start (-> (bare [{:name "oak_log" :count 12}])
                           (assoc-in [:blocks behind] "chest")
                           (assoc :containers {behind [{:name "wooden_pickaxe" :count 1}]}))
                       [own-zone])]
          (await (get-tool! s {:kind "pickaxe"} 60))
          (is (seq (calls s "look")) "it looked around")
          (is (= 1 (count (transfers s))) "the chest behind was used")
          (is (empty? (tables s)) "no table was crafted"))))))
