(ns engine.fetch-test
  "The :fetch option (jobs.lib.fetch), jobs.items.obtain, jobs.items.get-tool and jobs.items.fetch-limits against
  the fake world, slice F1: carried and chest sources only, chests the body has seen."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [jobs.items.craft :as craft]
            [jobs.items.obtain :as obtain]
            [jobs.lib.fetch :as fetch]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(def sight-opts {:radius 16 :ray-deg 2})

(defn start
  "An engine over a fake world on an andesite floor, seeing through perception (one sight pass first), with zones."
  [world zones]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake-on-floor (assoc world :floor-block "andesite"))
        p (perception/wrap raw (perception/create (fake-raw/create raw) (assoc sight-opts :now #(deref clock))))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock) :world (ew/of-data {} {} zones)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (perception/pass! (aget p "perception"))
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async run-ticks [{:keys [eng clock]} n]
  (dotimes [_ n]
    (swap! clock + 700)
    (await (core/tick! eng))))

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
   :blocks {stone-at "stone" chest-at "chest"}
   :containers containers
   :drops {"stone" "cobblestone"}})

(def own-zone {:name "home" :min [-6 60 -6] :max [6 70 6] :owner "Fake"})
(def foreign-zone {:name "vault" :min [-3 60 2] :max [-1 70 4] :owner "Miles"})

(defn dig-spec [extra] (list 'jobs.blocks.dig (merge {:pos [2 64 3]} extra)))

(defn transfers [s] (calls s "transfer"))
(defn inspects [s] (calls s "inspectContainer"))

;; ------------------------------------------------------------------ the option

(deftest fetch-arg-shapes-parse
  (is (nil? (fetch/parse-arg nil)))
  (is (nil? (fetch/parse-arg false)))
  (is (= {} (fetch/parse-arg true)))
  (is (= {:what #{:tool}} (fetch/parse-arg #{:tool})))
  (is (= {:how #{:chest} :depth 2} (fetch/parse-arg {:how #{:chest} :depth 2})))
  (is (:error (fetch/parse-arg "yes")))
  (is (:error (fetch/parse-arg {:depth -1})))
  (is (:error (fetch/parse-arg {:what #{:tools}}))))

(deftest limits-precedence-call-over-body-job-over-body-all-over-job-default-over-built-in
  (let [body {:all {:depth 5 :minutes 4} :jobs {'jobs.blocks.dig {:depth 3}}}]
    (is (= 7 (:depth (fetch/merge-limits nil nil nil nil))))
    (is (= 10 (:minutes (fetch/merge-limits nil nil nil nil))))
    (is (= 6 (:depth (fetch/merge-limits nil {:depth 6} nil nil))) "job code default over the built-in")
    (is (= 5 (:depth (fetch/merge-limits 'jobs.blocks.place {:depth 6} body nil))) "body default for all jobs over code")
    (is (= 3 (:depth (fetch/merge-limits 'jobs.blocks.dig {:depth 6} body nil))) "body default for this job over all")
    (is (= 4 (:minutes (fetch/merge-limits 'jobs.blocks.dig nil body nil))))
    (is (= 1 (:depth (fetch/merge-limits 'jobs.blocks.dig {:depth 6} body {:depth 1}))) "the call wins")))

(deftest a-wait-maps-to-a-fetch-only-for-wanted-kinds
  (let [o (fetch/merge-limits nil nil nil nil)]
    (is (= 'jobs.items.get-tool (:job (fetch/plan-for {:reason :no-tool :needs "wooden_pickaxe" :block "stone"} o))))
    (is (= {:block "stone"} (:args (fetch/plan-for {:reason :no-tool :needs "wooden_pickaxe" :block "stone"} o))))
    (is (= {:item "oak_planks" :count 1} (:args (fetch/plan-for {:reason :need :item "oak_planks" :pos {}} o))))
    (is (= {:any-of ["dirt" "cobblestone"] :count 1} (:args (fetch/plan-for {:reason :need :any-of ["dirt" "cobblestone"]} o))))
    (is (nil? (fetch/plan-for {:reason :no-tool :block "stone"} (assoc o :what #{:item}))))
    (is (nil? (fetch/plan-for {:reason :inventory-full :pos {}} o)))
    (is (nil? (fetch/plan-for {:reason :not-allowed :pos {}} o)))))

;; ------------------------------------------------------------------ dig with :fetch, the chest source

(deftest dig-fetches-a-pickaxe-from-an-own-seen-chest-then-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (core/submit! (:eng s) (dig-spec {:fetch true}) {})
          (await (run-ticks s 40))
          (is (empty? (listed s)) "the dig ended")
          (is (= "air" (block-at s 2 64 3)))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (= 1 (get (inv s) "cobblestone")))
          (is (= 1 (count (transfers s))))
          (is (empty? (filter #(= "wooden_pickaxe" (:name %)) (chest-items s [-2 64 3]))) "the chest lost its pickaxe")
          (is (= 1 (count (events-of s :fetch.started))))
          (is (= 1 (count (events-of s :fetch.done)))))))))

;; the default call (no :fetch) fetches: dig, place and stair
(deftest dig-by-default-fetches-a-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (core/submit! (:eng s) (dig-spec {}) {})
          (await (run-ticks s 40))
          (is (= "air" (block-at s 2 64 3)))
          (is (= 1 (count (events-of s :fetch.done)))))))))

(deftest place-by-default-fetches-the-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "cobblestone" :count 5}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.blocks.place {:pos [1 64 1] :item "cobblestone"}) {})
          (await (run-ticks s 40))
          (is (= "cobblestone" (block-at s 1 64 1)))
          (is (= 1 (count (events-of s :fetch.done)))))))))

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
          (is (= 1 (count (events-of s :fetch.done)))))))))

(deftest dig-and-place-fetch-and-act-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (core/submit! (:eng s) (dig-spec {:fetch true}) {})
          (swap! (:clock s) + 700)
          (await (core/tick! (:eng s)))
          (is (empty? (listed s)))
          (is (= "air" (block-at s 2 64 3))))
        (let [s (start (world {chest-at [{:name "cobblestone" :count 5}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.blocks.place {:pos [1 64 1] :item "cobblestone" :fetch true}) {})
          (swap! (:clock s) + 700)
          (await (core/tick! (:eng s)))
          (is (empty? (listed s)))
          (is (= "cobblestone" (block-at s 1 64 1))))))))

(deftest dig-without-fetch-waits-no-tool-and-touches-no-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])
              id (core/submit! (:eng s) (dig-spec {:fetch false}) {})]
          (await (run-ticks s 10))
          (is (= :no-tool (:reason (core/waiting (:eng s) id))))
          (is (empty? (inspects s)))
          (is (empty? (transfers s)))
          (is (= "stone" (block-at s 2 64 3))))))))

(deftest a-chest-in-another-zone-is-not-used-and-the-dig-waits-with-the-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [foreign-zone])
              id (core/submit! (:eng s) (dig-spec {:fetch true}) {})]
          (await (run-ticks s 15))
          (let [w (core/waiting (:eng s) id)]
            (is (= :no-tool (:reason w)))
            (is (= :no-source (:failed (:fetch w)))))
          (is (empty? (inspects s)))
          (is (empty? (transfers s)))
          (is (= 1 (count (events-of s :fetch.failed)))))))))

(deftest a-chest-in-another-zone-that-allows-take-is-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [(assoc foreign-zone :allow #{:take})])]
          (core/submit! (:eng s) (dig-spec {:fetch true}) {})
          (await (run-ticks s 40))
          (is (= "air" (block-at s 2 64 3)))
          (is (= 1 (count (transfers s)))))))))

(deftest an-open-unzoned-seen-chest-is-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [])]
          (core/submit! (:eng s) (dig-spec {:fetch true}) {})
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
        (let [s (start (update (world pickaxe-stock) :blocks merge walled-chest) [own-zone])
              id (core/submit! (:eng s) (dig-spec {:fetch true}) {})]
          (await (run-ticks s 15))
          (is (empty? (inspects s)))
          (is (empty? (transfers s)))
          (is (= :no-source (:failed (:fetch (core/waiting (:eng s) id))))))))))

(deftest a-failure-is-remembered-and-a-second-dig-does-not-look-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc-in (world {chest-at [{:name "dirt" :count 3}]}) [:blocks "2,65,3"] "stone") [own-zone])
              a (core/submit! (:eng s) (dig-spec {:fetch true}) {})]
          (await (run-ticks s 25))
          (is (= 1 (count (inspects s))) "the chest was looked into once")
          (is (empty? (transfers s)))
          (is (= :no-source (:failed (:fetch (core/waiting (:eng s) a)))))
          (let [b (core/submit! (:eng s) (list 'jobs.blocks.dig {:pos [2 65 3] :fetch true}) {})]
            (await (run-ticks s 15))
            (is (= 1 (count (inspects s))) "not again within the failure's time")
            (is (= :no-source (:failed (:fetch (core/waiting (:eng s) b)))))
            (is (= 1 (count (events-of s :fetch.failed))))))))))

(deftest a-failure-expires-after-fail-minutes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "dirt" :count 3}]}) [own-zone])
              _ (core/submit! (:eng s) (dig-spec {:fetch {:fail-minutes 1}}) {})]
          (await (run-ticks s 25))
          (is (= 1 (count (events-of s :fetch.failed))))
          (swap! (:clock s) + 70000)
          (await (run-ticks s 25))
          (is (= 2 (count (events-of s :fetch.failed))) "tried again after the minute"))))))

(deftest fetch-what-item-does-not-fetch-a-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])
              id (core/submit! (:eng s) (dig-spec {:fetch #{:item}}) {})]
          (await (run-ticks s 10))
          (is (= :no-tool (:reason (core/waiting (:eng s) id))))
          (is (nil? (:fetch (core/waiting (:eng s) id))))
          (is (empty? (inspects s))))))))

(deftest a-bad-fetch-arg-ends-the-job-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (core/submit! (:eng s) (dig-spec {:fetch "please"}) {})
          (await (run-ticks s 5))
          (is (empty? (listed s)))
          (is (empty? (inspects s)))
          (is (= 1 (count (events-of s :fetch.bad-args)))))))))

;; ------------------------------------------------------------------ place with :fetch

(deftest place-fetches-the-block-from-a-seen-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "cobblestone" :count 5}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.blocks.place {:pos [1 64 1] :item "cobblestone" :fetch true}) {})
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
              id (core/submit! (:eng s) (list 'jobs.access.stair {:dir :down :heading :south :steps 1 :fetch true}) {})]
          (await (run-ticks s 60))
          (is (= 1 (get (inv s) "wooden_pickaxe")) (pr-str (core/waiting (:eng s) id)))
          (is (= "air" (block-at s 0 64 1)))
          (is (= 1 (count (events-of s :fetch.done))))
          (is (empty? (events-of s :stair.stopped)) "the stair went on from where it began")
          (is (empty? (listed s)))
          (let [me (:pos (fake/self (:p s)))]
            (is (= [0 0] [(js/Math.floor (:x me)) (js/Math.floor (:z me))]) "back on the stair line at its origin")))))))

(defn ^:async with-waiting-fetch-child
  "Run (f counter) with the :fetch child of every job answering :continue (a child waiting on the world); counter
  counts its rounds."
  [f]
  (let [orig ctx/call-child
        n (atom 0)]
    (set! ctx/call-child (fn [c slot job args]
                           (if (= :fetch slot)
                             (do (swap! n inc) (js/Promise.resolve :continue))
                             (orig c slot job args))))
    (try (await (f n))
         (finally (set! ctx/call-child orig)))))

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
              id (core/submit! (:eng s) (list 'jobs.access.stair {:dir :down :heading :south :steps 1 :fetch true}) {})]
          (await (with-waiting-fetch-child
                   (fn ^:async waiting-round [n]
                     (await (run-ticks s 1))
                     (is (= 1 @n) "one fetch round per scheduler round, no busy loop")
                     (is (= 1 (count (listed s))) "the job yielded and is still there"))))
          (await (run-ticks s 60))
          (is (= 1 (get (inv s) "wooden_pickaxe")) (pr-str (core/waiting (:eng s) id)))
          (is (= 1 (count (events-of s :fetch.done)))))))))

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
              id (core/submit! (:eng s) (list 'jobs.access.stair {:dir :down :heading :south :steps 1 :fetch true}) {})]
          (await (run-ticks s 40))
          (is (seq (events-of s :fetch.failed)))
          (is (nil? (:fetch-return (core/job-memory (:eng s) id))) (pr-str (core/job-memory (:eng s) id))))))))

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
              id (core/submit! (:eng s) (list 'jobs.access.stair {:dir :down :heading :south :steps 1 :fetch false}) {})]
          (await (run-ticks s 10))
          (is (= :no-tool (:reason (core/waiting (:eng s) id))))
          (is (empty? (inspects s))))))))

;; ------------------------------------------------------------------ the fetch jobs themselves

(deftest a-remembered-chest-out-of-view-is-trusted-a-fresh-one-is-read
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at []}) [own-zone])
              chests #(mapv fetch/cell-of (fetch/seen-chests {:primitives (:p s)}))]
          (is (= [[-2 64 3]] (chests)))
          (fake/remove-block! (:p s) [-2 64 3])
          (is (empty? (chests)) "in view just now: the live block says it is gone")
          (swap! (fake/state (:p s)) fake/put-block [-2 64 3] "chest")
          (swap! (:clock s) + 60000)
          (fake/remove-block! (:p s) [-2 64 3])
          (is (= [[-2 64 3]] (chests)) "a minute old memory is not checked against the live block"))))))

(deftest obtain-with-no-seen-chest-waits-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (world {}) (update :blocks dissoc chest-at) (assoc :inventory [{:name "dirt" :count 2}])) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "dirt" :count 3}) {})
          (await (run-ticks s 5))
          (is (empty? (inspects s)) "no chest seen: no look either")
          (is (= 1 (count (filter #(= :no-source (:reason %)) (events-of s :waiting)))))
          (is (re-find #"no seen chest that may hold dirt" (:why (first (events-of s :waiting)))))
          (is (re-find #"no recipe makes dirt" (:why (first (events-of s :waiting))))))))))

(deftest obtain-craft-wait-names-the-short-ingredient
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (-> (world {}) (update :blocks dissoc chest-at) (assoc :inventory [{:name "stick" :count 1}])) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "wooden_pickaxe" :how #{:craft}}) {})
          (await (run-ticks s 5))
          (is (re-find #"needs 2 stick, have 1" (:why (first (events-of s :waiting))))))))))

(deftest obtain-any-of-takes-the-first-name-the-chest-holds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "stone_pickaxe" :count 1} {:name "iron_pickaxe" :count 1}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:any-of ["wooden_pickaxe" "stone_pickaxe" "iron_pickaxe"]}) {})
          (await (run-ticks s 30))
          (is (empty? (listed s)))
          (is (= {"stone_pickaxe" 1} (select-keys (inv s) ["stone_pickaxe" "iron_pickaxe"]))))))))

(deftest obtain-on-a-cycle-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "stick" :chain ["wooden_pickaxe" "stick"]}) {})
          (await (run-ticks s 5))
          (is (empty? (listed s)))
          (is (= :cycle (:reason (first (events-of s :stopped)))))
          (is (empty? (inspects s))))))))

(deftest get-tool-for-a-hand-block-is-not-needed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.get-tool {:block "dirt"}) {})
          (await (run-ticks s 5))
          (is (empty? (listed s)))
          (is (empty? (inspects s))))))))

(deftest get-tool-waits-no-source-when-nothing-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (world {}) :blocks dissoc chest-at) [own-zone])
              id (core/submit! (:eng s) (list 'jobs.items.get-tool {:block "stone"}) {})]
          (await (run-ticks s 5))
          (is (= :no-source (:reason (core/waiting (:eng s) id))))
          (is (empty? (inspects s))))))))

(deftest stock-is-remembered-after-a-withdraw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "dirt" :count 5}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.storage.withdraw {:chest [-2 64 3] :items {"dirt" 2}}) {})
          (await (run-ticks s 20))
          (is (= {"dirt" 3} (get (fetch/stock-of (mem/view (:store (:eng s)))) [-2 64 3]))))))))

;; ------------------------------------------------------------------ per-body limits

(deftest fetch-limits-writes-the-body-defaults
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {}) [])]
          (core/submit! (:eng s) (list 'jobs.items.fetch-limits {:depth 3}) {})
          (core/submit! (:eng s) (list 'jobs.items.fetch-limits {:job 'jobs.blocks.dig :minutes 2}) {})
          (await (run-ticks s 4))
          (is (= {:all {:depth 3} :jobs {'jobs.blocks.dig {:minutes 2}}}
                 (fetch/body-limits (mem/view (:store (:eng s))))))
          (core/submit! (:eng s) (list 'jobs.items.fetch-limits {:depth -2}) {})
          (await (run-ticks s 2))
          (is (= 1 (count (events-of s :fetch-limits.refused))))
          (core/submit! (:eng s) (list 'jobs.items.fetch-limits {:clear true}) {})
          (await (run-ticks s 2))
          (is (= {} (fetch/body-limits (mem/view (:store (:eng s)))))))))))

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
          (core/submit! (:eng s) (dig-spec {:fetch true}) {})
          (await (run-ticks s 80))
          (is (empty? (listed s)) "the dig ended")
          (is (= "air" (block-at s 2 64 3)))
          (is (= 1 (get (inv s) "wooden_pickaxe")))
          (is (= 1 (count (events-of s :fetch.done)))))))))

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

(deftest get-tool-with-nothing-usable-waits-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare [{:name "dirt" :count 5}]) [own-zone])
              id (await (get-tool! s {:kind "pickaxe"} 6))]
          (is (= :no-source (:reason (core/waiting (:eng s) id))))
          (is (empty? (calls s "craft"))))))))

(deftest a-stone-pickaxe-without-cobblestone-waits-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare [{:name "wooden_pickaxe" :count 1} {:name "stick" :count 2} {:name "oak_log" :count 3}]) [own-zone])
              id (await (get-tool! s {:item "stone_pickaxe"} 6))]
          (is (= :no-source (:reason (core/waiting (:eng s) id)))))))))

(deftest crafting-is-off-when-how-leaves-it-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare [{:name "oak_log" :count 12}]) [own-zone])
              id (await (get-tool! s {:kind "pickaxe" :how #{:chest}} 6))]
          (is (= :no-source (:reason (core/waiting (:eng s) id))))
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
          (is (= 1 (count (events-of s :fetch.done)))))))))

(deftest fertilize-fetches-bone-meal-from-an-own-seen-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc-in (world {chest-at [{:name "bone_meal" :count 4}]}) [:blocks "3,63,1"] "grass_block") [own-zone])]
          (core/submit! (:eng s) (list 'jobs.farm.fertilize {:grass true :max 1 :fetch true}) {})
          (await (run-ticks s 40))
          (is (empty? (listed s)) "the job ended")
          (is (= 1 (count (calls s "useOn"))))
          (is (= 1 (count (events-of s :fetch.done)))))))))

(def sapling-spot {:x 4 :y 64 :z 3})

(deftest plant-sapling-fetches-a-sapling-from-an-own-seen-chest-then-plants
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "oak_sapling" :count 2}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.forestry.plant-sapling {:at sapling-spot :species "oak"}) {})
          (await (run-ticks s 40))
          (is (empty? (listed s)) "the job ended")
          (is (= "oak_sapling" (block-at s 4 64 3)))
          (is (= 1 (count (events-of s :fetch.done)))))))))

(deftest plant-sapling-without-fetch-waits-no-sapling-and-touches-no-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "oak_sapling" :count 2}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.forestry.plant-sapling {:at sapling-spot :species "oak" :fetch false}) {})
          (await (run-ticks s 10))
          (is (= 1 (count (listed s))) "still queued")
          (is (empty? (transfers s)))
          (is (= [:no-sapling] (mapv :reason (events-of s :waiting)))))))))

(deftest plant-sapling-with-nothing-to-fetch-waits-no-sapling-with-the-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.forestry.plant-sapling {:at sapling-spot :species "oak"}) {})
          (await (run-ticks s 20))
          (is (= 1 (count (listed s))) "still queued")
          (is (= 1 (count (events-of s :fetch.failed))))
          (is (= :no-sapling (:reason (last (events-of s :waiting))))))))))

(deftest plant-sapling-fetches-a-mangrove-propagule-for-the-species
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world {chest-at [{:name "mangrove_propagule" :count 2}]}) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.forestry.plant-sapling {:at sapling-spot :species "mangrove"}) {})
          (await (run-ticks s 40))
          (is (empty? (listed s)) "the job ended")
          (is (= "mangrove_propagule" (block-at s 4 64 3)))
          (is (= 1 (count (events-of s :fetch.done)))))))))

(defn plan-ops
  "The step ops craft-plan makes for a pickaxe from carried logs, with mem as the job memory and no table in sight."
  [mem]
  (let [s (start (-> (world {}) (assoc :inventory [{:name "oak_planks" :count 12} {:name "stick" :count 4}])) [own-zone])]
    (with-redefs [ctx/mem (constantly mem)
                  craft/nearest-table (constantly nil)]
      (mapv :op (:steps (obtain/craft-plan {:primitives (:p s)} ["wooden_pickaxe"] 1))))))

(deftest obtain-plan-uses-the-remembered-table
  (is (= [:craft] (plan-ops {:table [1 64 0]}))))

(deftest obtain-plan-puts-a-table-down-once-the-table-is-unreachable
  (is (some #{:place} (plan-ops {:table [1 64 0] :craft {:table-unreachable true}}))))

(deftest get-tool-for-an-unknown-block-is-bad-args-not-a-hand-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])]
          (await (get-tool! s {:block "not_a_block"} 5))
          (is (= 1 (count (events-of s :get-tool.declined))))
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

(deftest obtain-gathers-the-cobblestone-a-craft-lacks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (world {}) :blocks merge (into {} (for [x [0 1] z [5 6]] [(str x ",64," z) "stone"]))) :inventory [{:name "wooden_pickaxe" :count 1} {:name "oak_planks" :count 4} {:name "stick" :count 4} {:name "crafting_table" :count 1}]) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "stone_pickaxe"}) {})
          (await (run-ticks s 80))
          (is (empty? (listed s)))
          (is (= 1 (get (inv s) "stone_pickaxe"))))))))

(deftest obtain-gather-waits-no-source-when-nothing-is-seen-to-gather
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare []) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "wooden_pickaxe"}) {})
          (await (run-ticks s 6))
          (is (= 1 (count (filter #(= :no-source (:reason %)) (events-of s :waiting)))))
          (is (re-find #"nothing seen to gather" (:why (first (events-of s :waiting))))))))))

(deftest obtain-gather-is-off-when-how-leaves-it-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (bare []) :blocks merge tree-blocks) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "wooden_pickaxe" :how #{:craft :chest}}) {})
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

(deftest obtain-sapling-waits-no-source-without-leaves-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (bare []) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_sapling"}) {})
          (await (run-ticks s 6))
          (is (= 1 (count (filter #(= :no-source (:reason %)) (events-of s :waiting)))))
          (is (empty? (calls s "dig"))))))))

(deftest obtain-sapling-gather-is-off-when-how-leaves-it-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (bare []) :blocks merge leaf-blocks) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "oak_sapling" :how #{:chest :craft}}) {})
          (await (run-ticks s 10))
          (is (empty? (calls s "dig"))))))))

(deftest obtain-gather-needs-every-raw-item-seen-before-it-starts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (update (bare []) :blocks merge tree-blocks) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "stone_pickaxe"}) {})
          (await (run-ticks s 10))
          (is (= 1 (count (filter #(= :no-source (:reason %)) (events-of s :waiting)))))
          (is (empty? (calls s "dig")) "no tree felled for a chain that then lacks stone")
          (let [why (:why (first (events-of s :waiting)))]
            (is (re-find #"cobblestone \(from stone\)" why))
            (is (not (re-find #"log" why)) "the log that is seen is not named")))))))

(deftest obtain-gathers-coal-from-a-deepslate-ore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (world {}) :blocks merge {"1,64,5" "deepslate_coal_ore" "2,64,5" "deepslate_coal_ore"})
                              :drops {"deepslate_coal_ore" "coal"}
                              :inventory [{:name "wooden_pickaxe" :count 1} {:name "stick" :count 4}]) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "torch"}) {})
          (await (run-ticks s 80))
          (is (empty? (listed s)))
          (is (pos? (get (inv s) "torch" 0))))))))

(deftest obtain-gather-that-brings-in-nothing-stops-no-source-with-tried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (bare []) :blocks merge tree-blocks) :unreachable (vec (keys tree-blocks))) [own-zone])]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "wooden_pickaxe"}) {})
          (await (run-ticks s 200))
          (is (empty? (listed s)))
          (is (nil? (get (inv s) "wooden_pickaxe")))
          (let [st (last (events-of s :stopped))]
            (is (= :no-source (:reason st)))))))))

(deftest obtain-sapling-leaf-breaking-is-bounded
  (let [need (obtain/gather-need {"oak_sapling" 1})]
    (is (= obtain/sapling-leaf-limit (:dry-digs (:args need))) "digs per child run")
    (is (= 1 (:max-runs need)) "one fruitless run ends the source")
    (is (nil? (:max-runs (obtain/gather-need {"cobblestone" 1}))) "other sources use the default bound")))

(deftest sapling-leaf-limit-is-60
  (is (= 60 obtain/sapling-leaf-limit)))

(deftest a-declined-child-is-not-a-fruitless-run
  (let [sapling (obtain/gather-need {"oak_sapling" 1})]
    (is (= 1 (obtain/fruitless-limit sapling :done)))
    (is (= obtain/max-fruitless (obtain/fruitless-limit sapling :declined)) "a child that could not start broke no leaves")))

(deftest gather-needs-mine-the-block-that-drops-the-material
  (is (= #{"stone"} (:seen (obtain/gather-need {"cobblestone" 1}))))
  (let [n (obtain/gather-need {"cobbled_deepslate" 2})]
    (is (= #{"deepslate"} (:seen n)))
    (is (= {:block "deepslate" :item "cobbled_deepslate" :count 2} (:args n)))))

(deftest the-stone-materials-are-those-of-the-bodys-version
  (is (= #{"deepslate"} (obtain/material-blocks "26.1" "cobbled_deepslate")))
  (is (nil? (obtain/material-blocks "1.16.5" "cobbled_deepslate")) "no deepslate in that version")
  (is (nil? (:seen (obtain/gather-need {"cobbled_deepslate" 1} "1.16.5")))))

(deftest obtain-mines-seen-deepslate-for-a-stone-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (update (bare [{:name "wooden_pickaxe" :count 1} {:name "oak_planks" :count 4} {:name "stick" :count 4} {:name "crafting_table" :count 1}])
                                    :blocks merge (into {} (for [x [0 1] z [5 6]] [(str x ",64," z) "deepslate"])))
                              :drops {"deepslate" "cobbled_deepslate"}
                              :recipes {"stone_pickaxe" {:count 1 :needs {"cobbled_deepslate" 3 "stick" 2} :table true}}) [own-zone])]
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
          (await (run-ticks s 60))
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
