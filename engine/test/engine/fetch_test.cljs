(ns engine.fetch-test
  "The :fetch option (jobs.lib.fetch), jobs.items.obtain, jobs.items.get-tool and jobs.items.fetch-limits against
  the fake world, slice F1: carried and chest sources only, chests the body has seen."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
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

(deftest dig-without-fetch-waits-no-tool-and-touches-no-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world pickaxe-stock) [own-zone])
              id (core/submit! (:eng s) (dig-spec {}) {})]
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
          (is (= 1 (count (events-of s :fetch.failed))))
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
              id (core/submit! (:eng s) (list 'jobs.access.stair {:dir :down :heading :south :steps 1}) {})]
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
          (is (= 1 (count (filter #(= :no-source (:reason %)) (events-of s :waiting))))))))))

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
