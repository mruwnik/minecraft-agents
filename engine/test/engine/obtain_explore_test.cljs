(ns engine.obtain-explore-test
  "items.obtain's long looks: logs in a foreign zone or cut off from the body, ore seen from far. Split from engine.obtain-iron-test, whose ray casts over a wide sight made one namespace outlast the test runner's limit."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fetch-test :as f]
            [engine.obtain-iron-test :refer [ore-world obtain!]]
            [engine.fake :as fake]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [engine.memory :as mem]
            [engine.ctx :as ctx]
            [jobs.items.obtain.gather :as gather]
            [jobs.items.obtain.base :as base]
            [jobs.items.obtain.plan :as plan]
            [jobs.items.obtain.seen :as seen]
            [jobs.items.obtain.table :as table]
            [jobs.lib.access :as access]
            [jobs.lib.look :as look]))

(def village-trunk
  "A trunk at x 3 inside a foreign zone."
  {:blocks (into {} (for [y [64 65 66]] [(str "3," y ",4") "oak_log"]))
   :zones [f/own-zone {:name "village" :min [2 60 3] :max [4 70 5] :owner "Miles"}]})

(defn ^:async obtain-small! [s args n]
  (await (obtain! s args n)))

(defn free-trees
  "A tree 30 blocks off on each side: the search starts the way its legs sort first, which the test must not depend on."
  []
  (into {"30,68,4" "oak_leaves" "31,67,4" "oak_leaves" "-30,68,4" "oak_leaves" "-31,67,4" "oak_leaves"}
        (for [x [30 -30] y [64 65 66 67]] [(str x "," y ",4") "oak_log"])))

(deftest a-search-centred-on-a-site-stays-within-the-site-radius
  (is (= 96 (:max-distance (gather/within-site {:max-distance 192} [490 61 250]))) "the far search is cut to the site radius")
  (is (= 64 (:max-distance (gather/within-site {:max-distance 64} [490 61 250]))) "a nearer search is kept")
  (is (= 192 (:max-distance (gather/within-site {:max-distance 192} nil))) "no centre: unbounded as before"))

(defn search-at
  "explore-opts of an obtain of args by a fake body."
  [args]
  (let [view (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)}))]
    (base/explore-opts {:primitives (tu/fake-on-floor {}) :view (constantly view) :args (merge {:explore true} args)})))

(def bed {:any-of ["white_bed" "red_bed"]})

(deftest a-priced-search-is-worth-what-the-find-is-and-no-count-binds-it
  (let [o (search-at bed)]
    (is (< 0 (:worth (search-at {:item "stone_axe"})) (:worth o)) "an axe is worth a shorter search than a bed")
    (is (= (:worth o) (get-in o [:spend :job-time])))
    (is (not-any? o [:max-legs :max-distance]) "the walks' price binds it, not a count or a radius")
    (is (zero? (:worth (search-at {:item "dirt" :count 1}))) "a find worth no more than a trip pays for no walk")))

(deftest logs-seen-only-in-a-foreign-zone-are-explored-past-and-then-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world (:blocks village-trunk) []) (:zones village-trunk))]
          (await (obtain-small! s {:worth 500 :any-of ["oak_log" "birch_log"] :count 2 :ways #{:gather} :explore true} 12))
          (is (seq (f/events-of s :obtain.exploring)) "walks to look for logs it may take")
          (await (f/run-ticks s 600))
          (is (re-find #"seen but not taken.*zone" (str (:why (first (f/events-of s :stopped))))) "when exploring fails the stop names the refusal"))))))

(deftest a-tree-it-may-take-is-preferred-to-logs-in-a-foreign-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [free (free-trees)
              s (f/start (ore-world (merge (:blocks village-trunk) free) []) (:zones village-trunk))]
          (await (obtain! s {:worth 500 :any-of ["oak_log"] :count 2 :ways #{:gather} :explore true} 400))
          (is (<= 2 (get (f/inv s) "oak_log" 0)) "logs were got from the tree outside the zone"))))))

(def arity1 "access/rules-input is called through its 1-arity property" "cljs$core$IFn$_invoke$arity$1")

(deftest the-seen-log-to-take-is-found-beyond-any-number-of-nearer-barred-logs
  (let [barred (for [i (range 3000)] {:name "oak_log" :pos {:x i :y 64 :z 0}})
        free {:name "oak_log" :pos {:x 150 :y 64 :z 0}}
        real (aget access/rules-input arity1)]
    (aset access/rules-input arity1 (constantly {}))
    (try
      (with-redefs [look/seen-blocks (fn [_ {:keys [max]}] (vec (take max (concat barred [free]))))
                    access/may? (fn [_ _ pos] {:ok (= 150 (:x pos))})
                    seen/cut-off? (constantly false)]
        (is (= free (first (seen/seen-blocks-of {:primitives nil} {:seen #{"oak_log"}} false)))
            "every nearer log is barred, the free one lies past them"))
      (finally (aset access/rules-input arity1 real)))))

(deftest a-tree-cut-off-from-the-body-is-explored-past-with-explore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [free (free-trees)
              s (f/start (assoc (ore-world (merge f/tree-blocks free) []) :unreachable ["0,64,4" "0,65,4" "0,66,4" "0,67,4"]) [f/own-zone])]
          (await (obtain! s {:worth 500 :any-of ["oak_log"] :count 2 :ways #{:gather} :explore true} 600))
          (is (<= 2 (get (f/inv s) "oak_log" 0)) "logs were got from the reachable tree"))))))

(deftest raw-iron-seen-38-blocks-away-sends-the-mine-not-a-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world (into (into {} (for [x (range -2 5) z (range 0 38)] [(str x ",63," z) "stone"])) (for [x [0 1 2] y [63 64 65]] [(str x "," y ",38") "iron_ore"])) [{:name "stone_pickaxe" :count 1}]) [] (assoc f/sight-opts :radius 48 :ray-deg 4))]
          (await (obtain! s {:worth 500 :item "raw_iron" :ways #{:craft :smelt :gather}} 200))
          (is (empty? (filter #(= :no-source (:reason %)) (f/events-of s :waiting))))
          (is (empty? (f/events-of s :obtain.exploring)))
          (is (<= 1 (get (f/inv s) "raw_iron" 0)) "the mine went for the ore and got it"))))))

(deftest logs-only-on-a-bedrock-pillar-are-explored-for-before-obtain-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pillar (into {} (concat (for [y (range 63 67)] [(str "0," y ",4") "bedrock"])
                                      (for [y (range 67 73)] [(str "0," y ",4") "oak_log"])))
              s (f/start (ore-world pillar []) [f/own-zone])]
          (await (obtain-small! s {:worth 500 :item "oak_log" :count 2 :explore true} 60))
          (is (seq (f/events-of s :obtain.exploring)) "walks to look for other logs")
          (is (empty? (filter #(= :no-tree (:reason %)) (f/events-of s :stopped))) "not stopped no-tree before that"))))))

(def sealed-cave
  "Stone to y 70 over x -22..48, z -22..22, with a hollow 5x2x5 inside it where the body stands: no way out but through rock. A tree stands on the stone 40 blocks off."
  (merge (into {} (for [x (range -22 49) z (range -22 23) y (range 63 71)
                         :when (not (and (<= -2 x 2) (<= -2 z 2) (<= 64 y 65)))]
                    [(str x "," y "," z) (if (= y 63) "andesite" "stone")]))
         (into {} (for [y [71 72 73 74]] [(str "24," y ",0") "oak_log"]))
         {"24,75,0" "oak_leaves"}))

(deftest a-body-sealed-in-a-cave-stairs-up-to-the-sky-before-it-looks-for-trees
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world sealed-cave []) [f/own-zone])]
          (swap! (fake/state (:p s)) assoc :light (into {} (for [x (range -20 21) z (range -20 21) y (range 64 71)] [[x y z] [0 0]])))
          (await (obtain! s {:worth 500 :item "oak_log" :count 1 :explore true} 900))
          (is (seq (f/events-of s :stair.step)) "a stair was cut up through the roof")
          (is (every? #(<= 71 (get-in % [:from 1])) (f/events-of s :obtain.exploring)) "no walk to look for trees starts under the roof")
          (is (<= 1 (get (f/inv s) "oak_log" 0)) "and the tree outside was felled"))))))

(deftest an-ore-whose-tunnel-is-priced-above-the-feet-is-climbed-to-once
  (let [p (tu/seeing-all (tu/fake-on-floor {:inventory [{:name "stone_pickaxe" :count 1}]}))
        view (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)}))
        c {:primitives p :view (constantly view) :args {:item "raw_iron" :count 1 :ways #{:gather} :ignore-zones? true}}
        need (first (table/gather-needs {"raw_iron" 1}))
        climb (fn [mem way] (with-redefs [ctx/mem (constantly mem) plan/search-way (constantly way)] (gather/climb-y c need)))]
    (is (= 68 (climb {} {:way :tunnel :y 68})) "the tunnel priced above the feet")
    (is (nil? (climb {} {:way :tunnel :y 60})) "a tunnel below the feet is the mine's own stair down")
    (is (nil? (climb {} {:way :look :y 68})))
    (is (nil? (climb {:gather {:climbed {"raw_iron" true}}} {:way :tunnel :y 68})) "climbed once already")
    (is (= 70 (climb {:gather {:climb 70}} {:way :look})) "a climb under way goes on whatever the price says now")))

(deftest a-search-held-on-the-body-says-it-is-looking-once-not-every-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (update (ore-world {} []) :self assoc :health 8 :food 5) [f/own-zone])]
          (await (obtain-small! s {:worth 500 :item "oak_log" :count 1 :ways #{:gather} :explore true} 60))
          (is (seq (f/events-of s :waiting)) "the hurt, foodless body's search waits")
          (is (= 1 (count (f/events-of s :obtain.exploring))) "and the notice is said once, not each round"))))))

(deftest planks-of-any-species-search-for-any-log
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {} []) [f/own-zone])]
          (await (obtain-small! s {:worth 500 :any-of ["oak_planks" "spruce_planks" "birch_planks"] :count 2 :ways #{:gather :craft} :explore true} 200))
          (is (seq (f/events-of s :obtain.exploring)))
          (is (every? #(< 1 (count (:for %))) (f/events-of s :obtain.exploring)) "searches for logs of every species, not oak alone"))))))

(deftest searches-that-keep-running-out-of-time-still-end-the-gather
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (ore-world {} []) [f/own-zone])
              short {:jobs.explore.search/job-time-s 0.01}]
          (await (settings/with-settings short #(obtain! s {:worth 500 :any-of ["oak_log"] :count 1 :ways #{:gather} :explore true} 400)))
          (is (= 1 (count (f/events-of s :obtain.exploring))) "one search, repeated while it runs out of time, then the gather ends")
          (is (= :no-source (:reason (first (f/events-of s :stopped))))))))))

(deftest water-out-of-sight-is-explored-for-and-the-bucket-filled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (assoc (update (f/bare [{:name "bucket" :count 1}]) :blocks merge {"30,63,4" "water"}) :floor [-60 -60 60 60]) [f/own-zone])]
          (await (obtain! s {:worth 500 :item "water_bucket" :explore true} 2000))
          (is (seq (f/events-of s :obtain.exploring)) "walks to look for water")
          (is (= 1 (get (f/inv s) "water_bucket" 0))))))))

(deftest a-search-that-found-its-need-leaves-no-start-for-the-next-need-to-walk-back-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [m (atom {})
              view (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)}))
              c {:primitives (tu/seeing-all (tu/fake-on-floor {})) :view (constantly view)
                 :args {:item "torch" :count 1 :ways #{:gather :search} :explore true :worth 500}}
              ends (atom [{:kind :done :data {:reason :found :found [{:what "oak_log" :pos [3 64 3]}]}}
                          {:kind :nothing-possible :data {:reason :not-found}}])
              logs {:key "log" :count 1 :seen #{"oak_log"}}
              coal {:key "coal" :count 1 :seen #{"coal_ore"}}]
          (with-redefs [ctx/mem (fn [_] @m)
                        ctx/update-mem! (fn [_ f & args] (apply swap! m f args))
                        ctx/emit! (fn ([_ _ _]) ([_ _ _ _]))
                        ctx/call (fn [& _] (let [e (first @ends)] (swap! ends rest) (js/Promise.resolve e)))
                        seen/seen-of (fn [_ need] (when (= "log" (:key need)) {:name "oak_log" :pos {:x 3 :y 64 :z 3}}))]
            (is (= :again (await (gather/explore-walk! c logs (constantly true)))))
            (is (= :again (await (gather/explore-walk! c coal (constantly false))))))
          (is (nil? (:explore-start @m)))
          (is (= "coal" (get-in @m [:tried :gather :searched])) "the coal search's verdict is kept, not lost to a walk back to the log search's start"))))))
