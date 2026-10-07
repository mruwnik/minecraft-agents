(ns engine.dig-niche-test
  "jobs.survival.dig-niche's choice of site: a solid face with a shell all round, never flat ground, fluid or sand."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as ew]
            [jobs.survival.dig-niche :as niche]))

(defn blocks [name [x0 x1] [y0 y1] [z0 z1]]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(def ground (blocks "dirt" [-2 8] [60 63] [-3 3]))

(defn site
  ([extra] (site extra [{:name "iron_pickaxe" :count 1}]))
  ([extra inventory] (niche/find-site (tu/seeing-all (tu/fake {:blocks (merge ground extra) :inventory inventory})) 16)))

(deftest a-hill-with-a-shell-gives-a-site-facing-it
  (is (= {:stand {:x 3 :y 64 :z 0} :dir [1 0]} (site (blocks "stone" [4 8] [64 66] [-3 3])))))

(deftest flat-ground-has-no-site
  (is (nil? (site {}))))

(deftest a-face-too-thin-or-low-for-the-shell-has-no-site
  (is (nil? (site (blocks "stone" [4 8] [64 65] [-3 3]))) "no ceiling over the niche")
  (is (nil? (site (blocks "stone" [4 8] [64 66] [0 0]))) "no side walls"))

(deftest a-cave-under-the-floor-the-body-has-not-seen-gives-no-site
  (let [cave (merge (blocks "stone" [3 8] [59 63] [-3 3]) (blocks "air" [2 3] [61 62] [0 0]))
        p (tu/fake {:blocks (merge ground cave) :inventory [{:name "iron_pickaxe" :count 1}]})
        seen? (fn [pos] (>= (.-y pos) 64))]
    (aset p "seenBlockAt" (fn [pos] (let [b (.blockAt p pos)]
                                      (if (seen? pos)
                                        #js {:name (.-name b) :pos pos :age-ms 0}
                                        #js {:unknown true :pos pos}))))
    (is (nil? (niche/find-site p 16)))))

(defn ok? [extra]
  (niche/niche-ok? (tu/seeing-all (tu/fake {:blocks (merge ground (blocks "stone" [4 8] [64 66] [-3 3]) extra)
                                           :inventory [{:name "iron_pickaxe" :count 1}]}))
                   {:x 3 :y 64 :z 0} [1 0]))

(deftest sand-gravel-or-water-in-or-round-the-niche-rules-it-out
  (is (true? (ok? {})))
  (is (false? (ok? (blocks "sand" [4 8] [64 66] [-3 3]))) "sand falls")
  (is (false? (ok? {"5,66,0" "gravel"})) "gravel over the niche")
  (is (false? (ok? {"4,64,1" "water"})) "water beside it")
  (is (false? (ok? {"6,64,0" "water"})) "water behind it"))

(deftest stone-needs-a-pickaxe-the-body-carries
  (is (nil? (site (blocks "stone" [4 8] [64 66] [-3 3]) [])))
  (is (some? (site (blocks "dirt" [4 8] [64 66] [-3 3]) []))))

;; ------------------------------------------------------------------ the job: dig, plug, refusals

(def hill (blocks "stone" [4 8] [64 66] [-3 3]))

(defn setup
  ([zones] (setup zones {}))
  ([zones spec]
   (let [clock (atom 1000000)
         [seen sink] (tu/legacy-capture-sink)
         p (tu/seeing-all (tu/fake (merge {:offlineScale 0.0001 :time 14000 :blocks (merge ground hill)
                                           :drops {"stone" "cobblestone"}
                                           :inventory [{:name "iron_pickaxe" :count 1}]}
                                          spec)))
         eng (core/create {:primitives p :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                           :world (ew/of-data {} {} zones)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     {:eng eng :p p :seen seen :clock clock})))

(defn ^:async run! [{:keys [eng]} args]
  (core/submit! eng (list 'jobs.survival.dig-niche args) {})
  (loop [i 0]
    (when (and (< i 300) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

(defn block-at [p [x y z]] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn failed [seen] (some #(when (= :dig_niche_failed (:kind %)) %) @seen))
(defn kinds-seen [seen] (set (map :kind @seen)))

(def hill-zone {:name "keep" :owner "Miles" :min [3 60 -3] :max [9 70 3]})

(deftest the-niche-is-dug-and-plugged-from-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [])]
          (await (run! s {}))
          (is (nil? (failed seen)))
          (is (contains? (kinds-seen seen) :dig-niche.sealed))
          (is (= ["air" "air"] (mapv #(block-at p %) [[5 64 0] [5 65 0]])) "inner cells dug")
          (is (= ["cobblestone" "cobblestone"] (mapv #(block-at p %) [[4 64 0] [4 65 0]])) "opening plugged with the dropped blocks"))))))

(deftest no-blocks-to-plug-with-fails-no-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (setup [] {:drops {}})]
          (await (run! s {:blocks ["dirt"]}))
          (is (= :no-blocks (:reason (failed seen)))))))))

(deftest a-face-in-another-zone-is-skipped-and-another-face-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [hill-zone]
                                            {:blocks (merge ground hill (blocks "dirt" [-10 -3] [60 63] [-3 3]) (blocks "stone" [-8 -4] [64 66] [-3 3]))})]
          (await (run! s {}))
          (is (nil? (failed seen)))
          (is (= "stone" (block-at p [5 64 0])) "the owned wall is untouched")
          (is (= "air" (block-at p [-5 64 0])) "the niche is cut in the free hill"))))))

(deftest only-foreign-faces-fail-refused-and-dig-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [hill-zone])]
          (await (run! s {}))
          (is (= :refused (:reason (failed seen))))
          (is (= "stone" (block-at p [5 64 0]))))))))

(deftest ignore-zones-digs-in-a-foreign-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [hill-zone])]
          (await (run! s {:ignore-zones? true}))
          (is (nil? (failed seen)))
          (is (= "air" (block-at p [5 64 0]))))))))

(deftest permitted-asks-the-dug-and-door-cells
  (let [in {:zones [hill-zone] :self "fake" :now 0 :footprints {} :claims {} :block-at (constantly "stone")}]
    (is (false? (niche/permitted? in {:x 3 :y 64 :z 0} [1 0])))
    (is (true? (niche/permitted? (assoc in :ignore-zones? true) {:x 3 :y 64 :z 0} [1 0])))
    (is (true? (niche/permitted? (assoc in :zones []) {:x 3 :y 64 :z 0} [1 0])))))

;; ------------------------------------------------------------------ failure reasons

(def far-hill (blocks "stone" [10 14] [64 66] [-3 3]))

(deftest a-walk-that-never-gets-there-fails-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:blocks (merge ground (blocks "dirt" [9 14] [60 63] [-3 3]) far-hill)})]
          (.override (.-world p) "steer"
                     (fn [_ _ _] (js/Promise.resolve #js {:status "timeout" :pose #js {}})))
          (await (run! s {}))
          (is (= :unreachable (:reason (failed seen)))))))))

(deftest a-dig-the-world-refuses-fails-dig-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (setup [] {})]
          (.override (.-world (:p s)) "dig" (fn ^:async f [_ _ _] #js {:status "failed"}))
          (await (run! s {}))
          (is (= :dig-failed (:reason (failed seen)))))))))

(deftest a-plug-the-world-refuses-fails-place-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {})]
          (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "failed"}))
          (await (run! s {}))
          (is (= :place-failed (:reason (failed seen))))
          (is (not (contains? (kinds-seen seen) :dig-niche.sealed))))))))

(defn mob-in-door-until
  "A place override refusing with a zombie in the cell while the fake clock is before until-ms."
  [clock until-ms]
  (fn ^:async f [token a impl]
    (if (< @clock until-ms)
      #js {:status "failed" :reason "Server refused" :refusal #js {:entities #js [#js {:name "zombie" :id 7}]}}
      (await (impl token a)))))

(defn ^:async run-timed!
  "Like run!, the fake clock moving 500 ms a tick."
  [{:keys [eng clock]} args]
  (core/submit! eng (list 'jobs.survival.dig-niche args) {})
  (loop [i 0]
    (when (and (< i 600) (seq (:list (core/state eng))))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i)))))

(deftest a-mob-in-the-door-cell-for-3-s-delays-the-plug-until-it-clears
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen clock] :as s} (setup [] {})]
          (.override (.-world p) "place" (mob-in-door-until clock (+ @clock 3000)))
          (await (run-timed! s {}))
          (is (nil? (failed seen)))
          (is (contains? (kinds-seen seen) :dig-niche.sealed))
          (is (= ["cobblestone" "cobblestone"] (mapv #(block-at p %) [[4 64 0] [4 65 0]]))))))))

(deftest a-mob-that-stays-past-the-limit-fails-place-failed-with-the-mob-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen clock] :as s} (setup [] {})]
          (.override (.-world p) "place" (mob-in-door-until clock (+ @clock 600000)))
          (await (run-timed! s {}))
          (is (= :place-failed (:reason (failed seen))))
          (is (re-find #"zombie" (:text (failed seen)))))))))

(deftest scan-reports-refusals-and-stops-at-the-first-allowed-site
  (let [p (tu/seeing-all (tu/fake {:blocks (merge ground hill) :inventory [{:name "iron_pickaxe" :count 1}]}))
        asked (atom 0)]
    (is (= {:site nil :refused? true} (niche/scan p 16 (fn [_ _] false))))
    (is (= {:site nil :refused? false} (niche/scan (tu/seeing-all (tu/fake {:blocks ground})) 16 (fn [_ _] true))))
    (is (= {:stand {:x 3 :y 64 :z 0} :dir [1 0]}
           (:site (niche/scan p 16 (fn [_ _] (swap! asked inc) true)))))
    (is (= 1 @asked) "stops at the first allowed site")))

(deftest a-fully-refused-hill-is-scanned-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (setup [hill-zone])
              scans (atom 0)
              scan niche/scan]
          (with-redefs [niche/scan (fn [& args] (swap! scans inc) (apply scan args))
                        niche/find-site (fn [& _] (swap! scans inc) nil)]
            (await (run! s {})))
          (is (= :refused (:reason (failed seen))))
          (is (= 1 @scans)))))))

(deftest stone-without-a-pickaxe-and-no-fetch-fails-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:inventory []})]
          (await (run! s {:fetch false}))
          (is (= :no-tool (:reason (failed seen))))
          (is (= "stone" (block-at p [5 64 0]))))))))

(deftest stone-without-a-pickaxe-tries-a-fetch-by-default-then-fails-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (setup [] {:inventory []})]
          (await (run! s {}))
          (is (contains? (kinds-seen seen) :fetch.started))
          (is (= :no-tool (:reason (failed seen)))))))))

(deftest flat-ground-without-a-pickaxe-is-still-no-site
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (setup [] {:inventory [] :blocks ground})]
          (await (run! s {}))
          (is (= :no-site (:reason (failed seen)))))))))

(deftest a-waiting-fetch-child-makes-the-niche-yield-not-fail-no-progress
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as s} (setup [] {:inventory []})
              orig ctx/call-child
              n (atom 0)]
          (core/submit! eng (list 'jobs.survival.dig-niche {}) {})
          (set! ctx/call-child (fn [c slot job args]
                                 (if (= :fetch slot)
                                   (do (swap! n inc) (js/Promise.resolve :continue))
                                   (orig c slot job args))))
          (try (await (core/tick! eng))
               (finally (set! ctx/call-child orig)))
          (is (= 1 @n) "one fetch round per scheduler round, no busy loop")
          (is (nil? (failed seen)))
          (is (= 1 (count (:list (core/state eng)))) "the job yielded and is still there"))))))
