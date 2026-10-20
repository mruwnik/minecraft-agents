(ns engine.block-arrow-gap-test
  "jobs.survival.block-arrow-gap: stop the arrows of a skeleton shooting through a doorway."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.solid :as solid]
            [jobs.lib.util :as u]
            [jobs.lib.world-files :as ew]
            [jobs.survival.block-arrow-gap :as gap]))

(defn blocks [name [x0 x1] [y0 y1] [z0 z1]]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(def ground (blocks "dirt" [-8 8] [60 63] [-3 12]))

(def shell
  "A stone cell round the body at (0 64 0), roof at 66, with a two-high doorway in the south wall at (0 64..65 1)."
  (merge (blocks "stone" [-1 1] [64 66] [-1 1])
         {"0,64,0" "air" "0,65,0" "air" "0,64,1" "air" "0,65,1" "air"}))

(def skeleton {:id 9 :name "skeleton" :kind "hostile" :visible true :pos {:x 0.5 :y 64 :z 8.5}})

(defn setup
  ([zones] (setup zones {}))
  ([zones spec]
   (let [clock (atom 1000000)
         [seen sink] (tu/legacy-capture-sink)
         p ((:seeing spec tu/seeing-all) (tu/fake (merge {:offlineScale 0.0001 :self {:pos {:x 0.5 :y 64 :z 0.5}}
                                           :blocks (merge ground shell) :entities [skeleton]
                                           :inventory [{:name "cobblestone" :count 8}]}
                                          (dissoc spec :act-ms :seeing))))
         now (tu/act-clock clock p (:act-ms spec 0))
         eng (core/create {:primitives p :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now now
                           :world (ew/of-data {} {} zones)
                           :events (events/make {:body "Fake" :sinks [sink] :now now})})]
     {:eng eng :p p :seen seen})))

(defn ^:async run-job! [{:keys [eng]} args]
  (core/submit! eng (list 'jobs.survival.block-arrow-gap args) {})
  (loop [i 0]
    (when (and (< i 300) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

(defn shot-at [p] (danger-q/dangers p 16 {:ranged-radius 16} {}))
(defn cobble [p] (some #(when (= "cobblestone" (:name %)) (:count %)) (u/inventory p)))
(defn block-at [p [x y z]] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn failed [seen] (some #(when (= :stopped (:kind %)) %) @seen))
(defn kinds-seen [seen] (set (map :kind @seen)))

(def pit
  "A trench the skeleton shoots up out of, so its arrows reach the body through both doorway cells."
  (blocks "air" [0 0] [62 63] [3 8]))

(def pit-skeleton (assoc skeleton :pos {:x 0.5 :y 62 :z 8.5}))

(deftest a-doorway-shot-through-is-stoned-and-the-line-of-fire-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:entities [pit-skeleton] :blocks (merge ground pit shell)})]
          (await (run-job! s {}))
          (is (nil? (failed seen)))
          (is (contains? (kinds-seen seen) :completed))
          (is (not-any? #{:block-arrow-gap.closed :block_arrow_gap_failed} (kinds-seen seen)) "the ending carries it")
          (is (empty? (shot-at p)) "no line of fire is left")
          (is (= 7 (cobble p)) "one block ended both rays where they meet")
          (is (= ["air" "air"] (mapv #(block-at p %) [[0 64 0] [0 65 0]])) "the body's own cells are left"))))))

(deftest a-gap-not-yet-seen-is-looked-for-before-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:entities [pit-skeleton] :blocks (merge ground pit shell) :seeing tu/seeing-after-look})]
          (await (run-job! s {}))
          (is (nil? (failed seen)))
          (is (empty? (shot-at p)) "the gap is stoned once the body has looked")
          (is (< (cobble p) 8)))))))

(deftest a-gap-that-needs-one-block-gets-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:blocks (merge ground pit shell {"0,64,1" "stone"}) :entities [pit-skeleton]})]
          (await (run-job! s {}))
          (is (nil? (failed seen)))
          (is (empty? (shot-at p)))
          (is (= 7 (cobble p))))))))

(deftest a-gap-in-a-foreign-zone-fails-refused-and-places-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [{:name "keep" :owner "Miles" :min [-4 60 -4] :max [4 70 4]}] {:entities [pit-skeleton] :blocks (merge ground pit shell)})]
          (await (run-job! s {}))
          (is (= :refused (:reason (failed seen))))
          (is (= "air" (block-at p [0 64 1]))))))))

(deftest ignore-zones-places-in-a-foreign-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [{:name "keep" :owner "Miles" :min [-4 60 -4] :max [4 70 4]}] {:entities [pit-skeleton] :blocks (merge ground pit shell)})]
          (await (run-job! s {:ignore-zones? true}))
          (is (nil? (failed seen)))
          (is (empty? (shot-at p))))))))

(deftest no-line-of-fire-declines-and-places-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:entities [(assoc skeleton :pos {:x 8.5 :y 64 :z 0.5})]})]
          (await (run-job! s {}))
          (is (empty? (:placed (some #(when (= :completed (:kind %)) %) @seen))))
          (is (= "air" (block-at p [0 64 1]))))))))

(deftest a-gap-no-cell-in-reach-can-stop-fails-no-gap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (setup [] {:blocks ground})]
          (await (run-job! s {:settings {:jobs.survival.block-arrow-gap/reach 0}}))
          (is (= :no-gap (:reason (failed seen)))))))))

(deftest a-placed-cell-perception-has-not-caught-up-with-still-counts-as-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:entities [pit-skeleton] :blocks (merge ground pit shell)})]
          (let [at (.-seenBlockAt p)]
            (aset p "seenBlockAt" (fn [pos] (let [b (.call at p pos)] (if (= "cobblestone" (.-name b)) #js {:name "air" :pos pos :age-ms 0} b)))))
          (await (run-job! s {}))
          (is (nil? (failed seen)) "a successful place is not undone by a lagging view")
          (is (= 1 (count (:placed (some #(when (= :completed (:kind %)) %) @seen)))) "the placed cell is reported")
          (is (= 7 (cobble p)) "the cell is not filled twice"))))))

(deftest cells-the-body-has-not-seen-are-no-plug-candidates
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:entities [pit-skeleton] :blocks (merge ground pit shell)})
              at (.-seenBlockAt p)]
          (aset p "seenBlockAt" (fn [pos] (if (>= (.-z pos) 1) #js {:unknown true :pos pos} (.call at p pos))))
          (await (run-job! s {}))
          (is (= :no-gap (:reason (failed seen))) "the doorway was never seen, so it is not filled")
          (is (= 8 (cobble p)) "nothing placed"))))))

(defn ^:async one-tick! [{:keys [eng] :as s} args]
  (core/submit! eng (list 'jobs.survival.block-arrow-gap args) {})
  (await (core/tick! eng))
  s)

(deftest no-blocks-fetches-them-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (one-tick! (setup [] {:inventory [] :entities [pit-skeleton] :blocks (merge ground pit shell)}) {}))]
          (is (some #(and (= :child_started (:kind %)) (= :need (:slot %))) @seen) "a block is fetched through a need child"))))))

(deftest an-invisible-light-block-is-no-support
  (let [at {"1,1,0" "light" "0,0,0" "stone"}
        solid-at? #(solid/solid? (at (str (:x %) "," (:y %) "," (:z %))))]
    (is (not (gap/supported? solid-at? {:x 1 :y 2 :z 0})))
    (is (gap/supported? solid-at? {:x 0 :y 1 :z 0}))))
