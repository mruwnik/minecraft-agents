(ns engine.irrigate-test
  "jobs.farm.irrigate against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(defn setup [world & [zones plans]]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake world))]
    {:eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                        :world (ew/of-data (or plans {}) {} (or zones []))
                        :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
     :p p}))

(defn block-at [p pos] (.-name (.blockAt p (clj->js pos))))
(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(def job 'jobs.farm.irrigate)
(def bucket [{:name "water_bucket" :count 1}])

(defn strip
  "A row of farmland from x 0 to n-1 at y 63, z 0, with stone beside it to walk on."
  [n]
  (into {} (mapcat (fn [x] [[(str x ",63,1") "stone"] [(str x ",63,-1") "stone"]
                            [(str x ",62,0") "dirt"] [(str x ",63,0") (if (< -1 x n) "farmland" "stone")]]))
        (range -2 (+ n 2))))

(defn args [n] {:area {:min {:x 0 :y 63 :z 0} :max {:x (dec n) :y 63 :z 0}}})

(deftest a-dry-field-gets-one-water-spot-that-wets-all-of-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory bucket :blocks (strip 9)})
              r (await (tu/child-outcome eng job (args 9) 40))
              water (filterv #(= "water" (block-at p {:x % :y 63 :z 0})) (range 9))]
          (is (= 1 (count water)))
          (is (= [] (:dry r)))
          (is (= 1 (count (:poured r)))))))))

(deftest a-field-wider-than-the-water-reach-keeps-its-far-cell-dry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory bucket :blocks (strip 10)})
              r (await (tu/child-outcome eng job (args 10) 40))]
          (is (= 1 (count (:poured r))))
          (is (= 1 (count (:dry r))) "one bucket wets nine of ten cells; the rest are reported"))))))

(deftest a-field-with-water-in-reach-is-left-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory bucket :blocks (assoc (strip 9) "4,63,3" "water")})
              r (await (tu/child-outcome eng job (args 9) 20))]
          (is (empty? (calls p "place")))
          (is (empty? (:poured r))))))))

(deftest no-bucket-and-no-fetch-refuses-the-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (strip 5)})
              r (await (tu/child-outcome eng job (assoc (args 5) :ways #{:carried}) 10))]
          (is (= {:reason :need :item "water_bucket"} (select-keys (:cause (:ending r)) [:reason :item]))))))))


(defn ending-of [r] (select-keys r [:status :reason]))

(deftest a-box-with-no-field-cell-seen-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory bucket :blocks (strip 0)})
              r (await (tu/child-outcome eng job (args 3) 20))]
          (is (= {:status :stopped :reason :nothing-seen} (ending-of r))))))))

(deftest a-box-with-no-field-ends-nothing-seen-in-the-dark-instead-of-waiting-for-light
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory bucket :blocks (strip 0) :time 15000})
              r (await (tu/child-outcome eng job (args 3) 20))]
          (is (= {:status :stopped :reason :nothing-seen} (ending-of r))))))))

(deftest a-cell-whose-whole-reach-is-seen-is-dry-and-gets-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory bucket :blocks (strip 3)})
              _ (tu/seeing-within p 8)
              r (await (tu/child-outcome eng job (args 3) 40))]
          (is (= [] (:unknown r)))
          (is (= [] (:dry r)))
          (is (= 1 (count (:poured r)))))))))

(deftest a-cell-with-unseen-reach-is-unknown-not-dry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory bucket :blocks (strip 3)})
              _ (tu/seeing-within p 3)
              r (await (tu/child-outcome eng job (args 3) 20))]
          (is (empty? (calls p "place")))
          (is (= [] (:dry r)))
          (is (= 3 (count (:unknown r))))
          (is (= {:status :stopped :reason :unseen} (ending-of r))))))))

(defn bank
  "A stone bank east of the strip (x 3-9, y 62-64, z -6..6) whose y 63 blocks past its face (x 4-8, z -5..5) are not seen: it holds no water a player could see."
  [p blocks]
  (let [hidden? (fn [pos] (and (= 63 (.-y pos)) (<= 4 (.-x pos) 8) (<= -5 (.-z pos) 5)))]
    (tu/set-sensed! p (fn [pos] (if (hidden? pos) #js {:unknown true} (.blockAt (tu/oracle p) pos))))
    (aset p "seenBlockAt" (fn [pos] (if (hidden? pos)
                                      #js {:unknown true :pos pos}
                                      (let [b (.blockAt (tu/oracle p) pos)] #js {:name (.-name b) :properties (.-properties b) :pos pos :age-ms 0}))))
    blocks))

(deftest unseen-reach-buried-in-a-bank-does-not-make-a-cell-unknown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stone (into {} (for [x (range 3 10) z (range -6 7) y [62 63 64]] [(str x "," y "," z) "stone"]))
              {:keys [eng p]} (setup {:inventory bucket :blocks (merge (strip 3) stone)})
              _ (bank p nil)
              r (await (tu/child-outcome eng job (args 3) 40))]
          (is (= [] (:unknown r)))
          (is (= [] (:dry r)))
          (is (= 1 (count (:poured r)))))))))

(deftest a-failed-bucket-fetch-names-the-bucket-in-the-text
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (strip 5)})
              r (await (tu/child-outcome eng job (assoc (args 5) :ways #{:carried :chest}) 20))]
          (is (re-find #"no water bucket" (str (:text r) (:text (:data (:ending r)))))))))))

(deftest a-spot-whose-side-is-open-is-not-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [open ["4,63,1" "4,63,-1" "4,62,0"]]
          (let [{:keys [eng p]} (setup {:inventory bucket :blocks (assoc (strip 9) open "air")})
                r (await (tu/child-outcome eng job (args 9) 40))
                water (filterv #(= "water" (block-at p {:x % :y 63 :z 0})) (range 9))]
            (is (not-any? #{4} water) open)
            (is (= 1 (count (:poured r))) open)))))))

(deftest a-plan-keeps-its-crop-ground-and-gets-water-in-its-own-water-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "p" :parts [{:id "water" :cells [[2 63 0]] :want "water"}
                                    {:id "wheat" :cells (mapv (fn [x] [x 64 0]) (remove #{2} (range 9))) :want {:crop "wheat"}}]}
              {:keys [eng p]} (setup {:inventory bucket :blocks (strip 9)} [] {"p" plan})
              r (await (tu/child-outcome eng job (assoc (args 9) :for-plan "p") 40))]
          (is (= "water" (block-at p {:x 2 :y 63 :z 0})) "the plan's water cell, not the cell that wets most")
          (is (= 1 (count (:poured r)))))))))

(deftest a-spot-in-anothers-zone-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory bucket :blocks (strip 9)} [{:name "farm" :min [4 60 0] :max [4 70 0] :owner "Miles"}])
              r (await (tu/child-outcome eng job (args 9) 40))]
          (is (not= "water" (block-at p {:x 4 :y 63 :z 0})))
          (is (= 1 (count (:poured r)))))))))

(deftest ignore-zones-lets-the-pour-into-anothers-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory bucket :blocks (strip 5)} [{:name "farm" :min [-5 60 -5] :max [10 70 5] :owner "Miles"}])
              r (await (tu/child-outcome eng job (assoc (args 5) :ignore-zones? true) 40))]
          (is (= 1 (count (:poured r))))
          (is (empty? (:holes r)))
          (is (= "water" (block-at p (first (:poured r))))))))))

(defn fail-places!
  "Make every place of item fail; other places run as the fake does."
  [p item]
  (.override (.-world p) "place"
             (fn ^:async f [tok a impl] (if (= item (.-item a)) #js {:status "failed" :reason "no"} (await (impl tok a))))))

(def dirt [{:name "water_bucket" :count 1} {:name "dirt" :count 12}])

(defn hide-floor!
  "Make every block under the field row (y 62) unseen, as a dug cell's floor can stay."
  [p]
  (let [hidden? (fn [pos] (= 62 (.-y pos)))
        sensed (fn [pos] (if (hidden? pos) #js {:unknown true} (.blockAt (tu/oracle p) pos)))]
    (tu/set-sensed! p sensed)
    (aset p "seenBlockAt" (fn [pos] (if (hidden? pos)
                                      #js {:unknown true :pos pos}
                                      (let [b (.blockAt (tu/oracle p) pos)] #js {:name (.-name b) :properties (.-properties b) :pos pos :age-ms 0}))))))

(deftest a-dug-cell-whose-floor-stays-unseen-is-refilled-not-poured
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory dirt :blocks (strip 9)})
              _ (hide-floor! p)
              r (await (tu/child-outcome eng job (args 9) 60))]
          (is (pos? (count (:restored r))) "every spot tried is refilled")
          (is (= [] (:poured r)))
          (is (not-any? #(#{"air" "water"} (block-at p {:x % :y 63 :z 0})) (range 9))))))))

(deftest a-failed-pour-has-the-cell-refilled-and-another-spot-is-tried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory dirt :blocks (strip 7)})
              failed (atom false)
              _ (.override (.-world p) "place"
                           (fn ^:async f [tok a impl]
                             (if (and (= "water_bucket" (.-item a)) (not @failed))
                               (do (reset! failed true) #js {:status "failed" :reason "no"})
                               (await (impl tok a)))))
              r (await (tu/child-outcome eng job (args 7) 80))]
          (is (= 1 (count (:restored r))))
          (is (= [] (:holes r)))
          (is (= 1 (count (:poured r))))
          (is (= [] (:dry r)))
          (is (= :done (:status r))))))))

(deftest a-cell-that-cannot-be-refilled-is-named-as-a-hole
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory dirt :blocks (strip 9)})
              _ (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "failed" :reason "no"}))
              r (await (tu/child-outcome eng job (args 9) 60))]
          (is (= 1 (count (:holes r))))
          (is (= [] (:restored r)))
          (is (= [] (:poured r))))))))

(deftest a-bucket-gone-after-the-dig-has-the-cell-refilled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory dirt :blocks (strip 9)})
              _ (.override (.-world p) "dig"
                           (fn ^:async f [tok a impl]
                             (let [r (await (impl tok a))]
                               (swap! (.-state (.-world p)) fake/take-one "water_bucket")
                               r)))
              r (await (tu/child-outcome eng job (args 9) 60))]
          (is (pos? (count (:restored r))) "every spot tried is refilled")
          (is (= [] (:holes r)))
          (is (= [] (:poured r)))
          (is (not-any? #(#{"air" "water"} (block-at p {:x % :y 63 :z 0})) (range 9))))))))
