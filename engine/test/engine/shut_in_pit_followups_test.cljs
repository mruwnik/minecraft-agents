(ns engine.shut-in-pit-followups-test
  "dig-in's zigzag pit: up-front fluid checks, the straight column only for a bad cell it refuses, a glance at unseen
  side columns, no side column with a hostile, and a bounded roof placement."
  (:require [cljs.test :refer [deftest is async]]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [engine.shut-in-pit-seen-test :as seen]
            [jobs.survival.pit-cells :as pit]))

(def tunnel
  "Rock round an east-west tunnel: air at x 0..3, y 65..66, z 0."
  (reduce #(dissoc %1 %2) (merge seen/ground (seen/stone -4 4 65 67 -4 4))
          (for [x (range 0 4) y [65 66]] (str x "," y ",0"))))

(defn ^:async see-cells! [p cells]
  (.setOwner p "t1")
  (loop [cs cells]
    (when-let [[x y z] (first cs)]
      (await (.look p "t1" #js {:pos #js {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}}))
      (recur (rest cs)))))

(defn ^:async run-seeing! [spec cells & [args]]
  (let [p (seen/sensing spec)]
    (await (see-cells! p cells))
    (await (seen/tick-out! (seen/setup (seen/hover-over-digs! p) 'jobs.survival.shut-in (seen/night (or args {})))))))

(def surface (for [x (range -3 4) z (range -3 4) y [64 65]] [x y z]))

(deftest water-where-the-body-stands-is-refused-before-any-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (run-seeing! {:blocks (assoc seen/ground "0,66,0" "water")}
                                                    (conj surface [0 66 0])))]
          (is (empty? (seen/digs p)))
          (is (re-find #"water" (str (:text (first (seen/failed s)))))))))))

(deftest water-beside-the-cell-to-dig-is-refused-before-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (run-seeing! {:blocks (assoc seen/ground "2,64,0" "water" "-2,64,0" "water"
                                                                    "0,64,2" "water" "0,64,-2" "water")}
                                                    surface))]
          (is (empty? (seen/digs p)))
          (is (re-find #"water" (str (:text (first (seen/failed s)))))))))))

(deftest only-a-bad-cell-in-the-first-two-of-the-column-keeps-the-straight-shape
  (let [start {:x 0 :y 65 :z 0}
        shape (fn [bad] (pit/refused-column (tu/fake {:self {:pos {:x 0 :y 65 :z 0}}
                                                         :blocks (assoc seen/ground bad "lava")})
                                               start {:depth 3}))]
    (is (some? (shape "0,64,0")))
    (is (some? (shape "0,63,0")))
    (is (nil? (shape "0,61,0")) "a bad cell deeper down is left to the zigzag")))

(deftest unseen-side-column-cells-are-looked-at-before-no-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (run-seeing! {:blocks tunnel} [[0 65 0]] {:ways #{:pit}}))]
          (is (empty? (seen/failed s)))
          (is (seq (seen/digs p)) "the side column was glanced at, so the zigzag was dug"))))))

(deftest a-side-column-with-a-hostile-in-it-is-not-used
  (let [zombie {:id 1 :uuid "u1" :name "zombie" :kind "hostile" :pos {:x 1.5 :y 65 :z 0.5}}
        cell {:x 1 :y 65 :z 0}
        p (fn [es] (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :blocks seen/ground :entities es}))]
    (is (true? (pit/side-open? {:primitives (p []) :args {:radius 8}} cell)))
    (is (false? (pit/side-open? {:primitives (p [zombie]) :args {:radius 8}} cell)))))

(deftest the-pit-reads-the-hostile-radius-from-the-danger-settings
  (let [zombie {:id 1 :uuid "u1" :name "zombie" :kind "hostile" :pos {:x 1.5 :y 65 :z 0.5}}
        c {:primitives (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :blocks seen/ground :entities [zombie]}) :args {}}
        cell {:x 1 :y 65 :z 0}]
    (is (false? (pit/side-open? c cell)) "the default radius sees the zombie")
    (is (true? (settings/with-settings {:jobs.lib.danger/hostile-radius 0} #(pit/side-open? c cell))))))

(deftest a-plug-answered-occupied-by-a-block-that-does-not-seal-ends-the-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seen/hover-over-digs! (await (seen/see-surface! (seen/sensing {:blocks seen/ground}))))
              n (atom 0)
              _ (.override (.-world p) "place" (fn ^:async f [_ _ _] (swap! n inc) #js {:status "occupied"}))
              s (await (seen/tick-out! (seen/setup p 'jobs.survival.shut-in (seen/night {}))))]
          (is (<= @n 5) "bounded")
          (is (re-find #"does not seal" (str (:text (first (seen/failed s)))))))))))

(deftest a-pit-dug-with-no-block-to-roof-it-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (seen/run-pit! {:blocks seen/ground :inventory [{:name "iron_pickaxe" :count 1}]
                                       :drops {"stone" "coal"}}))]
          (is (some #(re-find #"nothing to roof the pit with" (str (:text %))) (seen/failed s)))
          (is (= [:stopped :no-way] ((juxt :status :reason) @(:out s))) "the call reports the stop, not success"))))))

(deftest walls-count-unsensed-rock-over-the-head-that-a-place-answers-occupied-as-sealed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sides [[1 65 0] [-1 65 0] [0 65 1] [0 65 -1] [1 66 0] [-1 66 0] [0 66 1] [0 66 -1]]
              p (seen/sensing {:blocks tunnel})
              _ (await (see-cells! p sides))
              n (atom 0)
              _ (.override (.-world p) "look" (fn ^:async f [token a impl]
                                                (if (< (swap! n inc) 3) #js {:status "ok"} (await (impl token a)))))
              {:keys [out] :as s} (await (seen/tick-out! (seen/setup p 'jobs.survival.shut-in {:ways #{:walls}})))]
          (is (= :done (:status @out)) "the first looks missed the rock over the head; the failed place looks again")
          (is (= "cobblestone" (.-name (.blockAt p (tu/pos 1 65 0)))) "the open end is walled")
          (is (= "cobblestone" (.-name (.blockAt p (tu/pos 1 66 0))))))))))

(def wall-shape
  {:steps [{:to {:x 1 :y 64 :z 0} :dig [{:x 1 :y 64 :z 0}]}] :plugs [] :roof {:x 0 :y 67 :z 0}
   :prep [{:x 1 :y 65 :z 0} {:x 1 :y 66 :z 0}]})

(defn opens-with? [beyond]
  (let [p (tu/fake {:self {:pos {:x 0.5 :y 65 :z 0.5}} :blocks (merge (seen/stone -1 1 60 70 -1 1) (when beyond (into {} (for [y [65 66]] [(str "2," y ",0") beyond]))))})]
    (pit/opens-onto-air? p {:x 0 :y 65 :z 0} wall-shape)))

(deftest a-zigzag-is-not-dug-through-a-wall-with-air-behind-it
  (is (opens-with? nil) "a one-block wall over air")
  (is (not (opens-with? "stone")) "rock behind the wall"))
