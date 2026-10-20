(ns engine.seen-scaffold-test
  "Q98 C2b follow-up: place, pillar and bridge look before they read a cell they have not sensed. Perception wraps the
  fake primitives; blockAt stays raw."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.dig-to-see-test :as d]
            [engine.test-util :as tu]
            [jobs.access.bridge :as bridge]
            [jobs.lib.dig-look :as look]
            [jobs.lib.blocks :as b]))

(defn looks [p] (d/calls p "look"))

(def hole "Rock with one hidden cell: nothing about it is sensed." (dissoc d/ground "1,63,0"))

(deftest place-waits-no-support-only-when-every-face-is-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [open (await (d/run! {:blocks d/ground} 'jobs.blocks.place {:pos {:x 3 :y 66 :z 0} :item "cobblestone" :ignore-zones? true}))
              hid (await (d/run! {:blocks hole} 'jobs.blocks.place {:pos {:x 1 :y 63 :z 0} :item "cobblestone" :ignore-zones? true :see false}))]
          (is (empty? (d/places (:p open))) "a seen cell in the air has nothing to place against: it waits")
          (is (some #{{:x 1 :y 63 :z 0}} (d/places (:p hid))) "a hidden one is tried: the primitive decides"))))))

(deftest see-target-turns-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (d/run! {:blocks hole} 'jobs.blocks.place {:pos {:x 1 :y 63 :z 0} :item "cobblestone" :ignore-zones? true}))
              at-target (filter #(= {:x 1.5 :y 63.5 :z 0.5} (:pos %)) (looks p))]
          (is (<= 1 (count at-target) 3) "place looks at the cell once; go-to's own looks at its unseen goal are capped"))))))

(deftest pillar-looks-at-an-unseen-ceiling-before-it-gives-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (d/sensing {:blocks d/ground})]
          (is (true? (look/unknown? p [0 67 0])) "the cell two above the head is unsensed at the start")
          (let [{:keys [out]} (await (d/run! {:blocks d/ground} 'jobs.access.pillar {:height 2 :ignore-zones? true}))]
            (is (= :done (:status @out)))
            (is (= 2 (:built @out)))))))))

(def doorway-cell
  "A 1-wide doorway along +z under open sky: stone walls at x -1 and 1 (feet and head), a floor, closed behind the body."
  (merge (d/stone -1 1 63 63 -1 3)
         (into {} (for [z (range -1 4) x [-1 1] y [64 65]] [(str x "," y "," z) "stone"]))
         (into {} (for [y [64 65]] [(str "0," y ",-1") "stone"]))))

(deftest a-cornered-retreat-looks-at-the-unseen-sky-before-it-rules-a-pillar-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:blocks doorway-cell :self {:pos {:x 0 :y 64 :z 0}} :inventory [{:name "cobblestone" :count 20}]
                    :entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 0 :y 64 :z 1} :health 20}]}
              p (d/sensing spec)]
          (is (true? (look/unknown? p [0 67 0])) "the sky over the doorway is unsensed at the start")
          (let [{:keys [eng clock]} (d/setup p 'jobs.survival.respond-to-hostile {})]
            (let [round (core/tick! eng)]
              (loop [i 0]
                (when (and (< i 100) (empty? (d/calls p "jumpPlace")))
                  (swap! clock + 500)
                  (await (js/Promise. (fn [r] (js/setTimeout r 20))))
                  (recur (inc i))))
              (swap! (fake/state p) assoc :entities [])
              round)
            (is (pos? (count (d/calls p "jumpPlace"))) "an unseen sky is looked at, not taken for rock: the pillar goes up")))))))

(deftest bridge-looks-at-the-stand-cell-before-stepping-onto-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (d/sensing {:blocks d/ground})]
          (is (true? (look/unknown? p [1 65 0])))
          (let [{:keys [out p]} (await (d/run! {:blocks d/ground} 'jobs.access.bridge {:heading :east :length 1 :ignore-zones? true}))]
            (is (= :done (:status @out)))
            (is (some #(= {:x 1.5 :y 65.5 :z 0.5} (:pos %)) (looks p)))))))))

(defn step [unknown?]
  (bridge/next-step {:feet [0 65 0] :start [0 65 0] :heading :east :length 1 :carried {"dirt" 9} :zones [] :footprints #{} :ledger #{}
                     :block-at (fn [[_ y _]] (if (< y 65) "stone" "air")) :unknown? unknown?}))

(deftest bridge-gives-up-unseen-for-a-cell-still-unsensed-after-looking
  (is (= {:step :give-up :reason :unseen :at [1 65 0]} (select-keys (step #{[1 65 0]}) [:step :reason :at])))
  (is (= {:step :give-up :reason :unseen :at [1 66 0]} (select-keys (step #{[1 66 0]}) [:step :reason :at])))
  (is (= :move (:step (step #{})))))

(deftest bridge-with-no-blocks-and-no-fetch-ends-after-looking-ahead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [gap (apply dissoc d/ground (for [x (range 1 4)] (str x ",64,0")))
              p (d/sensing {:blocks gap :inventory []})
              {:keys [eng] :as s} (d/setup p 'jobs.access.bridge {:heading :east :length 3 :ways #{:carried} :ignore-zones? true})]
          (is (true? (look/unknown? p [1 65 0])) "the cells ahead are unsensed at the start")
          (loop [i 0]
            (when (< i 20)
              (swap! (:clock s) + 500)
              (await (core/tick! eng))
              (recur (inc i))))
          (is (= :too-few-blocks (:reason @(:out s))) "blocks missing at the start end the job")
          (is (empty? (:list (core/state eng)))))))))

(deftest dig-looks-at-water-beside-the-cell-before-it-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (d/run! {:blocks (assoc d/ground "2,64,0" "water")} 'jobs.blocks.dig
                                         {:pos {:x 1 :y 64 :z 0} :ignore-zones? true}))]
          (is (false? (look/unknown? p [2 64 0])) "the look at the target shows the water beside it")
          (is (empty? (d/digs p)) "and the default waits on it instead of letting it flow in"))))))

(deftest dig-accepting-water-beside-goes-ahead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (d/run! {:blocks (assoc d/ground "2,64,0" "water")} 'jobs.blocks.dig
                                         {:pos {:x 1 :y 64 :z 0} :accept #{:fluid-adjacent} :ignore-zones? true}))]
          (is (some #{{:x 1 :y 64 :z 0}} (d/digs p))))))))
