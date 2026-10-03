(ns engine.jobs.survival
  "Reflex-sized jobs: get away from danger, sleep in a known bed. Contracts
  in README.md, section Job library."
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.memory :as mem]))

(def default-radius 8)
(def default-step 8)
(def max-moves 5)

(defn hostiles [p radius]
  (array-seq (.entities p #js {:radius radius :kind "hostile" :max 8})))

(defn away-from
  "The point step blocks from from, directly away from threat on the
  horizontal plane; +x when they coincide."
  [from threat step]
  (let [dx (- (:x from) (:x threat))
        dz (- (:z from) (:z threat))
        n (js/Math.hypot dx dz)
        [ux uz] (if (zero? n) [1 0] [(/ dx n) (/ dz n)])]
    {:x (js/Math.round (+ (:x from) (* ux step)))
     :y (:y from)
     :z (js/Math.round (+ (:z from) (* uz step)))}))

(defn ^:async retreat-round
  "args {:radius 8 :step 8}. Walks step blocks directly away from the nearest
  hostile within radius, one walk per round, at most five walks in total.
  Done when no hostile is within radius."
  [c]
  (let [{:keys [radius step] :or {radius default-radius step default-step}} (:args c)
        threat (first (hostiles (:primitives c) radius))
        moves (:moves (ctx/mem c) 0)]
    (cond
      (nil? threat) :done
      (>= moves max-moves) (do (ctx/emit! c :retreat_gave_up :warn {:text "hostile keeps up; stopped retreating"})
                               :done)
      :else
      (let [target (away-from (u/self-pos c) (u/pos-of (.-pos threat)) step)
            r (await (ctx/act c :moveTo (clj->js {:pos target :range 1})))]
        (ctx/commit! c #(assoc % :moves (inc moves)))
        :continue))))

(def retreat {:name :retreat :check (constantly true) :round retreat-round})

(defn bed-of [memory]
  (:pos (first (mem/places memory :bed))))

(defn sleep-check
  "A bed is known and it is night."
  [c]
  (and (some? (bed-of {:common (ctx/mem c :common)}))
       (not (.-isDay (.self (:primitives c))))))

(defn ^:async sleep-round
  "Walks to the first known bed in common places and sleeps. Done once asleep,
  or when it turns out not to be night. A taken bed or a monster nearby is
  retried three times; a missing bed warns and ends."
  [c]
  (let [bed (bed-of {:common (ctx/mem c :common)})
        w (if bed (await (u/walk-near! c bed 2)) :blocked)]
    (case w
      :partial :continue
      :blocked (u/fail! c :bed_unreachable "cannot reach the bed")
      (let [r (await (ctx/act c :sleep (clj->js {:pos bed})))]
        (case (.-status r)
          ("sleeping" "not-night") :done
          "missing" (do (ctx/emit! c :bed_missing :warn {:pos bed :text "no bed at the remembered place"})
                        :done)
          (u/fail! c :bed_unusable (str "cannot sleep: " (.-status r))))))))

(def sleep {:name :sleep :check sleep-check :round sleep-round})
