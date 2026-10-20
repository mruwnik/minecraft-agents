(ns jobs.lib.pen-entry
  "The gates of the pens that hold animals near the body: a walk made for a block job keeps out of them (a go-to toll of ##Inf),
  because a gate opened for a body lets the crowd out. A pen is the closed enclosure (jobs.lib.pen/check) round a passive
  animal in sight, or a pen an open gate leaks from; its gates are all of the pen's."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.combat :as combat]
            [jobs.lib.pen :as pen]
            [jobs.lib.pen-world :as pen-world]
            [jobs.lib.pen-blocks :as pb]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [engine.ctx :as ctx]
            [clojure.string :as str]))

(a/defargs settings
  {::radius {:default 24 :doc "Blocks round the body in which animals are looked for to find the pens whose gates a block job's walk keeps out of." :spec (a/int-in 1 nil)}
   ::max-animals {:default 12 :doc "The nearest animals checked for a pen." :spec (a/int-in 1 nil)}
   ::max-cells {:default 400 :doc "Most cells searched for one pen." :spec (a/int-in 1 nil)}
   ::open-radius {:default 6 :doc "A go-to that starts within this many blocks of an open gate of a pen holding animals warns when its goal is far from the gate." :spec (a/int-in 1 nil)}
   ::leave-dist {:default 8 :doc "The goal of such a walk is this far from the gate or farther." :spec (a/int-in 1 nil)}})

(defn animal-cell [e]
  (let [{:keys [x y z]} (u/pos-of (.-pos e))]
    {:x (js/Math.floor x) :y (js/Math.floor (+ y 0.01)) :z (js/Math.floor z)}))

(defn fenced?
  "True when a cell beside one of inside ([x y z]) holds a seen fence, fence gate or wall: the fill ran along a barrier."
  [block-at inside]
  (boolean (some (fn [[x y z]]
                   (some (fn [[dx dz]] (some->> (block-at {:x (+ x dx) :y y :z (+ z dz)}) .-name (re-find #"(fence|fence_gate|_wall)$")))
                         pen-world/orthogonal))
                 inside)))

(defn pens
  "[{:gates [{:x :y :z}] :animals n}] of the pens round the passive animals in sight, one per pen; metadata :unseen?
  when a pen has cells the body has not seen. Those are noted for sense/decide! (u/*reads*) only for an animal whose
  fill met no leak but unseen cells: plausibly inside a fence ring, not loose in the open. An animal whose own floor
  is unseen (the pen is out of view) notes that floor, so it gets looked at."
  [c]
  (let [p (:primitives c)
        block-at (u/seen-block-at-fn p)
        animals (->> (combat/sensed p {:radius (settings/get settings ::radius) :max 64})
                     (filter #(= "passive" (.-kind %)))
                     (take (settings/get settings ::max-animals)))]
    (loop [todo animals found [] unseen? false]
      (if-let [e (first todo)]
        (let [cell (animal-cell e)
              k [(:x cell) (:y cell) (:z cell)]]
          (if-let [i (first (keep-indexed (fn [i f] (when (contains? (:inside f) k) i)) found))]
            (recur (rest todo) (update-in found [i :animals] inc) unseen?)
            (let [answer (binding [u/*reads* nil]
                           (pen/check {:block-at block-at :at cell :near k :max-cells (settings/get settings ::max-cells)}))
                  ring? (and (#{:unloaded :leak} (:reason answer))
                             (fenced? block-at (into (:inside answer) (map (comp (juxt :x :y :z) :pos)) (:leaks answer))))]
              (when ring?
                (doseq [l (:leaks answer) :let [leak ((juxt :x :y :z) (:pos l))] :when (sense/unknown? p leak)]
                  (u/note-read! :cells leak)))
              (when (empty? (:inside answer))
                (doseq [at [[(:x cell) (dec (:y cell)) (:z cell)] k] :when (sense/unknown? p at)]
                  (u/note-read! :cells at)))
              (recur (rest todo)
                     (if (and (contains? (:inside answer) k) (seq (:gates answer)))
                       (conj found {:inside (:inside answer) :gates (mapv :pos (:gates answer)) :open (mapv :pos (filter :open? (:gates answer))) :animals 1})
                       found)
                     (or unseen? ring?)))))
        (with-meta (mapv #(dissoc % :inside) found) {:unseen? unseen?})))))

(defn ^:async pens!
  "pens judged after a look at the unseen cells of the pens it read (sense/decide!: bounded per call, each cell looked
  at once per run and body cell, unloaded cells never). Call it from a round."
  [c]
  (await (sense/decide! c ::pens #(pens c) {:retry? #(or (empty? %) (:unseen? (meta %)))})))

(defn open-gates-near
  "The seen open fence gates {:x :y :z} within ::open-radius of here along x and z (lazily): the cheap test before any
  pen is searched."
  [c here]
  (let [block-at (u/seen-block-at-fn (:primitives c))
        r (settings/get settings ::open-radius)
        [x y z] (map #(js/Math.floor (% here)) [:x :y :z])]
    (for [dx (range (- r) (inc r)) dz (range (- r) (inc r)) dy [0 1 -1 2]
          :let [cell {:x (+ x dx) :y (+ y dy) :z (+ z dz)}
                b (block-at cell)]
          :when (and (pb/gate? b) (pb/on? b "open"))]
      cell)))

(defn ^:async warn-left-open!
  "Warn (once a call) when the walk to goal {:x :y :z} leads away from a pen with animals in it whose gate stands open
  within ::open-radius of the body: the pen empties in about a minute. Not for a :leave-open walk (doors), which leaves
  gates open on purpose."
  [c goal doors]
  (when (and (not= :leave-open doors) (every? number? (map goal [:x :y :z])))
    (let [here (u/self-pos c)
          leaving? #(and (<= (u/dist here %) (settings/get settings ::open-radius))
                         (>= (u/dist % goal) (settings/get settings ::leave-dist)))]
      (when (some leaving? (open-gates-near c here))
        (let [gates (->> (await (pens! c)) (mapcat :open) (filter leaving?) vec)]
          (when (seq gates)
            (ctx/warn-once! c :pen-gate-open :go-to.pen-gate-open
                            {:gates gates :text (str "walking away from a pen with animals in it whose gate stands open: " (str/join " " (map (fn [{:keys [x y z]}] [x y z]) gates)))})))))))
