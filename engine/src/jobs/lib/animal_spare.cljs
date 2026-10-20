(ns jobs.lib.animal-spare
  "Which food animals a hunt leaves alone: kinds on the body's off-limits list, animals in the body's own pens or in any closed enclosure, and the
  last few adults of a kind so a breeding pair survives."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.apiary :as apiary]
            [jobs.lib.pen :as pen]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::off-limits {:default #{} :doc "Animal kinds (\"cow\") a hunt for food never takes, wherever they are." :spec (a/set-of a/item?)}
   ::spare-pens {:default true :doc "Whether a hunt leaves alone animals in a zone or plan of the body's own or in a closed enclosure." :spec boolean?}
   ::keep-adults {:default 2 :doc "Adults of a kind in sight a hunt for food leaves alone so they can breed." :spec (a/int-in 0 nil)}
   ::pen-max-cells {:default 400 :doc "Most cells searched to tell whether an animal stands in a closed enclosure (then it counts as penned)." :spec (a/int-in 1 nil)}})

(defn off-limits
  "The kinds a hunt never takes: the ::off-limits setting."
  []
  (set (settings/get settings ::off-limits)))

(defn spare-pens? "The ::spare-pens setting." [] (settings/get settings ::spare-pens))

(defn keep-adults "The ::keep-adults setting." [] (settings/get settings ::keep-adults))

(defn why
  "Why a hunt leaves an adult animal alone, or nil: :off-limits, :pen (in a zone or plan of the body's own, unless
  ::spare-pens is false) or :kept (no more than `keep` adults of its kind in sight, unless starved?)."
  [keep kind adults-of-kind in-own-area? starved?]
  (cond (contains? (off-limits) kind) :off-limits
        (and (spare-pens?) in-own-area?) :pen
        (and (<= adults-of-kind keep) (not starved?)) :kept))

(defn allowed-by
  "Which rule let a hunt take an adult why left alone: :pen-taken (::spare-pens false and it stood in a pen), :surplus (more
  than `keep` of its kind) or :starved (the kept pair, lifted by hunger)."
  [keep adults-of-kind in-own-area?]
  (cond (and in-own-area? (not (spare-pens?))) :pen-taken
        (> adults-of-kind keep) :surplus
        :else :starved))

(defn enclosed?
  "Whether the animal stands inside a closed fence or wall the body has seen: the pen check from its feet cell, the
  closed pens found so far kept in `pens` (an atom of cell sets)."
  [c pens e]
  (let [{:keys [x y z]} (u/pos-of (.-pos e))
        cell [(js/Math.floor x) (js/Math.floor (+ y 0.01)) (js/Math.floor z)]]
    (or (boolean (some #(contains? % cell) @pens))
        (let [answer (pen/check {:block-at (apiary/seen-block-at-fn (:primitives c)) :at {:x (cell 0) :y (cell 1) :z (cell 2)}
                                 :max-cells (settings/get settings ::pen-max-cells)})]
          (when (:closed? answer) (swap! pens conj (:inside answer)))
          (boolean (:closed? answer))))))

(defn penned?
  "Whether an adult animal stands in a zone or plan of the body's own or in a closed enclosure (see enclosed?)."
  [c pens e]
  (or (access/in-own-area? c (u/pos-of (.-pos e))) (enclosed? c pens e)))
