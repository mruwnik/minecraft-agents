(ns jobs.lib.animal-spare
  "Which food animals a hunt leaves alone: kinds on the body's off-limits list, animals in the body's own pens or in any closed enclosure with a fence or wall, and the
  last few adults of a kind so a breeding pair survives."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.built :as built]
            [jobs.lib.pen :as pen]
            [jobs.lib.storage :as storage]
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
  ::spare-pens is false) or :kept (a breeding pair: 2 up to `keep` adults of its kind in sight, unless starved?; a lone adult is no pair)."
  [keep kind adults-of-kind in-own-area? starved?]
  (cond (contains? (off-limits) kind) :off-limits
        (and (spare-pens?) in-own-area?) :pen
        (and (<= 2 adults-of-kind keep) (not starved?)) :kept))

(def wool-keep "Sheep of a colour combat.hunt spares (its :keep default for animals); shears spare them all." 2)

(defn takeable-adults
  "Of n adults of a kind in sight, how many a hunt sparing `keep` takes: a lone one is no pair (allowed-by :lone)."
  [keep n]
  (if (< n 2) n (max 0 (- n keep))))

(defn shears-at-hand
  "How the inventory gives shears: :carried, :craft (2 iron ingots), else nil."
  [inv]
  (cond (pos? (storage/carried inv "shears")) :carried
        (>= (storage/carried inv "iron_ingot") 2) :craft))

(defn allowed-by
  "Which rule let a hunt take an adult why left alone: :pen-taken (::spare-pens false and it stood in a pen), :lone (the only
  one of its kind in sight), :surplus (more than `keep` of its kind) or :starved (the kept pair, lifted by hunger)."
  [keep adults-of-kind in-own-area?]
  (cond (and in-own-area? (not (spare-pens?))) :pen-taken
        (< adults-of-kind 2) :lone
        (> adults-of-kind keep) :surplus
        :else :starved))

(def built-block #"_fence$|_fence_gate$|_wall$|_planks$|bricks?$|^cobblestone$|^mossy_cobblestone$|_slab$")

(defn fence-cells
  "The [x y z] cells of the fences, walls, gates and other player-made blocks beside a feet cell of the enclosure (head or foot height)."
  [c inside]
  (let [block-at (u/seen-block-at-fn (:primitives c))]
    (into #{}
          (for [[x y z] inside
                [dx dy dz] [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [1 1 0] [-1 1 0] [0 1 1] [0 1 -1]]
                :let [cell [(+ x dx) (+ y dy) (+ z dz)]]
                :when (some->> (block-at {:x (cell 0) :y (cell 1) :z (cell 2)}) .-name (re-find built-block))]
            cell))))

(defn fenced?
  "Whether a fence, wall, gate or other player-made block (planks, bricks, cobblestone) stands beside a feet cell of the
  enclosure: a pit or hollow closed by plain terrain is no pen."
  [c inside]
  (boolean (seq (fence-cells c inside))))

(defn own-built?
  "Whether every fence or wall block of the enclosure is one this body placed itself (jobs.lib.built): its own pen, whatever the zones say."
  [c inside]
  (let [fence (fence-cells c inside)
        own? (and (seq fence) (every? (built/cells (ctx/view c)) fence))]
    (when own? (built/use! c fence))
    own?))

(defn enclosure
  "The feet cells ([x y z] set) of the fenced enclosure around cell, an open gate counted shut; nil when there is none."
  [c cell]
  (let [answer (pen/check {:block-at (u/seen-block-at-fn (:primitives c)) :at {:x (cell 0) :y (cell 1) :z (cell 2)}
                           :max-cells (settings/get settings ::pen-max-cells)})
        inside (:inside answer)]
    (when (and (seq inside) (fenced? c inside)) inside)))

(defn cell-enclosed?
  "Whether the feet cell lies in a closed enclosure the body has seen with a fence, wall or gate in it: the pen check from it,
  the fenced enclosures found so far kept in `pens` (an atom of cell sets). With others-only? an enclosure built wholly by the body (own-built?) does not count."
  ([c pens cell] (cell-enclosed? c pens cell false))
  ([c pens cell others-only?]
   (or (boolean (some #(contains? % cell) @pens))
       (let [answer (pen/check {:block-at (u/seen-block-at-fn (:primitives c)) :at {:x (cell 0) :y (cell 1) :z (cell 2)}
                                :max-cells (settings/get settings ::pen-max-cells)})
             penned? (and (:closed? answer) (fenced? c (:inside answer))
                          (not (and others-only? (own-built? c (:inside answer)))))]
         (when penned? (swap! pens conj (:inside answer)))
         penned?))))

(defn enclosed?
  "Whether the animal stands inside a closed enclosure the body has seen with a fence, wall or gate in it (cell-enclosed? at its feet; others-only? as there)."
  ([c pens e] (enclosed? c pens e false))
  ([c pens e others-only?]
   (let [{:keys [x y z]} (u/pos-of (.-pos e))]
     (cell-enclosed? c pens [(js/Math.floor x) (js/Math.floor (+ y 0.01)) (js/Math.floor z)] others-only?))))

(defn penned?
  "Whether an adult animal stands in a zone or plan of the body's own or in a closed enclosure (see enclosed?)."
  [c pens e]
  (or (access/in-own-area? c (u/pos-of (.-pos e))) (enclosed? c pens e)))
