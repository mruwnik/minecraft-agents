(ns jobs.lib.danger
  "Whether a hostile mob is a real danger to the body (see jobs.lib.reach for the walk and arrow proofs).
  A melee mob is one when it has a walkable way to the body, a ranged mob when it has a line of fire; a mob walled off,
  across a pit, or with the body sealed in, is not.
  Candidates are the mobs the body knows of (known-hostiles): the perception's mob memory, seen or heard. A heard one
  is judged by its direction and band (mob-pos), never its exact place.
  The mobs of one query share block reads (reach/lookup) and walk proofs (reach.proofs)."
  (:require [engine.entity-observations :as obs]
            [jobs.lib.combat :as combat]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.proofs :as proofs]
            [jobs.lib.util :as u]))

(defn seen-mob?
  "Whether the body has seen or heard hostile e: the seen or heard flag of a known-hostiles entry, else (raw entity)
  its visible field."
  [e]
  (let [s (.-seen e)]
    (if (some? s) (or (true? s) (true? (.-heard e))) (true? (.-visible e)))))

(defn seen-only?
  "Whether the body has seen hostile e (not merely heard it): the seen flag of a known-hostiles entry, else (raw
  entity) its visible field."
  [e]
  (let [s (.-seen e)]
    (if (some? s) (true? s) (true? (.-visible e)))))

(defn in-tunnel?
  "Whether the body's cell is in a tunnel by kind-at: solid over the head and solid at both ends of one horizontal
  axis, at the feet and head cells."
  [kind-at [x y z]]
  (let [solid? #(keyword-identical? :solid (kind-at %1 %2 %3))]
    (and (solid? x (+ y 2) z)
         (or (and (solid? (dec x) y z) (solid? (inc x) y z) (solid? (dec x) (inc y) z) (solid? (inc x) (inc y) z))
             (and (solid? x y (dec z)) (solid? x y (inc z)) (solid? x (inc y) (dec z)) (solid? x (inc y) (inc z)))))))

(def band-distance
  "Blocks a heard mob's band stands for when a maths needs a place: half the near band's edge, twice it."
  {:near (/ obs/near-band 2) :far (* obs/near-band 2)})

(defn rough-pos
  "The place a remembered mob entry stands for: its :pos, else the point its band away from :from toward its
  direction (nil when it has neither)."
  [{:keys [pos direction band from]}]
  (or pos
      (when (and direction band from)
        (let [a (* (/ js/Math.PI 4) (.indexOf obs/directions direction))
              d (band-distance band)]
          (assoc from :x (+ (:x from) (* d (js/Math.sin a))) :z (- (:z from) (* d (js/Math.cos a))))))))

(defn mob-pos
  "Where the body takes hostile e to be: its exact place when seen, else (heard only) the rough spot its direction and
  band from the body give (rough-pos); the exact place of a heard mob is never read."
  [p e]
  (if (seen-only? e)
    (u/pos-of (.-pos e))
    (let [from (u/pos-of (.-pos (.self p)))]
      (rough-pos (assoc (obs/rough-hearing from (u/pos-of (.-pos e))) :from from)))))

(defn mob-distance
  "Blocks from the body to hostile e: exact when seen, else to its rough spot (mob-pos)."
  [p e]
  (if (seen-only? e)
    (.-distance e)
    (u/dist (u/pos-of (.-pos (.self p))) (mob-pos p e))))

(def known-scan "Blocks out to which the mobs a distance filter weighs are fetched." 64)

(defn known-hostiles
  "The hostiles the body knows of within radius (ranged ones within :ranged-radius), nearest first; a heard one by
  its band (mob-distance). Uses the perception's mob memory (p.knownMobs) when p has one, else the raw hostile list."
  [p radius {:keys [ranged-radius]}]
  (let [rr (or ranged-radius radius)
        within (fn [e] (<= (mob-distance p e) (if (combat/ranged? e) rr radius)))]
    (->> (combat/known-or-raw p (max known-scan radius rr))
         (filter within)
         (sort-by #(mob-distance p %))
         vec)))

(defn danger-in?
  "danger? reading blocks through kind-at (shared by the mobs of one query) and, with pr, sharing walk proofs. opts
  :arrow-at is the query's shared arrow lookup (reach/lookup p reach/arrow-kind-of); without it a ranged mob builds its own."
  ([p kind-at e opts] (danger-in? p kind-at nil e opts))
  ([p kind-at pr e {:keys [sight? arrow-at] :or {sight? true}}]
   (if (combat/ranged? e)
     (and (or (not sight?) (seen-only? e) (not (in-tunnel? kind-at (reach/cell-of (u/pos-of (.-pos (.self p)))))))
          (reach/line-of-fire? (or arrow-at (reach/lookup p reach/arrow-kind-of)) (mob-pos p e) (u/pos-of (.-pos (.self p)))))
     (and (or (seen-mob? e) (not sight?))
          (let [mob (reach/cell-of (mob-pos p e))
                body (reach/cell-of (u/pos-of (.-pos (.self p))))]
            (if pr (proofs/proved-way? kind-at pr mob body) (reach/way? kind-at mob body)))))))

(defn danger?
  "Whether hostile e (JS entity) is a real danger to the body.
  A ranged mob needs a line of fire and, when the body is in a tunnel (in-tunnel?), to have been seen, not only heard
  (unless :sight? is false). A melee mob needs a walkable way to the body and, unless :sight? is false, to
  have been seen (seen-mob?)."
  ([p e] (danger? p e {}))
  ([p e opts] (danger-in? p (reach/lookup p) e opts)))

(defn query-proofs [p] (proofs/proofs (reach/cell-of (u/pos-of (.-pos (.self p))))))

(defn dangers
  "The hostiles of known-hostiles (same opts) that are real dangers, nearest first."
  ([p radius opts] (dangers p radius opts {}))
  ([p radius opts danger-opts]
   (let [kind-at (reach/lookup p)
         pr (query-proofs p)
         danger-opts (assoc danger-opts :arrow-at (reach/lookup p reach/arrow-kind-of))]
     (filterv #(danger-in? p kind-at pr % danger-opts) (known-hostiles p radius opts)))))

(defn nearest-danger
  "The nearest of known-hostiles (same opts) that is a real danger, or nil. danger-opts as danger?, plus :skip, a set of
  mob ids to leave out. Stops at the first danger."
  [p radius opts {:keys [skip] :as danger-opts}]
  (let [skip (set skip)
        kind-at (reach/lookup p)
        pr (delay (query-proofs p))
        danger-opts (assoc (dissoc danger-opts :skip) :arrow-at (reach/lookup p reach/arrow-kind-of))]
    (some #(when (and (not (contains? skip (.-id %))) (danger-in? p kind-at @pr % danger-opts)) %)
          (known-hostiles p radius opts))))

(defn danger-near?
  "Whether a real danger (nearest-danger) is within radius, ranged mobs within :ranged-radius (default radius). The one
  definition of danger for the hostile-near trigger and fact and the tidy safety check; opts as nearest-danger's
  danger-opts."
  ([p radius ranged-radius] (danger-near? p radius ranged-radius {}))
  ([p radius ranged-radius danger-opts]
   (some? (nearest-danger p radius {:ranged-radius ranged-radius} danger-opts))))

(defn seen-hostiles
  "The hostiles the body knows of within 64 (known-hostiles: the perception's mob memory, seen or
  heard and remembered while likely still near, as a player would; none it never sensed). Primitives without that
  memory: the ones in sight now."
  [p]
  (let [known (known-hostiles p 64 {})]
    (if (.-knownMobs p) known (filterv seen-mob? known))))
