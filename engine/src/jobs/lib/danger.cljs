(ns jobs.lib.danger
  "Whether a hostile mob is a real danger to the body (see jobs.lib.reach for the walk and arrow proofs).
  A melee mob is one when it has a walkable way to the body, a ranged mob when it has a line of fire; a mob walled off,
  across a pit, or with the body sealed in, is not.
  Candidates are the mobs the body knows of (known-hostiles): the perception's mob memory, seen or heard. A heard one
  is judged by its direction and band (mob-pos), never its exact place; in closed quarters (closed-quarters?) a heard melee
  mob counts only beside the body.
  The mobs of one query share block reads (reach/lookup) and walk proofs (reach.proofs)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.entity-observations :as obs]
            [jobs.lib.body :as body]
            [jobs.lib.combat :as combat]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.proofs :as proofs]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::hostile-radius {:default 8 :doc "Blocks within which a known hostile mob that can reach the body counts as a danger (the hostile-near trigger and go-to's cut check read it)." :spec (a/int-in 0 nil)}
   ::ranged-radius {:default 16 :doc "Blocks within which a known ranged mob with a line of fire counts as a danger." :spec (a/int-in 0 nil)}
   ::fight-reserve {:default 4 :doc "Health a fight must be expected to leave for the hostile reflexes to take it (respond-to-hostile and retreat)." :spec (a/num-in 0 20)}
   ::known-scan {:default 64 :doc "Blocks out to which the mobs a distance filter weighs are fetched." :spec (a/int-in 1 nil)}
   ::close-hearing {:default 2 :doc "Blocks within which a heard mob is loud enough to be beside the body (heard-close?)." :spec (a/num-in 0 nil)}
   ::quarters-reach {:default 3 :doc "Blocks within which a roof and walls every way make the body's place closed quarters (closed-quarters?)." :spec (a/int-in 1 nil)}})

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

(defn quarters-reach [] (settings/get settings ::quarters-reach))

(defn closed-quarters?
  "Whether the body's cell [x y z] is in closed quarters by kind-at, where a sound alone does not show a mob is near: a
  tunnel (solid over the head and at both ends of one horizontal axis, feet and head cells), or a small room or pocket
  (solid over the head and a wall, solid at feet and head height, three of the four ways along the axes (a door may
  open the fourth), all within quarters-reach). Outside or in a large cave it is not."
  [kind-at [x y z]]
  (let [solid? #(keyword-identical? :solid (kind-at %1 %2 %3))
        r (range 1 (inc (quarters-reach)))
        wall? (fn [[dx dz]] (some #(let [cx (+ x (* % dx)) cz (+ z (* % dz))] (and (solid? cx y cz) (solid? cx (inc y) cz))) r))]
    (or (and (solid? x (+ y 2) z)
             (or (and (solid? (dec x) y z) (solid? (inc x) y z) (solid? (dec x) (inc y) z) (solid? (inc x) (inc y) z))
                 (and (solid? x y (dec z)) (solid? x y (inc z)) (solid? x (inc y) (dec z)) (solid? x (inc y) (inc z)))))
        (and (some #(solid? x (+ y 1 %) z) r)
             (<= 3 (count (filter wall? [[1 0] [-1 0] [0 1] [0 -1]])))))))

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

(defn known-scan [] (settings/get settings ::known-scan))
(defn close-hearing [] (settings/get settings ::close-hearing))
(defn hostile-radius [] (settings/get settings ::hostile-radius))
(defn ranged-radius [] (settings/get settings ::ranged-radius))
(defn fight-reserve [] (settings/get settings ::fight-reserve))

(defn radius-in "The :radius of args, else the ::hostile-radius setting." [args] (or (:radius args) (hostile-radius)))
(defn ranged-radius-in "The :ranged-radius of args, else the ::ranged-radius setting." [args] (or (:ranged-radius args) (ranged-radius)))
(defn reserve-in "The :reserve of args, else the ::fight-reserve setting." [args] (or (:reserve args) (fight-reserve)))

(defn known-hostiles
  "The hostiles the body knows of within radius (ranged ones within :ranged-radius), nearest first; a heard one by
  its band (mob-distance). Uses the perception's mob memory (p.knownMobs) when p has one, else the raw hostile list."
  [p radius {:keys [ranged-radius]}]
  (let [rr (or ranged-radius radius)
        within (fn [e] (<= (mob-distance p e) (if (combat/ranged? e) rr radius)))]
    (->> (combat/known-or-raw p (max (known-scan) radius rr))
         (filter within)
         (sort-by #(mob-distance p %))
         vec)))

(defn way-distance
  "Blocks from point q to the straight segment from a to b ({:x :y :z})."
  [a b q]
  (let [d (mapv #(- (% b) (% a)) [:x :y :z])
        len2 (reduce + (map * d d))
        t (if (zero? len2) 0 (max 0 (min 1 (/ (reduce + (map * d (map #(- (% q) (% a)) [:x :y :z]))) len2))))]
    (u/dist q (zipmap [:x :y :z] (map #(+ (%1 a) (* t %2)) [:x :y :z] d)))))

(defn hostiles-near-way
  "The hostiles the body knows of (known-hostiles: seen or heard, a heard one at its rough spot) within radius
  (ranged ones within :ranged-radius) of the straight way from the body to goal ({:x :y :z}), nearest the way first."
  [p goal radius {:keys [ranged-radius]}]
  (let [rr (or ranged-radius radius)
        body (u/pos-of (.-pos (.self p)))
        off (fn [e] (way-distance body goal (mob-pos p e)))]
    (->> (known-hostiles p (+ (u/dist body goal) (max radius rr)) {})
         (filter #(<= (off %) (if (combat/ranged? %) rr radius)))
         (sort-by off)
         vec)))

(defn calm-enderman?
  "Whether e is an enderman the body's client says is not angry (its angry field is false; one without the field counts as angry):
  it does nothing unless provoked, so it is no danger."
  [e]
  (and (combat/enderman? e) (false? (.-angry e))))

(defn heard-close?
  "Whether hostile e is heard only and within close-hearing of the body: loud enough that a player knows it is beside them."
  [p e]
  (and (true? (.-heard e)) (not (seen-only? e))
       (<= (u/dist (u/pos-of (.-pos (.self p))) (u/pos-of (.-pos e))) (close-hearing))))

(defn beside-toward?
  "Whether a mob could stand beside the body's cell [x y z] toward compass direction d, within 45 degrees of it: a
  standable cell one down, level or one up in one of those three neighbour columns. From there it hits the body, even
  across a corner it cannot walk round."
  [kind-at [x y z] d]
  (let [i (.indexOf obs/directions d)]
    (boolean (some (fn [j]
                     (let [a (* (/ js/Math.PI 4) j)
                           nx (+ x (js/Math.round (js/Math.sin a)))
                           nz (- z (js/Math.round (js/Math.cos a)))]
                       (some #(reach/standable? kind-at nx (+ y %) nz) [-1 0 1])))
                   [(dec i) i (inc i)]))))

(def swell-range "Blocks within which a creeper swells and its blast reaches." 7)

(defn open-cell-from
  "The cell [x y z] of a heard mob's rough spot (mob-pos) when it is not rock; else the first non-solid cell outward from the body
  along the same line, up to half obs/near-band beyond it; nil when the line is rock to there (no mob stands in rock)."
  [kind-at body-pos rough]
  (let [dx (- (:x rough) (:x body-pos)) dz (- (:z rough) (:z body-pos))
        len (js/Math.hypot dx dz)
        cell-at (fn [d] (reach/cell-of (assoc rough :x (+ (:x body-pos) (* d (/ dx len))) :z (+ (:z body-pos) (* d (/ dz len))))))
        open? (fn [[x y z]] (not (keyword-identical? :solid (kind-at x y z))))]
    (if (zero? len)
      (reach/cell-of rough)
      (->> (range len (+ len (/ obs/near-band 2) 1) 1) (map cell-at) (filter open?) first))))

(defn danger-in?
  "danger? reading blocks through kind-at (shared by the mobs of one query) and, with pr, sharing walk proofs. opts
  :arrow-at is the query's shared arrow lookup (reach/lookup p reach/arrow-kind-of); without it a ranged mob builds its own.
  :ignore-heard? leaves out every mob the body has not seen (a muted heard mob, jobs.lib.body/heard-muted?).
  :way? false takes a melee mob without a way to the body (on a pillar or behind a door) as one: it stands where the body's walk will land.
  A melee mob heard but not seen counts as seen (by its rough spot) outside or in a large cave; in closed quarters
  (closed-quarters?) it is one only when heard close (heard-close?) and it could stand beside the body that way
  (beside-toward?): a heard mob in clear, lit sight is already seen (engine.perception), so a look would find nothing.
  A fusing creeper (hissing, seen or heard) within swell-range is always one; beyond it the usual tests apply."
  ([p kind-at e opts] (danger-in? p kind-at nil e opts))
  ([p kind-at pr e {:keys [sight? arrow-at ignore-heard? way?] :or {sight? true way? true}}]
   (cond
     (calm-enderman? e) false
     ;; it swells only while it has a line of sight to the body within 7 blocks, so its blast reaches; heard only, its
     ;; rough spot may fall in rock and fail the way test
     (and (true? (.-fusing e)) (<= (mob-distance p e) swell-range)) true
     (and ignore-heard? (not (seen-only? e))) false
     (combat/ranged? e)
     (and (or (not sight?) (seen-only? e) (<= (mob-distance p e) obs/near-band))
          (or (not sight?) (seen-only? e) (not (closed-quarters? kind-at (reach/cell-of (u/pos-of (.-pos (.self p)))))))
          (reach/line-of-fire? (or arrow-at (reach/lookup p reach/arrow-kind-of)) (mob-pos p e) (u/pos-of (.-pos (.self p)))))
     :else
     (and (or (seen-mob? e) (not sight?))
          (or (not way?)
              (let [from (u/pos-of (.-pos (.self p)))
                    body (reach/cell-of from)
                    mob (if (seen-only? e) (reach/cell-of (mob-pos p e)) (open-cell-from kind-at from (mob-pos p e)))]
                (or (and (heard-close? p e)
                         (beside-toward? kind-at body (:direction (obs/rough-hearing from (u/pos-of (.-pos e))))))
                    (and mob
                         (or (seen-only? e) (not sight?) (not (closed-quarters? kind-at body)))
                         (if pr (proofs/proved-way? kind-at pr mob body) (reach/way? kind-at mob body))))))))))

(defn danger?
  "Whether hostile e (JS entity) is a real danger to the body.
  A ranged mob needs a line of fire, to have been seen or (heard only) to be in the near band, and, when the body is in closed quarters (closed-quarters?), to have been seen, not only heard
  (unless :sight? is false). A melee mob needs a walkable way to the body and, unless :sight? is false, to
  have been sensed (heard only: see danger-in?)."
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

(defn hostile-near?
  "Whether the hostile-near trigger would hold (triggers.survival.hostile-near; args :radius, :ranged-radius,
  :visible-only): a dead body sees no danger, heard mobs are muted while jobs.lib.body/heard-muted?."
  [world memory args]
  (boolean (and (not (body/dead? memory))
                (danger-near? world (:radius args (hostile-radius)) (:ranged-radius args (ranged-radius))
                              {:sight? (:visible-only args true) :ignore-heard? (body/heard-muted? memory)}))))
