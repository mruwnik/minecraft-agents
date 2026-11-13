(ns jobs.lib.rooms
  "Closed rooms, for a flight and for shutting in: the body's home or any closed room it has seen (jobs.lib.reach.room/room: walls, a
  roof or none needed, doors that shut) that it gets into before every melee chaser reaches a door of it
  (jobs.lib.cost/reach-first?). Shared by jobs.survival.respond-flight and jobs.survival.enter-room."
  (:require [jobs.lib.block-sets :as bs]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.choose :as choose]
            [jobs.lib.click :as click]
            [jobs.lib.look :as look]
            [jobs.lib.pass :as pass]
            [jobs.lib.pillar :as pillar]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.util :as u]))

(defn mid [{:keys [x y z]}] {:x (+ x 0.5) :y y :z (+ z 0.5)})

(defn food "The body's food, 20 (full) when unknown." [p] (or (.-food (.self p)) 20))

(a/defargs settings
  {::door-radius {:default 32 :doc "Blocks from the body within which a seen door or gate may lead into a closed building to flee to." :spec (a/num-in 0 64)}})
(defn door-radius [] (settings/get settings ::door-radius))

(defn cell-key [{:keys [x y z]}] (str x "," y "," z))

(defn door-sides
  "The cells on either side of the lower half of a seen wooden door or gate {:pos :properties}, along its facing."
  [{:keys [pos properties]}]
  (when-let [[dx dz] (reach-room/door-panel-step (:facing properties))]
    [(-> pos (update :x + dx) (update :z + dz)) (-> pos (update :x - dx) (update :z - dz))]))

(defn starts
  "Cells a closed room may hold: home ({:x :y :z}, its feet cell, or nil) first, then the sides (door-sides) of the
  wooden doors and gates the body has seen within door-radius, nearest first. Each {:pos :home? :door} (:door the
  door's cell)."
  [p home]
  (let [doors (->> (look/seen-blocks p {:match #(and (or (str/ends-with? % "_door") (str/ends-with? % "_fence_gate"))
                                                     (not (str/starts-with? % "iron_")))
                                        :radius (door-radius) :properties? true})
                   (remove #(= "upper" (:half (:properties %)))))]
    (cond->> (for [d doors s (door-sides d)] {:pos s :home? false :door (:pos d)})
      home (cons {:pos home :home? true}))))

(def open-policy
  "Cells judged no closed room, kept in body memory so each new flight does not flood an open home again."
  {:cap 16 :ttl (* 2 60 1000)})

(defn known-open?
  "Whether a flight within open-policy's ttl judged cell pos no closed room."
  [c pos]
  (let [k (cell-key pos)]
    (boolean (some #(= k (:cell (:data %))) (mem/entries (ctx/view c) :refuge-open)))))

(defn closed-past-sight?
  "Whether room's walls hold past the edge of sight: a flood from it (doors shut) that also steps onto any column free
  at feet and head over a floor not sensed stays within reach-room/room-cells. Unseen floor inside walls changes nothing; past
  a gap the flood runs on over ground not seen, which no wall is known to bound."
  [p {:keys [cells]}]
  (let [kind-at (reach-room/shut-kind-at p)
        next-cells (fn [x y z]
                     (let [out (reach/forward kind-at x y z)]
                       (doseq [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                               :let [nx (+ x dx) nz (+ z dz)]
                               :when (and (reach/passable? kind-at nx y nz) (reach/passable? kind-at nx (inc y) nz)
                                          (nil? (u/seen-block p {:x nx :y (dec y) :z nz})))]
                         (.push out #js [nx y nz]))
                       out))]
    (some? (reach-room/flood-cells next-cells (first cells) reach-room/room-cells))))

(defn flood-refuge?
  "Whether cell pos holds a closed room (jobs.lib.reach.room/room, or look's :room when given) that stays closed past the
  edge of sight (closed-past-sight?)."
  [p pos look]
  (boolean (some->> (if look (:room look) (reach-room/room p pos)) (closed-past-sight? p))))

(defn refuge-key [{:keys [x y z]}] (str "refuge|" x "|" y "|" z))

(defn refuge-room?
  "flood-refuge? of cell pos, kept per body while the world near it stays the same (reach-room/kept-with: a flight asks for
  each door side from its price, its survey and every enter-room quote); not kept without a perception."
  ([p pos] (refuge-room? p pos nil))
  ([p pos look]
   (reach-room/kept p (refuge-key pos) (fn [_] (flood-refuge? p pos look)))))

(defn closed?
  "Whether cell pos holds a closed room (refuge-room?), kept in mem :refuge-rooms for the job; no closed room at all
  (reach-room/room nil) also in body memory (known-open?) for a while. :unseen, kept nowhere, when a cell of the room is not
  seen yet (reach-room/unseen-room?): judged again at the next look. :cut, kept nowhere, when a flood ran out of the
  search's meter (reach/*search*). keep? false keeps nothing."
  ([c pos] (closed? c pos true))
  ([c pos keep?]
   (let [k (cell-key pos)
         known (get (:refuge-rooms (ctx/mem c)) k)
         n (reach/cuts)]
     (cond
       (some? known) known
       (known-open? c pos) false
       :else (let [p (:primitives c) look (reach-room/look p pos) v (refuge-room? p pos look) unseen? (and (not v) (some? @(:unseen look)))]
               (cond
                 (not= n (reach/cuts)) :cut
                 unseen? :unseen
                 (not keep?) v
                 :else (do (ctx/update-mem! c assoc-in [:refuge-rooms k] v)
                           (when-not (or v (:room look)) (mem/write! (:store (:engine c)) :refuge-open {:cell k} open-policy))
                           v)))))))

(defn world-key
  "What the body's picture of the world stands on (reach-room/steady-key: a block changed or newly seen, a remembered
  cell's change chance moved a level); a constant offline, where nothing changes."
  [p]
  (or (some-> (aget p "perception") reach-room/steady-key) ""))

(defn room-doors
  "{:doors [...] :unseen? bool}: the doors of the room around cell pos as the body sees it now: reach-room/room's :doors when
  it is closed past the edge of sight (closed-past-sight?), the doors of what is seen when a cell of the room is not seen
  yet (:unseen? true); nil when it is no closed room or known-open?. A closed room's doors are kept in mem
  :refuge-room-doors while the world-key stays the same, so a wall broken or a gap filled since prices anew."
  [c pos]
  (when-not (known-open? c pos)
    (let [p (:primitives c) k (cell-key pos) wk (world-key p)
          kept (get (:refuge-room-doors (ctx/mem c)) k)]
      (if (= wk (:key kept))
        {:doors (:doors kept) :unseen? false}
        ;; one flood answers both: the closed room, else the room an unseen cell leaves open
        (let [{:keys [room unseen]} (reach-room/look p pos)]
          (if room
            (when (closed-past-sight? p room)
              (ctx/update-mem! c assoc-in [:refuge-room-doors k] {:key wk :doors (:doors room)})
              {:doors (:doors room) :unseen? false})
            (when-let [u @unseen] {:doors (:doors u) :unseen? true})))))))

(declare shut-by)

(defn gap-room
  "The closed room a hut with a bare doorway gap makes at cell pos once the gap is filled (doorway-room), nil when none;
  ::no-gap when no doorway shuts it."
  [c pos]
  (let [p (:primitives c)
        e (:cell (reach-room/unroofed-cell p pos nil true))
        d (when e (shut-by p pos e any?))]
    (if-not d
      ::no-gap
      (let [head (update d :y inc)
            plug? #{[(:x d) (:y d) (:z d)] [(:x head) (:y head) (:z head)]}
            base (reach-room/shut-kind-at p)
            kind-at (reach-room/room-kind-at p (reach-room/memo-kind-at (fn [x y z] (if (plug? [x y z]) :solid (base x y z)))))
            room (reach-room/room-from p kind-at (:x pos) (:y pos) (:z pos))
            inside (first (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                                :let [cell [(+ (:x d) dx) (:y d) (+ (:z d) dz)]]
                                :when (contains? (:cells room) cell)]
                            (zipmap [:x :y :z] cell)))]
        (when (and room inside)
          (assoc room :doors (conj (:doors room) {:cell d :inside inside}) :gap [d head]))))))

(defn doorway-room
  "The closed room a hut with a bare doorway gap makes at cell pos once the gap is filled, as reach-room/room's answer plus
  :doors [{:cell gap-feet :inside}], :gap [feet head] and :fill? (a block is carried to fill it; else the room stays open
  to one mob at a time); nil when it is none or a cell of it is unseen. A cell with no doorway that shuts it is kept in body
  memory :refuge-no-gap (open-policy) so a flight does not flood open ground again; the room in mem :refuge-gap-rooms
  while the world-key stays the same. :cut, kept nowhere, when a flood ran out of the search's meter."
  [c pos]
  (let [k (cell-key pos)
        wk (world-key (:primitives c))
        kept (get (:refuge-gap-rooms (ctx/mem c)) k)
        n (reach/cuts)]
    (when-not (some #(= k (:cell (:data %))) (mem/entries (ctx/view c) :refuge-no-gap))
      (when-let [room (if (= wk (:key kept))
                        (:room kept)
                        (let [r (gap-room c pos)]
                          (cond
                            (not= n (reach/cuts)) :cut
                            (= ::no-gap r) (do (mem/write! (:store (:engine c)) :refuge-no-gap {:cell k} open-policy) nil)
                            :else (do (ctx/update-mem! c assoc-in [:refuge-gap-rooms k] {:key wk :room r}) r))))]
        (cond-> room (map? room) (assoc :fill? (boolean (seq (lb/carried c (vec (or (ctx/use c :blocks) (choose/filler-names c :build))))))))))))

(defn flood-s
  "Seconds of CPU one room flood to its cap costs: reach-room/room-cells cells read at cost/room-read-s. A fresh judgement
  runs one (an open side's; a closed room's two run over its cells, mostly far under the cap)."
  []
  (* reach-room/room-cells (cost/room-read-s)))

(defn judge-s
  "Seconds of CPU a door side judged afresh may cost: its room's flood and closed-past-sight?'s (flood-s each)."
  []
  (* 2 (flood-s)))

(defn side-s
  "Seconds of CPU judging cell pos costs now: none when its floods are kept (refuge-room? and reach-room/look, kept
  while the world near it stays the same), else judge-s."
  [p pos]
  (if (and (reach-room/kept? p (refuge-key pos)) (reach-room/kept? p (reach-room/look-key pos))) 0 (judge-s)))

(defn look-s
  "Seconds of CPU a look's boundary (reach-room/boundary) around pos costs now: none when its floods are kept, else one flood-s."
  [p pos]
  (if (reach-room/boundary-kept? p pos) 0 (flood-s)))

(defn way-s
  "Seconds of CPU a walk search to a room's door (way-in) may cost: reach/node-budget cells at cost/walk-cell-s."
  []
  (* reach/node-budget (cost/walk-cell-s)))

(defn arrival-s
  "Seconds the first melee threat of threats (ranged ones shoot, they do not race: reaches-first?) takes to within its
  blow of the nearest of cells ({:x :y :z}), straight lines (cost/arrival-s); ##Inf with none."
  [p cells threats]
  (reduce min ##Inf (for [t (remove combat/ranged? threats) cell cells] (cost/arrival-s (cost/mob-kind t) (danger-q/mob-pos p t) cell))))

(defn lead-s
  "The chasers' lead at cells, the body blocks from them (cost/lead-s of arrival-s); ##Inf with no melee threat."
  [p cells threats blocks]
  (cost/lead-s (arrival-s p cells threats) blocks (food p)))

(defn here-s "The chasers' lead at the body (lead-s where it stands)." [p threats] (lead-s p [(u/pos-of (.-pos (.self p)))] threats 0))

(declare way-in race-won?)

(defn search-meter
  "A reach/meter of the one search a flight step's or a quote's searches make (cost/search-budget-s, Q141), spent-s spent:
  a flood's cell at cost/room-read-ceiling-s, a walk search's at cost/walk-cell-ceiling-s, a flee search's planner node at
  cost/search-cell-ceiling-s; the budget is a wall-time ceiling, so the upper-bound prices (flood-s and way-s price at the means)."
  [spent-s]
  (reach/meter (cost/search-budget-s) spent-s (cost/room-read-ceiling-s) (cost/walk-cell-ceiling-s) (cost/search-cell-ceiling-s)))

(defn step-spent-s "Seconds of CPU the flight step's searches have run (job memory :search-s, new-step! clears it)." [c] (:search-s (ctx/mem c) 0))

(defn spent!
  "Keep s as the flight step's searches' CPU, what search by (a keyword) ran since the last noted in the step's log
  (memory :search-log, step-searches!)."
  [c s by]
  (ctx/update-mem! c (fn [m] (-> (assoc m :search-s s) (update :search-log (fnil conj []) [by (- s (:search-s m 0))])))))

(defn new-step! "A new flight step: its searches start a fresh search-meter." [c] (ctx/update-mem! c #(-> % (assoc :search-s 0) (dissoc :search-log))))

(defn step-searches!
  "One debug retreat.step-searches of the step's log, what each search spent of its meter, ms, and what was left
  (:left-ms, at most 0 when the meter ran out); none when no search ran."
  [c]
  (when-let [log (seq (:search-log (ctx/mem c)))]
    (let [ms #(/ (js/Math.round (* 10000 %)) 10)]
      (ctx/emit! c :retreat.step-searches :debug {:searches (mapv (fn [[by s]] [by (ms s)]) log)
                                                  :left-ms (ms (- (cost/search-budget-s) (step-spent-s c)))}))
    (ctx/update-mem! c dissoc :search-log)))

(defn step-meter "The flight step's search-meter, from what its searches have run (step-spent-s)." [c] (search-meter (step-spent-s c)))

(defn spent-meter! "Keep what meter m has spent as the flight step's searches' CPU, by (spent!)." [c m by] (spent! c (reach/spent-s m) by))

(defn survey
  "The closed rooms to flee to, {:rooms [...]} (jobs.lib.reach.room/room plus :start, :home? and, when its walk search
  ran whole, uncut, :way-in, way-in's answer): the home's (home {:x :y :z} or nil) and those behind the doors the body has
  seen (starts), nearest cell first (the home on a tie); and {:dropped [{:at [x y z] :own? :why}]}, the cells that are
  none: :run-failed (a run to it failed this flight), :not-closed, :unseen (a cell of the room not seen yet), or
  :unjudged (not judged this step: a door side whose side-s does not pay, cost/refuge-search-pays?, against threats'
  lead at its door, lead-s straight, or fire, and the run it saves over a room with a run on; or a flood cut by the
  meter, a known closed room's too). Its floods and walk searches run under one search-meter from spent-s (default 0): each stops where it runs
  out, :spent-s what they all ran (:ways-s of it the walk searches). The home (judged whatever its price) and sides judged before or kept (side-s none)
  come first, then the rest nearest first; each closed room found gets its walk search (way-s) as verdicts prices it. A
  home with a bare doorway gap counts as a room (doorway-room: its :gap; :fill? when a block is carried to fill it).
  keep? false: the judgements but the home's are kept nowhere (closed?: the home is judged whatever its price, so a
  flight's steps judge it alike). Defaults: no threats, no fire, keep? true."
  ([c home] (survey c home {}))
  ([c home {:keys [threats fire keep? spent-s] :or {keep? true spent-s 0}}]
   (binding [reach/*search* (search-meter spent-s)]
     (let [p (:primitives c)
           me (u/self-pos c)
           failed (:refuges-failed (ctx/mem c) #{})
           here (here-s p threats)
           best (volatile! ##Inf)
           found (volatile! [])
           ways-s (volatile! 0)
           known (fn [pos] (get (:refuge-rooms (ctx/mem c)) (cell-key pos)))
           free? (fn [{:keys [pos home?]}] (or home? (some? (known pos)) (known-open? c pos) (zero? (side-s p pos))))
           judge (fn [{:keys [pos home? door]}]
                   (let [k (known pos)
                         at (mid (or door pos))
                         cost (side-s p pos)]
                     (cond
                       (some? k) (when-not k :not-closed)
                       (known-open? c pos) :not-closed
                       (or home? (zero? cost)
                           (cost/refuge-search-pays? (reach/spent-s) cost here (lead-s p [at] threats (u/dist me at)) (cost/run-saved-s (u/dist me at) @best (food p)) fire))
                       (case (closed? c pos (or keep? home?)) true nil :unseen :unseen :cut :unjudged :not-closed)
                       :else :unjudged)))
           with-way (fn [{:keys [home?] :as r}]
                      (let [doors (map (comp mid :cell) (:doors r))
                            blocks (reduce min ##Inf (map #(u/dist me %) doors))
                            cost (* (count (:doors r)) (way-s))
                            n (reach/cuts)]
                        (if-not (or home? (cost/refuge-search-pays? (reach/spent-s) cost here (lead-s p doors threats blocks) (cost/run-saved-s blocks @best (food p)) fire))
                          r
                          (let [s0 (reach/spent-s)
                                w (way-in p r)
                                _ (vswap! ways-s + (- (reach/spent-s) s0))]
                            (when (and w (race-won? p r w threats)) (vswap! best min (:steps w)))
                            ;; a door's search the meter cut may hold a shorter way: verdicts prices the room again
                            (cond-> r (= n (reach/cuts)) (assoc :way-in w))))))
           room-of (fn [{:keys [pos home? gap-room]}]
                     (when-not (some #(contains? (:cells %) (mapv pos [:x :y :z])) @found)
                       (let [n (reach/cuts)
                             r (or gap-room (reach-room/room p pos))]
                         (if (and (nil? r) (not= n (reach/cuts)))
                           ::cut
                           (when-let [r (some-> r (assoc :start pos :home? home?))]
                             (let [r (with-way r)] (vswap! found conj r) r))))))
           judged (->> (starts p home)
                       (map-indexed vector)
                       (sort-by (fn [[i s]] [(if (free? s) 0 1) i]))
                       (mapv (fn [[i {:keys [pos home?] :as s}]]
                               (let [why (if (failed (cell-key pos)) :run-failed (judge s))
                                     gap (when (and home? (#{:not-closed :unseen} why)) (doorway-room c pos))
                                     why (cond (= :cut gap) :unjudged gap nil :else why)
                                     s (cond-> (assoc s :why why) (map? gap) (assoc :gap-room gap))
                                     r (when (nil? why) (room-of s))]
                                 [i (cond (= ::cut r) (assoc s :why :unjudged) r (assoc s :room r) :else s)])))
                       (sort-by first)
                       (mapv second))]
       {:spent-s (reach/spent-s)
        :ways-s @ways-s
        :dropped (vec (for [{:keys [pos home? why]} judged :when why] {:at (mapv pos [:x :y :z]) :own? home? :why why}))
        :rooms (->> judged
                    (keep :room)
                    ;; the home's room keeps its :home? start over its door side's
                    (sort-by (fn [{:keys [cells home?]}] [(apply min ##Inf (map (fn [[x y z]] (u/dist me (mid {:x x :y y :z z}))) cells)) (if home? 0 1)]))
                    vec)}))))

(defn reaches-first?
  "Whether the body walks steps blocks before any melee threat (ranged ones shoot, they do not race), each at its own
  speed (the body's a flight's pace), gets to the nearest of cells (straight line): cost/reach-first?."
  [p steps cells threats]
  (let [racers (remove combat/ranged? threats)]
    (cost/reach-first? steps
                       (map (fn [t] (apply min (map #(u/dist (danger-q/mob-pos p t) (mid %)) cells))) racers)
                       (food p)
                       (map cost/mob-kind racers))))

(def door-glance-radius
  "Blocks from a building's door side within which a flight looks in once when the building was dropped :unseen."
  6)

(defn door-cell
  "The seen door or gate cell beside door side pos {:x :y :z}, nil when none."
  [p {:keys [x y z]}]
  (first (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
               :let [cell {:x (+ x dx) :y y :z (+ z dz)}]
               :when (some-> (u/seen-block p cell) .-name reach/openable?)]
           cell)))

(defn glance-steps
  "Blocks the body's time at door cell d from me weighs in the glance's race: the walk to the door, the two blocks through
  it and back to the spot, and the seconds of the click and of a look at each of the room's looks unseen sides (cells)
  from each of the two spots, at a flight's pace (cost/flee-pace food). A shut that waits on an animal is not counted: it comes
  after the chaser's reach is settled."
  [me d food looks]
  (+ (u/dist me (mid d)) 2
     (* (cost/flee-pace food) (cost/look-s (inc (* 2 looks))))))

(defn door-glances
  "The door sides survey dropped :unseen (a cell beside a seen door or gate), and the :unseen home's door sides (a side or
  the cell beyond its door one of the home room's unseen sides) a step's search left :unjudged, not looked into this
  flight (memory :door-glanced): each {:k :pos :d :looks :home?} (door-cell d, looks its unseen sides, :home? the home's own). Its floods run under
  reach/*search*."
  [c survey]
  (let [p (:primitives c)
        glanced (:door-glanced (ctx/mem c) #{})
        home-sides (when-let [h (first (filter #(and (:own? %) (= :unseen (:why %))) (:dropped survey)))]
                     (set (reach-room/unseen-sides p (let [[x y z] (:at h)] {:x x :y y :z z}))))
        home-door? (fn [{:keys [x y z]} d]
                     (some #(contains? home-sides %) [[x y z] [(- (* 2 (:x d)) x) y (- (* 2 (:z d)) z)]]))]
    (for [{[x y z] :at why :why own? :own?} (:dropped survey)
          :let [k (cell-key {:x x :y y :z z})
                pos {:x x :y y :z z}
                d (door-cell p pos)]
          :when (and d (not (glanced k)) (or (= :unseen why) (and (= :unjudged why) (home-door? pos d))))]
      {:k k :pos pos :d d :looks (count (reach-room/unseen-sides p pos)) :home? (boolean (or own? (= :unjudged why)))})))

(defn glance-at
  "The door side of door-glances the flight looks in at: the first within door-glance-radius whose
  glance-steps a melee threat does not reach the door first in (reaches-first?); nil when none."
  [c survey threats]
  (let [p (:primitives c)
        me (u/self-pos c)]
    (first (filter (fn [{:keys [pos d looks]}]
                     (and (<= (u/dist me pos) door-glance-radius)
                          (reaches-first? p (glance-steps me d (food p) looks) [d] threats)))
                   (door-glances c survey)))))

(defn glance-s
  "Seconds a glance at door side g (door-glances') takes the body: its glance-steps at a flight's pace."
  [c {:keys [d looks]}]
  (let [food (food (:primitives c))]
    (/ (glance-steps (u/self-pos c) d food looks) (cost/flee-pace food))))

(declare glance-through!)

(defn ^:async glance-in!
  "Once a flight per building: when survey dropped a door side (a cell beside a seen door or gate; a home cell with
  none is looked into from its door sides) only for :unseen cells, or left it :unjudged as a door side of the :unseen home, and the body is within door-glance-radius of it,
  open a shut door with a click (shut again after the looks), step into the doorway (a go-to child; a wall of a 1-wide door shows a wedge from
  outside) and look at the nearest unseen cell beside its room, again for each still unseen, at most as many looks at each
  spot as it had unseen sides when the glance was raced (glance-steps); cells a door gap shows only at a graze stay unseen, so
  the body then steps to the far side of the door and looks again (sight passes; the time is part of the flight). The door's :opened entry
  expires, so a cut flight's door is shut by the door-left trigger. Not when a melee threat reaches the door first (reaches-first?).
  The door side is glance-at's (glance-through! takes a given one). True when it looked: the caller surveys again."
  [c survey threats]
  (let [m (step-meter c)
        at (binding [reach/*search* m] (glance-at c survey threats))]
    (spent-meter! c m :glance)
    (await (glance-through! c at))))

(defn ^:async glance-through!
  "glance-in!'s look through door side at (a door-glances entry; nothing with nil): true when it looked."
  [c at]
  (let [p (:primitives c)
        me (u/self-pos c)]
    (when at
      (ctx/update-mem! c update :door-glanced (fnil conj #{}) (:k at))
      (let [d (:d at)
            walk-to! (fn ^:async w [cell]
                       (when-not (= (cell-key (u/self-pos c)) (cell-key cell))
                         (await (ctx/call c :door-glance 'jobs.movement.go-to {:pos (mid cell) :range 0.7 :ways #{:walk} :spend {:job-time 4 :tries 1}}))))
            look-at-sides! (fn ^:async l [n looked]
                             (loop [n n looked looked]
                               (let [me (u/self-pos c)
                                     target (->> (reach-room/unseen-sides p (:pos at))
                                                 (remove looked)
                                                 (sort-by #(u/dist me {:x (% 0) :y (% 1) :z (% 2)}))
                                                 first)]
                                 (if (and target (< n (:looks at)))
                                   (do (await (sense/look-at! c target))
                                       (recur (inc n) (conj looked target)))
                                   [n looked]))))
            beyond (let [{:keys [x y z]} (:pos at)] {:x (- (* 2 (:x d)) x) :y y :z (- (* 2 (:z d)) z)})
            door (u/seen-block p d)
            opened (when (and (= :openable (click/kind-of (.-name door))) (click/reached? :closed (click/props-of door)))
                     (let [r (await (pass/open-block! c d false))]
                       (when (= :opened (:result r)) (:column r))))]
        (await (walk-to! d))
        (let [[n _] (await (look-at-sides! 0 #{}))
              inner (when (reach-room/unseen-room? p (:pos at))
                      (if (> (u/dist me beyond) (u/dist me (:pos at))) beyond (:pos at)))
              looked? (if-not inner
                        (pos? n)
                        (do (await (walk-to! inner))
                            (await (look-at-sides! 0 #{}))
                            true))]
          (when opened (await (pass/shut-column! c opened)))
          looked?)))))

(defn inside?
  "Whether the body is in room (jobs.lib.reach.room/room): its feet cell or the cell it stands on (as go-to arrives) is one of
  the room's cells."
  [p {:keys [cells]}]
  (boolean (some #(contains? cells (mapv % [:x :y :z])) [(sh-sense/feet p) (reach/standing-cell p)])))

(defn in-home?
  "Whether the body is in the closed room that holds home ({:x :y :z} or nil); no other room is looked at."
  [c home]
  (let [p (:primitives c)]
    (boolean (when (and home (true? (closed? c home)))
               (some->> (reach-room/room p home) (inside? p))))))

(declare home-room)

(defn home-room!
  "Whether the body stands in the closed room of home ({:x :y :z} or nil): the room round its feet within r blocks across
  (reach-room/room-near, part of the step's one search) holds the home cell; nil when not known (no home, a cut flood).
  Kept for the feet cell while the world-key stays the same (memory :home-room, read by home-room); a cut flood leaves
  the answer from the same cell before it."
  [c home r]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        k (cell-key feet)
        wk (world-key p)
        kept (:home-room (ctx/mem c))]
    (cond
      (nil? home) (do (ctx/update-mem! c dissoc :home-room) nil)
      (= [k wk] [(:cell kept) (:key kept)]) (:in? kept)
      :else (let [m (step-meter c)
                  room (binding [reach/*search* m] (reach-room/room-near p feet r))
                  _ (spent-meter! c m :home-room)
                  cut? (pos? @(:cuts m))
                  in? (when-not cut? (boolean (some-> room :cells (contains? (mapv home [:x :y :z])))))]
              (cond
                (not cut?) (ctx/update-mem! c assoc :home-room {:cell k :key wk :in? in?})
                (not= k (:cell kept)) (ctx/update-mem! c dissoc :home-room))
              (if cut? (home-room c) in?)))))

(defn home-room
  "home-room!'s answer for where the body stands now, nil when it was not asked there."
  [c]
  (let [{:keys [cell in?]} (:home-room (ctx/mem c))]
    (when (= cell (cell-key (sh-sense/feet (:primitives c)))) in?)))

(defn way-in
  "The door of room the body walks to in fewest steps over the blocks it has seen (jobs.lib.reach.room/walk-steps to the room
  cell inside it): {:cell :inside :steps}, nil when it finds none. Its searches run under reach/*search*, a fresh
  search-meter when none is bound: those the meter cut leave the best of the rest. Kept per body from the feet cell
  while the world stays the same (reach-room/kept: a flight's decision and its first step ask it from the same cell)."
  [p room]
  (binding [reach/*search* (or reach/*search* (search-meter 0))]
    (let [feet (sh-sense/feet p)]
      (reach-room/kept p (str "way-in|" (cell-key feet) "|" (pr-str (map :inside (:doors room))))
                       (fn [_]
                         (->> (:doors room)
                              (keep (fn [d] (when-let [n (reach-room/walk-steps p feet (:inside d))] (assoc d :steps n))))
                              (sort-by :steps)
                              first))))))

(defn race-won?
  "Whether the body, at its own speed (food), walks way-in's steps before any melee threat gets to a door of room:
  reaches-first?."
  [p room {:keys [steps]} threats]
  (reaches-first? p steps (map :cell (:doors room)) threats))

(defn verdicts
  "What the flight makes of each of rooms (rooms, nearest first), {:verdicts [...] :spent-s}: [{:room r :inside? true}]
  alone for the room the body is in, else per room {:room r :way w} with :why it is not run to: :no-run (way? false: no
  run may be looked for now), :creeper (one chases: it would follow to the door and blow the door, chest and bed),
  :unpriced (way-in's way-s a door does not pay, cost/refuge-search-pays?, against threats' lead at its doors, lead-s
  straight, or fire, and the run it saves over the room a run is on to in fewest steps; or the meter cut its search
  before it found a way; the home's runs whatever its price, and a room survey already searched, :way-in, costs nothing), :no-way
  (way-in finds none) or :race-lost (a melee chaser gets to a door first: race-won?); no :why: a run there is on. Its
  walk searches run under one search-meter from spent-s, :spent-s what they all ran."
  ([p rooms threats way?] (verdicts p rooms threats way? 0 nil))
  ([p rooms threats way? spent-s fire]
   (binding [reach/*search* (search-meter spent-s)]
     (let [creeper? (some combat/creeper? threats)
           me (u/pos-of (.-pos (.self p)))
           here (here-s p threats)
           in (some #(when (inside? p %) %) rooms)
           vs (if in
                [{:room in :inside? true}]
                (loop [[r & more] rooms best ##Inf out []]
                  (if-not r
                    out
                    (let [cost (* (count (:doors r)) (way-s))
                          doors (map (comp mid :cell) (:doors r))
                          blocks (reduce min ##Inf (map #(u/dist me %) doors))]
                      (cond
                        (not way?) (recur more best (conj out {:room r :why :no-run}))
                        creeper? (recur more best (conj out {:room r :why :creeper}))
                        (not (or (:home? r) (contains? r :way-in) (cost/refuge-search-pays? (reach/spent-s) cost here (lead-s p doors threats blocks) (cost/run-saved-s blocks best (food p)) fire))) (recur more best (conj out {:room r :why :unpriced}))
                        :else (let [n (reach/cuts)
                                    w (if (contains? r :way-in) (:way-in r) (way-in p r))
                                    won? (and w (race-won? p r w threats))]
                                (recur more (if won? (min best (:steps w)) best)
                                       (conj out (cond
                                                   (and (nil? w) (not= n (reach/cuts))) {:room r :why :unpriced}
                                                   (nil? w) {:room r :why :no-way}
                                                   won? {:room r :way w}
                                                   :else {:room r :way w :why :race-lost})))))))))]
       {:verdicts vs :spent-s (reach/spent-s)}))))

(defn pick
  "Where the flight goes among verdicts: the room the body is in, else the one a run is on to in fewest steps, the home
  on a tie; nil when none."
  [verdicts]
  (->> verdicts
       (remove :why)
       (sort-by (comp :steps :way))
       first))

;; ------------------------------------------------------------------ doorways

(defn carried-door
  "A wooden door item the body carries, or nil."
  [p]
  (some #(let [n (:name %)] (when (and (.endsWith n "_door") (not= "iron_door" n)) n)) (u/inventory p)))

(defn around
  "The cell and its four horizontal neighbours."
  [{:keys [x y z] :as cell}]
  (cons cell (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]] {:x (+ x dx) :y y :z (+ z dz)})))

(defn shut-by
  "The first doorway (sh-sense/doorway?, not the feet, passing ok?) among the cells round e whose two cells [d, d above] shut the
  room: the flood from the feet has no unroofed cell once they are solid. An overhang beside a gap has one."
  [p feet e ok?]
  (first (filter #(and (not= feet %) (sh-sense/doorway? p %) (ok? %)
                       (nil? (:cell (reach-room/unroofed-cell p feet #{[(:x %) (:y %) (:z %)] [(:x %) (inc (:y %)) (:z %)]} true))))
                 (around e))))

(defn closed-home?
  "Whether the body stands in a home closed to the night but for a doorway: every cell of its room is roofed, or one
  doorway (sh-sense/doorway?) shuts it so that every cell is (shut-by). Under an overhang or a lone roof: false."
  [p]
  (let [feet (sh-sense/feet p)
        cell (:cell (reach-room/unroofed-cell p feet))]
    (or (nil? cell) (some? (shut-by p feet cell any?)))))

(def doorway-tried-policy
  "The :doorway-tried entries: doorways filled or refused tonight; a day at most."
  {:cap 4 :ttl 86400000})

(defn gap
  "The cells [feet head] that shut the room around the body: the doorway where the room's flood first leaves the roof
  (reach-room/unroofed-cell, closed flood or not: a walled yard is open to the night too) or beside that cell that shuts the
  room (shut-by), those still free; nil when the room is shut, no doorway shuts it (open ground), the body or a seen
  animal or mob stands in it, the place is refused, or this doorway was tried tonight (body memory :doorway-tried, doorway-tried-policy)."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        seen (delay (look/seen-entities p {:radius 8}))
        free? (fn [{:keys [x y z] :as cell}]
                (and (bs/air (u/seen-name p cell)) (not (pillar/mob-in @seen [x y z]))))]
    (when-let [e (:cell (reach-room/unroofed-cell p feet))]
      (when-let [d (shut-by p feet e free?)]
        (when-not (or (some #(= d (:data %)) (ctx/entries c :doorway-tried))
                      (access/trespass-refusal (access/rules-input c) :place d))
          (seq (filter free? [d (update d :y inc)])))))))


;; ------------------------------------------------------------------ named places

(defn roofed-places-in
  "The remembered places of names within radius and roofed within roof-height, as
  [{:name :pos}] nearest first. A roof cell the body has not seen is guessed solid (u/block-name-or): from the ground the
  cell over a home hides behind the roof's near edge; a column seen open is no roof."
  [c names radius]
  (let [p (:primitives c)
        roof-height (sh-sense/roof-height)
        here (sh-sense/feet p)
        roofed? (fn [pos] (some #(solid/solid? (u/block-name-or p (update pos :y + %) "stone")) (range 1 (inc roof-height))))]
    (->> names
         (keep (fn [nm] (when-let [pos (mem/place (ctx/view c) nm)] {:name nm :pos pos})))
         (filter (fn [{:keys [pos]}] (and (<= (u/dist here pos) radius) (roofed? pos))))
         (sort-by #(u/dist here (:pos %))))))

(defn ^:async route-cost
  "Seconds go-to's quote plans for the walk go-to-args, dark cells priced in (the planner's own cost), else what the dig route it
  would escalate to costs (:dig-seconds), or nil with no route."
  [c go-to-args]
  (let [r (:route (await (ctx/quote c 'jobs.movement.go-to go-to-args)))]
    (if (:found r) (+ (:seconds r) (or (:dark-seconds r) 0)) (:dig-seconds r))))
