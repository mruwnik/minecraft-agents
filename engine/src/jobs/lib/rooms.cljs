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
  seen yet (reach-room/unseen-room?): judged again at the next look. keep? false keeps nothing."
  ([c pos] (closed? c pos true))
  ([c pos keep?]
   (let [k (cell-key pos)
         known (get (:refuge-rooms (ctx/mem c)) k)]
     (cond
       (some? known) known
       (known-open? c pos) false
       :else (let [p (:primitives c) look (reach-room/look p pos) v (refuge-room? p pos look)]
               (cond
                 (and (not v) (some? @(:unseen look))) :unseen
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
        (let [room (reach-room/room p pos)]
          (if room
            (when (closed-past-sight? p room)
              (ctx/update-mem! c assoc-in [:refuge-room-doors k] {:key wk :doors (:doors room)})
              {:doors (:doors room) :unseen? false})
            (when-let [unseen (reach-room/unseen-room p pos)] {:doors (:doors unseen) :unseen? true})))))))

(declare shut-by)

(defn gap-known?
  "Whether doorway-room at cell pos answers from what is kept: body memory :refuge-no-gap, or the room kept in mem
  :refuge-gap-rooms while the world-key stays the same."
  [c pos]
  (let [k (cell-key pos)]
    (boolean (or (some #(= k (:cell (:data %))) (mem/entries (ctx/view c) :refuge-no-gap))
                 (= (world-key (:primitives c)) (:key (get (:refuge-gap-rooms (ctx/mem c)) k)))))))

(defn gap-room
  "The closed room a hut with a bare doorway gap makes at cell pos once the gap is filled (doorway-room), nil when none."
  [c pos]
  (let [p (:primitives c)
        e (:cell (reach-room/unroofed-cell p pos nil true))
        d (when e (shut-by p pos e any?))]
    (when-not d (mem/write! (:store (:engine c)) :refuge-no-gap {:cell (cell-key pos)} open-policy))
    (when d
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
  while the world-key stays the same (gap-known?)."
  [c pos]
  (let [k (cell-key pos)
        wk (world-key (:primitives c))
        kept (get (:refuge-gap-rooms (ctx/mem c)) k)]
    (when-not (some #(= k (:cell (:data %))) (mem/entries (ctx/view c) :refuge-no-gap))
      (when-let [room (if (= wk (:key kept))
                        (:room kept)
                        (let [r (gap-room c pos)] (ctx/update-mem! c assoc-in [:refuge-gap-rooms k] {:key wk :room r}) r))]
        (assoc room :fill? (boolean (seq (lb/carried c (vec (or (ctx/use c :blocks) (choose/filler-names c :build)))))))))))

(defn flood-s
  "Seconds of CPU one room flood to its cap costs: reach-room/room-cells cells read at cost/search-read-s. A fresh judgement
  runs one (an open side's; a closed room's two run over its cells, mostly far under the cap)."
  []
  (* reach-room/room-cells (cost/search-read-s)))

(defn judge-s
  "Seconds of CPU a door side judged afresh may cost: its room's flood and closed-past-sight?'s (flood-s each)."
  []
  (* 2 (flood-s)))

(defn side-s
  "Seconds of CPU judging cell pos costs now: none when its floods are kept (refuge-room? and reach-room/look, kept
  while the world near it stays the same), else judge-s."
  [p pos]
  (if (and (reach-room/kept? p (refuge-key pos)) (reach-room/kept? p (reach-room/look-key pos))) 0 (judge-s)))

(defn way-s
  "Seconds of CPU a walk search to a room's door (way-in) may cost: reach/node-budget cells at cost/search-cell-s."
  []
  (* reach/node-budget (cost/search-cell-s)))

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

(defn survey
  "The closed rooms to flee to, {:rooms [...]} (jobs.lib.reach.room/room plus :start, :home? and, when its walk search
  was paid for, :way-in, way-in's answer): the home's (home {:x :y :z} or nil) and those behind the doors the body has
  seen (starts), nearest cell first (the home on a tie); and {:dropped [{:at [x y z] :own? :why}]}, the cells that are
  none: :run-failed (a run to it failed this flight), :not-closed, :unseen (a cell of the room not seen yet), or
  :unjudged (a door side not judged this step: its side-s does not pay, cost/refuge-search-pays?, against threats' lead
  at its door, lead-s straight, or fire, and the run it saves over a room with a run on, or the step's CPU is at
  cost/search-budget-s). The home (always judged: once a flight, its answer is kept) and sides judged before or kept (side-s none) come first, then the rest nearest first; each closed room found gets its walk search (way-s) as verdicts prices it. :spent-s the CPU the step's searches ran: flood-s a fresh judgement, the home's doorway-room unless gap-known? (judge-s), each walk search's cells at cost/search-cell-s. A home with a bare doorway
  gap counts as a room (doorway-room: its :gap; :fill? when a block is carried to fill it). keep? false: the
  judgements are kept nowhere (closed?), so the flee price surveys what the flight's step then surveys. Defaults: no
  threats, no fire, keep? true."
  ([c home] (survey c home {}))
  ([c home {:keys [threats fire keep?] :or {keep? true}}]
   (let [p (:primitives c)
         me (u/self-pos c)
         failed (:refuges-failed (ctx/mem c) #{})
         here (here-s p threats)
         spent (volatile! 0)
         best (volatile! ##Inf)
         found (volatile! [])
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
                         (cost/refuge-search-pays? @spent cost here (lead-s p [at] threats (u/dist me at)) (cost/run-saved-s (u/dist me at) @best (food p)) fire))
                     (let [v (closed? c pos keep?)]
                       (when (pos? cost) (vswap! spent + (flood-s)))
                       (case v true nil :unseen :unseen :not-closed))
                     :else :unjudged)))
         with-way (fn [{:keys [home?] :as r}]
                    (let [doors (map (comp mid :cell) (:doors r))
                          blocks (reduce min ##Inf (map #(u/dist me %) doors))
                          cost (* (count (:doors r)) (way-s))]
                      (if-not (or home? (cost/refuge-search-pays? @spent cost here (lead-s p doors threats blocks) (cost/run-saved-s blocks @best (food p)) fire))
                        r
                        (let [w (way-in p r #(vswap! spent + (* % (cost/search-cell-s))))]
                          (when (and w (race-won? p r w threats)) (vswap! best min (:steps w)))
                          (assoc r :way-in w)))))
         room-of (fn [{:keys [pos home? gap-room]}]
                   (when-not (some #(contains? (:cells %) (mapv pos [:x :y :z])) @found)
                     (when-let [r (some-> (or gap-room (reach-room/room p pos)) (assoc :start pos :home? home?))]
                       (let [r (with-way r)] (vswap! found conj r) r))))
         judged (->> (starts p home)
                     (map-indexed vector)
                     (sort-by (fn [[i s]] [(if (free? s) 0 1) i]))
                     (mapv (fn [[i {:keys [pos home?] :as s}]]
                             (let [why (if (failed (cell-key pos)) :run-failed (judge s))
                                   gap-room (when (and home? (#{:not-closed :unseen} why)) (when-not (gap-known? c pos) (vswap! spent + (judge-s))) (doorway-room c pos))
                                   s (cond-> (assoc s :why (if gap-room nil why)) gap-room (assoc :gap-room gap-room))]
                               [i (cond-> s (nil? (:why s)) (assoc :room (room-of s)))])))
                     (sort-by first)
                     (mapv second))]
     {:spent-s @spent
      :dropped (vec (for [{:keys [pos home? why]} judged :when why] {:at (mapv pos [:x :y :z]) :own? home? :why why}))
      :rooms (->> judged
                  (keep :room)
                  ;; the home's room keeps its :home? start over its door side's
                  (sort-by (fn [{:keys [cells home?]}] [(apply min ##Inf (map (fn [[x y z]] (u/dist me (mid {:x x :y y :z z}))) cells)) (if home? 0 1)]))
                  vec)})))

(defn reaches-first?
  "Whether the body walks steps blocks before any melee threat (ranged ones shoot, they do not race), each at its own
  speed, gets to the nearest of cells (straight line): cost/reach-first?."
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
  from each of the two spots, at the body's own pace (food). A shut that waits on an animal is not counted: it comes
  after the chaser's reach is settled."
  [me d food looks]
  (+ (u/dist me (mid d)) 2
     (* (cost/body-speed food) (cost/look-s (inc (* 2 looks))))))

(defn ^:async glance-in!
  "Once a flight per building: when survey dropped a door side (a cell beside a seen door or gate; a home cell with
  none is looked into from its door sides) only for :unseen cells and the body is within door-glance-radius of it,
  open a shut door with a click (shut again after the looks), step into the doorway (a go-to child; a wall of a 1-wide door shows a wedge from
  outside) and look at the nearest unseen cell beside its room, again for each still unseen, at most as many looks at each
  spot as it had unseen sides when the glance was raced (glance-steps); cells a door gap shows only at a graze stay unseen, so
  the body then steps to the far side of the door and looks again (sight passes; the time is part of the flight). The door's :opened entry
  expires, so a cut flight's door is shut by the door-left trigger. Not when a melee threat reaches the door first (reaches-first?).
  True when it looked: the caller surveys again."
  [c survey threats]
  (let [p (:primitives c)
        me (u/self-pos c)
        glanced (:door-glanced (ctx/mem c) #{})
        at (first (for [{[x y z] :at why :why} (:dropped survey)
                        :let [k (cell-key {:x x :y y :z z})
                              pos {:x x :y y :z z}
                              d (door-cell p pos)
                              looks (when d (count (reach-room/unseen-sides p pos)))]
                        :when (and (= :unseen why) d (not (glanced k)) (<= (u/dist me pos) door-glance-radius)
                              (reaches-first? p (glance-steps me d (food p) looks) [d] threats))]
                    {:k k :pos pos :d d :looks looks}))]
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

(defn way-in
  "The door of room the body walks to in fewest steps over the blocks it has seen (jobs.lib.reach.room/walk-steps to the room
  cell inside it): {:cell :inside :steps}, nil when it finds no walk to any. expanded!, when given, is called with the cells
  each door's search expanded."
  ([p room] (way-in p room nil))
  ([p room expanded!]
   (->> (:doors room)
        (keep (fn [d] (when-let [n (reach-room/walk-steps p (sh-sense/feet p) (:inside d) expanded!)] (assoc d :steps n))))
        (sort-by :steps)
        first)))

(defn race-won?
  "Whether the body, at its own speed (food), walks way-in's steps before any melee threat gets to a door of room:
  reaches-first?."
  [p room {:keys [steps]} threats]
  (reaches-first? p steps (map :cell (:doors room)) threats))

(defn verdicts
  "What the flight makes of each of rooms (rooms, nearest first): [{:room r :inside? true}] alone for the room the body
  is in, else per room {:room r :way w} with :why it is not run to: :no-run (way? false: no run may be looked for now),
  :creeper (one chases: it would follow to the door and blow the door, chest and bed), :unpriced (way-in's way-s a door
  does not pay, cost/refuge-search-pays?, spent-s of the step's CPU already spent, against threats' lead at its doors, lead-s straight, or fire,
  and the run it saves over the room a run is on to in fewest steps; the home's is always searched, and a room
  survey already searched, :way-in, costs nothing), :no-way (way-in finds none) or :race-lost (a melee chaser gets to a
  door first: race-won?); no :why: a run there is on."
  ([p rooms threats way?] (verdicts p rooms threats way? 0 nil))
  ([p rooms threats way? spent-s fire]
   (if-let [r (some #(when (inside? p %) %) rooms)]
     [{:room r :inside? true}]
     (let [creeper? (some combat/creeper? threats)
           me (u/pos-of (.-pos (.self p)))
           here (here-s p threats)]
       (loop [[r & more] rooms spent spent-s best ##Inf out []]
         (if-not r
           out
           (let [cost (* (count (:doors r)) (way-s))
                 doors (map (comp mid :cell) (:doors r))
                 blocks (reduce min ##Inf (map #(u/dist me %) doors))]
             (cond
               (not way?) (recur more spent best (conj out {:room r :why :no-run}))
               creeper? (recur more spent best (conj out {:room r :why :creeper}))
               (not (or (:home? r) (contains? r :way-in) (cost/refuge-search-pays? spent cost here (lead-s p doors threats blocks) (cost/run-saved-s blocks best (food p)) fire))) (recur more spent best (conj out {:room r :why :unpriced}))
               :else (let [ran (volatile! 0)
                           w (if (contains? r :way-in) (:way-in r) (way-in p r #(vswap! ran + (* % (cost/search-cell-s)))))
                           won? (and w (race-won? p r w threats))]
                       (recur more (+ spent @ran) (if won? (min best (:steps w)) best)
                              (conj out (cond
                                          (nil? w) {:room r :why :no-way}
                                          won? {:room r :way w}
                                          :else {:room r :way w :why :race-lost}))))))))))))

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
