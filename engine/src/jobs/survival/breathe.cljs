(ns jobs.survival.breathe
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.tidy :as tidy]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.dig-look :as look]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.escape :as escape]
            [jobs.lib.land :as land]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.pace :as pace]
            [jobs.lib.result :as result]
            [jobs.lib.shore :as shore]
            [jobs.lib.sense :as sense]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.breath :as breath]))

(def doc
  "Get air when drowning or stuck inside a block, then out of the water, in one run (a reflex; never yields).
  Each pass reads the world again:
  - Drowning (in water, head under, oxygen below :min-oxygen): swim up the own column when it reaches air within
    :reach blocks, else step sideways, at the feet's height, to the nearest column within :radius that does; else a go-to
    child to the nearest air-reaching surface within :air-radius, else to the nearest seen air below the feet within :depth
    (the way the body came; a failed target is not tried again); else dig up through a cap within 3 blocks over the head
    when it is natural (jobs.lib.escape/natural?), not protected and not sand or gravel, one cell a pass, at most 3 digs
    (zones respected, trespass only as a last resort), with the best tool, and only when the dig fits ::cap-dig-share of the air left. Kelp, seagrass and upward bubble columns
    count as water; a downward (drag) bubble column is never swum into or crossed by a side step or a straight walk (nor
    its corner cut); a go-to child's planner rides one to its bottom and swims back up, so it routes round one when it can. Air is looked for only in cells the body has seen or looked at (perception memory), never through
    stone: it looks up the own column first, and when no block is seen over it (a dark column learns nothing) swims
    up it at once; only a seen block over the own column sends the looks to the neighbour columns.
  - Enclosed (head cell holds a suffocating block, see jobs.lib.breath): step to a side cell with room
    to stand, once; then dig the head block, the block above it if solid, and step up.
  - Surfaced and still in water: the ways out are read from what is seen; when none shows, the unseen cells that read touched are looked at (jobs.lib.sense/decide!, bounded) before the run gives up. Each pass takes the first way left: swim to the nearest shore cell (land with its
    rim at most one block above the water) within :shore-radius, another direction after each failed swim; a go-to
    child onto land within :search-radius (at most 3 targets per spot, a failed target excludes its direction);
    then a step block: with a carried pillar block (jobs.lib.escape/pillar-items), one placed (jobs.blocks.place child,
    zones respected) in the nearest top water cell within :shore-radius that has support and two air cells above,
    written to the scaffold ledger (purpose :breathe-step, for jobs.access.cleanup), at most 2 per spot; the next
    pass's shore swim climbs onto it; then a swim leg, a go-to to the farthest loaded surface water 8..:leg-length blocks out, outward from the run's
    start only, inside :swim-range of it, at most :max-legs legs (a leg that moved starts afresh at its end).
  Completed when the head is clear and, after a swim, the body stands on solid ground out of the water. Stopped
  :no_blocks when only a step cell was left and no pillar block is carried. Stopped
  :no_land_in_range (fields :searched :swum :legs :headings-failed :step, one of :no-wall :no-blocks :refused :failed) when every way is spent or the run has gone
  3 x :swim-range blocks; the searched area (the start and the failed headings) is remembered (:breathe-afloat, 5 min), so a refire there swims no leg the run already did. Stopped
  :no_air (fields :air-radius, :cap why no cap was dug: :no-cap :unseen :not-natural :protected :gravity :too-slow :dig-failed) or
  :no_way_out (with a warn) after three failed tries in the run. It never holds still while afloat.
  Memory: one :breathe entry per run.")

(a/defargs args
  {:min-oxygen {:doc "oxygen (of 20) below which being in water with the head submerged is drowning"
                :spec (a/num-in 0 20) :default breath/default-min-oxygen}
   :radius {:doc "columns this far sideways are searched for air" :spec (a/num-in 0 nil) :default 2}
   :reach {:doc "blocks above the feet the search climbs" :spec (a/num-in 0 nil) :default 10}
   :air-radius {:doc "when no air is near, a go-to child searches for air-reaching water this many blocks sideways, 1 to 32" :spec (a/int-in 1 32) :default 8}
   :depth {:doc "blocks below the feet a drowning body looks for seen air, the way it came (within :air-radius sideways)" :spec (a/int-in 0 64) :default 24}
   :shore-radius {:doc "after surfacing, land this many blocks sideways is swum to" :spec (a/num-in 0 nil) :default 6}
   :search-radius {:doc "land beyond :shore-radius up to this many blocks sideways is gone to with go-to" :spec (a/num-in 0 nil) :default 48}
   :leg-length {:doc "blocks one swim leg goes out at most" :spec (a/num-in 0 nil) :default 32}
   :swim-range {:doc "blocks from the run's start no swim leg goes beyond" :spec (a/num-in 0 nil) :default 96}
   :max-legs {:doc "swim legs one run makes at most" :spec (a/int-in 1 nil) :default 6}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(a/defargs settings
  {::max-cap-digs {:default 3 :doc "Cap digs one run makes at most." :spec (a/int-in 0 nil)}
   ::land-tries {:default 3 :doc "Go-to targets one spot tries before it swims a leg." :spec (a/int-in 0 nil)}
   ::min-leg {:default 8 :doc "Blocks a swim leg must move to count as moved." :spec (a/int-in 0 nil)}
   ::max-passes {:default 120 :doc "Passes one run makes before it stops :no_way_out, a safety net against a world that never changes." :spec (a/int-in 0 nil)}
   ::afloat-near {:default 16 :doc "Blocks around a spot in which a later run takes over the start and failed headings of an earlier stop." :spec (a/int-in 0 nil)}
   ::max-steps {:default 2 :doc "Step blocks one spot places at most." :spec (a/int-in 0 nil)}
   ::cap-dig-share {:default 0.5 :doc "Share of the air left (oxygen, then health) a cap dig may take; a slower dig is not started and the dig gives up at it." :spec (a/num-in 0 1)}})
(defn max-cap-digs [] (settings/get settings ::max-cap-digs))
(defn land-tries [] (settings/get settings ::land-tries))
(defn min-leg [] (settings/get settings ::min-leg))
(defn max-passes [] (settings/get settings ::max-passes))
(defn afloat-near [] (settings/get settings ::afloat-near))
(defn cap-dig-share [] (settings/get settings ::cap-dig-share))
(def afloat-policy {:cap 5 :ttl (* 5 60 1000)})

(def breathe-policy {:cap 20 :ttl (* 60 60 1000)})

(declare go!)

(defn land-footing?
  "The support under the feet is solid ground: not air, water, lava, fire or magma, and loaded. It is the cell below
  the feet cell, or the feet cell itself on a slab, farmland, path or carpet (feet above the cell's floor). A body
  pressed against a wall over water is onGround for a tick without it."
  [p self]
  (let [y (.. self -pos -y) fy (js/Math.floor y)
        below (u/feel-name p {:x (js/Math.floor (.. self -pos -x)) :y (if (> (- y fy) 0.001) fy (dec fy))
                               :z (js/Math.floor (.. self -pos -z))})]
    (boolean (and below (not (breath/air? below)) (not (contains? land/unsafe-below below))))))

(defn on-land?
  "Out of the water and standing on solid ground. A swim or walk that ends on a jump crest against a wall over water
  is out of the water for that moment, without a footing."
  [p]
  (let [self (.self p)]
    (and (not (.-inWater self)) (.-onGround self) (land-footing? p self))))

(defn surfaced-in-water?
  "A swim surfaced earlier in this job and the body is still in water, or not standing on land yet: a body bobbing
  at the surface or climbing out is out of the water for a moment on every crest, and one pressed against a wall
  over water is onGround without a footing. A ctx without job memory (no :view) has not surfaced."
  [c]
  (boolean (and (:view c)
                (:surfaced (ctx/mem c))
                (not (on-land? (:primitives c))))))

(defn check [c]
  (or (some? (breath/situation (:primitives c) (:min-oxygen (:args c))))
      (boolean (surfaced-in-water? c))
      (ctx/wait c {:reason :not-underwater})))

(defn rose?
  "A swim that ended with the feet at floor y is progress only above the best height reached so far (best, nil before
  the first swim) and the feet height the pass started at (fy): a body pushed back down between passes is not progress."
  [best fy y]
  (> y (max (or best fy) fy)))

(defn solid-at? [p cell] (breath/suffocates? p cell))

(defn status [r] (.-status r))

(defn surface-pos
  "pos with y raised to the feet cell at the top of the own water column (breath/surface-in-column), so a body that has
  sunk a few blocks below the surface still finds a bank flush with it; pos itself when the column has no surface."
  [p pos reach]
  (let [cell (breath/surface-in-column p (js/Math.floor (:x pos)) (js/Math.floor (:z pos)) (js/Math.floor (:y pos)) reach)]
    (if cell (assoc pos :y (:y cell)) pos)))

(defn same-way?
  "cell lies within 45 degrees of a failed target as seen from the offset (dx, dz): a swim toward that way already
  failed (a wall), so its neighbours would too."
  [dx dz failed fx fz]
  (some (fn [t]
          (let [tx (- (:x t) fx) tz (- (:z t) fz)
                dot (+ (* dx tx) (* dz tz))]
            (and (pos? dot)
                 (>= (* dot dot) (* 0.5 (+ (* dx dx) (* dz dz)) (+ (* tx tx) (* tz tz)))))))
        failed))

(defn shore-cell?
  "A land cell a swimming body can climb onto: a side neighbour has room (feet and head cells air or water) over water
  one or two blocks below the land cell's feet, so its rim is at most one block above the water. Land behind a wall,
  or on a wall two above the water, is not one."
  [p cell]
  (some (fn [[dx dz]]
          (let [n (-> cell (update :x + dx) (update :z + dz))
                at (fn [dy] (breath/seen-water-name p (update n :y + dy)))]
            (and (breath/passable-water-or-air? (at 0)) (breath/passable-water-or-air? (at 1))
                 (or (= "water" (at -1)) (= "water" (at -2))))))
        [[1 0] [-1 0] [0 1] [0 -1]]))

(defn nearest-land
  "The nearest land cell within radius sideways of self-pos, feet y from one below to two above, that keep? accepts;
  nil if none. A failed target (a seq of cells) and the cells in its direction are skipped."
  ([p self-pos radius] (nearest-land p self-pos radius nil (constantly true)))
  ([p self-pos radius failed keep?]
   (let [fx (js/Math.floor (:x self-pos))
         fy (js/Math.floor (:y self-pos))
         fz (js/Math.floor (:z self-pos))]
     (->> (for [[dx dz] (breath/columns radius)
                :when (not (same-way? dx dz failed fx fz))
                dy [0 -1 1 2]]
            {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)})
          (remove (set failed))
          (filter #(and (shore/standable? p %) (keep? p %)))
          first))))

(defn cap-beyond-dig?
  "The own column over the head is water or air, unseen included, up to a seen block within reach that lies farther than
  the 3 cells dig-cap reaches: the body must rise under it first."
  [p head reach]
  (loop [k 0]
    (let [n (when (<= k (inc reach)) (breath/seen-water-name p (update head :y + k)))]
      (cond
        (> k (inc reach)) false
        (or (nil? n) (breath/passable-water-or-air? n)) (recur (inc k))
        :else (> k 3)))))

(defn ^:async swim-up!
  "Drowning: look, then swim up the own column when it reaches air, else walk (through
  water, at the feet's height) to the nearest column that does. With no air seen and no block seen over the own
  column, swim up anyway: a body in the dark learns by moving, as a player does; so does one under a seen cap more than 3
  above its head (cap-beyond-dig?), to dig it from below. True when the action succeeded (a swim
  counts when it surfaced or rose above the best height reached); :no-air
  when there is no air in reach; nil when the move failed (failure counted)."
  [c]
  (let [{:keys [reach]} (:args c)
        p (:primitives c)
        pos (u/self-pos c)
        fx (js/Math.floor (:x pos))
        fy (js/Math.floor (:y pos))
        fz (js/Math.floor (:z pos))
        target (await (breath/find-air! c pos))
        ;; a swim that timed out but rose is progress, not a failed try: a deep column takes several 3 s swims
        swim! (fn ^:async f []
                (let [surfaced? (= "surfaced" (status (await (ctx/act c :swim #js {}))))
                      y (js/Math.floor (:y (u/self-pos c)))
                      rose (rose? (:best-y (ctx/mem c)) fy y)]
                  (if surfaced?
                    (ctx/update-mem! c #(-> % (assoc :surfaced true) (dissoc :best-y)))
                    (ctx/update-mem! c update :best-y #(max (or % y) y)))
                  (or surfaced? rose)))]
    (cond
      (and (nil? target) (breath/column-open? p fx fz fy reach))
      (or (await (swim!)) :no-air)

      (and (nil? target) (cap-beyond-dig? p (breath/eye-cell (.self p)) reach))
      (or (await (swim!)) :no-air)

      (nil? target) :no-air
      (breath/surface-in-column p fx fz fy reach) (await (swim!))

      :else
      ;; raw moveTo kept: an emergency step to air or out of water (range 0), where the planner may have no standable cell; no time for a plan.
      (= "arrived" (status (await (ctx/act c :moveTo (clj->js {:pos {:x (:x target) :y fy :z (:z target)}
                                                                :range 0}))))))))

(def falling "Blocks that fall when dug from below: they would drop into the water." #{"sand" "red_sand" "gravel" "suspicious_sand" "suspicious_gravel"})

(defn ^:async dig-out!
  "Enclosed: dig the head block (and the one above if solid), step up. True
  when the dig worked and the body stepped up; :dug when a dig worked but the step did not (the head cell may have
  refilled); false when nothing changed."
  [c]
  (let [p (:primitives c)
        head (breath/eye-cell (.self p))
        above (update head :y inc)
        _ (access/trespass! c "breathe" (:trespass (access/choose c :dig [(cond-> [head] (solid-at? p above) (conj above))] identity)))
        dug (status (await (tidy/dig! c head true)))]
    (if-not (contains? #{"dug" "missing"} dug)
      false
      (do (when (solid-at? p above)
            (await (tidy/dig! c above true)))
          ;; raw moveTo kept: an emergency step to air or out of water (range 0), where the planner may have no standable cell; no time for a plan.
          (if (= "arrived" (status (await (ctx/act c :moveTo (clj->js {:pos head :range 0})))))
            true
            :dug)))))

(defn cap-above
  "The own column's cap: {:cells [..]} to dig, or {:why kw} when none may be. The first block within 3 above the head
  that is neither air nor water is the cap (the only cell read, as seen); it must be natural, not protected and not a
  falling block; a cell not seen (look first, see look-up!) gives :unseen. What lies over the cap is learned by digging,
  a cell a pass."
  [p head]
  (let [cell (fn [k] (update head :y + k))]
    (loop [k 0]
      (let [n (when (<= k 3) (breath/seen-water-name p (cell k)))]
        (cond
          (> k 3) {:why :no-cap}
          (nil? n) {:why :unseen}
          (breath/passable-water-or-air? n) (recur (inc k))
          (escape/protected? n) {:why :protected}
          (not (escape/natural? n)) {:why :not-natural}
          (contains? falling n) {:why :gravity}
          :else {:cells [(cell k)]})))))

(defn ^:async look-up!
  "Look at each cell within 3 above head that the body has not seen, so cap-above reads what is there."
  [c head]
  (loop [k 0]
    (when (<= k 3)
      (let [cell (update head :y + k)]
        (when (look/unknown? (:primitives c) [(:x cell) (:y cell) (:z cell)])
          (await (look/look-at! c [(:x cell) (:y cell) (:z cell)])))
        (recur (inc k))))))

(defn ^:async cap-dig-ms
  "Milliseconds the cap dig of the own column would take, after looking up; nil when it may not be dug, would not fit
  ::cap-dig-share of the air left or the run has made :max-cap-digs digs."
  [c]
  (let [p (:primitives c)
        head (breath/eye-cell (.self p))
        _ (await (look-up! c head))
        {:keys [cells]} (cap-above p head)
        ms (reduce + (map #(.digTime p (clj->js %)) cells))]
    (when (and cells (< (:cap-digs (ctx/mem c) 0) (max-cap-digs))
               (<= ms (* (cap-dig-share) (breath/air-left-ms (.self p)))))
      ms)))

(defn ^:async go-air!
  "Drowning with no air near: a go-to child to the nearest air-reaching surface within :air-radius, else to the nearest
  seen air below the feet joined to the body by water or air within :depth (breath/nearest-pocket: the way the body
  came), not failed before, when the swim there fits the air left and is not dearer than the cap dig (cap-dig-ms). True
  on arrival (the body has surfaced); nil when it did not, :none when there is no target. The target is remembered as
  tried either way, so a world that stays drowning does not loop on it."
  [c]
  (let [{:keys [air-radius reach depth]} (:args c)
        p (:primitives c)
        pos (u/self-pos c)
        failed (:failed-air (ctx/mem c))
        surface (breath/nearest-air p pos air-radius reach failed false)
        pocket (when-not surface (breath/nearest-pocket p pos air-radius depth failed))
        swim-ms (* (:steps pocket 0) (breath/swim-ms))
        pocket? (and pocket
                     (< swim-ms (breath/air-left-ms (.self p)))
                     (let [dig-ms (await (cap-dig-ms c))] (or (nil? dig-ms) (<= swim-ms dig-ms))))
        target (or surface (when pocket? (:cell pocket)))]
    (if-not target
      :none
      (let [arrived? (await (go! c :air target 0))]
        (ctx/update-mem! c update :failed-air (fnil conj []) target)
        ;; a pocket below is the way the body came, not open water: no shore to head for from it
        (when arrived? (ctx/update-mem! c #(cond-> (dissoc % :best-y) surface (assoc :surfaced true))))
        (when arrived? true)))))

(defn ^:async dig-cap!
  "Drowning under a cap: dig it (and the solid cell over it) from below, with the best carried tool. A floating dig
  under water is 25 times slower than on land, so the dig is started only when its expected time, all cells together,
  fits ::cap-dig-share of the air left (breath/air-left-ms), and each dig gives up at what is left of that. True when a
  dig worked, so the next pass swims up; else the reason (see cap-above, :too-slow, or :dig-failed, also after
  :max-cap-digs digs that left the body drowning)."
  [c]
  (let [p (:primitives c)
        self (.self p)
        head (breath/eye-cell self)
        _ (await (look-up! c head))
        {:keys [cells why]} (cap-above p head)
        _ (when (and cells (< (:cap-digs (ctx/mem c) 0) (max-cap-digs))) (await (tools/equip! c (u/seen-name p (first cells)))))
        air-ms (breath/air-left-ms self)
        allowed-ms (* (cap-dig-share) air-ms)]
    (cond
      (not cells) why
      (<= (max-cap-digs) (:cap-digs (ctx/mem c) 0)) :dig-failed
      (< allowed-ms (reduce + (map #(.digTime p (clj->js %)) cells))) :too-slow
      :else
      (let [_ (ctx/update-mem! c update :cap-digs (fnil inc 0))
            _ (access/trespass! c "breathe" (:trespass (access/choose c :dig [cells] identity)))
            left-ms #(- allowed-ms (- air-ms (breath/air-left-ms (.self p))))
            dug (loop [todo cells ok? false]
                  (if-let [cell (first todo)]
                    (let [r (status (await (tidy/dig! c cell true {:timeoutS (/ (max 0 (left-ms)) 1000)})))]
                      (if (contains? #{"dug" "missing"} r) (recur (rest todo) true) ok?))
                    ok?))]
        (or dug :dig-failed)))))

(defn ^:async drown-out!
  "Drowning: swim up, else go-to air further out, else dig up through a natural cap. True on progress; nil after a
  failed try, the cap's reason kept for the stop."
  [c]
  (let [r (await (swim-up! c))]
    (if (not= :no-air r)
      r
      (if (true? (await (go-air! c)))
        true
        (let [d (await (dig-cap! c))]
          (when-not (true? d) (ctx/update-mem! c assoc :cap d))
          (true? d))))))

(def headings
  "Swim headings [dx dz], east first, then south, west, north, then the diagonals."
  [[1 0] [0 1] [-1 0] [0 -1] [1 1] [-1 1] [-1 -1] [1 -1]])

(defn hdist [a b] (js/Math.hypot (- (:x a) (:x b)) (- (:z a) (:z b))))

(defn legal-headings
  "The headings not in failed and not back toward start from pos: once the body has moved, only outward ones."
  [pos start failed]
  (let [ox (- (:x pos) (:x start)) oz (- (:z pos) (:z start))]
    (filterv (fn [[dx dz :as h]] (and (not (contains? failed h)) (>= (+ (* dx ox) (* dz oz)) 0))) headings)))

(defn after-leg
  "Job memory after a leg: one that moved starts afresh at its end (a way blocked before may be open from here), one
  that did not fails its heading."
  [m heading moved?]
  (if moved?
    (-> m (dissoc :failed-shores :failed-land :land-tries :failed-headings) (update :legs (fnil inc 0)))
    (update m :failed-headings (fnil conj #{}) heading)))

(defn initial-search
  "{:start :failed} from the latest :breathe-afloat entry within afloat-near of pos: the start of the run that stopped
  there (its swim-range still bounds the swim) and the headings it found failed; {:start nil :failed #{}} for none."
  [entries pos]
  (let [near (last (filter #(<= (hdist pos (:pos (:data %))) (afloat-near)) entries))
        d (:data near)]
    {:start (:start d) :failed (into #{} (map vec) (:headings d))}))

(defn leg-target
  "The farthest loaded surface water cell along heading, min-leg to :leg-length blocks out, inside :swim-range of
  start; nil if none."
  [p pos start [hx hz] {:keys [leg-length swim-range reach]}]
  (let [fx (js/Math.floor (:x pos)) fy (js/Math.floor (:y pos)) fz (js/Math.floor (:z pos))]
    (some (fn [k]
            (let [x (+ fx (* hx k)) z (+ fz (* hz k))
                  cell (when (<= (hdist {:x x :z z} start) swim-range) (breath/surface-in-column p x z fy reach))]
              (when (and cell (= "water" (u/seen-name p cell))) cell)))
          (range leg-length (dec (min-leg)) -1))))

(defn ^:async go!
  "One go-to child call to target: true when it reports arrival. A call that waits or is declined is a failed try
  (go-to with :escalate false waits on nothing)."
  [c slot target range]
  (let [r (await (ctx/call-child c slot 'jobs.movement.go-to {:pos target :range range :escalate false :warn false :costs {:lethal-air 1} :at-night :go}))]
    (and (= :done r) (boolean (:arrived (ctx/child-result c slot))))))

(defn stop-afloat!
  "Every way out of the water is spent: remember the spot's failed headings and stop with reason (:no_land_in_range, or
  :no_blocks when a step cell was left and no block to place)."
  [c reason]
  (let [{:keys [search-radius swim-range]} (:args c)
        m (ctx/mem c)
        pos (u/self-pos c)
        failed (:failed-headings m #{})]
    (ctx/remember! c :breathe-afloat {:pos pos :start (:start m pos) :reason reason :headings (vec failed)} afloat-policy)
    (result/stop! c reason
                  (str "afloat; no land within " search-radius " blocks or a swim of " swim-range
                       (when (= :no_blocks reason) ", and no block to step out on"))
                  :searched search-radius :swum (js/Math.round (hdist pos (:start m pos))) :legs (:legs m 0)
                  :headings-failed (count failed)
                  :step (cond (= :no_blocks reason) :no-blocks (:step-refused m) :refused (seq (:failed-steps m)) :failed :else :no-wall))))

(defn max-steps [] (settings/get settings ::max-steps))

(defn step-cell
  "The nearest top water cell within radius of the body that a step block can go in: water with two air cells above it
  and a neighbour to place against; not the body's own cell, not a failed one (a seq of cells). nil if none."
  [p pos radius failed]
  (let [fx (js/Math.floor (:x pos)) fy (js/Math.floor (:y pos)) fz (js/Math.floor (:z pos))
        self (u/pos-of (.-pos (.self p)))
        own {:x (js/Math.floor (:x self)) :y (js/Math.floor (:y self)) :z (js/Math.floor (:z self))}]
    (->> (for [[dx dz] (breath/columns radius) dy [0 -1]] {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)})
         (remove #(or (= own %) (contains? (set failed) %)))
         (filter #(and (= "water" (u/seen-name p %))
                       (breath/air? (u/seen-name p (update % :y inc)))
                       (breath/air? (u/seen-name p (update % :y + 2)))
                       (blocks/support? p %)))
         first)))

(defn ^:async step!
  "Place a pillar block in cell with a jobs.blocks.place child; the scaffold ledger gets the intent first and the
  settled entry after. :again either way; a refused or failed cell is not tried again."
  [c cell]
  (let [p (:primitives c)
        block-at (escape/block-at-of p)
        at [(:x cell) (:y cell) (:z cell)]
        args {:pos at :any-of escape/pillar-items :fetch false}
        item (blocks/chosen c escape/pillar-items)
        refused (= :not-allowed (:reason (blocks/child-wait c :step 'jobs.blocks.place args)))
        l (ledger/reconcile (ledger/open-entries (ctx/view c)) block-at)]
    (if refused
      (ctx/update-mem! c #(-> % (assoc :step-refused true) (update :failed-steps (fnil conj []) cell)))
      (let [intended (ledger/intend l {:cell at :item item :before (block-at at) :job (:id c) :purpose :breathe-step})
            _ (ledger/remember! c intended)
            r (await (ctx/call-child c :step 'jobs.blocks.place args))]
        (ledger/remember! c (ledger/reconcile intended block-at))
        (if (and (= :done r) (= item (block-at at)))
          (ctx/update-mem! c update :steps (fnil inc 0))
          (ctx/update-mem! c update :failed-steps (fnil conj []) cell))))
    :again))

(defn ^:async land-by-go-to!
  "A go-to child onto land at target. :done when the body then stands on land, else :again (target excluded)."
  [c target]
  (ctx/update-mem! c (fn [m] (-> m (update :land-tries (fnil inc 0)) (update :travelled (fnil + 0) (hdist (u/self-pos c) target)))))
  (await (go! c :land target 0))
  (if (on-land? (:primitives c))
    :done
    (do (ctx/update-mem! c update :failed-land (fnil conj []) target)
        :again)))

(defn ^:async swim-leg!
  "One swim leg with a go-to child to target along heading; the run's memory is updated by after-leg."
  [c heading target]
  (let [before (u/self-pos c)]
    (ctx/update-mem! c update :travelled (fnil + 0) (hdist before target))
    (await (go! c :leg target 2))
    (ctx/update-mem! c after-leg heading (>= (hdist before (u/self-pos c)) (min-leg)))
    (if (on-land? (:primitives c)) :done :again)))

(defn ^:async head-for-land!
  "Surfaced and still in water: the first way left of shore swim (nearest shore cell within :shore-radius, another
  direction after each failure; the swim primitive with toward climbs out onto a rim the planner cannot path to), a
  go-to onto land within :search-radius, a swim leg, then stopped :no_land_in_range. :done when out of the water."
  [c]
  (let [p (:primitives c)
        {:keys [reach shore-radius search-radius swim-range max-legs]} (:args c)
        pos (surface-pos p (u/self-pos c) reach)
        _ (when-not (:start (ctx/mem c))
            (let [{:keys [start failed]} (initial-search (ctx/entries c :breathe-afloat) pos)]
              (ctx/update-mem! c assoc :start (or start (u/self-pos c)) :failed-headings failed)))
        m (ctx/mem c)
        carried? (boolean (escape/pillar-item p))
        in-budget? (< (:travelled m 0) (* 3 swim-range))
        ways (fn []
               (let [shore (nearest-land p pos shore-radius (:failed-shores m) shore-cell?)
                     land (when (and in-budget? (< (:land-tries m 0) (land-tries)))
                            (nearest-land p pos search-radius (:failed-land m) (constantly true)))
                     stepcell (when (< (:steps m 0) (max-steps)) (step-cell p pos shore-radius (:failed-steps m)))
                     legs (when (and in-budget? (< (:legs m 0) max-legs))
                            (some (fn [h] (when-let [t (leg-target p pos (:start m) h (:args c))] [h t]))
                                  (legal-headings pos (:start m) (:failed-headings m #{}))))]
                 {:shore shore :land land :stepcell stepcell :legs legs}))
        ;; a way out is looked for in the cells the decision read and the body never saw before the run gives up
        {:keys [shore land stepcell legs]} (await (sense/decide! c :breathe-land ways
                                                                 {:retry? (fn [w] (not (or (:shore w) (:land w)
                                                                                           (and carried? (:stepcell w)))))}))]
    (cond
      shore
      (let [r (await (ctx/act c :swim (clj->js {:toward shore})))]
        (if (or (= "landed" (status r)) (on-land? p))
          :done
          (do (ctx/update-mem! c update :failed-shores (fnil conj []) shore)
              :again)))

      land (await (land-by-go-to! c land))
      (and stepcell carried?) (await (step! c stepcell))
      legs (await (swim-leg! c (first legs) (second legs)))
      stepcell (stop-afloat! c :no_blocks)
      :else (stop-afloat! c :no_land_in_range))))

(defn note!
  "Write the :breathe entry once per job instance."
  [c why]
  (when-not (:noted (ctx/mem c))
    (let [self (.self (:primitives c))]
      (ctx/remember! c :breathe {:why why :pos (u/pos-of (.-pos self)) :oxygen (.-oxygen self)} breathe-policy)
      (ctx/update-mem! c assoc :noted true))))

(defn fail!
  "One failed try in this run: :again until u/max-failures, then the warn kind and stopped with reason kind."
  [c kind text]
  (if-not (u/count-fail! c)
    :again
    (do (ctx/emit! c kind :warn {:tries (u/max-failures) :text text})
        (result/stop! c kind text))))

(defn fail-air!
  "fail! for :no_air: the stop says how far air was searched and why no cap was dug."
  [c]
  (if-not (u/count-fail! c)
    :again
    (let [text "drowning and no air within reach"
          fields {:air-radius (:air-radius (:args c)) :cap (:cap (ctx/mem c) :no-cap)}]
      (ctx/emit! c :no_air :warn (assoc fields :tries (u/max-failures) :text text))
      (apply result/stop! c :no_air text (mapcat identity fields)))))

(defn ^:async pass!
  "One look at the world and one try. :again or :held for another pass, else :done (perhaps stopped)."
  [c]
  (let [p (:primitives c)
        min-oxygen (:min-oxygen (:args c))
        why (breath/situation p min-oxygen)]
    (cond
      (and (nil? why) (surfaced-in-water? c)) (await (head-for-land! c))
      (nil? why) :done

      :else
      (let [side (when (and (= :enclosed why) (not (:side-tried (ctx/mem c))))
                   (breath/side-cell p (.self p)))]
        (ctx/hold-still! c nil)
        (note! c why)
        (if side
          (do (ctx/update-mem! c assoc :side-tried true)
              ;; raw moveTo kept: an emergency step to air or out of water (range 0), where the planner may have no standable cell; no time for a plan.
              (await (ctx/act c :moveTo (clj->js {:pos side :range 0})))
              :again)
          (let [drowning? (= :drowning why)
                ok (await (if drowning? (drown-out! c) (dig-out! c)))]
            (cond
              (nil? (breath/situation p min-oxygen)) :again
              (and drowning? ok) :again
              (= :dug ok) (do (ctx/update-mem! c dissoc :failures) :again)
              drowning? (fail-air! c)
              :else (fail! c :no_way_out "could not dig out of the block"))))))))

(defn ^:async round [c]
  (loop [i 0]
    (cond
      (not (ctx/alive? c)) :done
      (<= (max-passes) i) (result/stop! c :no_way_out "still not out after many tries")
      :else (let [r (await (pass! c))]
              (case r
                :held (recur (inc i))
                :again (do (await (pace/pace!)) (recur (inc i)))
                r)))))
