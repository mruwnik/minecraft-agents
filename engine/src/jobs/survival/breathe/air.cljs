(ns jobs.survival.breathe.air
  "The air search of jobs.survival.breathe, for a body that is drowning or enclosed: swim up a column, go-to air, dig a
  cap, make a pocket, dig the head free."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.breath :as breath]
            [jobs.lib.dig-look :as look]
            [jobs.lib.escape :as escape]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.breathe-pocket :as pocket]))

(a/defargs settings
  {::radius {:default 2 :doc "Columns this far sideways are searched for air." :spec (a/num-in 0 nil)}
   ::reach {:default 10 :doc "Blocks above the feet the air search climbs." :spec (a/num-in 0 nil)}
   ::air-radius {:default 8 :doc "When no air is near, a go-to child searches for air-reaching water this many blocks sideways." :spec (a/int-in 1 32)}
   ::depth {:default 24 :doc "Blocks below the feet a drowning body looks for seen air, the way it came (within air-radius sideways)." :spec (a/int-in 0 64)}
   ::max-cap-digs {:default 3 :doc "Cap digs one run makes at most." :spec (a/int-in 0 nil)}
   ::cap-dig-share {:default 0.5 :doc "Share of the air left (oxygen, then health) a cap dig may take; a slower dig is not started and the dig gives up at it." :spec (a/num-in 0 1)}})
(defn radius [] (settings/get settings ::radius))
(defn reach [] (settings/get settings ::reach))
(defn air-radius [] (settings/get settings ::air-radius))
(defn depth [] (settings/get settings ::depth))
(defn max-cap-digs [] (settings/get settings ::max-cap-digs))
(defn cap-dig-share [] (settings/get settings ::cap-dig-share))

(declare go!)

(defn rose?
  "A swim that ended with the feet at floor y is progress only above the best height reached so far (best, nil before
  the first swim) and the feet height the pass started at (fy): a body pushed back down between passes is not progress."
  [best fy y]
  (> y (max (or best fy) fy)))

(defn solid-at? [p cell] (breath/suffocates? p cell))

(defn status [r] (.-status r))

(defn cap-beyond-dig?
  "The own column over the head is water or air, unseen included, up to a seen block within rise that lies farther than
  the 3 cells dig-cap reaches: the body must rise under it first."
  [p head rise]
  (loop [k 0]
    (let [n (when (<= k (inc rise)) (breath/seen-water-name p (update head :y + k)))]
      (cond
        (> k (inc rise)) false
        (or (nil? n) (breath/passable-water-or-air? n)) (recur (inc k))
        :else (> k 3)))))

(defn ^:async swim-up!
  "Drowning: swim up the own column when it reaches air, else walk (through
  water, at the feet's height) to the nearest column that does (target, from breath/find-air!). With no air seen and no block seen over the own
  column, swim up anyway: a body in the dark learns by moving, as a player does; so does one under a seen cap more than 3
  above its head (cap-beyond-dig?), to dig it from below. True when the action succeeded (a swim
  counts when it surfaced or rose above the best height reached); :no-air
  when there is no air in reach; nil when the move failed (failure counted)."
  [c target]
  (let [p (:primitives c)
        pos (u/self-pos c)
        fx (js/Math.floor (:x pos))
        fy (js/Math.floor (:y pos))
        fz (js/Math.floor (:z pos))
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
      (and (nil? target) (breath/column-open? p fx fz fy (reach)))
      (or (await (swim!)) :no-air)

      (and (nil? target) (cap-beyond-dig? p (breath/eye-cell (.self p)) (reach)))
      (or (await (swim!)) :no-air)

      (nil? target) :no-air
      (breath/surface-in-column p fx fz fy (reach)) (await (swim!))

      :else
      ;; raw moveTo kept: an emergency step to air or out of water (range 0), where the planner may have no standable cell; no time for a plan.
      (= "arrived" (status (await (ctx/act c :moveTo (clj->js {:pos {:x (:x target) :y fy :z (:z target)}
                                                                :range 0}))))))))

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
          (contains? pocket/falling n) {:why :gravity}
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
  "Drowning with no air near: a go-to child to the nearest air-reaching surface within the air-radius setting, else to the nearest
  seen air below the feet joined to the body by water or air within the depth setting (breath/nearest-pocket: the way the body
  came), not failed before, when the swim there fits the air left and is not dearer than the cap dig (cap-dig-ms). True
  on arrival (the body has surfaced); nil when it did not, :none when there is no target. The target is remembered as
  tried either way, so a world that stays drowning does not loop on it."
  [c]
  (let [p (:primitives c)
        pos (u/self-pos c)
        failed (:failed-air (ctx/mem c))
        surface (breath/nearest-air p pos (air-radius) (reach) failed false)
        pocket (when-not surface (breath/nearest-pocket p pos (air-radius) (depth) failed))
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
  under water is 25 times slower than on land, so the dig is started only when its expected time
  fits share of the air left (breath/air-left-ms), and each dig gives up at what is left of that. True when a
  dig worked, so the next pass swims up; else the reason (see cap-above, :too-slow, or :dig-failed, also after
  :max-cap-digs digs that left the body drowning)."
  [c share]
  (let [p (:primitives c)
        self (.self p)
        head (breath/eye-cell self)
        _ (await (look-up! c head))
        {:keys [cells why]} (cap-above p head)
        _ (when (and cells (< (:cap-digs (ctx/mem c) 0) (max-cap-digs))) (await (tools/equip! c (u/seen-name p (first cells)))))
        air-ms (breath/air-left-ms self)
        allowed-ms (* share air-ms)]
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

(defn recovery-spot?
  "A seen still-water cell with room above it over a solid floor that does not burn: a pile left there lies where it can be fetched."
  [p cell]
  (let [b (u/seen-block p cell)
        floor (u/sensed p (update cell :y dec))]
    (boolean (and b (not (breath/suffocates? p (update cell :y inc))) (= "water" (.-name b)) (zero? (js/Number (or (some-> (.-properties b) .-level) 0)))
                  (not (true? (.-unknown floor))) (.-fullCube floor) (not (contains? breath/harmful-in-cell (.-name floor)))))))

(defn recovery-spot
  "The nearest recovery-spot? cell joined to from by seen water within steps cells; nil when from is one itself or none is."
  [p from steps]
  (when-not (recovery-spot? p from)
    (loop [frontier [from] seen #{from} k 0]
      (when (and (seq frontier) (< k steps))
        (let [next (->> (for [c frontier [dx dy dz] breath/neighbours] (-> c (update :x + dx) (update :y + dy) (update :z + dz)))
                        (remove seen)
                        distinct
                        (filter #(contains? breath/swimmable (breath/seen-water-name p %))))]
          (or (first (filter #(recovery-spot? p %) next))
              (recur next (into seen next) (inc k))))))))

(defn ^:async settle!
  "Drowning for good (the cap dig is too slow): with the air left, swim to the nearest recovery-spot? within it, so the
  drops can be fetched after; nothing when the body already lies in one or none is near."
  [c]
  (let [p (:primitives c)
        self (.self p)
        steps (js/Math.floor (/ (* (cap-dig-share) (breath/air-left-ms self)) (breath/swim-ms)))]
    (when-let [spot (recovery-spot p (breath/feet-cell self) steps)]
      ;; raw moveTo: the planner refuses a swim past the oxygen to a cell that is no air, and this one spends the health left
      (await (ctx/act c :moveTo (clj->js {:pos spot :range 0}))))))

(defn ^:async drown-out!
  "Drowning: to seen air near; with none seen (or after a failed try) a dry pocket where it stands, after the go-to to
  seen air further out; else swim up a column not seen capped, go-to air further out, dig up through a natural cap;
  with those spent a footing block for the pocket, then a cap dig that fits all the air left. True on progress; nil
  after a failed try, the pocket's and the cap's reasons kept for the stop."
  [c]
  (let [target (await (breath/find-air! c (u/self-pos c) (radius) (reach)))
        site (when (or (nil? target) (pos? (:failures (ctx/mem c) 0))) (await (pocket/site! c)))
        _ (when (:why site) (ctx/update-mem! c assoc :pocket (:why site)))
        far (when (:cells site) (await (go-air! c)))
        made (when (= :none far) (await (pocket/make! c site)))]
    (cond
      (or (true? far) (true? made)) true
      ;; the go-to moved the body: the site is read again next pass
      (and (:cells site) (nil? far)) nil
      :else
      (let [r (await (swim-up! c target))]
        (cond
          (not= :no-air r) r
          (true? (await (go-air! c))) true
          :else
          (let [d (await (dig-cap! c (cap-dig-share)))]
            (cond
              (true? d) true
              (await (pocket/footing! c)) true
              :else (let [d (if (= :too-slow d) (await (dig-cap! c 1)) d)]
                      (when-not (true? d) (ctx/update-mem! c assoc :cap d))
                      (when (= :too-slow d) (await (settle! c)))
                      (true? d)))))))))

(defn ^:async go!
  "One go-to child call to target: true when it reports arrival. A call that yields or does not end :done is a failed
  try (the walk-only go-to waits on nothing)."
  [c slot target range]
  (let [e (await (ctx/call c slot 'jobs.movement.go-to {:pos target :range range :ways #{:walk :clear-seal} :warn false :settings {:jobs.lib.cost.planner/lethal-air true}}))]
    (and (= :done (:kind e)) (boolean (:arrived (:data e))))))
