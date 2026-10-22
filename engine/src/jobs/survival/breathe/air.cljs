(ns jobs.survival.breathe.air
  "The air search of jobs.survival.breathe, for a body that is drowning or enclosed: swim up a column, go-to air, dig a
  cap, make a pocket, dig the head free."
  (:require [jobs.lib.cost.health :as cost-health]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.breath :as breath]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.look :as look]
            [jobs.lib.escape :as escape]
            [jobs.lib.swim :as swim]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.breathe-pocket :as pocket]))

(a/defargs settings
  {::radius {:default 2 :doc "Columns this far sideways are searched for air." :spec (a/num-in 0 nil)}
   ::depth {:default 24 :doc "Blocks below the feet a drowning body looks for seen air, the way it came (within air-radius sideways)." :spec (a/int-in 0 64)}
   ::slice-s {:default 10 :doc "Seconds one go-to child of a run may take; a body that did not get there is looked at again (hit? fight?) before the next try." :spec (a/num-in 0.1 nil)}
   ::max-cap-digs {:default 3 :doc "Cap digs one run makes at most." :spec (a/int-in 0 nil)}
   ::crawl-columns {:default 8 :doc "Brighter columns one run swims up blind under a roof, before it digs the cap." :spec (a/int-in 0 nil)}
   ::crawl-radius {:default 4 :doc "Columns this far sideways are searched for the light of a hole in a roof." :spec (a/int-in 0 nil)}
   ::cap-dig-share {:default 0.5 :doc "Share of the air left (oxygen, then health) a cap dig may take; a slower dig is not started and the dig gives up at it." :spec (a/num-in 0 1)}})
(defn radius [] (settings/get settings ::radius))
(def status swim/status)
(defn depth [] (settings/get settings ::depth))
(defn slice-s [] (settings/get settings ::slice-s))
(defn max-cap-digs [] (settings/get settings ::max-cap-digs))
(defn cap-dig-share [] (settings/get settings ::cap-dig-share))
(defn crawl-radius [] (settings/get settings ::crawl-radius))
(defn crawl-columns [] (settings/get settings ::crawl-columns))

(declare go!)

(def quick-dig-ms "A cap dig up to this long is made before the roof is felt along for a hole." 1500)

(defn solid-at? [p cell] (breath/suffocates? p cell))

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
          (not (rules/natural? n)) {:why :not-natural}
          (contains? pocket/falling n) {:why :gravity}
          :else {:cells [(cell k)]})))))

(defn ^:async look-up!
  "Look at each cell within 3 above head that the body has not seen, so cap-above reads what is there."
  [c head]
  (loop [k 0]
    (when (<= k 3)
      (let [cell (update head :y + k)]
        (when (dig-look/unknown? (:primitives c) [(:x cell) (:y cell) (:z cell)])
          (await (dig-look/look-at! c [(:x cell) (:y cell) (:z cell)])))
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

(def ^:private edge-looks
  "Cells looked at per round beside the water joined to the body, and the rounds."
  {:cells 6 :rounds 4})

(defn ^:async look-along-water!
  "Look at the unseen cells beside the water joined to the body, nearest toward (a cell or nil) first, so a flooded stair
  that goes on out of sight, or flows, is read before the body swims it; true when it looked at any."
  [c pos toward]
  (let [p (:primitives c)
        cells (breath/unseen-beside-joined p (js/Math.floor (:x pos)) (js/Math.floor (:y pos)) (js/Math.floor (:z pos))
                                           (swim/air-radius) (swim/air-radius) (:cells edge-looks) toward)
        unseen (filterv #(dig-look/unknown? p [(:x %) (:y %) (:z %)]) cells)]
    (doseq [cell unseen] (await (dig-look/look-at! c [(:x cell) (:y cell) (:z cell)])))
    (boolean (seq unseen))))

(defn ^:async go-air!
  "Drowning with no air near: a go-to child to the nearest air-reaching surface within the air-radius setting (also the
  air at the top of a flooded stair, joined by seen water, however high), else to the nearest
  seen air below the feet joined to the body by water or air within the depth setting (breath/nearest-pocket: the way the body
  came), not failed before, when the swim there fits the air left and is not dearer than the cap dig (cap-dig-ms). True
  on arrival (the body has surfaced); nil when it did not, :none when there is no target. The target is remembered as
  tried either way, so a world that stays drowning does not loop on it."
  [c]
  (let [p (:primitives c)
        pos (u/self-pos c)
        failed (:failed-air (ctx/mem c))
        column (fn [] (breath/nearest-air p pos (swim/air-radius) (swim/reach) failed false))
        find-surface #(or (column) (breath/flood-air p pos (swim/air-radius) (swim/air-radius) failed))
        until (breath/look-until (.self p))
        surface (loop [k 0]
                  (let [found (find-surface)]
                    (if (and (< k (:rounds edge-looks)) (< (js/Date.now) until) (await (look-along-water! c pos found)))
                      (recur (inc k))
                      found)))
        stair? (and surface (nil? (column)))
        pocket (when-not surface (breath/nearest-pocket p pos (swim/air-radius) (depth) failed))
        swim-ms (* (:steps pocket 0) (breath/swim-ms))
        pocket? (and pocket
                     (< swim-ms (breath/air-left-ms (.self p)))
                     (let [dig-ms (await (cap-dig-ms c))] (or (nil? dig-ms) (<= swim-ms dig-ms))))
        target (or surface (when pocket? (:cell pocket)))]
    (if-not target
      :none
      (let [arrived? (or (await (go! c :air target 0))
                         ;; raw moveTo kept: the go-to planner may find no route up a flooded stair it cannot see whole (range 0, no time for a plan)
                         (and stair? (= "arrived" (swim/status (await (ctx/act c :moveTo (clj->js {:pos {:x (+ 0.5 (:x target)) :y (:y target) :z (+ 0.5 (:z target))} :range 0 :timeoutS 6})))))))]
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

(defn crawl-columns-near
  "The water columns (feet and head cell seen water) within ::crawl-radius of the body at its height, not tried in this run and
  brighter than the body's own cell, brightest first, then nearest: light seeping down a hole in the roof shows in the water under it."
  [c]
  (let [p (:primitives c)
        raw (look/raw-of p)
        feet (breath/feet-cell (.self p))
        tried (set (:crawled (ctx/mem c)))
        r (crawl-radius)
        glow (fn [{:keys [x y z]}] (if (and raw (look/light-known? raw x y z)) (bit-and (.lightAt raw x y z) 15) 0))
        own (glow feet)
        open? (fn [cell] (every? #(contains? breath/swimmable (breath/seen-water-name p (update cell :y + %))) [0 1]))]
    (->> (for [dx (range (- r) (inc r)) dz (range (- r) (inc r))
               :let [cell (-> feet (update :x + dx) (update :z + dz))]
               :when (and (not (contains? tried [(:x cell) (:z cell)])) (> (glow cell) own) (open? cell))]
           cell)
         (sort-by (fn [cell] [(- (glow cell)) (+ (js/Math.abs (- (:x cell) (:x feet))) (js/Math.abs (- (:z cell) (:z feet))))])))))

(defn ^:async crawl-roof!
  "Under a roof with no air seen but light in the water (a hole in the dark shows nothing from the side): swim to the brighter
  water column near the body (crawl-columns-near) and up it blind, at most ::crawl-columns of them. True when a swim rose or surfaced; nil when none did."
  [c]
  (let [p (:primitives c)]
    (loop [k 0]
      (let [feet (breath/feet-cell (.self p))
            next (first (crawl-columns-near c))]
        (when (and next (< k (crawl-columns)) (pos? (breath/air-left-ms (.self p))))
          (ctx/update-mem! c update :crawled (fnil into []) [[(:x feet) (:z feet)] [(:x next) (:z next)]])
          (ctx/emit! c :breathe.crawl :info {:text (str "no air seen under the roof: swimming to the light at " (:x next) " " (:z next)) :target next})
          (let [y0 (:y (u/self-pos c))
                _ (await (ctx/act c :moveTo (clj->js {:pos next :range 0 :timeoutS 2})))
                surfaced? (= "surfaced" (status (await (ctx/act c :swim #js {}))))]
            (if (or surfaced? (> (:y (u/self-pos c)) (+ y0 1.2)))
              (do (when surfaced? (ctx/update-mem! c assoc :surfaced true)) true)
              (recur (inc k)))))))))

(defn recovery-spot?
  "A seen still-water cell with room above it over a solid floor that does not burn: a pile left there lies where it can be fetched."
  [p cell]
  (let [b (u/seen-block p cell)
        floor (u/sensed p (update cell :y dec))]
    (boolean (and b (not (breath/suffocates? p (update cell :y inc))) (= "water" (.-name b)) (zero? (js/Number (or (some-> (.-properties b) .-level) 0)))
                  (not (true? (.-unknown floor))) (.-fullCube floor) (not (contains? bs/harms-in-cell (.-name floor)))))))

(defn recovery-spot
  "The nearest recovery-spot? cell joined to from by seen water within steps cells; nil when from is one itself or none is."
  [p from steps]
  (when-not (recovery-spot? p from)
    (loop [frontier [from] seen #{from} k 0]
      (when (and (seq frontier) (< k steps) (< (count seen) breath/flood-cells))
        (let [next (into [] (comp (mapcat (fn [c] (map (fn [[dx dy dz]] (-> c (update :x + dx) (update :y + dy) (update :z + dz))) breath/neighbours)))
                                                 (remove seen)
                                                 (distinct)
                                                 (filter #(contains? breath/swimmable (breath/seen-water-name p %))))
                                           frontier)]
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
  (let [target (await (breath/find-air! c (u/self-pos c) (radius) (swim/reach)))
        site (when (or (nil? target) (pos? (:failures (ctx/mem c) 0))) (await (pocket/site! c)))
        _ (when (:why site) (ctx/update-mem! c assoc :pocket (:why site)))
        far (when (:cells site) (await (go-air! c)))
        made (when (= :none far) (await (pocket/make! c site)))]
    (cond
      (or (true? far) (true? made)) true
      ;; the go-to moved the body: the site is read again next pass
      (and (:cells site) (nil? far)) nil
      :else
      (let [r (await (swim/swim-up! c target))]
        (cond
          ;; a swim up a column that was not seen to reach air and failed leaves a flooded stair to go-air!, when one is seen
          (and (not= :no-air r)
               (let [self (u/self-pos c) fx (js/Math.floor (:x self)) fy (js/Math.floor (:y self)) fz (js/Math.floor (:z self))]
                 (or (true? r)
                     (breath/straight-up-reaches-air? (:primitives c) fx fy fz (swim/reach))
                     (nil? (breath/flood-air (:primitives c) self (swim/air-radius) (swim/air-radius) (:failed-air (ctx/mem c)))))))
          ;; a swim that rose and did not surface came up under something: dig it now, before the body sinks back for the next search
          (do (when (and (true? r) (not (:surfaced (ctx/mem c))))
                (let [ms (await (cap-dig-ms c))]
                  (when-not (and ms (<= ms quick-dig-ms) (true? (await (dig-cap! c (cap-dig-share)))))
                    (when-not (true? (await (crawl-roof! c)))
                      (await (dig-cap! c (cap-dig-share)))))))
              r)
          (true? (await (go-air! c))) true
          (true? (await (crawl-roof! c))) true
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
  (let [e (await (ctx/call c slot 'jobs.movement.go-to {:pos target :range range :ways #{:walk :clear-seal} :floor cost-health/spend-down :spend {:job-time (slice-s)} :settings {:jobs.lib.cost.planner/lethal-air true :jobs.lib.walk.world/dive-pause-s 0}}))]
    (and (= :done (:kind e)) (boolean (:arrived (:data e))))))
