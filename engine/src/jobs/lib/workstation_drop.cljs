(ns jobs.lib.workstation-drop
  "Picking up a broken workstation's drop, for the calls that break one (jobs.village.roll, jobs.village.pen). The
  caller books in its memory :stand (where the break was made from) and clears fresh-keys at each break; pos is the
  workstation cell {:x :y :z}."
  (:require [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]))

(def fresh-keys "Memory of one break's pickup, dropped at the next break." [:stand :crouched :reach-tried :drop-last :to-drop-walked])

(defn on-cell?
  "Whether the body's feet or head fill the cell pos."
  [c pos]
  (let [[x y z] (b/feet-cell c)]
    (and (= x (:x pos)) (= z (:z pos)) (contains? #{(:y pos) (dec (:y pos))} y))))

(defn ^:async flush-to-cell!
  "A body in the cell beside the workstation steps to its edge nearest the workstation, so the pickup reach (hitbox grown by a block)
  covers the whole workstation cell and the drop is taken where it falls, even when a slab keeps the body out of the cell."
  [c pos]
  (let [[x y z] (b/feet-cell c)
        dx (- (:x pos) x)
        dz (- (:z pos) z)]
    (when (and (= y (:y pos)) (= 1 (+ (Math/abs dx) (Math/abs dz))))
      (await (ctx/act c :moveTo #js {:pos #js {:x (+ x 0.5 (* 0.2 dx)) :y y :z (+ z 0.5 (* 0.2 dz))} :range 0.1})))))

(defn ^:async to-stand!
  "Back to where the break was made from when the body is elsewhere: placing from there would shut it in behind the
  workstation. :again, :continue, or nil when it is there (or not known)."
  [c]
  (let [stand (:stand (ctx/mem c))]
    (when (and stand (not (u/within? (u/self-pos c) stand 1.5)))
      (let [r (await (child/call-walk* c :to-stand {:pos stand :range 1 :ways #{:walk}}))]
        (cond (= :continue r) :continue
              (:arrived (:data r)) :again
              :else nil)))))

(defn seen-drop
  "Where the nearest seen item of item within 4 blocks of the cell lies, or nil."
  [c pos item]
  (->> (look/seen-items (:primitives c) {:radius 4 :max 32})
       (filter #(and (= item (.-name (.-item %))) (u/within? (u/pos-of (.-pos %)) pos 4)))
       (sort-by #(u/dist (u/pos-of (.-pos %)) pos))
       first
       (#(some-> % .-pos u/pos-of))))

(defn drop-spot
  "Where the drop lies, for the body to reach: seen-drop, else the cell beyond the workstation from where the break
  was made (a drop slides on, away from the digger). Nil when neither is known."
  [c pos item]
  (let [stand (:stand (ctx/mem c))]
    (or (seen-drop c pos item)
        (when stand
          (let [dx (- (+ (:x pos) 0.5) (:x stand))
                dz (- (+ (:z pos) 0.5) (:z stand))
                [sx sz] (if (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])]
            {:x (+ (:x pos) 0.5 sx) :y (:y pos) :z (+ (:z pos) 0.5 sz)})))))

(defn drop-cell [c pos item]
  (let [spot (or (drop-spot c pos item) pos)]
    {:x (js/Math.floor (:x spot)) :y (js/Math.floor (:y spot)) :z (js/Math.floor (:z spot))}))

(defn far-side
  "The point 2 blocks beyond spot on the line from the body through it, along the longer axis: the other side of the wall or cell the drop lies in."
  [c spot]
  (let [{:keys [x z]} (u/self-pos c)
        dx (- (:x spot) x)
        dz (- (:z spot) z)]
    (if (>= (js/Math.abs dx) (js/Math.abs dz))
      (assoc spot :x (+ (:x spot) (* 2 (js/Math.sign dx))))
      (assoc spot :z (+ (:z spot) (* 2 (js/Math.sign dz)))))))

(defn crouch-decider
  "A steer decide function: sneaking, straight to each [x z hold] of legs in turn, held there hold ticks; a leg not reached in
  40 ticks is left as it is. Done after the last leg."
  [legs]
  (let [leg (volatile! 0)
        ticks (volatile! 0)
        held (volatile! 0)]
    (fn [js-pose]
      (let [[tx tz hold] (get legs @leg)
            x (.-x js-pose) z (.-z js-pose)
            at? (and (<= (js/Math.abs (- tx x)) 0.1) (<= (js/Math.abs (- tz z)) 0.1))]
        (vswap! ticks inc)
        (when at? (vswap! held inc))
        (if (or (> @held hold) (>= @ticks 40))
          (do (vswap! leg inc) (vreset! ticks 0) (vreset! held 0)
              (if (< @leg (count legs)) #js {:controls #js {:sneak true :forward false}} #js {:done true}))
          (if at?
            #js {:controls #js {:sneak true :forward false}}
            #js {:controls #js {:sneak true :forward true} :yaw (js/Math.atan2 (- x tx) (- z tz))}))))))

(defn ^:async crouch-in!
  "Take a drop that slid into the cell behind the emptied workstation cell (a villager's cell): sneak from the break's stand into
  the workstation cell, up against its far side, so the pickup reach covers the whole cell behind, hold a second and sneak back.
  A crouching body is 1.5 high, so it fits under a top slab over the cell where a villager (1.95) does not: the cell stays
  shut and nothing is dug. Once a break. :again (after a walk back to the stand, or the crouch), or nil."
  [c pos item]
  (let [stand (:stand (ctx/mem c))]
    (when (and stand (not (:crouched (ctx/mem c))))
      (or (await (to-stand! c))
          (do (ctx/update-mem! c assoc :crouched true)
              (when (u/within? (u/self-pos c) stand 1.5)
                (let [dx (- (+ (:x pos) 0.5) (:x stand))
                      dz (- (+ (:z pos) 0.5) (:z stand))
                      [sx sz] (if (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])
                      in [(+ (:x pos) 0.5 (* 0.2 sx)) (+ (:z pos) 0.5 (* 0.2 sz))]]
                  (ctx/emit! c :roll.crouch-in :info {:pos pos :text (str "the " item " drop is out of reach: sneaking into its emptied cell to take it")})
                  (await (ctx/act c :steer (walk/steer-args 8 (crouch-decider [[(:x stand) (:z stand) 0] (conj in 20) [(:x stand) (:z stand) 0]]))))
                  :again)))))))

(defn ^:async reach-drop!
  "Take the drop through a gap in a wall (a back passage behind a sealed cell): walk (go-to, walk only) within pickup range
  of the drop's spot, then to the far side of it from where the body stood, pressing up against whatever keeps the body
  out, so the pickup box reaches through. Each way once per cell the drop lies in (a drop that moved is tried again).
  :again, :continue, or nil when both were tried or no spot is known."
  [c pos item]
  (let [spot (drop-spot c pos item)
        cell (when spot (mapv js/Math.floor ((juxt :x :y :z) spot)))
        tried (:reach-tried (ctx/mem c) #{})
        way (when spot (some #(when-not (tried [% cell]) %) [:near :far]))]
    (when way
      (ctx/update-mem! c update :reach-tried (fnil conj #{}) [way cell])
      (let [r (await (child/call-walk* c (keyword (str "reach-drop-" (name way) "-" (count tried)))
                                       (if (= :near way)
                                         {:pos spot :range 2.5 :ways #{:walk}}
                                         {:pos (far-side c spot) :range 1.2 :ways #{:walk}})))]
        (if (= :continue r)
          :continue
          (do (when (:arrived (:data r))
                (await (ctx/act c :moveTo #js {:pos #js {:x (:x spot) :y (:y spot) :z (:z spot)} :range 0.1})))
              (await (child/pause! c 1 :picking-up-the-drop))
              :again))))))

(defn ^:async wait-on-cell!
  "Stand on the emptied cell when the body can enter it (one walk a break), and wait a second each time the drop is
  seen somewhere new (it slides or falls). :again, :continue, or nil when the drop lies where it lay (or is not seen):
  waiting longer changes nothing."
  [c pos item]
  (let [m (ctx/mem c)
        seen (some-> (seen-drop c pos item) (#(mapv js/Math.floor ((juxt :x :y :z) %))))]
    (cond
      (and (not (on-cell? c pos)) (not (:to-drop-walked m)))
      (do (ctx/update-mem! c assoc :to-drop-walked true)
          (let [r (await (child/call-walk* c :to-drop {:pos pos :range 0 :ways #{:walk}}))]
            (if (= :continue r) :continue :again)))
      (and seen (not= seen (:drop-last m)))
      (do (ctx/update-mem! c assoc :drop-last seen)
          (await (child/pause! c 1 :picking-up-the-drop))
          :continue))))

(defn ^:async recover!
  "The broken workstation is not carried (its drop slid out of reach, e.g. into the sealed cell beside it): crouch-in!,
  reach-drop!, wait-on-cell!, each while it still has something new to try; then {:lost cell}, the cell the drop lies
  in, for the caller to end :workstation-lost. Nothing above the cell is ever dug: the cell stays shut. :again,
  :continue or {:lost cell}."
  [c pos item]
  (or (await (crouch-in! c pos item))
      (await (reach-drop! c pos item))
      (await (wait-on-cell! c pos item))
      {:lost (drop-cell c pos item)}))
