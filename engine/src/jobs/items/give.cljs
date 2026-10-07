(ns jobs.items.give
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.storage :as storage]))

(def doc
  "Walk to the player :player, toss them :count of :item (everything carried when nil) and confirm the drop was
  taken.
  Before the toss it ends with {:given 0 :reason r} when:
  - nothing is carried: \"no-item\" (warn give.no-item).
  - the player is not seen within :radius for 2 s: \"gone\" (info give.gone).
  - three walks in a row are blocked: \"unreachable\" (warn give.unreachable).
  - three arrivals in a row are still out of reach: \"out-of-range\" (warn give.out-of-range).
  A player farther than :reach is walked to (doors :shut). In reach the body looks at the player's head and tosses.
  The item entities of that name within 10 blocks of the toss spot that were not there before are the drop.
  After the toss:
  - The drop is gone once seen: {:given n} (info give.done).
  - The drop is never seen within 3 s: {:given tossed :reason \"unconfirmed\"} (info give.unconfirmed).
  - The drop still lies after :wait-s: it is collected back, never left as litter. The result is {:given (tossed -
    back) :reason \"not-taken\" :returned back} (info give.returned). The same happens if the body picks it up on
    its own.
  A toss refused three times in a row, or three collects in a row that change nothing, end through u/fail! (warn give.gave-up)
  with the status or \"litter\" as the reason.
  A cut after the toss leaves the drop where it lies.
  One call is the whole give; it yields :continue only while it waits for the player to appear or to take the drop.")

(a/defargs args
  {:player {:spec a/name? :default nil}
   :item {:spec a/item? :default nil}
   :count {:doc "how many; nil gives everything carried" :spec (a/int-in 1 nil) :default nil}
   :reach {:doc "toss from within this many blocks" :spec (a/num-in 0 nil) :default 2}
   :radius {:spec (a/num-in 0 nil) :default 32}
   :wait-s {:doc "how long a drop may lie before it is taken back" :spec (a/num-in 0 nil) :default 6}})

(def grace-ms 2000)
(def idle-ms 500)
(def walk-leg-s 5)
(def max-blocked 3)
(def unseen-ms 3000)
(def head-height 1.6)

(defn check
  "A player name and an item name are given."
  [c]
  (let [{:keys [player item]} (:args c)]
    (or (and (string? player) (string? item))
        (ctx/wait c {:reason :bad-args :why "needs a player name and an item name"}))))

(defn find-player
  "The position {:x :y :z} of player named name within radius, or nil."
  [p name radius]
  (some->> (array-seq (.entities p #js {:radius radius :kind "player"}))
           (filter #(or (= name (.-username %)) (= name (.-name %))))
           first
           .-pos
           u/pos-of))

(defn finish!
  "Hand the parent result and return :done."
  [c result]
  (ctx/result! c result)
  :done)

(defn ^:async idle!
  "A neutral wait for the world (the player to appear or to take the drop), then :continue: the yield."
  [c]
  (await (ctx/act c :wait #js {:ms idle-ms}))
  :continue)

(defn ^:async walk!
  "Walk to pos within :reach (a go-to child, a leg of walk-leg-s so a moving player is aimed at again). An arrival or a
  leg resets the blocked count; any other end counts, and the third in a row ends the job. An arrival that leaves the
  body not u/within? reach counts in :out-of-range (reset when within), the third in a row ends as out-of-range.
  :again, :continue (go-to waits) or :done."
  [c pos]
  (let [reach (:reach (:args c))
        r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos pos :range reach :doors :shut :leg-s walk-leg-s
                                                               :escalate false :warn false :retry false :zone-tolls true}))
        res (when-not (= :continue r) (ctx/child-result c :walk))]
    (if (= :continue r)
      :continue
      (let [blocked (if (or (:arrived res) (:leg res)) 0 (inc (:blocked (ctx/mem c) 0)))
            out (if (or (> blocked 0) (u/within? (u/self-pos c) pos reach)) 0 (inc (:out-of-range (ctx/mem c) 0)))]
        (ctx/update-mem! c assoc :blocked blocked :out-of-range out)
        (cond
          (>= out max-blocked)
          (do (ctx/emit! c :give.out-of-range :warn {:text (str "still out of reach of " (:player (:args c)))})
              (finish! c {:given 0 :reason "out-of-range"}))

          (< blocked max-blocked) :again

          :else
          (do (ctx/emit! c :give.unreachable :warn {:text (str "cannot reach " (:player (:args c)))})
              (finish! c {:given 0 :reason "unreachable"})))))))

(defn ^:async toss!
  "In reach: remember the drops already lying, look at the player's head and
  toss. :continue, or the u/fail! verdict when the toss was refused."
  [c pos have]
  (let [{:keys [item count radius]} (:args c)
        p (:primitives c)
        n (if count (min count have) have)]
    (ctx/update-mem! c assoc :before (set (map :id (look/drops p item radius))) :had have)
    (await (ctx/act c :look (clj->js {:pos (update pos :y + head-height)})))
    (let [r (await (ctx/act c :toss (clj->js {:item item :count n})))
          status (.-status r)]
      (if (= "tossed" status)
        (do (ctx/update-mem! c assoc :tossed (.-count r) :tossed-t (ctx/now c) :toss-at (u/self-pos c))
            (u/progress! c)
            :again)
        (let [v (u/fail! c :give.gave-up (str "give gave up: " status))]
          (if (= :done v)
            (finish! c {:given 0 :reason status})
            :again))))))

(defn returned!
  "The body holds back more of the item: info and finish with what was given."
  [c given back]
  (ctx/emit! c :give.returned :info {:text (str back " came back") :returned back})
  (finish! c {:given given :reason "not-taken" :returned back}))

(defn ^:async collect!
  "Take the nearest drop back. Each collect that gathers nothing counts through
  u/fail!; the third gives up as litter."
  [c lying given back]
  (let [r (await (ctx/act c :collect (clj->js {:id (:id (first lying))})))]
    (ctx/update-mem! c assoc :collecting true)
    (if (= "collected" (.-status r))
      (do (u/progress! c) :again)
      (let [v (u/fail! c :give.gave-up "give gave up: litter")]
        (if (= :done v)
          (finish! c {:given given :reason "litter" :returned back})
          :again)))))

(defn ^:async after-toss!
  "The item was thrown: see whether it was taken, wait, or take it back. An
  empty view only means taken once the drop has been seen (its spawn may not
  have arrived yet); a drop never seen within unseen-ms ends as unconfirmed."
  [c]
  (let [{:keys [item radius wait-s]} (:args c)
        {:keys [tossed tossed-t toss-at before had collecting seen-drop]} (ctx/mem c)
        p (:primitives c)
        lying (remove #(contains? before (:id %)) (look/drops p item radius toss-at))
        back (max 0 (- (storage/carried (u/inventory p) item) (- had tossed)))
        given (- tossed back)
        since (- (ctx/now c) tossed-t)]
    (when (seq lying) (ctx/update-mem! c assoc :seen-drop true))
    (cond
      (and (pos? back) (not collecting))
      (returned! c given back)

      (and (empty? lying) (not seen-drop) (< since unseen-ms))
      (await (idle! c))

      (and (empty? lying) (not seen-drop))
      (do (ctx/emit! c :give.unconfirmed :info {:text (str "never saw the drop of " item)})
          (finish! c {:given tossed :reason "unconfirmed"}))

      (empty? lying)
      (if collecting
        (returned! c given back)
        (do (ctx/emit! c :give.done :info {:text (str "gave " tossed)})
            (finish! c {:given tossed})))

      (and (not collecting) (< since (* 1000 wait-s)))
      (await (idle! c))

      :else
      (await (collect! c lying given back)))))

(defn ^:async before-toss!
  "Nothing thrown yet: end when nothing is carried or the player is gone or out
  of reach for good, walk when too far, else toss."
  [c now]
  (let [{:keys [player item radius reach]} (:args c)
        p (:primitives c)
        have (storage/carried (u/inventory p) item)
        pos (find-player p player radius)]
    (cond
      (zero? have)
      (do (ctx/emit! c :give.no-item :warn {:text (str "no " item " to give")})
          (finish! c {:given 0 :reason "no-item"}))

      (and (nil? pos) (< (- now (:started (ctx/mem c))) grace-ms))
      (await (idle! c))

      (nil? pos)
      (do (ctx/emit! c :give.gone :info {:text (str player " is not here")})
          (finish! c {:given 0 :reason "gone"}))

      (not (u/within? (u/self-pos c) pos reach))
      (await (walk! c pos))

      :else
      (await (toss! c pos have)))))

(defn ^:async step!
  "One piece: before the toss walk and toss, after it watch the drop. :again, :continue (waiting for the player or the
  world) or :done."
  [c]
  (let [now (ctx/now c)
        _ (when-not (contains? (ctx/mem c) :started)
            (ctx/update-mem! c assoc :started now))]
    (if (some? (:tossed (ctx/mem c)))
      (await (after-toss! c))
      (await (before-toss! c now)))))

(def max-steps "Steps of one call before it gives the round back with :continue." 400)

(defn ^:async round
  "One call walks, tosses and collects what lies back until the give is done or stopped; it yields while it waits
  for the player to appear or to take the drop, and after max-steps steps."
  [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) max-steps) (step! c) :continue)))))
