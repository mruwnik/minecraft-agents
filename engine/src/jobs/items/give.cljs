(ns jobs.items.give
  (:require [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.storage :as storage]))

(def doc
  "Walk to the player :player, toss them :count of :item (everything carried when nil) and confirm the drop was
  taken. Needs the item carried and the player in sight. Quote: A, walks, the toss and the wait.
  Impossible: :bad-args (a player and an item name are needed). Conditions (a listed root job waits): :no-item (nothing is
  carried before the toss), :gone (the player is not seen within :radius); can-run? asks both before the toss.
  Nothing-possible, {:given 0}:
  - :unreachable, three walks in a row are blocked.
  - :out-of-range, three arrivals in a row are still out of reach.
  A player farther than the reach setting (2) is walked to (doors :shut). In reach the body looks at the player's head and tosses.
  The item entities of that name within 10 blocks of the toss spot that were not there before are the drop.
  After the toss:
  - The drop is gone once seen: done {:given n :text}.
  - The drop is never seen within 3 s: done {:given tossed :unconfirmed true}.
  - The drop still lies after the wait-s setting (6): it is collected back, never left as litter. Done {:given (tossed - back)
    :returned back} when some was taken (the call's :least, default 1), else nothing-possible :not-taken. The same happens if the body picks it up on its own.
  A toss refused three times in a row, or three collects in a row that change nothing, end nothing-possible with the status or :litter as the cause.
  The cleanup export takes back a drop still lying however the call ends (a cut included), at most :max-blocked
  collects in a row that change nothing.
  One call is the whole give; it waits for the player to appear or to take the drop in jobs.time.noop pauses (:giving) and yields :continue.
  done? is ok once the drop is seen taken.")

(a/defargs args
  {:player {:spec a/name? :default nil}
   :item {:spec a/item? :default nil}
   :count {:doc "how many; nil gives everything carried" :spec (a/int-in 1 nil) :default nil}
   :radius {:spec (a/num-in 0 nil) :default 32}})

(a/defargs settings
  {::reach {:default 2 :doc "Toss from within this many blocks." :spec (a/num-in 0 nil)}
   ::wait-s {:default 6 :doc "How long a drop may lie before it is taken back, in seconds." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 60 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::idle-ms {:default 500 :doc "Wait while there is nothing to do, in ms." :spec (a/int-in 0 nil)}
   ::walk-leg-s {:default 5 :doc "Seconds one walk leg may take." :spec (a/int-in 0 nil)}
   ::max-blocked {:default 3 :doc "Blocked attempts before the job gives up." :spec (a/int-in 0 nil)}
   ::unseen-ms {:default 3000 :doc "How long a target may stay unseen before it counts as gone, in ms." :spec (a/int-in 0 nil)}
   ::head-height {:default 1.6 :doc "Height of a player's head above its position, in blocks." :spec (a/num-in 0 nil)}
   ::walk-s {:default 10 :doc "Seconds the walk to the player takes (the quote)." :spec (a/num-in 0 nil)}
   ::toss-s {:default 2 :doc "Seconds the toss and the confirmation take (the quote)." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn wait-s [] (settings/get settings ::wait-s))
(defn idle-ms [] (settings/get settings ::idle-ms))
(defn walk-leg-s [] (settings/get settings ::walk-leg-s))
(defn max-blocked [] (settings/get settings ::max-blocked))
(defn unseen-ms [] (settings/get settings ::unseen-ms))
(defn head-height [] (settings/get settings ::head-height))

(def target [:player :item :count])

(def safe? ready/threatened)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn named
  "Never :bad-args unless a player name and an item name are given."
  [c]
  (let [{:keys [player item]} (:args c)]
    (if (and (string? player) (string? item))
      (contract/ok :args "a player and an item")
      (contract/never :bad-args "needs a player name and an item name"))))

(defn carried "Later :no-item while none of the item is carried." [c]
  ((cond/because :no-item (cond/> (cond/inventory (:item (:args c))) 0)) c))

(defn find-player
  "The position {:x :y :z} of player named name within radius, or nil."
  [p name radius]
  (some->> (array-seq (.entities p #js {:radius radius :kind "player"}))
           (filter #(or (= name (.-username %)) (= name (.-name %))))
           first
           .-pos
           u/pos-of))

(defn in-sight "Later :gone while the player is not seen within :radius." [c]
  (let [{:keys [player radius]} (:args c)]
    (if (find-player (:primitives c) player radius)
      (contract/ok :in-sight (str player " is in sight"))
      (contract/later :gone (str player " is not here")))))

(def can-start? (cond/and named carried))

(defn can-run?
  "Before the toss the item is carried (later :no-item) and the player in sight (later :gone); once tossed nothing
  more is asked."
  [c]
  (if (some? (:tossed (ctx/mem c)))
    (contract/ok :tossed "the item was thrown")
    ((cond/and carried in-sight) c)))

(defn given-so-far
  "Items given and not taken back so far."
  [c]
  (let [{:keys [tossed back] :or {tossed 0 back 0}} (ctx/mem c)]
    (max 0 (- tossed back))))

(defn done?
  "Ok once the toss is seen taken (and nothing is collected back); until then a count."
  [c]
  (let [{:keys [confirmed collecting]} (ctx/mem c)
        n (given-so-far c)
        want (or (:count (:args c)) (:had (ctx/mem c)))
        left (max 0 (- (or want n) n))
        data [:units :items :done n :left left :result {:given n}]]
    (if (and confirmed (not collecting) (zero? left))
      (apply contract/ok :given (str "give: " n " given") data)
      (apply contract/later :giving (str "give: " n " given") data))))

(defn quote-of [c]
  (let [n (or (:count (:args c)) 1)
        walk (settings/get settings ::walk-s)
        toss (settings/get settings ::toss-s)]
    {:uses {:job-time [toss (+ toss walk) (+ toss (* 2 walk) (wait-s))]}
     :done [0 n n]}))

(defn finish!
  "The ending of kind with cause; data carries what was given (0 unless said)."
  [c kind cause data]
  (let [d (merge {:given 0} data)]
    (ctx/end c kind cause (merge {:text (str "gave " (:given d) " " (:item (:args c)) (when cause (str " (" (name cause) ")")))} d))))

(defn enough?
  "Whether n given meets the call's :least (default 1; :all wants every one asked)."
  [c n]
  (let [least (:least (ctx/frozen-call c) 1)]
    (>= n (if (= :all least) (or (:count (:args c)) (:had (ctx/mem c))) least))))

(defn gave!
  "End after a toss: done when enough was given, else nothing-possible with cause."
  [c given cause data]
  (if (and (pos? given) (enough? c given))
    (finish! c :done nil (assoc data :given given))
    (finish! c :nothing-possible cause (assoc data :given given))))

(defn ^:async idle!
  "A pause (jobs.time.noop, holding :giving) for the world (the player to appear or to take the drop), then :continue: the yield."
  [c]
  (await (child/pause! c (/ (idle-ms) 1000) :giving))
  :continue)

(defn ^:async walk!
  "Walk to pos within the reach setting (a go-to child, a leg of walk-leg-s so a moving player is aimed at again). An arrival or a
  leg resets the blocked count; any other end counts, and the third in a row ends the job. An arrival that leaves the
  body not u/within? reach counts in :out-of-range (reset when within), the third in a row ends as out-of-range.
  :again, :continue (go-to waits) or :done."
  [c pos]
  (let [r (await (child/call-walk* c :walk {:pos pos :range (reach) :doors :shut :spend {:job-time (walk-leg-s)}
                                                               :ways #{:walk :clear-seal} :warn false :retry false :zone-tolls true}))
        res (when-not (= :continue r) (:data r))]
    (if (= :continue r)
      :continue
      (let [blocked (if (or (:arrived res) (:leg res)) 0 (inc (:blocked (ctx/mem c) 0)))
            out (if (or (> blocked 0) (u/within? (u/self-pos c) pos (reach))) 0 (inc (:out-of-range (ctx/mem c) 0)))]
        (ctx/update-mem! c assoc :blocked blocked :out-of-range out)
        (cond
          (>= out (max-blocked))
          (finish! c :nothing-possible :out-of-range {:text (str "still out of reach of " (:player (:args c)))})

          (< blocked (max-blocked)) :again

          :else
          (finish! c :nothing-possible :unreachable {:text (str "cannot reach " (:player (:args c)))}))))))

(defn ^:async toss!
  "In reach: remember the drops already lying, look at the player's head and
  toss. :continue, or the u/fail! verdict when the toss was refused."
  [c pos have]
  (let [{:keys [item count radius]} (:args c)
        p (:primitives c)
        n (if count (min count have) have)]
    (ctx/update-mem! c assoc :before (set (map :id (look/drops p item radius))) :had have)
    (await (ctx/act c :look (clj->js {:pos (update pos :y + (head-height))})))
    (let [r (await (ctx/act c :toss (clj->js {:item item :count n})))
          status (.-status r)]
      (if (= "tossed" status)
        (do (ctx/update-mem! c assoc :tossed (.-count r) :tossed-t (ctx/now c) :toss-at (u/self-pos c))
            (u/progress! c)
            :again)
        (if (u/count-fail! c)
          (finish! c :nothing-possible (keyword status) nil)
          :again)))))

(defn returned!
  "The body holds back more of the item: finish with what was given."
  [c given back]
  (ctx/update-mem! c assoc :back back)
  (gave! c given :not-taken {:returned back}))

(defn ^:async collect!
  "Take the nearest drop back. Each collect that gathers nothing counts through
  u/fail!; the third gives up as litter."
  [c lying given back]
  (let [r (await (ctx/act c :collect (clj->js {:id (:id (first lying))})))]
    (ctx/update-mem! c assoc :collecting true)
    (if (= "collected" (.-status r))
      (do (u/progress! c) :again)
      (if (u/count-fail! c)
        (do (ctx/update-mem! c assoc :litter true)
            (finish! c :nothing-possible :litter {:given given :returned back}))
        :again))))

(defn ^:async after-toss!
  "The item was thrown: see whether it was taken, wait, or take it back. An
  empty view only means taken once the drop has been seen (its spawn may not
  have arrived yet); a drop never seen within unseen-ms ends as unconfirmed."
  [c]
  (let [{:keys [item radius]} (:args c)
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

      (and (empty? lying) (not seen-drop) (< since (unseen-ms)))
      (await (idle! c))

      (and (empty? lying) (not seen-drop))
      (gave! c tossed :unconfirmed {:unconfirmed true :text (str "never saw the drop of " item)})

      (empty? lying)
      (if collecting
        (returned! c given back)
        (do (ctx/update-mem! c assoc :confirmed true)
            (gave! c tossed :short {:text (str "gave " tossed)})))

      (and (not collecting) (< since (* 1000 (wait-s))))
      (await (idle! c))

      :else
      (await (collect! c lying given back)))))

(defn ^:async before-toss!
  "Nothing thrown yet: wait when nothing is carried or the player is not seen (can-run? answers), walk when too far,
  else toss."
  [c]
  (let [{:keys [player item radius]} (:args c)
        p (:primitives c)
        have (storage/carried (u/inventory p) item)
        pos (find-player p player radius)]
    (cond
      (or (zero? have) (nil? pos))
      (await (idle! c))

      (not (u/within? (u/self-pos c) pos (reach)))
      (await (walk! c pos))

      :else
      (await (toss! c pos have)))))

(defn ^:async step!
  "One piece: before the toss walk and toss, after it watch the drop. :again, :continue (waiting for the player or the
  world) or :done."
  [c]
  (if (some? (:tossed (ctx/mem c)))
    (await (after-toss! c))
    (await (before-toss! c))))

(defn ^:async cleanup
  "However the call ends: the drop of this call's toss that still lies is collected back, never more than was tossed.
  Done, or nothing-possible :litter when collects change nothing."
  [c _]
  (let [{:keys [item radius]} (:args c)
        {:keys [before toss-at tossed had litter]} (ctx/mem c)
        lying #(when (and tossed (not litter))
                 (let [back (- (storage/carried (u/inventory (:primitives c)) item) (- had tossed))
                       owed (- tossed back)]
                   (filter (fn [d] (and (not (contains? before (:id d))) (<= (:count d) owed)))
                           (look/drops (:primitives c) item radius toss-at))))]
    (loop [failed 0]
      (let [l (lying)]
        (cond
          (empty? l) (ctx/end c :done nil)
          (>= failed (max-blocked)) (ctx/end c :nothing-possible :litter nil)
          :else (let [r (await (ctx/act c :collect (clj->js {:id (:id (first l))})))]
                  (recur (if (= "collected" (.-status r)) 0 (inc failed)))))))))

(defn ^:async round
  "One call walks, tosses and collects what lies back until the give is done or stopped; it yields while it waits
  for the player to appear or to take the drop."
  [c]
  (await (pace/steps! c #(step! c))))
