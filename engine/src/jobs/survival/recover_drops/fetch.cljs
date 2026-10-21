(ns jobs.survival.recover-drops.fetch
  "The walk to a pile and the collect passes at it: legs of go-to, taking back the body's own seal, looking round, collecting."
  (:require [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.result :as result]
            [jobs.lib.util :as u]
            [jobs.survival.recover-drops.config :as cfg]
            [jobs.survival.recover-drops.danger :as dg]
            [jobs.survival.recover-drops.deaths :as dth]
            [jobs.survival.recover-drops.ending :as end]
            [jobs.survival.recover-drops.pile :as pl]))

(defn own-seal-near
  "The open scaffold ledger entries (jobs.lib.ledger) within seal-radius of pos: blocks this body placed round the pile."
  [c pos]
  (ledger/near-of (ledger/open-entries (ctx/view c)) {:pos pos :radius (cfg/seal-radius)}))

(defn ^:async clear-seal!
  "When the body's own placed blocks stand near the pile (its flee seal): take back those within seal-radius of it
  (jobs.access.cleanup :near) and walk again, after a walk to where the eye sees the nearest of them, within ::seal-sight (the cleanup offers only blocks the body has seen). Tried twice a trip at most; :seal-cleared is set once a run removed a block.
  :next when it ran, else nil."
  [c pos]
  (when-let [seal (and (not (:seal-cleared (ctx/mem c))) (not (child/exhausted? c :seal 2)) (seq (own-seal-near c pos)))]
    (child/fruitless! c :seal)
    (ctx/update-mem! c assoc :phase :go)
    (dth/save-trip! c)
    (let [[x y z] (:cell (apply min-key #(u/dist (u/self-pos c) (zipmap [:x :y :z] (:cell %))) seal))]
      (await (child/run!* c :seal-walk 'jobs.movement.go-to {:pos {:x x :y y :z z} :range (cfg/seal-sight) :see true :ways #{:walk} :checks cfg/own-checks})))
    (let [r (await (child/run!* c :seal 'jobs.access.cleanup {:of :all :near {:pos pos :radius (cfg/seal-radius)} :checks (assoc cfg/own-checks :fit? '(always))}))]
      (when (and (= :done (:kind r)) (seq (:removed (:data r))))
        (ctx/update-mem! c assoc :seal-cleared true)))
    :next))

(def blocked-leg-factor
  "A leg after one that got no nearer is this many times longer: go-to's ways out (a stair, by hand last) start over each leg and a 6 s leg ends before the last one is reached."
  3)

(defn ^:async walk-leg!
  "One leg of the walk to pos: [:danger d] when a danger the trip has not priced is near (no walk is made), else [r] of
  a chase leg of ::leg-s seconds (:continue, or its ending; its :data has the result, with :arrived or :leg)."
  [c pos]
  (if-let [d (dg/threatened? c)]
    [:danger d]
    [(await (child/call-chase* c :go {:pos pos :range (cfg/arrive-range) :leg-s (* (cfg/leg-s) (if (:blocked (ctx/mem c)) blocked-leg-factor 1)) :walk {:checks cfg/own-checks}}))]))

(defn refused?
  "Whether the walk's ending r is go-to's verdict on the goal cell alone, given before any search (as chase passes it on)."
  [r]
  (boolean (some #{:goal-not-standable :goal-on-magma} [(result/why r) (result/why (:because r)) (result/why (get-in r [:cause :because]))])))

(defn nearer?
  "Whether a leg from `before` to `after` made progress toward pos: more than a block nearer, or a block closer in height
  (a body under ground climbs out one block at a time, which a straight-line distance of hundreds of blocks hides)."
  [before after pos]
  (or (< (u/dist after pos) (- (u/dist before pos) 1))
      (<= 1 (- (js/Math.abs (- (:y before) (:y pos))) (js/Math.abs (- (:y after) (:y pos)))))))

(defn ^:async go!
  "Walk one leg (:leg-s) toward the death point: :next once there (the collect phase) or after a leg (the danger and the
  cost are weighed again; one that got no nearer counts as a walk that did not arrive), else :declined (go-to waits on the
  world, or it did not arrive (the body's own seal near the pile is taken back first): the next run walks again while the pile is worth it, and the window closes it). A walk
  go-to refused for the body's own shelter (:inside-own-shelter) is no failed try: the window closes the trip."
  [c pos]
  (let [before (u/self-pos c)
        [r danger] (await (walk-leg! c pos))
        walked (:data r)]
    (cond
      (= :danger r) (dg/meet-danger! c danger)
      (or (= :continue r) (and (not= :done (:kind r)) (nil? walked)))
      (dg/decline! c :unreachable (str "the walk to " (pr-str pos) " is waiting on the world") {:pos pos})
      (:inside-own-shelter walked)
      (dg/decline! c :unreachable (str "the walk to " (pr-str pos) " is refused: the body is sealed in its own shelter, which opens by day") {:pos pos})
      (and (not (:arrived walked)) (not (:leg walked)) (await (clear-seal! c pos))) :next
      (:leg walked)
      (let [nearer (nearer? before (u/self-pos c) pos)]
        (ctx/update-mem! c #(cond-> (assoc % :recheck true)
                              (not nearer) (assoc :blocked true)))
        (if nearer (child/progress! c :go) (child/fruitless! c :go))
        (dth/save-trip! c)
        :next)
      (not (:arrived walked))
      (do (ctx/update-mem! c assoc :blocked true)
          (when-not (and (refused? r) (not (nearer? before (u/self-pos c) pos)))
            (child/fruitless! c :go))
          (dth/save-trip! c)
          (dg/decline! c :unreachable (str "the walk to " (pr-str pos) " did not arrive") {:pos pos}))
      :else (do (ctx/update-mem! c assoc :phase :collect :blocked false) (dth/save-trip! c) :next))))

(defn finish-collect!
  "End the collect phase: :collected when all of the pile is back, else :partial naming what is left and why."
  [c pile ids]
  (let [items (pl/recovered-items c pile)
        left (pl/left-over c pile)
        decided (:decided (ctx/mem c))]
    (cond
      (empty? left) (end/finish! c :collected (assoc decided :items items))
      (zero? items) (end/finish! c :abandoned (assoc decided :items 0 :reason :nothing-found))
      :else (end/finish! c :partial (assoc decided :items items :left left :reason (if (seq ids) :unreachable :not-visible))))))

(defn ^:async find-ids!
  "Ids of the pile's items in sight; when none, turn to each heading and check after every look (an item is seen only inside the view cone, and a walk ends facing wherever)."
  [c pos names radius]
  (or (seq (pl/pile-ids c pos names radius))
      (loop [dirs (vals look/headings)]
        (when-let [d (first dirs)]
          (await (look/glance! c [d]))
          (or (seq (pl/pile-ids c pos names radius)) (recur (rest dirs)))))))

(defn ^:async look-round!
  "Drops of the pile went out of sight (down a shaft): once, walk to the cell the first was last seen in (go-to, range 1)
  and look again. :next with a pass counted when drops are in sight now, else the end of the trip (what is left, with its
  reason). Not an arrival ends the trip :partial too."
  [c [x y z] pos names radius pile]
  (ctx/update-mem! c assoc :looked true)
  (await (child/run!* c :look 'jobs.movement.go-to {:pos {:x x :y y :z z} :range 1 :ways #{:walk :clear-seal} :checks cfg/own-checks}))
  (let [again (vec (await (find-ids! c pos names radius)))]
    (if (seq again)
      (do (ctx/update-mem! c update :passes (fnil inc 0)) (dth/save-trip! c) :next)
      (finish-collect! c pile again))))

(defn ^:async search-round!
  "Once: items of the pile are nowhere in sight at the pile (behind a corner, in a hollow). Walk to cells around it
  (go-to, range 1, ::cfg/search-radius out on the four sides) until some are in sight. True when some are."
  [c [x y z] pos names radius]
  (ctx/update-mem! c assoc :searched true)
  (loop [cells (let [r (cfg/search-radius)]
                 (map-indexed (fn [i [dx dz]] [i {:x (+ x dx) :y y :z (+ z dz)}]) [[r 0] [0 r] [(- r) 0] [0 (- r)]]))]
    (when-let [[i cell] (first cells)]
      (await (child/run!* c (keyword (str "search" i)) 'jobs.movement.go-to {:pos cell :range 1 :ways #{:walk :clear-seal} :checks cfg/own-checks}))
      (or (seq (await (find-ids! c pos names radius))) (recur (rest cells))))))

(defn ^:async collect!
  "One collect pass over the pile's visible items: :next after a pass that left some in sight (walk back, at most
  max-collect-passes), else the job's end."
  [c pos radius pile]
  (let [names (vec (distinct (map :name pile)))
        total (reduce + 0 (map :count pile))]
    (if (>= (pl/recovered-items c pile) total)
      (finish-collect! c pile [])
      (let [ids (vec (await (find-ids! c pos names radius)))]
        (if (empty? ids)
          (or (await (clear-seal! c pos))
              (when-not (:searched (ctx/mem c))
                (when (await (search-round! c (mapv pos [:x :y :z]) pos names radius))
                  :next))
              (finish-collect! c pile ids))
          (let [swept (await (child/run!* c :collect 'jobs.items.collect-drops
                                          {:radius (+ radius (cfg/stray-range) 4) :ids ids :items names :checks cfg/own-checks}))
                cell (pl/look-cell (:data swept))]
            (if (empty? (pl/left-over c pile))
              (finish-collect! c pile [])
              (let [again (vec (await (find-ids! c pos names radius)))
                    passes (:passes (ctx/mem c) 0)]
                (cond
                  (and (seq again) (< passes (cfg/max-collect-passes)))
                  (do (ctx/update-mem! c assoc :passes (inc passes) :phase :go) (dth/save-trip! c) :next)

                  (and (empty? again) (not (:looked (ctx/mem c))) cell)
                  (await (look-round! c cell pos names radius pile))

                  (and (empty? again) (not (:searched (ctx/mem c))) (await (search-round! c (mapv pos [:x :y :z]) pos names radius)))
                  :next

                  :else (finish-collect! c pile again))))))))))
