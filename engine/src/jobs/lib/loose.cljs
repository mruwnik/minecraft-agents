(ns jobs.lib.loose
  "Loose items: the dropped items a body sees nearby that are worth stepping off its way for. The :loose-items trigger
  and jobs.items.pick-up-loose judge them with the same rule (judge), so the job takes what the trigger fired for.

  An item is taken when it is seen (not behind a wall), within :radius of the body, not on :never, and on :always or
  worth the detour: jobs.lib.cost item-value (:value options) over detour-cost (:cost options) at least :min-ratio.
  It is left (the reason judge gives) when:
    :thrown       the body threw it (make-room's junk, a give), ever; another player threw it under :thrown-s ago
                  (thrower: from the spawn engine/js records; the body's own toss by item and time, a player's by its drop point);
    :zone         it lies in another's zone or claim (a :take verdict; no zone list never blocks);
    :collecting   a live job's collect-drops is collecting round it (the :collecting hold, written by collect-drops);
    :owed         it appeared under :owed-s ago while some other job runs: that job's own drop (a felled log, a dug ore);
    :left         a run left it (:loose-left, not reached or not picked up) in the last :left-s (longer than
                  :thrown-s and :owed-s: a spot a run failed on stays failed, where those two only guess at ownership)."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.world-files :as world]))

(a/defargs settings
  {::radius {:default 8 :spec (a/num-in 0 32) :doc "Blocks from the body within which seen items count."}
   ::min-ratio {:default 1 :spec (a/num-in 0 nil) :doc "Item worth over detour cost at or above which an item is picked up."}
   ::always {:default [] :spec (a/coll-of a/item?) :doc "Item names always picked up (still not thrown, owed or in another's zone)."}
   ::never {:default [] :spec (a/coll-of a/item?) :doc "Item names never picked up."}
   ::thrown-s {:default 60 :spec (a/num-in 0 nil) :doc "Seconds an item another player threw is left to them."}
   ::owed-s {:default 120 :spec (a/num-in 0 nil) :doc "Seconds an item that appeared while another job runs is left to that job."}
   ::left-s {:default 300 :spec (a/num-in 0 nil) :doc "Seconds an item a run could not pick up is not tried again."}
   ::drop-dy {:default 1.32 :spec (a/num-in 0 2) :doc "Height above a player's feet where its thrown items appear (vanilla: eye 1.62 less 0.3)."}
   ::throw-dxz {:default 0.1 :spec (a/num-in 0 1) :doc "Blocks across from a player's drop point within which an item counts as its throw."}
   ::throw-dy {:default 0.1 :spec (a/num-in 0 1) :doc "Blocks up or down from a player's drop point within which an item counts as its throw."}
   ::toss-ms {:default 2000 :spec (a/num-in 0 nil) :doc "An item counts as the body's own throw only this many ms after a toss of that item."}
   ::toss-reach {:default 1.5 :spec (a/num-in 0 4) :doc "Blocks from the body's drop point within which an item of its toss may appear (it may be moving)."}
   ::hold-cap {:default 16 :spec (a/int-in 1 nil) :doc "Most :collecting holds kept (one per collect-drops run)."}
   ::hold-ttl-s {:default 3600 :spec (a/num-in 1 nil) :doc "Seconds a :collecting hold lasts when its job never released it."}
   ::left-cap {:default 32 :spec (a/int-in 1 nil) :doc "Most :loose-left entries kept."}
   ::check-radius {:default 32 :spec (a/num-in 1 nil) :doc "Blocks within which pick-up-loose looks for the items it tried, after the run."}
   ::collect-pad {:default 1 :spec (a/num-in 0 nil) :doc "Blocks added to :radius for the collect-drops child's area."}})

(def option-keys [:radius :min-ratio :always :never :thrown-s :owed-s :left-s
                  :drop-dy :throw-dxz :throw-dy :toss-ms :toss-reach :hold-cap :hold-ttl-s :left-cap :check-radius :collect-pad])

(defn options
  "The settings with the non-nil keys of args over them; :value (item-value options) and :cost (detour-cost options)
  pass through."
  [args]
  (merge (into {} (map (fn [k] [k (settings/get settings (keyword "jobs.lib.loose" (name k)))])) option-keys)
         (into {} (remove (comp nil? val)) (select-keys args (into option-keys [:value :cost])))))

(def hold-kind :collecting)
(def left-kind :loose-left)

(defn hold!
  "Mark the items within radius of anchor as this ctx's root job's to collect (collect-drops, while it runs)."
  [c anchor radius]
  (let [o (options {})]
    (ctx/remember! c hold-kind {:job (:root c) :anchor anchor :radius radius}
                   {:cap (:hold-cap o) :ttl (* 1000 (:hold-ttl-s o))})))

(defn release! [c] (ctx/forget-where! c hold-kind #(= (:root c) (:job %))))

(defn left!
  "Remember entity ids a run could not pick up, for :left-s."
  [c ids o]
  (when (seq ids)
    (ctx/remember! c left-kind {:ids (vec ids)} {:cap (:left-cap o) :ttl (* 1000 (:left-s o))})))

(defn left-ids [view] (into #{} (mapcat (comp :ids :data)) (mem/entries view left-kind)))

(defn live-ids
  "The ids of the listed jobs (each has job memory in view from listing to its end), but except's. The trigger and
  the job both count live jobs this way, so they judge alike."
  [view except]
  (into #{} (comp (filter mem/job-kind?) (map name) (remove #{except})) (keys (:entries (:data view)))))

(defn own-toss?
  "Whether spawn record s (item name) is the body's own toss: one of its :tosses (the toss primitive's {:at :item :count},
  engine/js) of that item came under :toss-ms before the spawn, and the item appeared within :toss-reach of the body's
  feet (when the nearest player is the body). A moving, sneaking or swimming body drops off the exact drop point."
  [o self item s]
  (let [player (some-> s .-player)
        pos (some-> s .-pos u/pos-of)
        feet (when (= self (some-> player .-name)) (some-> player .-pos u/pos-of))
        reach (:toss-reach o)]
    (boolean
     (and (some #(and (= item (.-item %)) (<= 0 (- (.-at s) (.-at %)) (:toss-ms o))) (some-> s .-tosses array-seq))
          (or (nil? feet) (nil? pos)
              (and (<= (js/Math.abs (- (:x pos) (:x feet))) reach)
                   (<= (js/Math.abs (- (:z pos) (:z feet))) reach)
                   (<= (js/Math.abs (- (:y pos) (:y feet) (:drop-dy o))) reach)))))))

(defn thrower
  "The name of the player who threw an item (name item) from its spawn record s (engine/js: :pos, :player {:name :pos}
  nearest then, :tosses the body's last tosses). The body itself: own-toss? (matched by item and time, not by the exact
  point). Another player: the item appeared at that player's drop point, :drop-dy over its feet, within :throw-dxz
  across and :throw-dy up or down. A block broken beside a player drops at the block's centre, off that point. nil when
  nobody threw it."
  [o self item s]
  (let [pos (some-> s .-pos u/pos-of)
        player (some-> s .-player)
        feet (some-> player .-pos u/pos-of)
        name (some-> player .-name)]
    (cond
      (own-toss? o self item s) self
      (and pos feet name (not= name self)
           (<= (js/Math.abs (- (:x pos) (:x feet))) (:throw-dxz o))
           (<= (js/Math.abs (- (:z pos) (:z feet))) (:throw-dxz o))
           (<= (js/Math.abs (- (:y pos) (:y feet) (:drop-dy o))) (:throw-dy o))) name)))

(defn item-pos [e] (u/pos-of (.-pos e)))

(defn worth
  "{:value :cost :ratio} of picking up entity e from feet."
  [o feet e]
  (let [{:keys [name count]} (js->clj (.-item e) :keywordize-keys true)
        pos (item-pos e)
        value (:value (apply cost/item-value [{:name name :count (or count 1)}] (mapcat identity (:value o))))
        detour (:cost (cost/detour-cost {:distance (u/dist feet pos)
                                         :climb (- (js/Math.floor (:y pos)) (js/Math.floor (:y feet)))}
                                        (:cost o)))]
    {:value value :cost detour :ratio (if (pos? detour) (/ value detour) js/Infinity)}))

(defn verdict
  "Whether the body named self may take an item at pos: a zones :take verdict; ok without a zone list."
  [kn self now pos]
  (let [zs (world/zones kn)]
    (or (nil? zs)
        (:ok (zones/verdict {:zones zs :claims (world/live-claims (world/area-claims kn) now) :footprints {}
                             :plan-cells #{} :self self :now now :action :take
                             :cell (mapv js/Math.floor [(:x pos) (:y pos) (:z pos)])})))))

(defn judge
  "Why entity e is left (a keyword, see the ns doc), or nil when it is taken. live is the set of live job ids; :zone stands for a zone or a claim."
  [{:keys [view kn o self feet live left]} e]
  (let [now (:now view)
        name (some-> (.-item e) .-name)
        pos (item-pos e)
        spawn (.-spawn e)
        age (when-let [t (some-> spawn .-at)] (- now t))
        by (thrower o self name spawn)
        holds (filter #(contains? live (:job %)) (map :data (mem/entries view hold-kind)))]
    (cond
      (nil? name) :unknown
      (some #{name} (:never o)) :never
      (and by (zones/same-owner? by self)) :thrown
      (and by (or (nil? age) (< age (* 1000 (:thrown-s o))))) :thrown
      (contains? left (.-id e)) :left
      (some #(<= (u/dist (:anchor %) pos) (:radius %)) holds) :collecting
      (and age (< age (* 1000 (:owed-s o))) (seq live)) :owed
      (not (verdict kn self now pos)) :zone
      (some #{name} (:always o)) nil
      (< (:ratio (worth o feet e)) (:min-ratio o)) :not-worth)))

(defn seen [p o] (look/seen-items p {:radius (:radius o) :max 32}))

(defn explain
  "Why no candidate: {:radius r :left {name why} :beyond {:name :distance}}: the reason (judge) each seen item within
  :radius is left for, and the nearest worthwhile-looking item seen beyond it up to :check-radius, if any."
  [p view kn args except]
  (let [o (options args)
        feet (u/pos-of (.-pos (.self p)))
        env {:view view :kn kn :o o :self (.-username (.self p)) :feet feet
             :live (live-ids view except) :left (left-ids view)}
        near (seen p o)
        near-ids (set (map #(.-id %) near))
        beyond (->> (look/seen-items p {:radius (:check-radius o) :max 32})
                    (remove #(near-ids (.-id %)))
                    (sort-by #(u/dist feet (item-pos %)))
                    first)]
    {:radius (:radius o)
     :left (into {} (keep (fn [e] (when-let [why (judge env e)] [(some-> (.-item e) .-name) why]))) near)
     :beyond (when beyond {:name (some-> (.-item beyond) .-name) :distance (js/Math.round (u/dist feet (item-pos beyond)))})}))

(defn candidates
  "The seen item entities within :radius of the body judge takes, nearest first. except is the caller's own job id
  (nil for the trigger): every other listed job counts as live (live-ids)."
  [p view kn args except]
  (let [o (options args)
        env {:view view :kn kn :o o :self (.-username (.self p)) :feet (u/pos-of (.-pos (.self p)))
             :live (live-ids view except) :left (left-ids view)}]
    (filterv #(nil? (judge env %)) (seen p o))))
