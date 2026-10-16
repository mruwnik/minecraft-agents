(ns jobs.survival.retreat-flight
  "The state of one flight (jobs.survival.retreat): the tried options and sweeps, who still chases, and the flight's end."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [jobs.lib.choose :as choose]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.survival.retreat-walk :as walk]
            [clojure.string :as str]))

(a/defargs settings
  {::wait-ms {:default 1000 :doc "One hold of a hidden or cornered body before it looks again." :spec (a/int-in 0 nil)}
   ::blast-reach {:default 6 :doc "Blocks within which a creeper's blast reaches the body through a wall or a shut door (twice its explosion power 3): no room is a refuge from it and a shut door does not end the flight." :spec (a/num-in 0 nil)}
   ::far-flight {:default 16 :doc "Blocks a flight must move the body to tell the agent (hostile.fled)." :spec (a/num-in 0 nil)}
   ::chain-window-ms {:default 60000 :doc "Flights this close together (end to end) add up to one move for hostile.fled." :spec (a/int-in 0 nil)}
   ::resume-gap-ms {:default 5000 :doc "A flight cut for longer than this starts its clocks afresh when it resumes." :spec (a/int-in 0 nil)}
   ::died-radius {:default 12 :doc "Blocks around a place the body died within which a flee step does not end, nor a goal lie, while another end is open; 0: off." :spec (a/num-in 0 nil)}
   ::died-ttl-ms {:default 3600000 :doc "Milliseconds after a death during which a flight keeps away from the place (::died-radius)." :spec (a/int-in 0 nil)}
   ::prefer-lit? {:default true :doc "By night or under ground a flee step ends on lit ground rather than dark (jobs.lib.look/dark-fn) while a lit end is open." :spec boolean?}})

(def displaced-policy
  "One :displaced entry, half an hour: the night job may then walk back to a bed up to :urgent-bed-radius away."
  {:cap 1 :ttl 1800000})

(defn wait-ms [] (settings/get settings ::wait-ms))

(defn blocks
  "The block names a cornered body may place: the call's :use {:blocks}, else every :build filler, carried or not (a
  pit or pocket plugs with the blocks it digs)."
  [c]
  (or (ctx/use c :blocks) (choose/fillers :build)))

(defn plug-purpose
  "The ledger purpose of the blocks a seal or plug places: the call's :purpose, else :retreat-plug (jobs.access.cleanup)."
  [c]
  (or (:purpose (:args c)) :retreat-plug))

(defn note-placed!
  "Job memory: item was placed at cell, for the plug's result (:placed)."
  [c cell item]
  (ctx/update-mem! c update :placed-cells (fnil conj []) {:cell (u/cell-vec cell) :item item}))

(defn floor
  "The lowest feet height a flight step may end at under rock: retreat-walk/max-descent below where the flight began."
  [c]
  (- (:y (or (:flight-from (ctx/mem c)) (u/self-pos c))) (walk/max-descent)))

(defn dead-ids
  "The ids of hostiles a cornered fight has killed: their corpses may stay listed a while."
  [c]
  (vec (:dead (ctx/mem c))))

(defn died-spots
  "The places the body died within ::died-ttl-ms (body memory :died), as positions; none with ::died-radius 0."
  [c]
  (if (pos? (settings/get settings ::died-radius))
    (let [now (ctx/now c)]
      (vec (keep #(when (< (- now (:t %)) (settings/get settings ::died-ttl-ms)) (:pos (:data %))) (ctx/entries c :died))))
    []))

(defn near-died?
  "Whether pos lies within ::died-radius of one of spots."
  [spots pos]
  (boolean (some #(<= (u/dist % pos) (settings/get settings ::died-radius)) spots)))

(defn dark-end?
  "A fn of a feet cell: whether it is dark (look/dark-fn) when it is night or the body is under ground and ::prefer-lit?
  holds, else nil."
  [p]
  (when (settings/get settings ::prefer-lit?)
    (when-let [{:keys [night? at]} (look/dark-fn p)]
      (when (or night? (sh/buried? p))
        (fn [{:keys [x y z]}] (= 1 (at x y z)))))))

(defn avoid-ends
  "walk/choose-target's :avoid fns for a step of the flight: near a death first, then dark ground."
  [c]
  (let [spots (died-spots c)]
    (cond-> []
      (seq spots) (conj #(near-died? spots %))
      (dark-end? (:primitives c)) (conj (dark-end? (:primitives c))))))

(defn tried? [c option] (contains? (:tried (ctx/mem c)) option))

(defn tried! [c option] (ctx/update-mem! c update :tried (fnil conj #{}) option))

(defn near-known
  "The hostiles p can be said to know within radius (ranged ones within ranged-radius), the dead skipped."
  [p dead radius ranged-radius]
  (remove #(contains? dead (.-id %)) (danger-q/known-hostiles p radius {:ranged-radius ranged-radius})))

(defn count-sweep
  "mem with one more sweep of every option failed."
  [mem]
  (update mem :sweeps (fnil inc 0)))

(defn cannot-escape?
  "Whether two sweeps of every option failed with no gain between."
  [mem]
  (>= (:sweeps mem 0) 2))

(defn note-gap
  "mem after a flight step that left the nearest chaser gap blocks off: a gain of a block over the best gap resets the
  sweeps and the no-gain count, else the count grows. No gap (no chaser left) is no sample: the flight is succeeding,
  the no-gain count resets and the best gap stays."
  [mem gap]
  (let [best (:best-gap mem)]
    (cond
      (nil? gap) (assoc mem :since-gain 0)
      (or (nil? best) (>= gap (inc best)))
      (assoc mem :best-gap gap :since-gain 0 :sweeps 0)
      :else
      (update mem :since-gain (fnil inc 0)))))

(defn nearest-gap
  "Blocks from the body to the nearest of hostiles, nil with none."
  [p hostiles]
  (when (seq hostiles)
    (apply min (map #(danger-q/mob-distance p %) hostiles))))

(defn no-gain?
  "Whether the flight went n steps without a gain."
  [mem n]
  (>= (:since-gain mem 0) n))

;; ------------------------------------------------------------------ who still chases

(defn stopped-chasing
  "Pure: nil while a mob still chases, else why it stopped. m is the mob as seen now ({:mob :distance :in-line? :way?};
  :way? false: no walkable way, nor a line of fire for a ranged mob), nil when no longer listed (dead, despawned, out
  of tracking); seen-t when it was last in line. Vanilla: a mob drops its target past its follow range or a while out
  of sight. A low body (:low?) is not let go by a ranged mob (:ranged?) that is merely out of line: it may come round
  a corner and throw again."
  [m seen-t now lost-ms]
  (cond
    (nil? m) :gone
    (> (:distance m) (threats/follow-range (:mob m))) :far
    (and (not (:in-line? m)) (> (- now seen-t) lost-ms) (not (and (:low? m) (:ranged? m)))) :lost
    (false? (:way? m)) :closed
    :else nil))

(defn blast-reach [] (settings/get settings ::blast-reach))

(defn blaster?
  "Whether hostile e is a creeper within blast-reach of the body: walls and doors do not keep its blast off."
  [p e]
  (boolean (and (combat/creeper? e) (<= (danger-q/mob-distance p e) (blast-reach)))))

(defn seen-now
  "What stopped-chasing needs of hostile e (a JS entity; visible: a clear line from the eye, whichever way the body
  faces). A way: a walkable way to the body, or for a ranged mob a line of fire (a shut door takes both), or a blast
  that reaches it (blaster?)."
  [p e low?]
  {:mob (.-name e) :low? low? :ranged? (boolean (combat/ranged? e)) :distance (danger-q/mob-distance p e) :in-line? (true? (.-visible e))
   :way? (or (reach/walkable-way? p (danger-q/mob-pos p e) (u/pos-of (.-pos (.self p))))
             (and (combat/ranged? e) (danger-q/danger? p e {:sight? false}))
             (blaster? p e))})

(defn known-chasers
  "The hostiles the body knows of (seen or heard, or remembered where last sensed) within the longest follow range."
  [p]
  (danger-q/known-hostiles p threats/max-follow-range {}))

(defn resume-gap-ms [] (settings/get settings ::resume-gap-ms))

(defn resume-flight
  "mem of a flight resumed at now: after a gap past resume-gap-ms since its last step, the flight starts now and every
  chaser counts as just seen (the world is judged afresh); else as it was."
  [mem now]
  (if (or (nil? (:last-step mem)) (<= (- now (:last-step mem)) (resume-gap-ms)))
    mem
    (cond-> (assoc mem :flight-start now :last-step now)
      (:chasers mem) (update :chasers update-vals #(assoc % :seen-t now)))))

(defn sensed-now?
  "Whether hostile e (a known-hostiles entry, or nil) is sensed now rather than remembered at its last place."
  [e]
  (and (some? e) (not (true? (.-remembered e)))))

(defn unsensed-loss?
  "Whether a chaser judged off for why may only be out of the body's senses (a silent mob behind it, a remembered place
  gone stale): lost, far or gone while not sensed now. Such a chaser is looked back at before it is let go."
  [e why]
  (and (#{:lost :far :gone} why) (not (sensed-now? e))))

(defn chaser-entry
  "A chaser's flight entry: its id, uuid, name and place (threats/place-of: exact only when seen)."
  [p e now]
  (merge {:id (.-id e) :uuid (.-uuid e) :mob (.-name e) :seen-t now} (threats/place-of p e)))

(def lost-s
  "Seconds out of line of sight after which a mob has stopped chasing."
  4)

(defn look!
  "Update the flight's chasers (job memory :chasers, by id): the real dangers within :radius (ranged :ranged-radius)
  join or are seen afresh; one that stopped chasing (stopped-chasing) moves to :fled with :ended why. The chasers
  still on, as JS entities, nearest first. A mob that hurt the body in its refuge (:reached) has a way whatever the map says."
  [c]
  (let [{:keys [radius ranged-radius hide-health]} (:args c)
        p (:primitives c)
        low? (<= (.-health (.self p)) hide-health)
        now (ctx/now c)
        dead (set (dead-ids c))
        live (remove #(dead (.-id %)) (known-chasers p))
        listed (into {} (map (juxt #(.-id %) identity)) live)
        reached (set (:reached (ctx/mem c)))
        joining (remove #(dead (.-id %)) (concat (danger-q/dangers p radius {:ranged-radius ranged-radius} {:sight? false})
                                                 (filter #(reached (.-id %)) live)))
        chasers (merge (:chasers (ctx/mem c)) (into {} (map (juxt #(.-id %) #(chaser-entry p % now))) joining))
        looked (set (:looked-back (ctx/mem c)))
        judged (for [[id ch] chasers
                     :let [e (listed id)
                           seen-t (if (and e (true? (.-visible e))) now (:seen-t ch))
                           ch (cond-> (assoc ch :seen-t seen-t) e (-> (dissoc :pos :direction :band :from) (merge (threats/place-of p e))))
                           m (when e (seen-now p e low?))
                           why (stopped-chasing (cond-> m (and m (contains? reached id)) (assoc :way? true)) seen-t now (* 1000 lost-s))]]
                 {:id id :ch ch :e e :why why
                  :look-back? (and (unsensed-loss? e why) (not (looked id)) (not (dead id)))})
        on (filter #(nil? (:why %)) judged)
        back (filter :look-back? judged)
        off (remove #(or (nil? (:why %)) (:look-back? %)) judged)]
    (ctx/update-mem! c #(-> %
                            (assoc :chasers (into {} (map (juxt :id :ch)) (concat on back)))
                            (assoc :look-back (mapv :ch back))
                            (update :looked-back (fn [ids] (vec (remove (set (keep (fn [{:keys [id e]}] (when (sensed-now? e) id)) judged)) ids))))
                            (update :fled (fnil into []) (map (fn [{:keys [ch why]}] (assoc ch :ended why))) off)))
    (sort-by #(danger-q/mob-distance p %) (map :e on))))

(defn ^:async look-back!
  "Turn to each chaser look! held back (job memory :look-back: lost while unsensed) at its last known place, once until it
  is sensed again; :again when it looked, nil when there was nothing to look back at."
  [c]
  (when-let [chs (seq (:look-back (ctx/mem c)))]
    (ctx/update-mem! c #(-> % (dissoc :look-back) (update :looked-back (fnil into []) (map :id chs))))
    (doseq [ch chs]
      (when-let [pos (danger-q/rough-pos ch)]
        (await (ctx/act c :look (clj->js {:pos (update pos :y + 0.9)})))))
    :again))

(defn far-flight [] (settings/get settings ::far-flight))

(defn chain-origin
  "Where the chain of flights ending at `to` began: the :from of the previous flight's entry (not yet told), else `to`'s
  own start `from`."
  [prev from]
  (or (:from (:data prev)) from))

(def not-chased-text
  "How a flight that ended with a chaser still on says so; any other end is a chaser that stopped."
  {:hidden "the body is hidden from it"
   :keeps-off "it kept up, no ground was gained"
   :cannot-escape "no way of escaping was left"})

(defn fled-notice
  "Pure: the hostile.fled fields for a flight that moved the body from `from` to `to` ({:x :y :z}) away from the mobs
  fled ([{:mob :id}]), ended why; nil when it moved less than far blocks."
  [from to mobs ended far]
  (let [d (when (and from to) (js/Math.round (u/dist from to)))
        at (fn [p] (mapv #(js/Math.floor (% p)) [:x :y :z]))
        names (mapv #(str (:mob %) " " (:id %)) mobs)]
    (when (and d (>= d far))
      {:from (at from) :to (at to) :distance d :mobs names :ended ended
       :text (str "fled " d " blocks from " (str/join ", " names) ": " (at from) " -> " (at to)
                  "; " (or (not-chased-text ended) "it stopped chasing") " (" (name ended) ")")})))

(defn end-flight!
  "Write a :threat entry per mob fled (jobs.lib.threats; one per mob, its last way out) and end the flight: done with
  {:fled [ids] :ended}, :ended why the last chaser stopped (or ended, given for the chasers still on: :hidden); ended
  :cannot-escape ends nothing-possible :cannot_escape {:fled [ids]}. A hidden end adds :hold, :quiet or :spent."
  [c ended & [extra]]
  (let [{:keys [chasers fled]} (ctx/mem c)
        by-id (merge (into {} (map (juxt :id identity)) fled)
                     (into {} (map (fn [[id ch]] [id (assoc ch :ended ended)])) chasers))
        all (vals by-id)
        why (or ended (:ended (peek (vec fled))) :none)
        ids (mapv :id all)]
    (doseq [t all] (threats/remember! c t))
    (let [origin (chain-origin (ctx/latest c :flight) (:flight-from (ctx/mem c)))
          n (fled-notice origin (u/self-pos c) all why (far-flight))]
      (ctx/forget-where! c :flight (constantly true))
      (if n
        (do (ctx/emit! c :hostile.fled :warn n)
            (ctx/remember! c :displaced {:from origin} displaced-policy))
        (when origin (ctx/remember! c :flight {:from origin} {:cap 1 :ttl (settings/get settings ::chain-window-ms)}))))
    (ctx/update-mem! c dissoc :chasers :fled :look-back :looked-back :pocket-tried :plugging :early-refuge-failed :refused)
    (if (= :cannot-escape ended)
      (ctx/end c :nothing-possible :cannot_escape {:fled ids :text "every way of escaping failed twice in a row"})
      (ctx/end c :done nil (merge {:fled ids :ended why} extra)))))
