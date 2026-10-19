(ns jobs.gather.mine.tunnel
  "The strip tunnel of jobs.gather.mine: where it starts, one :level stair step at a time, turning to another heading,
  and its end."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.gather.mine-stay :as stay]
            [jobs.gather.mine.end :refer [to-mend!]]
            [jobs.gather.mine.targets :as targets :refer [kinds carried rules-in book-ground! step-cell within-own-zone]]
            [jobs.gather.mine.torches :as torches]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.child :as child]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look :refer [cell-of headings glance!]]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]))

(a/defargs settings
  {::max-open-skip {:default 64 :doc "The most open cells the strip tunnel skips along its line before it starts." :spec (a/int-in 0 nil)}
   ::open-skip-drop {:default 3 :doc "The most blocks the strip tunnel's start steps down while it skips open cells on its line (a stair); the tunnel then runs at that level." :spec (a/int-in 0 nil)}})

(def passable "Blocks a body walks through in a tunnel cell: air and torches (never dug)." #{"air" "cave_air" "torch" "wall_torch"})

(defn max-open-skip [] (settings/get settings ::max-open-skip))

(defn open-skip-drop [] (settings/get settings ::open-skip-drop))

(defn open-start
  "Where a strip tunnel really starts: from cell, along heading dir [dx dz] over cells the body cut itself (own?, on a feet cell) that are
  open (feet and head passable, a solid floor, as seen), stepping down where the line drops to an open cell with a floor (a stair), at most max-drop (default
  ::open-skip-drop) blocks below cell in all; the last such cell."
  [name-at cell [dx dz] own? & [max-drop]]
  (let [open? (fn [p] (and (own? p) (passable (name-at p)) (passable (name-at (update p :y inc)))
                           (rules/solid-floor? name-at (update p :y dec))))
        ahead (fn [p] (-> p (update :x + dx) (update :z + dz)))]
    (loop [p cell n 0]
      (let [q (ahead p) down (update q :y dec)]
        (cond
          (>= n (max-open-skip)) p
          (open? q) (recur q (inc n))
          (and (passable (name-at q)) (open? down) (<= (- (:y cell) (:y down)) (or max-drop (open-skip-drop)))) (recur down (inc n))
          :else p)))))

(def heading-stops
  "Stops that say only this heading is blocked: the stair or tunnel at 0 steps tries the next heading."
  #{:breach-sealed :undercuts-way :own-zone :floor-unseen :not-loaded :base-block :crop :fluid-in-cut :place-failed :no-support-floor :no-floor :cave-below :walk-failed :step-failed :refused})

(def leg-stops
  "Stops after which a stay-level run goes on along another heading from the cell it stands in."
  (into heading-stops #{:fluid-adjacent :lava-adjacent :falling-block}))

(def opposite {"north" "south" "south" "north" "east" "west" "west" "east"})

(defn turn-heading!
  "Turn the run to the next of the four headings not yet tried (memory :tried-headings), forgetting the stop and the tunnel
  begun on the old one; info mine.turned. A tunnel of steps > 0 is kept as a leg (memory :legs, :spent); the way back along
  it, or up the stair down, is not a heading. False (nothing changed) when a heading arg was given or all are tried."
  [c stop]
  (let [m (ctx/mem c)
        {:keys [heading direction]} (:args c)
        {:keys [steps] :as tunnel} (:tunnel m)
        mid? (pos? (or steps 0))
        tried (cond-> (conj (set (:tried-headings m)) (:heading m))
                (or mid? (pos? (:steps (:descent m) 0))) (conj (opposite (:heading m))))
        cycle ["north" "east" "south" "west"]
        order (map #(nth cycle (mod (+ (.indexOf cycle (:heading m)) %) 4)) (range 1 4))
        to (first (remove tried order))]
    (when (and to (nil? heading) (nil? direction))
      (ctx/emit! c :mine.turned :info {:from (:heading m) :to to :stop stop :length (or steps 0)
                                       :text (str "mine's " (name stop) " blocks its heading " (:heading m) (when mid? (str " after " steps " blocks")) "; it turns " to)})
      (when mid? (stay/record-tunnel! c tunnel (headings (:heading m))))
      (ctx/update-mem! c #(cond-> (-> % (assoc :heading to :tried-headings tried) (dissoc :tunnel :behind :torch-at :glanced :looked) (update :descent dissoc :stop))
                            tunnel (-> (update :legs (fnil conj []) (cond-> {:heading (:heading m) :length (or steps 0) :stop stop} (:behind m) (assoc :behind (:behind m))))
                                       (update :spent (fnil + 0) (or steps 0)))))
      true)))

(defn tunnel-reason [stop] (if (= :tunnel-length stop) :tunnel-length :tunnel-stopped))

(defn walk-text [{:keys [status reason cell floor]}]
  (str " (walk " (str/join " " (map #(if (keyword? %) (name %) %) (remove nil? [status reason])))
       (when cell (str ", cell " cell ", floor " floor)) ")"))

(defn tunnel-end!
  "The tunnel is done (stop: :tunnel-length or why it stopped at cell at): one warn mine.tunnel-end, then the mend.
  walk: for :walk-failed, what stopped the walk (step-to!)."
  [c stop at & [next walk]]
  (let [{:keys [tunnel]} (ctx/mem c)
        {:keys [heading steps]} tunnel
        fluid (when (and at (= :fluid-in-cut stop)) (u/seen-name (:primitives c) at))
        got (targets/got c)]
    (if (and (if (:stay-level (:args c)) (leg-stops stop) (and (zero? steps) (heading-stops stop)))
             (< (+ steps (:spent (ctx/mem c) 0)) (:tunnel-length (:args c)))
             (turn-heading! c stop))
      :again
      (do
    (ctx/update-mem! c update :legs (fnil conj []) (cond-> {:heading heading :length steps :stop stop} (:behind (ctx/mem c)) (assoc :behind (:behind (ctx/mem c)))))
    (ctx/emit! c :mine.tunnel-end :warn
               (cond-> {:reason stop :heading heading :length steps :y (:y (:origin tunnel)) :got got
                        :text (str "mine's tunnel ended after " steps " blocks " heading " at y " (:y (:origin tunnel)) ": "
                                   (if (= :tunnel-length stop) "its length is used up" (str "stopped, " (name stop) (when fluid (str " (" fluid ")"))))
                                   (when at (str " at " (str/join "," (access/cell at))))
                                   (when walk (walk-text walk))
                                   "; got " got " of " (targets/owed c))}
                 at (assoc :at (access/cell at))
                 fluid (assoc :fluid (keyword fluid))
                 next (assoc :next (access/cell next))
                 (:behind (ctx/mem c)) (assoc :behind (:behind (ctx/mem c)))
                 walk (assoc :walk walk)))
    (ctx/update-mem! c assoc-in [:tunnel :stop] stop)
    (stay/record-tunnel! c tunnel (headings heading))
    (to-mend! c (tunnel-reason stop))))))

(defn ^:async end!
  "tunnel-end!, after a torch at the cut's end when one is due."
  [c stop at & [next walk]]
  (if (torches/end-torch-due? c)
    (await (torches/torch-step! c false))
    (tunnel-end! c stop at next walk)))

(defn step-accept [{:keys [accept wet]}]
  (cond-> [] (some #{:falling-block} accept) (conj :falling-block) wet (conj :fluid-adjacent)))

(defn step-stop
  "The tunnel's stop from the stair step's result res (nil: it declined): a hazard is named by its key (:lava-adjacent :fluid-adjacent
  :falling-block) at the hazard's cell; else the stair's reason (:refused when it declined) at its cell, as a map."
  [res]
  (let [h (first (:hazards res))
        at (some-> (or (when (= :hazard (:reason res)) (:at h)) (:cell res)) (#(zipmap [:x :y :z] %)))]
    {:reason (cond (nil? res) :refused (and h (= :hazard (:reason res))) (access/stair-hazard-key h) :else (:reason res)) :at at}))

(defn book-cut!
  "After a stair step: its dug cells booked for the mend (ground under the start), and the last mined block among them
  collected like a target."
  [c dug]
  (doseq [{:keys [cell block]} dug] (book-ground! c (zipmap [:x :y :z] cell) block))
  (when-let [cell (last (keep #(when (contains? (kinds (:args c)) (:block %)) (:cell %)) dug))]
    (ctx/update-mem! c assoc :collecting true :dug-at cell)))

(defn result-of
  "A child's ending e as the result map the tunnel reads: its data with :status and the cause's :reason, nil while it
  goes on."
  [e]
  (when (map? e)
    (assoc (:data e) :status (if (= :done (:kind e)) :done :stopped) :reason (contract/cause-key e))))

(defn walk-of
  "What stopped a walk into pos: the go-to result r's status and reason, and the names the body sees at pos and under it."
  [c pos r]
  (let [name-at #(u/seen-name (:primitives c) %)]
    {:status (:status r) :reason (:reason r) :cell (name-at pos) :floor (name-at (update pos :y dec))}))

(defn ^:async back-on-line!
  "Walk into the tunnel's cell pos (a go-to child) unless there: nil when there, :continue while it walks, else what
  stopped the walk (walk-of)."
  [c pos]
  (when-not (= (cell-of (u/self-pos c)) pos)
    (let [r (await (child/walk-into!* c :walk (access/cell pos)))]
      (cond
        (= :continue r) :continue
        (= pos (cell-of (u/self-pos c))) nil
        :else (walk-of c pos (result-of r))))))

(defn ^:async stair-step!
  "The next cells by a jobs.access.stair child, one :level step (it judges, digs, seals exposed lava, plugs laid-open
  water, bridges a missing floor and steps in). With :stay-level the stair only cuts: a cut that opened into air beyond
  the tunnel is sealed from the line cell, else the body walks in (a go-to child)."
  [c from next cut]
  (let [{:keys [heading]} (ctx/mem c)
        args (:args c)
        name-at #(u/seen-name (:primitives c) %)
        _ (await (watch/watch! c {:risky? true :before-dig (first (remove #(passable (name-at %)) cut))}))
        _ (ctx/update-mem! c assoc :stepping true)
        r (await (ctx/call c :step 'jobs.access.stair
                                 {:dir :level :heading (keyword heading) :steps 1 :accept (step-accept args) :fetch (:fetch args)
                                  :pick-up-ores false :ignore-zones? (:ignore-zones? args)
                                  :cut-only (boolean (:stay-level args))}))
        res (result-of r)]
    (when-not (= :continue r) (ctx/update-mem! c dissoc :stepping))
    (book-cut! c (:dug res))
    (cond
      (= :continue r) :continue
      (not= :done (:status res)) (let [{:keys [reason at]} (step-stop res)]
                                   (end! c reason at next (some->> (:walk res) (walk-of c next))))
      :else
      (let [own (ledger/own-cells c)
            open (when (:stay-level args) (seq (stay/breaches name-at from cut own)))]
        (if open
          (let [b (stay/behind name-at from cut own open)]
            (stay/remember-behind! c (access/cell (first open)) b name-at open)
            (ctx/update-mem! c assoc :behind b)
            (end! c (if (await (stay/seal! c (rules-in c) open)) :breach-sealed :breach-open) (first open) next))
          (do (ctx/update-mem! c #(-> % (update-in [:tunnel :steps] inc) (assoc :looked next)))
              (let [w (await (back-on-line! c next))]
                (cond
                  (= :continue w) :continue
                  w (end! c :walk-failed next nil w)
                  :else (do (await (glance! c [(headings heading)])) :again)))))))))

(defn ^:async tunnel-round!
  "One step of the strip tunnel: back onto its last cell (a go-to child), a torch when due, then the stair step (a step
  under way is gone on with first)."
  [c]
  (let [m (ctx/mem c)
        t (or (:tunnel m)
              (let [start (cell-of (u/self-pos c))
                    own (ledger/own-cells c)
                    origin (open-start #(u/seen-name (:primitives c) %) start (headings (:heading m)) #(contains? own [(:x %) (:y %) (:z %)])
                                       (when (:stay-level (:args c)) 0))]
                (when (not= origin start)
                  (ctx/emit! c :mine.tunnel-skipped :info {:from (access/cell start) :to (access/cell origin) :heading (:heading m)
                                                           :text (str "mine's tunnel line is open from " (str/join "," (access/cell start)) " to "
                                                                      (str/join "," (access/cell origin)) "; it starts there")}))
                {:origin origin :heading (:heading m) :steps 0}))
        _ (when-not (:tunnel m) (ctx/update-mem! c assoc :tunnel t))
        {:keys [steps stop heading]} t
        from (step-cell t steps)
        next (step-cell t (inc steps))
        cut [(update next :y inc) next]
        walk (atom nil)]
    (cond
      stop (to-mend! c (tunnel-reason stop))
      (:stepping m) (await (stair-step! c from next cut))
      (some #(within-own-zone c 0 (access/cell %)) cut) (end! c :own-zone next)
      (>= (+ steps (:spent m 0)) (:tunnel-length (:args c))) (end! c :tunnel-length nil)
      (do (reset! walk (await (back-on-line! c from))) (= :continue @walk)) :continue
      @walk (end! c :walk-failed from nil @walk)
      (and (look/unseen? (:primitives c) cut) (not= next (:glanced m)))
      (do (ctx/update-mem! c assoc :glanced next)
          (await (glance! c [(headings heading)]))
          :again)
      (torches/torch-due? c) (await (torches/torch-step! c false))
      :else (await (stair-step! c from next cut)))))
