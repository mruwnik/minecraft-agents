(ns jobs.gather.mine.tunnel
  "The strip tunnel of jobs.gather.dig-for: where it starts, the stretches cut by jobs.gather.strip-tunnel children, turning to
  another heading, and its end."
  (:require [jobs.gather.mine.way-set :as ways]
            [clojure.string :as str]
            [jobs.lib.need :as need]
            [jobs.lib.waits :as waits]
            [jobs.lib.block-sets :as bs]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.gather.mine-stay :as stay]
            [jobs.gather.mine.branch :as branch]
            [jobs.gather.mine.end :refer [stop!]]
            [jobs.gather.mine.targets :as targets :refer [kinds book-ground! step-cell main-block]]
            [jobs.gather.mine.torches :as torches]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.child :as child]
            [jobs.lib.ore-pick :as ore-pick]
            [jobs.lib.look :refer [cell-of headings]]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::max-open-skip {:default 64 :doc "The most open cells the strip tunnel skips along its line before it starts." :spec (a/int-in 0 nil)}
   ::open-skip-drop {:default 3 :doc "The most blocks the strip tunnel's start steps down while it skips open cells on its line (a stair); the tunnel then runs at that level." :spec (a/int-in 0 nil)}})

(def passable "Blocks a body walks through in a tunnel cell: air and torches (never dug)." (into bs/air #{"torch" "wall_torch"}))

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
  it, or up the stair down, is not a heading. False (nothing changed) when all are tried, or a direction arg was given, or a heading arg was given and the stop is not water in the cut (a player picks another way round water, not round an order)."
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
    (when (and to (nil? direction) (or (nil? heading) (= :fluid-in-cut stop)))
      (ctx/emit! c :mine.turned :info {:from (:heading m) :to to :stop stop :length (or steps 0)
                                       :text (str "mine's " (name stop) " blocks its heading " (:heading m) (when mid? (str " after " steps " blocks")) "; it turns " to)})
      (when mid? (stay/record-tunnel! c tunnel (headings (:heading m))))
      (ctx/update-mem! c #(cond-> (-> % (assoc :heading to :tried-headings tried) (dissoc :tunnel :behind :glanced :looked) (update :descent dissoc :stop))
                            tunnel (-> (update :legs (fnil conj []) (cond-> {:heading (:heading m) :length (or steps 0) :stop stop} (:behind m) (assoc :behind (:behind m))))
                                       (update :spent (fnil + 0) (or steps 0)))))
      true)))

(defn tunnel-reason [stop] (case stop :tunnel-length :tunnel-length :no-tool :no-tool :tunnel-stopped))

(defn tunnel-end!
  "The tunnel is done (stop: :tunnel-length or why it stopped at cell at): then the mend; the ending carries :stop :length :at :next :walk :fluid :behind.
  walk: for :walk-failed, what stopped the walk (step-to!)."
  [c stop at & [next walk]]
  (let [{:keys [tunnel]} (ctx/mem c)
        {:keys [heading steps]} tunnel
        fluid (when (and at (= :fluid-in-cut stop)) (u/seen-name (:primitives c) at))]
    (if (and (if (ways/stay-level? (:args c)) (leg-stops stop) (and (zero? steps) (heading-stops stop)))
             (< (+ steps (:spent (ctx/mem c) 0)) (:tunnel-length (:args c)))
             (turn-heading! c stop))
      :again
      (do
    (ctx/update-mem! c update :legs (fnil conj []) (cond-> {:heading heading :length steps :stop stop} (:behind (ctx/mem c)) (assoc :behind (:behind (ctx/mem c)))))
    (ctx/update-mem! c assoc-in [:tunnel :stop] stop)
    (stay/record-tunnel! c tunnel (headings heading))
    (stop! c (tunnel-reason stop)
           (cond-> {:stop stop :length steps}
             at (assoc :at (access/cell at))
             fluid (assoc :fluid (keyword fluid))
             next (assoc :next (access/cell next))
             (:behind (ctx/mem c)) (assoc :behind (:behind (ctx/mem c)))
             walk (assoc :walk walk)))))))

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

(defn book-cut!
  "After a stretch of tunnel: the cells it dug booked for the mend (ground under the start), and the last mined block among them
  collected like a target."
  [c dug]
  (doseq [{:keys [cell block]} dug] (book-ground! c (zipmap [:x :y :z] cell) block))
  (when-let [cell (last (keep #(when (contains? (kinds (:args c)) (:block %)) (:cell %)) dug))]
    (ctx/update-mem! c assoc :collecting true :dug-at cell)))

(defn watch-kinds
  "The blocks whose coming into view the strip tunnel stops for: the mined block and the ores the pick-up takes on the way."
  [c]
  (into (kinds (:args c)) (ore-pick/names)))

(defn ^:async tunnel-round!
  "One stretch of the strip tunnel: a jobs.gather.strip-tunnel child (slot :tunnel), which ends when a block that is dug for or picked
  up comes into view, so the dig's round runs between; the tunnel's end is tunnel-end!."
  [c]
  (let [m (ctx/mem c)
        {:keys [steps stop heading] :as t} (:tunnel m)
        args (:args c)]
    (if stop
      (stop! c (tunnel-reason stop))
      (let [_ (ctx/update-mem! c assoc :stepping true)
            r (waits/waiting-out c (await (ctx/call c :tunnel 'jobs.gather.strip-tunnel
                               {:heading (:heading m) :origin (:origin t) :steps (or steps 0) :limit (branch/segment-limit c (or steps 0) (- (:tunnel-length args) (:spent m 0)))
                                :hung (vec (:hung m)) :torches (:torches args) :watch-kinds (vec (watch-kinds c))
                                :radius (:radius args) :wet (boolean (:wet args)) :accept (vec (ways/accept args)) :stay-level (ways/stay-level? args)
                                :ways (need/pass-ways c) :ignore-zones? (:ignore-zones? args)
                                :block (main-block args)})))]
        (if (= :continue r)
          :continue
          (let [res (:data r)
                stop (:stop res)
                now (:steps res)
                {:keys [origin at next walk]} res
                cell (fn [v] (when v (zipmap [:x :y :z] v)))]
            (ctx/update-mem! c #(cond-> (-> % (dissoc :stepping) (assoc :tunnel (merge t (select-keys res [:origin :heading :steps])) :hung (:hung res))
                                            (cond-> (:behind res) (assoc :behind (:behind res))))
                                  (> now (or steps 0)) (assoc :looked (step-cell {:origin origin :heading (:heading res)} now))))
            (book-cut! c (:dug res))
            (cond
              (:cause res) (stop! c (:reason (:cause res)) (select-keys (:cause res) [:tool :why :text]))
              (= :watch stop) :again
              (and (= :tunnel-length stop) (branch/due? c now)) (do (branch/begin! c (:tunnel (ctx/mem c))) :again)
              (= :no-torches stop) (stop! c :no-torches {:text (torches/no-torches-text false)})
              :else (tunnel-end! c stop (cell at) (cell next) walk))))))))
