(ns jobs.gather.mine.home
  "The walk home of jobs.gather.mine: back to the start cell through go-to, over every way but a pillar first."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.gather.mine.targets :refer [not-solid]]
            [jobs.lib.child :as child]
            [jobs.lib.look :refer [cell-of]]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::home-dip {:default 16 :doc "Blocks under the body (and over the start) the walk home may plan through while the start lies fewer than this many blocks above the body (go-to :y-margin): no way round through deep caves; the walk's escalation digs out instead. From deeper, go-to's own climb rules hold." :spec (a/num-in 0 nil)}})

(defn home-dip [] (settings/get settings ::home-dip))

(defn beside?
  "Whether here is a cell go-to's range 1 accepts as next to the start cell: one step along one axis, and never below it (that is the
  hole the digging left)."
  [here start]
  (and (>= (:y here) (:y start))
       (<= (reduce + (map #(let [d (- (% here) (% start))] (* d d)) [:x :y :z])) 1)))

(defn solid-floor?
  "Whether the floor under the start cell is solid: as seen now (seen), else as felt at the start (felt); an unknown floor is no place
  to aim a walk at."
  [seen felt]
  (let [name (or seen felt)]
    (boolean (and name (not (not-solid name))))))

(defn home-margin
  "The go-to :y-margin of a walk home to a start rise blocks above the body, or nil."
  [rise]
  (when (< rise (home-dip)) {:y-margin (home-dip)}))

(defn home-goal
  "Where this walk home goes, decided once: {:cell c :range 0} the start cell when its floor is solid (seen now, else felt at the
  start); else, a hole the mend left or a floor never seen, a seen standable cell next to it ({:range 0}: the body must not end in
  the hole), else {:cell start :range 1}. :y-margin (::home-dip) while the start is fewer than that above the body."
  [c start]
  (let [seen #(u/seen-name (:primitives c) %)
        margin (home-margin (- (:y start) (:y (cell-of (u/self-pos c)))))
        below (fn [{:keys [x y z]}] {:x x :y (dec y) :z z})
        solid-at? #(solid-floor? (seen (below %)) nil)
        open-at? #(some-> (seen %) not-solid boolean)
        beside (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                     :let [cell (-> start (update :x + dx) (update :z + dz))]
                     :when (and (solid-at? cell) (open-at? cell))]
                 cell)]
    (merge margin
           (cond
             (solid-floor? (seen (below start)) (:start-floor (ctx/mem c))) {:cell start :range 0}
             (seq beside) {:cell (first beside) :range 0}
             :else {:cell start :range 1}))))

(def home-ways
  "Every way but :pillar (and its :fill-gap): a pillar spends the mined blocks the run counts and leaves one in the shaft, so it is the last resort."
  #{:walk :clear-seal :clear-path :stair :dig-out})

(defn ^:async home-by-go-to!
  "The way home through a go-to child, with its escalation, to the goal home-round! decided (mem :home-goal): the ways of home-ways
  first, then, if that walk did not arrive, every way (mem :home-pillar). :continue while the walk goes on, else whether it arrived."
  [c start]
  (let [{:keys [cell range y-margin]} (:home-goal (ctx/mem c))
        arrived? #(let [here (cell-of (u/self-pos c))] (if (zero? range) (= here cell) (beside? here start)))
        walk! (fn ^:async walk [slot ways]
                (await (child/call-walk* c slot
                                         (cond-> {:pos {:x (+ (:x cell) 0.5) :y (:y cell) :z (+ (:z cell) 0.5)} :range range
                                                  :ignore-zones? (boolean (:ignore-zones? (:args c)))}
                                           ways (assoc :ways ways)
                                           y-margin (assoc :settings {:jobs.lib.walk.world/y-margin y-margin})))))
        r (cond
            (arrived?) :arrived
            (:home-pillar (ctx/mem c)) (await (walk! :home-last nil))
            :else (await (walk! :home home-ways)))]
    (cond
      (= :continue r) :continue
      (or (arrived?) (:home-pillar (ctx/mem c))) (arrived?)
      :else (do (ctx/update-mem! c assoc :home-pillar true)
                (await (home-by-go-to! c start))))))

(defn ^:async home-round!
  "The last step of every normal run: back to the start cell through a go-to child (beside it when its floor is open or unseen),
  whatever the digging left open behind it. :continue while it walks, else whether the body arrived."
  [c]
  (let [{:keys [start homing]} (ctx/mem c)]
    (when-not homing
      (ctx/update-mem! c assoc :homing true :home-goal (home-goal c start)))
    (await (home-by-go-to! c start))))
