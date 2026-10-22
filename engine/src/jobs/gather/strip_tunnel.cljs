(ns jobs.gather.strip-tunnel
  (:require [clojure.string :as str]
            [engine.args :as a]
            [jobs.lib.work-place :as work-place]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.gather.mine-stay :as stay]
            [jobs.gather.mine.cause :as cause]
            [jobs.gather.mine.targets :as targets :refer [rules-in step-cell within-own-zone]]
            [jobs.gather.mine.torches :as torches]
            [jobs.gather.mine.tunnel :as tunnel :refer [passable open-start result-of walk-of back-on-line!]]
            [jobs.lib.waits :as waits]
            [jobs.lib.access :as access]
            [jobs.lib.access-tool :as atool]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.need :as need]
            [jobs.lib.look :as look :refer [cell-of headings glance!]]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]))

(def doc
  "Cut a straight level tunnel along :heading from where the body stands (its start moves on over open cells the body cut itself,
  tunnel/open-start), one jobs.access.stair {:dir :level :steps 1} child per step, torches hung where jobs.lib.torch-plan says (see :torches): the
  tunnel way of jobs.gather.dig-for. A call is one stretch of a tunnel: :origin and :steps name the stretch already cut (nil and 0
  at the start), :limit the most steps the tunnel may have in all. With :stay-level the air a cut opened into beyond the tunnel
  is filled with a carried block from the new cell (ending :breach-sealed, else :breach-open).

  Ends when the stretch is done: :done with data {:stop :tunnel-length} at :limit, or {:stop :watch} when a block of :watch-kinds with
  a seen air face is in view that was not when the call began, or a step dug one (the caller digs it, then calls again). Any other
  stop ends the call with data {:stop reason :at :next :walk} (the stair's stops, :own-zone, :walk-failed, :breach-sealed,
  :breach-open): condition-failed for facts, else nothing-possible. No torch to hang and none to craft ends condition-failed
  :no-torches. The data always carries :origin :heading :steps :hung :dug [{:cell :block}] :behind. Needs a pickaxe as the
  stair does (ways passed on).")

(a/defargs args
  {:heading {:doc "north, east, south or west" :spec #{"north" "east" "south" "west"}}
   :origin {:doc "the tunnel's first cell {:x :y :z}; nil: the body's cell, moved on over open cells it cut itself" :spec (a/pred "a cell map or nil" #(or (nil? %) (map? %))) :default nil}
   :steps {:doc "steps already cut" :spec (a/int-in 0 nil) :default 0}
   :limit {:doc "the most steps the tunnel has in all" :spec (a/int-in 0 nil)}
   :hung {:doc "the cells [x y z] torches are already hung from" :spec (a/coll-of a/position?) :default []}
   :torches {:doc "hang torches where the planner says (jobs.lib.torch-plan: no dark cell of the last few or the next stays unlit); false: none" :spec boolean? :default true}
   :watch-kinds {:doc "block names whose coming into view (with a seen air face) or being dug ends the call" :spec (a/coll-of string?) :default []}
   :radius {:doc "blocks round the body within which :watch-kinds are looked for" :spec (a/num-in 0 nil) :default 16}
   :wet {:doc "water beside a watched block does not hide it; water beside the cut is taken" :spec boolean? :default false}
   :accept {:doc "dig hazards taken (jobs.access.stair)" :spec (a/coll-of #{:falling-block :fluid-adjacent :lava-adjacent}) :default []}
   :stay-level {:doc "keep the level; fill air a cut opens into beyond the tunnel" :spec boolean? :default false}
   :block {:doc "the block the dig is for (the tool, hunger and water checks of each step)" :spec string? :default "stone"}
   :through {:doc "the id of a plan the tunnel is cut for (a rail line): its footprint is no obstacle" :spec string? :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::max-redigs {:default 32 :doc "The most times one step is cut again because a falling block landed in it (a column of gravel above the tunnel)." :spec (a/int-in 0 nil)}})

(def ways need/fetch-ways)

(def target [:heading])

(def safe? (ready/safe-at-or work-place/ahead ready/safe-if-lit true))

(def fit? ready/fit?)

(def defaults {:spend {:job-time ::job-time-s}})

(defn exposed
  "The watched blocks in view with a seen air face (not buried), as a set of positions."
  [c]
  (let [{:keys [watch-kinds radius wet]} (:args c)]
    (if (empty? watch-kinds)
      #{}
      (into #{} (comp (map :pos) (filter #(not= :buried (targets/classify c wet %))))
            (look/seen-blocks (:primitives c) {:names (vec watch-kinds) :radius radius :max 128 :live? true})))))

(defn side-ore?
  "Whether a seen watched block lies in the wall to the side of the body's cell, level with it, for a stay-level side cut."
  [c]
  (let [{:keys [watch-kinds radius stay-level heading]} (:args c)
        here (cell-of (u/self-pos c))]
    (boolean (and stay-level (seq watch-kinds)
                  (some #(stay/side-cut here (headings heading) (settings/get stay/settings ::stay/ore-reach) %)
                        (map :pos (look/seen-blocks (:primitives c) {:names (vec watch-kinds) :radius radius :max 128 :live? true})))))))

(defn watch-hit?
  "Whether the dig has something to do: a step dug a watched block; or, level-bound, a watched block beside the body is exposed or lies
  for a side cut; else one came into view exposed that was not at the call's start."
  [c]
  (let [{:keys [watching dug]} (ctx/mem c)
        kinds (set (:watch-kinds (:args c)))
        seen (exposed c)]
    (boolean (or (some #(contains? kinds (:block %)) dug)
                 (if (:stay-level (:args c))
                   (or (some #(stay/beside-body? (cell-of (u/self-pos c)) %) seen) (side-ore? c))
                   (seq (remove (set watching) seen)))))))

(defn data-of [c stop at next walk]
  (let [m (ctx/mem c)
        {:keys [origin heading steps]} (or (:tunnel m) (select-keys (:args c) [:origin :heading :steps]))]
    (cond-> {:stop stop :origin origin :heading heading :steps steps :hung (:hung m) :dug (:dug m []) :behind (:behind m)}
      at (assoc :at (access/cell at))
      next (assoc :next (access/cell next))
      walk (assoc :walk walk))))

(defn finish!
  "End the call: :done for the stretch's own ends (:tunnel-length :watch), else by the stop a kind."
  [c stop at & [next walk]]
  (ctx/update-mem! c assoc :finished true)
  (let [data (assoc (data-of c stop at next walk) :text (str "strip-tunnel " (name stop) " after " (get-in (ctx/mem c) [:tunnel :steps]) " steps"))]
    (cond
      (#{:tunnel-length :watch} stop) (ctx/end c :done nil data)
      (or (atool/facts stop) (#{:own-zone :no-torches} stop)) (ctx/end c :condition-failed {:reason stop} data)
      :else (ctx/end c :nothing-possible {:reason stop} data))))

(defn step-accept [{:keys [accept wet]}]
  (cond-> [] (some #{:falling-block} accept) (conj :falling-block) wet (conj :fluid-adjacent)))

(defn step-stop
  "The tunnel's stop from the stair step's result res (nil: it declined): a hazard is named by its key (:lava-adjacent :fluid-adjacent
  :falling-block) at the hazard's cell; else the stair's reason (:refused when it declined) at its cell, as a map."
  [res]
  (let [h (first (:hazards res))
        at (some-> (or (when (= :hazard (:reason res)) (:at h)) (:cell res)) (#(zipmap [:x :y :z] %)))]
    {:reason (cond (nil? res) :refused (and h (= :hazard (:reason res))) (access/hazard-key h) :else (:reason res)) :at at}))

(defn landed?
  "The walk into the cut stalled on a line cell that is solid again (a falling block landed in it after the cut): the step is cut again."
  [c name-at cell]
  (and (not (passable (name-at cell)))
       (< (:redigs (ctx/mem c) 0) (settings/get settings ::max-redigs))))

(defn under [cell] (update (zipmap [:x :y :z] cell) :y dec))

(defn ^:async redig-line!
  "A falling block (gravel) filled the line cell the walk was bound for, and the walk ended on top of it: dig that cell again,
  as a player would, then the step goes on from it. :continue while the dig waits."
  [c cell]
  (let [e (await (ctx/call c :redig 'jobs.blocks.dig (merge (select-keys (:args c) [:ignore-zones?])
                                                          {:pos (access/cell cell) :ways #{:carried} :accept [:falling-block]})))]
    (if (= :continue e)
      :continue
      (do (ctx/update-mem! c update :redigs (fnil inc 0)) :again))))

(defn ^:async stair-step!
  "The next cells by a jobs.access.stair child, one :level step (it judges, digs, seals exposed lava, plugs laid-open
  water, bridges a missing floor and steps in). With :stay-level a cut that opened into air beyond the tunnel has that air
  filled from the new cell."
  [c from next cut]
  (let [{:keys [heading]} (:tunnel (ctx/mem c))
        args (:args c)
        name-at #(u/seen-name (:primitives c) %)
        _ (await (watch/watch! c {:risky? true :before-dig (first (remove #(passable (name-at %)) cut))}))
        _ (ctx/update-mem! c assoc :stepping true)
        r (waits/waiting-out c (await (ctx/call c :step 'jobs.access.stair
                                 {:dir :level :heading (keyword heading) :steps 1 :accept (step-accept args) :ways (need/pass-ways c) :need-drop true
                                  :ignore-zones? (:ignore-zones? args) :through (:through args)})))
        res (result-of r)]
    (when-not (= :continue r) (ctx/update-mem! c dissoc :stepping))
    (when-not (= :continue r) (ctx/update-mem! c update :dug into (:dug res)))
    (cond
      (= :continue r) :continue
      (and (= :off-stair (:reason res)) (:cell res) (landed? c name-at (under (:cell res))))
      (await (redig-line! c (under (:cell res))))
      (not= :done (:status res)) (let [{:keys [reason at]} (step-stop res)]
                                   (finish! c reason at next (when (= :step-failed reason) (some->> (:because r) result-of (walk-of c next)))))
      :else
      (let [own (ledger/own-cells c)
            open (when (:stay-level args) (seq (stay/breaches name-at from cut own)))]
        (if open
          (let [b (stay/behind name-at from cut own open)
                air (stay/air-beside name-at from cut own)]
            (stay/remember-behind! c (access/cell (first open)) b name-at air)
            (ctx/update-mem! c assoc :behind b)
            (ctx/update-mem! c update-in [:tunnel :steps] inc)
            (finish! c (if (await (stay/seal! c (rules-in c) air)) :breach-sealed :breach-open) (first open) next))
          (do (ctx/update-mem! c update-in [:tunnel :steps] inc)
              (let [w (await (back-on-line! c next))]
                (cond
                  (= :continue w) :continue
                  (and w (do (await (glance! c [(headings heading)])) (landed? c name-at next)))
                  (do (ctx/update-mem! c #(-> % (update :redigs (fnil inc 0)) (update-in [:tunnel :steps] dec))) :again)
                  w (finish! c :walk-failed next nil w)
                  :else (do (ctx/update-mem! c dissoc :redigs)
                            (await (glance! c [(headings heading)]))
                            (if (watch-hit? c) (finish! c :watch nil nil nil) :again))))))))))

(defn start!
  "The tunnel's first cell (the :origin arg, else the body's cell moved on over open cells it cut itself) and what is in view."
  [c]
  (let [{:keys [heading steps hung stay-level origin]} (:args c)
        start (cell-of (u/self-pos c))
        own (ledger/own-cells c)
        origin (or origin
                   (let [o (open-start #(u/seen-name (:primitives c) %) start (headings heading) #(contains? own [(:x %) (:y %) (:z %)]) (when stay-level 0))]
                     (when (not= o start)
                       (ctx/emit! c :mine.tunnel-skipped :info {:from (access/cell start) :to (access/cell o) :heading heading
                                                                :text (str "mine's tunnel line is open from " (str/join "," (access/cell start)) " to "
                                                                           (str/join "," (access/cell o)) "; it starts there")}))
                     o))]
    (ctx/update-mem! c assoc :tunnel {:origin origin :heading heading :steps steps} :hung (vec hung) :watching (vec (exposed c)) :dug [])))

(defn ^:async step [c]
  (let [m (ctx/mem c)
        why (cause/stop-cause c)]
    (cond
      why (do (ctx/update-mem! c assoc :finished true :cause why)
              (ctx/end c :condition-failed {:reason (:reason why) :text (:text why)} (assoc (data-of c (:reason why) nil nil nil) :cause why)))
      (not (:tunnel m)) (do (start! c) :again)
      :else
      (let [t (:tunnel m)
            {:keys [steps heading]} t
            from (step-cell t steps)
            next (step-cell t (inc steps))
            cut [(update next :y inc) next]
            walk (atom nil)]
        (cond
          (:stepping m) (await (stair-step! c from next cut))
          (some #(within-own-zone c 0 (access/cell %)) cut) (finish! c :own-zone next)
          (>= steps (:limit (:args c))) (finish! c :tunnel-length nil)
          (do (reset! walk (await (back-on-line! c from))) (= :continue @walk)) :continue
          (and @walk (= :done (:status @walk)) (landed? c #(u/seen-name (:primitives c) %) from))
          (await (redig-line! c from))
          @walk (finish! c :walk-failed from nil @walk)
          (and (look/unseen? (:primitives c) cut) (not= next (:glanced m)))
          (do (ctx/update-mem! c assoc :glanced next)
              (await (glance! c [(headings heading)]))
              :again)
          (torches/torch-due? c) (let [r (await (torches/torch-round! c false))]
                                   (if (= :no-torches r) (finish! c :no-torches nil) r))
          :else (await (stair-step! c from next cut)))))))

(defn done?
  [c]
  (if (:finished (ctx/mem c))
    (contract/ok :done "the stretch of tunnel is cut" :units :tunnels :done 1 :left 0)
    (contract/later :cutting "the tunnel is being cut" :units :tunnels :done 0 :left 1)))

(defn quote-of [c]
  (let [n (max 0 (- (:limit (:args c)) (:steps (:args c) 0)))
        s (* n 2 1.2)]
    {:uses {:job-time [(* 0.5 s) s (* 4 s)]} :done [0 1 1]}))

(defn ^:async round [c] (await (pace/steps! c #(step c))))

(def meta {:category :phase :part-of '[jobs.gather.dig-for] :tags [:mine :dig]})
