(ns jobs.survival.dig-pit
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.pen-gate :as pen-gate]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.threats :as threats]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.waits :as waits]
            [jobs.lib.dig-look :as dig-look]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-pit-site :as site]
            [jobs.survival.plug-way :as way]
            [jobs.survival.shut-in-night :as night]
            [jobs.survival.respond.state :as flight]
            [jobs.survival.pit-cells :as pit]))

(def doc
  "A plug way: dig down 2 (3 on flat ground) under a solid side and plug the hole over the head with carried or dug blocks.
  Needs a pit that fits (every cell solid rock, no fluid or hazard in or beside it, none at the roof, solid under the bottom) and a
  block to plug with, carried or dug (a block the tools cannot harvest is dug by hand once one is in hand and the hand breaks it within pit-cells's ::night-hand-clear-ms for :night, else jobs.lib.tools quick-clear-ms).
  One call is a whole attempt: :done {:pos :way :pit :roof :start} once enclosed, at once when it already is; nothing-possible :failed
  {:reason :text :needs} when a pit fails, the reason a stop of jobs.survival.pit-cells: :no-floor, :fluid-here, :fluid-adjacent,
  :fluid-above, :hazard-below, :no-tool (:needs the block), :hostile-near (a melee hostile stands in the way), :no-blocks, :dig-failed, :descent-stalled, :roof-failed, :lays-open (a dig
  laid a cave or fluid open). The pit is never dug into what the body sees as fluid, a hazard or open air, and it is planned again
  when the body has left its column. Lava a dig lays open is filled first (:on-lava :seal). Every plug goes to the scaffold ledger
  with :for (default :danger, ledger :retreat-plug, taken back by jobs.access.cleanup once no hostile is near; :night: ledger :shelter, left alone). Quote:
  time, with :for :night plus the walk (##Inf when no pit fits, or with :for :night when the pit would cut into a plan the body made or stand on a plan's doorstep, :spared; a cell another's zone refuses adds plug-way's ::trespass-s). With :for :night, can-start? is :later (:no-site, or :spared at the feet) before any round. Picked by jobs.survival.shut-in, which shuts the body in by the cheapest way.
  With :for :night, when a pit failed at the feet tonight (shut-in's :shut-in-failed) or another's zone refuses them and a free site is near, it first walks (go-to) to the nearest seen ground within jobs.survival.dig-pit-site's ::reach (16) that looks like a pit fits (clear of every failed site, no known mob it would flee near), climbing out of its own failed pit first; with no such ground it ends :failed :no-site.")

(a/defargs args
  {:ignore-zones? :shared
   :for {:doc "why the body shuts itself in: :danger (blocks are taken back by the cleanup once no hostile is near; a hostile in a cell fails it), :night (blocks stay for the night; a mob in a cell is waited out)" :spec #{:danger :night} :default :danger}
   :trespass? {:doc "false: a pit that would need a cell of another's zone, claim or plan fails instead (the caller tries again with true as a last resort)" :spec boolean? :default true}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pit is dropped and the lava left" :spec #{:seal :stop} :default :seal}})

(a/defargs settings
  {::job-time-s {:default 90 :doc "Seconds a pit attempt may take." :spec (a/num-in 0 nil)}
   ::walk-s-per-block {:default 0.3 :doc "Seconds a block of walk to a night pit's site is guessed to take, for the quote." :spec (a/num-in 0 nil)}
   ::pit-s {:default 12 :doc "Seconds a pit is guessed to take, for the quote, while a floor under it is not yet seen (a pit that fits is priced by its digging)." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(defn done? [c] (way/done? c :pit))

(defn door-hole
  "The verdict {:cell :plan :door} for the first of cells within a block of a door or gate some plan wants (its doorstep: a
  hole there leaves the closed building without a way in), else nil."
  [c cells]
  (let [openings (pen-gate/opening-index (:world (:engine c)))]
    (some (fn [{:keys [x y z]}]
            (some (fn [[[gx gy gz] plan]]
                    (when (and (<= (abs (- x gx)) 1) (<= (abs (- z gz)) 1) (<= (- gy 2) y gy))
                      {:cell [x y z] :plan plan :door [gx gy gz]}))
                  openings))
          cells)))

(defn spared
  "With :for :night, the verdict ({:zone :plan :cell}) of the first cell the pit plan digs that lies in a plan the body made (access/own-keep) or on a plan's doorstep (door-hole): not dug."
  [c plan]
  (when (= :night (:for (:args c)))
    (let [in (access/rules-input c {:spare-own? true})
          cells (concat (:prep plan) (mapcat :dig (:steps plan)))]
      (or (some #(access/own-keep in %) cells) (door-hole c cells)))))

(defn spared-stop
  "The stop of a pit plan the body's own zone or plan forbids, nil when none."
  [c plan]
  (when-let [{:keys [zone plan cell door]} (spared c plan)]
    {:stop :spared
     :text (str "the pit would cut into the body's own " (cond zone (str "zone " zone) door (str "plan " plan "'s doorstep (door " (pr-str door) ")") :else (str "plan " plan)) " at " (pr-str cell) "; not digging")}))

(defn trespass-stop
  "With :trespass? false, the stop of a pit plan that digs or plugs a cell of another's zone, claim or plan."
  [c plan]
  (when (and (false? (:trespass? (:args c)))
             (way/refused? c {:dig (concat (:prep plan) (mapcat :dig (:steps plan))) :plug (conj (:plugs plan) (:roof plan))}))
    {:stop :trespass :text "the pit needs a cell of another's area"}))

(defn assess
  "{:plan p} for a pit that fits (p is nil while only a floor not yet seen rules it out: the pit looks first; not so for :night,
  which looks before it asks), else {:stop stop} (pit-cells/stop-of, or :spared)."
  [c]
  (let [feet (sh-sense/feet (:primitives c))
        blocks (flight/blocks c)
        plan (pit/pit-plan c feet blocks)
        stop (if plan (or (spared-stop c plan) (trespass-stop c plan)) (pit/stop-of c blocks feet))]
    (cond
      (and (not plan) (:unseen stop) (not= :night (:for (:args c)))) {:plan nil}
      stop {:stop stop}
      :else {:plan plan})))

(defn why-not
  "The stop (pit-cells/stop-of's {:stop reason :text}) that rules a pit out here, nil when one may fit."
  [c]
  (:stop (assess c)))

(defn ^:async look!
  "A look at the roof's surroundings and the side columns the body has not sensed (pit-cells/plan-of!), so the quote reads what a pit would."
  [c]
  (await (pit/plan-of! c (flight/blocks c)))
  nil)

(defn priced-name
  "The block name a dig of cell is priced by: what is seen there, else the nearest seen solid block above it (the ground the pit is
  dug into: dirt stays dirt below the surface), else the rock guess."
  [p cell]
  (or (u/seen-name p cell)
      (->> (range 1 (inc (dig-look/rock-depth))) (keep #(u/seen-name p (update cell :y + %))) (filter solid/solid?) first)
      (dig-look/rock-name p cell)))

(defn dig-seconds
  "Seconds the plan's cells take to dig with the best carried tool or the hand."
  [c plan]
  (let [p (:primitives c)]
    (/ (reduce + (map #(tools/clear-ms p (priced-name p %)) (concat (:prep plan) (mapcat :dig (:steps plan))))) 1000)))

(defn quote-of [c]
  (let [{:keys [plan stop]} (assess c)
        s (when (night/night? c) (site/site c))]
    (if (and (night/night? c) (not= :here (:at s)))
      (way/quote-of c (if s
                        (+ (* (u/dist (sh-sense/feet (:primitives c)) (:cell s)) (settings/get settings ::walk-s-per-block)) (settings/get settings ::pit-s))
                        ##Inf)
                    (settings/get settings ::job-time-s))
    (way/quote-of c (if stop
                      ##Inf
                      (+ (if plan
                           (+ (dig-seconds c plan) (* (way/place-s c) (inc (count (:plugs plan)))))
                           (settings/get settings ::pit-s))
                         (if plan (way/zone-penalty c {:dig (concat (:prep plan) (mapcat :dig (:steps plan))) :plug (conj (:plugs plan) (:roof plan))}) 0)))
                  (settings/get settings ::job-time-s)))))

(defn ^:async step!
  "One step of the pit: its plan is made once and kept in job memory, made again when the body has left its column. :done,
  :again, or nil with no pit that fits or one that failed (the stop in memory)."
  [c]
  (let [kept (:pit (ctx/mem c))
        plan (if (and kept (pit/pit-index kept (sh-sense/feet (:primitives c)))) kept (await (pit/plan! c)))]
    (if-let [stop (or (:stop plan) (when-not kept (or (spared-stop c plan) (trespass-stop c plan))))]
      (way/stop! c stop)
      (do (when (and (night/night? c) (not kept))
            (ctx/remember! c :shut-in-failed {:pos (sh-sense/feet (:primitives c)) :way :pit :reason :interrupted} night/failed-policy))
          (ctx/update-mem! c assoc :pit plan)
          (let [r (await (pit/pit-round! c plan))]
            (if (:stop r)
              (way/stop! c r)
              (case r
                :hidden :done
                :again :again
                (way/stop! c {:stop :failed :text (str "the pit round ended with " (pr-str r) " at " (pr-str (sh-sense/feet (:primitives c))))}))))))))

(defn can-start?
  "With :for :night: ground where a pit fits is in sight, and the pit does not cut into a plan the body made. Floors not yet seen are no fact here: the round looks first."
  [c]
  (let [s (when (night/night? c) (site/site c))
        stop (when (= :here (:at s)) (why-not c))]
    (cond
      (not (night/night? c)) (contract/ok :pit "a pit is tried when it starts")
      (nil? s) (contract/later :no-site "no ground in sight where a pit fits")
      (= :spared (:stop stop)) (contract/later :spared (:text stop))
      :else (contract/ok :site "ground for a night pit"))))

(def safe? ready/ok)
(def fit? ready/ok)

(defn fail! [c reason text]
  (ctx/end c :nothing-possible :failed {:text text :reason reason}))

(defn ^:async walk!
  "The night walk to ground where a pit fits, once: nil once there, :continue while a climb or walk waits, or the ending of one that fails."
  [c cell]
  (if-let [start (site/pit-start c)]
    (let [r (await (dig-leave/climb! c {:start start} cell))]
      (if (map? r) (fail! c :trapped "could not climb out of the failed pit") :continue))
    (let [e (waits/waiting-out c (await (ctx/call c :walk 'jobs.movement.go-to {:pos cell :range 0})))]
      (cond (= :continue e) :continue
            (or (not (:arrived (:data e))) (:re-aimed (:data e))) (fail! c :walk-failed "could not walk to ground for a pit")
            :else (do (ctx/update-mem! c dissoc :walk-to)
                      (ctx/emit! c :shut-in.relocated :info {:pos cell :sites (site/failed c) :text "the last site failed; moved to dig in elsewhere"})
                      nil)))))

(defn ^:async approach!
  "With :for :night and no pit begun: get to a site (walk!). nil once there, else :continue or the ending."
  [c]
  (when (and (night/night? c) (not (:pit (ctx/mem c))) (not (way/enclosed? c)))
    (let [m (:walk-to (ctx/mem c))
          m (when-not (and m ((threats/danger-near-fn c) m)) m)
          s (if m {:at :there :cell m} (site/site c))]
      (cond
        (nil? s) (fail! c :no-site "no ground in sight where a pit fits")
        (= :here (:at s)) nil
        :else (do (ctx/update-mem! c assoc :walk-to (:cell s))
                  (await (walk! c (:cell s))))))))

(defn ^:async round [c]
  (or (await (approach! c)) (await (way/run! c :pit step!))))
