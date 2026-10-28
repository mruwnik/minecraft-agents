(ns jobs.gather.mine.descent
  "The descent of jobs.gather.dig-for: whether and how it goes down (tunnel level, stair-or-shaft tests, the pick and the walk down a mineshaft)."
  (:require [clojure.string :as str]
            [jobs.lib.need :as need]
            [jobs.lib.waits :as waits]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.stair-time :as stair-time]
            [jobs.lib.places :as places]
            [jobs.lib.price :as price]
            [jobs.lib.escape :as escape]
            [jobs.gather.shaft-descent :refer [pick-shaft xyz]]
            [jobs.lib.util :as u]
            [jobs.lib.look :refer [cell-of headings]]
            [jobs.lib.block-sets :as bs]
            [jobs.gather.mine.targets :refer [base-ore main-block work-s step-cell within-keep-clear within-own-zone]]
            [jobs.gather.mine.end :refer [add-cut stop!]]
            [jobs.gather.mine.way-set :as ways]
            [jobs.gather.mine.torches :refer [mouth-torch-due? stair-torch-due? torch-step!]]
            [jobs.gather.mine.tunnel :refer [heading-stops turn-heading! result-of]]))

(a/defargs settings
  {::level-slack {:default 12 :doc "Blocks above the tunnel level a stopped stair may leave the body and the strip tunnel still run there; higher, the run ends :descent-stopped (ore is not found that far above its level)." :spec (a/int-in 0 nil)}
   ::own-stair-reach {:default 32 :doc "Blocks from the body the top of a stair it cut earlier may lie for the descent to walk down that stair instead of cutting a new one." :spec (a/int-in 0 nil)}
   ::dig-here-reach {:default 8 :doc "Stone gathered on soil or sand: a seen stone block farther than this from the body is not walked to; the stair down is cut where the body stands, as a player digs down to stone." :spec (a/num-in 0 nil)}
   ::back-reach {:default 32 :doc "Blocks from the stair's last step the body may be for the descent to walk back onto it; farther, only a whole way back quoted no longer than cutting a stair down to it again is walked, else it starts a new stair where it stands." :spec (a/int-in 0 nil)}
   ::max-restairs {:default 3 :doc "Times the descent starts a new stair where the body stands after the stair found it off its line (a reflex, a fall or a drop pick-up moved it) before the stop stands." :spec (a/int-in 0 nil)}
   ::max-moves {:default 3 :doc "Times the stair down walks to another spot after all four headings were blocked at its start (water, a plug, a hazard) before the stop stands." :spec (a/int-in 0 nil)}
   ::move-reach {:default 8 :doc "Blocks from the blocked start the stair's next spot lies; it grows by 4 each move." :spec (a/int-in 1 nil)}})

(defn max-restairs [] (settings/get settings ::max-restairs))
(defn level-slack [] (settings/get settings ::level-slack))

(defn tunnel-level
  "The feet height the strip tunnel should run at (args a): :level, else the ore's cost/ore-level; nil for none (:level false, or no descending way)."
  [{:keys [level] :as args}]
  (cond (or (not (ways/descends? args)) (false? level)) nil
        (sequential? level) (second level)
        (map? level) (:y level)
        (some? level) level
        :else (cost/ore-level (base-ore (main-block args)))))

(def stone-types #{"stone" "cobblestone" "deepslate" "andesite" "granite" "diorite" "tuff"})

(def natural-ground #{"dirt" "grass_block" "coarse_dirt" "podzol" "rooted_dirt" "mycelium" "sand" "red_sand" "gravel" "clay"})

(defn to-level?
  "Whether the dig phase should first stair down to the tunnel level: one is set, the body stands above it, the tunnel is owed and not
  begun, and the stair has not stopped."
  [c]
  (let [level (tunnel-level (:args c))
        m (ctx/mem c)]
    (boolean (and level (ways/tunnel? (:args c)) (nil? (:tunnel m)) (nil? (:stop (:descent m)))
                  (> (:y (cell-of (u/self-pos c))) level)))))

(defn descent-wanted?
  [c]
  (let [{:keys [descend-limit]} (:args c)
        block (main-block (:args c))]
    (or (to-level? c)
        (and (nil? (tunnel-level (:args c))) (ways/descends? (:args c)) (contains? stone-types block) (pos? descend-limit) (nil? (:tunnel (ctx/mem c)))))))

(defn shaft-state [c] (:state (:shaft (ctx/mem c))))

(defn descend-due?
  "Whether the dig phase should first stair down: to the tunnel level (to-level?), or the block is stone-type, with no seen target (nor
  seen block skipped for water) and the strip tunnel not begun. Not while a shaft is used or being used."
  [c]
  (and (contains? #{nil :none} (shaft-state c)) (descent-wanted? c)))

(defn dig-here-due?
  "Whether the stair down should start where the body stands instead of a walk to the seen stone block at pos: the descent is due for a stone
  block, the body stands on natural soil or sand outside any plan, zone or home it keeps clear of (a built floor is walked off, never cut
  through), and pos is farther than ::dig-here-reach."
  [c pos]
  (let [feet (cell-of (u/self-pos c))
        under (u/seen-name (:primitives c) (update feet :y dec))]
    (boolean (and (descend-due? c) (contains? stone-types (main-block (:args c)))
                  (contains? natural-ground under)
                  (not (within-keep-clear c 0 (access/cell feet) true))
                  (> (u/dist (u/self-pos c) pos) (settings/get settings ::dig-here-reach))))))

(defn shaft-under-way?
  "Whether the shaft child has been called and has not ended."
  [c]
  (and (ways/shaft? (:args c)) (= :calling (shaft-state c))))

(defn shaft-due?
  "Whether the descent goes by a mineshaft: it is due and :shaft is not false, or a shaft descent is under way."
  [c]
  (and (ways/shaft? (:args c))
       (or (shaft-under-way? c)
           (and (nil? (shaft-state c)) (descent-wanted? c)))))

(defn no-stone!
  "The stair down found no stone: the mend and the walk home with :reason :no-stone-found."
  [c]
  (stop! c :no-stone-found))

(defn stair-stopped!
  "The stair down to the tunnel level stopped above it: one mine.descent-stopped, then the tunnel runs where the body stands,
  or, more than ::level-slack above the level, the mend and the walk home with :reason :descent-stopped."
  [c]
  (let [{:keys [steps stop]} (:descent (ctx/mem c))
        y (:y (cell-of (u/self-pos c)))
        level (tunnel-level (:args c))
        far? (> (- y level) (level-slack))]
    (ctx/emit! c :mine.descent-stopped (if far? :warn :info)
               {:steps (or steps 0) :stop stop :y y :level level
                :text (str "mine's stair down stopped at y " y " (" (name stop) "), short of y " level
                           (if far? "; too high to tunnel there" "; it tunnels there"))})
    (if far? (stop! c :descent-stopped) :again)))

(defn stair-in-own-zone?
  "The own zone the stair's next step (the cell ahead along the heading, its head and the one below) lies in, else nil."
  [c]
  (let [here (cell-of (u/self-pos c))
        ahead (step-cell {:origin here :heading (:heading (ctx/mem c))} 1)]
    (some #(within-own-zone c 0 (access/cell %)) [ahead (update ahead :y dec) (update ahead :y inc)])))

(defn shaft-top-here?
  "Whether the body stands at the top of the shaft it would use."
  [c]
  (let [{:keys [top none]} (when (ways/shaft? (:args c)) (pick-shaft c))]
    (boolean (and (not none) (u/within? (u/self-pos c) (cell-of (xyz top)) 1)))))

(defn shaft-none!
  "End the shaft descent for reason (shaft: its id when there is one; steps: what its stair cut). The usual descent goes on."
  [c reason shaft steps]
  (ctx/emit! c :mine.no-shaft :info (cond-> {:reason reason
                                             :text (str "mine uses no mineshaft" (some->> shaft (str " ")) ": " (name reason)
                                                        (when shaft (str "; name it with :shaft \"" shaft "\" to try it again")))}
                                      shaft (assoc :shaft shaft)))
  (ctx/update-mem! c #(-> % (assoc-in [:descent :steps] (or steps 0)) (assoc :shaft {:state :none :reason reason})))
  (if (ways/stair? (:args c))
    :again
    (stop! c :no-shaft)))

(defn shaft-used!
  "The shaft child ended :done (res): remember the shaft used, the tunnel's heading is the branch's."
  [c {:keys [shaft to-y n branch heading steps]}]
  (ctx/update-mem! c #(cond-> (-> % (assoc-in [:descent :steps] steps) (assoc :shaft {:state :used :id shaft :to-y to-y :n n :branch branch}))
                        heading (assoc :heading heading)))
  (ctx/emit! c :mine.shaft :info {:shaft shaft :to-y to-y :branch n :heading (some-> heading keyword)
                                  :text (str "mine took shaft " shaft " down to y " to-y (when heading (str ", branch " n " " heading)))})
  :again)

(defn ^:async shaft-round!
  "One step of a descent by mineshaft: the jobs.gather.shaft-descent child. A shaft that cannot be used or a stair that stops
  ends in :none: the usual descent."
  [c]
  (let [shaft (:shaft (:args c))]
    (when-not (= :calling (shaft-state c)) (ctx/update-mem! c assoc :shaft {:state :calling}))
    (let [r (waits/waiting-out c (await (ctx/call c :shaft 'jobs.gather.shaft-descent
                             {:shaft (when (string? shaft) shaft) :ignore-zones? (:ignore-zones? (:args c))})))
          res (result-of r)]
      (cond
        (= :continue r) :continue
        (= :done (:status res)) (shaft-used! c res)
        :else (shaft-none! c (:reason res) (:shaft res) (:steps res))))))

(defn own-stair-bottom
  "The lowest stand of a stair this body cut earlier (memory :stair-way) worth walking down: its top within ::own-stair-reach of the
  body and level with it (4 up or down), its bottom 3 or more below the feet and not below the tunnel level. {:cell [x y z] :heading name}, or nil."
  [c]
  (let [p (u/self-pos c)
        feet (cell-of p)
        dim (.-dimension (.self (:primitives c)))
        level (tunnel-level (:args c))
        stand (fn [[x y z]] [x (inc y) z])
        ways (for [e (ctx/entries c :stair-way)
                   :when (= dim (:dim (:data e)))
                   :let [floors (sort-by second (map stand (:floors (:data e))))
                         [bx by bz :as bottom] (first floors)
                         [tx ty tz] (last floors)]
                   :when (and bottom (<= (u/dist p {:x (+ tx 0.5) :y ty :z (+ tz 0.5)}) (settings/get settings ::own-stair-reach))
                              (<= (js/Math.abs (- ty (:y feet))) 4) (<= by (- (:y feet) 3)) (or (nil? level) (>= by level)))]
               {:cell bottom :heading (let [[nx _ nz] (second floors)]
                                        (some-> (get escape/heading-names [(- bx nx) (- bz nz)]) name))})]
    (first (sort-by (comp second :cell) ways))))

(defn ^:async reuse-stair!
  "Before the first cut of the stair down: a go-to child (slot :stair-own) down the stair this body cut earlier (own-stair-bottom),
  so the new steps go on from its foot. nil once that is done, failed or not due, else :again / :continue while it walks."
  [c]
  (let [d (:descent (ctx/mem c))
        {:keys [state cell heading]} (:own-stair d)]
    (cond
      (or (:top d) (contains? #{:none :done :failed} state)) nil
      (nil? state) (let [way (own-stair-bottom c)]
                     (ctx/emit! c :mine.stair-plan :info (if way
                                                          {:own-stair (:cell way) :text (str "mine walks down its stair at " (str/join "," (:cell way)) " and goes on from its foot")}
                                                          {:text "mine cuts a new stair down"}))
                     (ctx/update-mem! c assoc-in [:descent :own-stair] (if way (assoc way :state :walking) {:state :none}))
                     :again)
      :else (let [r (await (child/walk-into!* c :stair-own cell))]
              (cond
                (= :continue r) :continue
                (not= cell (access/cell (cell-of (u/self-pos c)))) (do (ctx/update-mem! c assoc-in [:descent :own-stair :state] :failed) :again)
                :else (do (ctx/update-mem! c #(cond-> (-> % (assoc-in [:descent :own-stair :state] :done)
                                                          (update :descent assoc :top cell :top-heading (keyword heading)))
                                                (and heading (nil? (:heading (:args c)))) (assoc :heading heading)))
                          :again))))))

(defn ^:async back-on-stair!
  "Before a step of the stair down: a go-to child (slot :stair-back) to the cell the last step ended on (else the stair's top) when
  something walked the body off it (a reflex took it home). Not while a step's stair child is under way: it walks back itself.
  nil when there, :continue while it walks, else the cell it could not reach."
  [c]
  (let [{:keys [at top]} (:descent (ctx/mem c))
        cell (or at top)]
    (when (and cell (nil? (get-in (ctx/mem c) [:children :stair])) (not= cell (access/cell (cell-of (u/self-pos c)))))
      (if (and (> (u/dist (u/self-pos c) (zipmap [:x :y :z] cell)) (settings/get settings ::back-reach))
               (not (await (price/walk-back-worth? c cell (stair-time/steps-s :down (max 0 (- (:y (u/self-pos c)) (cell 1))))))))
        (do (ctx/emit! c :mine.restair :info {:at (access/cell (cell-of (u/self-pos c))) :text (str "mine's stair at " (str/join "," cell) " is dearer to walk back to than a new stair; it starts a new stair here")})
            (ctx/update-mem! c update :descent dissoc :top :at)
            nil)
        (let [r (await (child/walk-into!* c :stair-back cell))]
          (cond
            (= :continue r) :continue
            (= cell (access/cell (cell-of (u/self-pos c)))) nil
            :else cell))))))

(defn move-goal
  "The next spot for a blocked stair start: a cell move-reach (+4 per earlier move) from the body along a heading, dry as far as seen,
  not near an earlier blocked start (memory :descent :blocked). nil when none."
  [c]
  (let [m (ctx/mem c)
        n (get-in m [:descent :moves] 0)
        here (cell-of (u/self-pos c))
        spent (get-in m [:descent :blocked] [])
        d (+ (settings/get settings ::move-reach) (* 4 n))
        seen #(u/seen-name (:primitives c) %)
        wet? (fn [p] (or (bs/fluids (seen p)) (bs/fluids (seen (update p :y dec)))))
        near? (fn [p] (some #(< (u/dist p (zipmap [:x :y :z] %)) 5) spent))
        cands (for [h ["north" "east" "south" "west"]
                    :let [[dx dz] (headings h)
                          p (-> here (update :x + (* d dx)) (update :z + (* d dz)))]
                    :when (not (near? p))]
                p)]
    (first (sort-by #(if (wet? %) 1 0) cands))))

(defn ^:async move-stair!
  "All four headings of the stair's start are blocked and it cut nothing: a go-to child (slot :move-n) to another spot, then the stair
  starts again there with every heading open. :continue while it walks, :again once it did, nil when the moves are spent or there is no spot."
  [c stop]
  (let [m (ctx/mem c)
        n (get-in m [:descent :moves] 0)
        slot (keyword (str "move-" n))]
    (when (and (not (get-in m [:descent :move])) (< n (settings/get settings ::max-moves)))
      (when-let [to (move-goal c)]
        (let [from (access/cell (cell-of (u/self-pos c)))]
          (ctx/emit! c :mine.moved :info {:from from :to (access/cell to) :stop stop :move (inc n)
                                          :text (str "mine's stair is blocked (" (name stop) ") on every heading at " (str/join "," from) "; it tries another spot near " (str/join "," (access/cell to)))})
          (ctx/update-mem! c update :descent #(-> % (assoc :move to) (update :blocked (fnil conj []) from))))))
    (when-let [to (get-in (ctx/mem c) [:descent :move])]
      (let [r (await (child/call-walk* c slot {:pos {:x (+ (:x to) 0.5) :y (:y to) :z (+ (:z to) 0.5)} :range 2 :stay-s (work-s c)
                                               :ignore-zones? (boolean (:ignore-zones? (:args c)))
                                               :settings {:jobs.movement.go-to.tuning/y-slack 8}}))]
        (if (= :continue r)
          :continue
          (do (ctx/update-mem! c #(-> % (dissoc :tried-headings)
                                      (update :descent (fn [d] (-> d (dissoc :move :stop :top :at) (update :moves (fnil inc 0)))))))
              :again))))))

(defn blocked-start?
  "Whether a stop that no heading cures leaves the stair worth a try at another spot: it cut nothing and no heading or direction was ordered."
  [c stop]
  (let [{:keys [heading direction]} (:args c)]
    (and (heading-stops stop)
         (zero? (get-in (ctx/mem c) [:descent :steps] 0))
         (nil? direction) (or (nil? heading) (= :fluid-in-cut stop)))))

(defn ^:async step-down!
  "One step of the stair down: the jobs.access.stair child and what its ending means for the descent. A stop that blocks only
  this heading (heading-stops) turns the stair, at any step (turn-heading!; a step taken clears the headings tried)."
  [c level?]
  (let [_ (when-not (get-in (ctx/mem c) [:descent :top])
            (ctx/update-mem! c update :descent assoc :top (access/cell (cell-of (u/self-pos c))) :top-heading (keyword (:heading (ctx/mem c)))))
        r (waits/waiting-out c (await (ctx/call c :stair 'jobs.access.stair
                                 {:dir :down :heading (keyword (:heading (ctx/mem c))) :steps 1 :ways (if level? (need/pass-ways c) (set need/fetch-ways))
                                  :accept (ways/stair-accept (:args c)) :need-drop true
                                  :ignore-zones? (:ignore-zones? (:args c))})))
        res (result-of r)]
    (cond
      (= :continue r) :continue
      (= :done (:status res))
      (do (ctx/update-mem! c #(-> % (dissoc :tried-headings) (update :cut add-cut {:down (count (:dug res))})
                                  (update :descent (fn [d] (assoc d :steps (+ (:steps d 0) (or (:steps res) 1)) :bottom-y (js/Math.floor (:y (u/self-pos c))) :at (:at res))))))
          :again)
      (and (= :off-stair (:reason res)) (< (get-in (ctx/mem c) [:descent :restairs] 0) (max-restairs)))
      (do (ctx/emit! c :mine.restair :info {:at (access/cell (cell-of (u/self-pos c))) :text (str "mine's stair finds the body off its line (" (:why res) "); it starts a new stair here")})
          (ctx/update-mem! c #(-> % (update :children dissoc :stair) (update :descent (fn [d] (-> d (dissoc :top :at) (update :restairs (fnil inc 0)))))))
          :again)
      :else (let [stop (or (:reason res) :declined)
                  ;; the world's lowest layer is bedrock on every heading, and nothing lies under it: the stair is done
                  ;; (above it, in the ragged layer, a turn may find stone)
                  turnable? (not (and (#{:unbreakable :not-loaded} stop) (some-> (:cell res) (get 1) (<= places/min-y))))]
              (or (when (and turnable? (heading-stops stop) (turn-heading! c stop)) :again)
                  (when (and turnable? (blocked-start? c stop)) (await (move-stair! c stop)))
                  (do (ctx/update-mem! c update :descent assoc :stop stop)
                      (if level? (stair-stopped! c) (no-stone! c))))))))

(defn ^:async cut-round!
  "One cut of the stair (see descend-round!)."
  [c]
  (let [n (:steps (:descent (ctx/mem c)) 0)
        level? (to-level? c)
        own (await (reuse-stair! c))
        back (when-not own (await (back-on-stair! c)))]
    (cond
      own own
      (= :continue back) :continue
      back (do (ctx/emit! c :mine.not-back :warn {:cell back :text (str "mine could not walk back to its stair down at " (str/join "," back))})
               (stop! c :not-back))
      (and level? (mouth-torch-due? c)) (await (torch-step! c :stair))
      (and level? (stair-torch-due? c)) (await (torch-step! c :stair))
      (and (not level?) (>= n (:descend-limit (:args c)))) (no-stone! c)
      (stair-in-own-zone? c) (do (ctx/update-mem! c assoc-in [:descent :stop] :own-zone)
                                 (ctx/emit! c :mine.own-zone :warn {:zone (:name (stair-in-own-zone? c))
                                                                    :text (str "mine's stair stops at the edge of its own zone " (:name (stair-in-own-zone? c)))})
                                 (if level? (stair-stopped! c) (stop! c :own-zone)))
      :else (await (step-down! c level?)))))

(defn ^:async descend-round!
  "One step of the stair down (jobs.access.stair child, :steps 1) along the tunnel heading; the next round looks round and scans.
  The body first walks back onto the stair (back-on-stair!); one that cannot ends the dig phase :not-back.
  To the tunnel level: a torch first when one is due (stair-torch-due?), ways passed on; the stair stopping ends the descent there.
  For stone: all fetch ways; at :descend-limit steps, or when the stair stops, ends :no-stone-found. Either way a heading-stop
  turns the stair first (step-down!)."
  [c]
  (if (get-in (ctx/mem c) [:descent :move])
    (await (move-stair! c nil))
    (await (cut-round! c))))
