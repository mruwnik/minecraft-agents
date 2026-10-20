(ns jobs.gather.mine.descent
  "The descent of jobs.gather.dig-for: whether and how it goes down (tunnel level, stair-or-shaft tests, the pick and the walk down a mineshaft)."
  (:require [clojure.string :as str]
            [jobs.lib.need :as need]
            [jobs.lib.waits :as waits]
            [jobs.lib.child :as child]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.escape :as escape]
            [jobs.gather.shaft-descent :refer [pick-shaft xyz]]
            [jobs.lib.util :as u]
            [jobs.lib.look :refer [cell-of]]
            [jobs.gather.mine.targets :refer [base-ore step-cell within-own-zone]]
            [jobs.gather.mine.end :refer [stop!]]
            [jobs.gather.mine.torches :refer [mouth-torch-due? stair-torch-due? torch-step!]]
            [jobs.gather.mine.tunnel :refer [heading-stops turn-heading! result-of]]))

(a/defargs settings
  {::ore-levels {:default {"iron_ore" 16 "copper_ore" 48 "gold_ore" -16 "lapis_ore" 0 "redstone_ore" -54 "diamond_ore" -54}
                 :doc "The feet height a strip tunnel for an ore runs at when :level is nil, by the ore's stone kind; an ore not listed tunnels where the body stands."
                 :spec (a/map-of string? (a/int-in nil nil))}
   ::level-slack {:default 12 :doc "Blocks above the tunnel level a stopped stair may leave the body and the strip tunnel still run there; higher, the run ends :descent-stopped (ore is not found that far above its level)." :spec (a/int-in 0 nil)}
   ::own-stair-reach {:default 32 :doc "Blocks from the body the top of a stair it cut earlier may lie for the descent to walk down that stair instead of cutting a new one." :spec (a/int-in 0 nil)}
   ::max-restairs {:default 3 :doc "Times the descent starts a new stair where the body stands after the stair found it off its line (a reflex, a fall or a drop pick-up moved it) before the stop stands." :spec (a/int-in 0 nil)}})

(defn max-restairs [] (settings/get settings ::max-restairs))
(defn level-slack [] (settings/get settings ::level-slack))

(defn tunnel-level
  "The feet height the strip tunnel should run at (args a): :level, else the ore's ::ore-levels entry; nil for none (:level false)."
  [{:keys [block level stay-level]}]
  (cond (or stay-level (false? level)) nil
        (sequential? level) (second level)
        (map? level) (:y level)
        (some? level) level
        :else (get (settings/get settings ::ore-levels) (base-ore block))))

(def stone-types #{"stone" "cobblestone" "deepslate" "andesite" "granite" "diorite" "tuff"})

(defn to-level?
  "Whether the dig phase should first stair down to the tunnel level: one is set, the body stands above it, the tunnel is owed and not
  begun, and the stair has not stopped."
  [c]
  (let [level (tunnel-level (:args c))
        m (ctx/mem c)]
    (boolean (and level (pos? (:tunnel-length (:args c))) (nil? (:tunnel m)) (nil? (:stop (:descent m)))
                  (> (:y (cell-of (u/self-pos c))) level)))))

(defn descent-wanted?
  [c]
  (let [{:keys [block descend-limit]} (:args c)]
    (or (to-level? c)
        (and (nil? (tunnel-level (:args c))) (not (:stay-level (:args c))) (contains? stone-types block) (pos? descend-limit) (nil? (:tunnel (ctx/mem c)))))))

(defn shaft-state [c] (:state (:shaft (ctx/mem c))))

(defn descend-due?
  "Whether the dig phase should first stair down: to the tunnel level (to-level?), or the block is stone-type, with no seen target (nor
  seen block skipped for water) and the strip tunnel not begun. Not while a shaft is used or being used."
  [c]
  (and (contains? #{nil :none} (shaft-state c)) (descent-wanted? c)))

(defn shaft-under-way?
  "Whether the shaft child has been called and has not ended."
  [c]
  (and (not (false? (:shaft (:args c)))) (= :calling (shaft-state c))))

(defn shaft-due?
  "Whether the descent goes by a mineshaft: it is due and :shaft is not false, or a shaft descent is under way."
  [c]
  (and (not (false? (:shaft (:args c))))
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
  (let [{:keys [top none]} (when-not (false? (:shaft (:args c))) (pick-shaft c))]
    (boolean (and (not none) (u/within? (u/self-pos c) (cell-of (xyz top)) 1)))))

(defn shaft-none!
  "End the shaft descent for reason (shaft: its id when there is one; steps: what its stair cut). The usual descent goes on."
  [c reason shaft steps]
  (ctx/emit! c :mine.no-shaft :info (cond-> {:reason reason
                                             :text (str "mine uses no mineshaft" (some->> shaft (str " ")) ": " (name reason)
                                                        (when shaft (str "; name it with :shaft \"" shaft "\" to try it again")))}
                                      shaft (assoc :shaft shaft)))
  (ctx/update-mem! c #(-> % (assoc-in [:descent :steps] (or steps 0)) (assoc :shaft {:state :none :reason reason})))
  (if (= :required (:shaft (:args c)))
    (stop! c :no-shaft)
    :again))

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
      (let [r (await (child/walk-into!* c :stair-back cell))]
        (cond
          (= :continue r) :continue
          (= cell (access/cell (cell-of (u/self-pos c)))) nil
          :else cell)))))

(defn ^:async step-down!
  "One step of the stair down: the jobs.access.stair child and what its ending means for the descent. A stop that blocks only
  this heading (heading-stops) turns the stair, at any step (turn-heading!; a step taken clears the headings tried)."
  [c level?]
  (let [_ (when-not (get-in (ctx/mem c) [:descent :top])
            (ctx/update-mem! c update :descent assoc :top (access/cell (cell-of (u/self-pos c))) :top-heading (keyword (:heading (ctx/mem c)))))
        r (waits/waiting-out c (await (ctx/call c :stair 'jobs.access.stair
                                 {:dir :down :heading (keyword (:heading (ctx/mem c))) :steps 1 :ways (if level? (need/ways-of-fetch (:fetch (:args c))) (set need/fetch-ways))
                                  :accept (filterv #{:falling-block} (:accept (:args c)))
                                  :ignore-zones? (:ignore-zones? (:args c))})))
        res (result-of r)]
    (cond
      (= :continue r) :continue
      (= :done (:status res))
      (do (ctx/update-mem! c #(-> % (dissoc :tried-headings)
                                  (update :descent (fn [d] (assoc d :steps (+ (:steps d 0) (or (:steps res) 1)) :bottom-y (js/Math.floor (:y (u/self-pos c))) :at (:at res))))))
          :again)
      (and (= :off-stair (:reason res)) (< (get-in (ctx/mem c) [:descent :restairs] 0) (max-restairs)))
      (do (ctx/emit! c :mine.restair :info {:at (access/cell (cell-of (u/self-pos c))) :text (str "mine's stair finds the body off its line (" (:why res) "); it starts a new stair here")})
          (ctx/update-mem! c #(-> % (update :children dissoc :stair) (update :descent (fn [d] (-> d (dissoc :top :at) (update :restairs (fnil inc 0)))))))
          :again)
      :else (let [stop (or (:reason res) :declined)]
              (if (and (heading-stops stop) (turn-heading! c stop))
                :again
                (do (ctx/update-mem! c update :descent assoc :stop stop)
                    (if level? (stair-stopped! c) (no-stone! c))))))))

(defn ^:async descend-round!
  "One step of the stair down (jobs.access.stair child, :steps 1) along the tunnel heading; the next round looks round and scans.
  The body first walks back onto the stair (back-on-stair!); one that cannot ends the dig phase :not-back.
  To the tunnel level: a torch first when one is due (stair-torch-due?), :fetch passed on; the stair stopping ends the descent there.
  For stone: :fetch true; at :descend-limit steps, or when the stair stops, ends :no-stone-found. Either way a heading-stop
  turns the stair first (step-down!)."
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
