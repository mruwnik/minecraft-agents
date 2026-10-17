(ns jobs.gather.mine.end
  "How jobs.gather.mine ends: the end texts, finish!, the dig phase hand-over to the mend (to-mend!) and the walk home."
  (:require [jobs.lib.child :as child]
            [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.shafts :as shafts]
            [jobs.lib.util :as u]
            [jobs.lib.ore-pick :as ore-pick]
            [jobs.lib.look :refer [cell-of]]
            [jobs.gather.mine.targets :refer [kinds not-solid carried left-behind]]))

(a/defargs settings
  {::home-dip {:default 16 :doc "Blocks under the body (and over the start) the walk home may plan through while the start lies fewer than this many blocks above the body (go-to :y-margin): no way round through deep caves; the walk's escalation digs out instead. From deeper, go-to's own climb rules hold." :spec (a/num-in 0 nil)}})

(defn home-dip [] (settings/get settings ::home-dip))

(defn shaft-suggest
  "{:at :heading :to-y} of the stair this run cut with no shaft (the top step's feet cell, the stair's heading, the feet y at
  its bottom), when it could be declared as a mineshaft: not when a shaft was used or :shaft is false."
  [{:keys [descent shaft]} args]
  (let [{:keys [top top-heading bottom-y]} descent]
    (when (and top bottom-y (not (false? (:shaft args))) (not= :used (:state shaft)) (< bottom-y (second top)))
      {:at top :heading top-heading :to-y bottom-y})))

(defn suggest-text [{:keys [at heading to-y]}]
  (str "; declare it with plans.mjs mineshaft <id> --at=" (str/join "," at) " --heading " (name heading) " --to-y " to-y " --by <you>"))

(defn dug-text
  "The descent and the tunnel of a run, for the end texts: \"; stair down 6 steps; tunnel 3 blocks east at y 58\" (empty for neither)."
  [{:keys [descent tunnel shaft unreached] suggest :shaft-suggest} reason]
  (if-let [{:keys [id n to-y branch]} (when (= :used (:state shaft)) shaft)]
    (str "; via shaft " id " (stair " (:steps descent 0) " steps to y " to-y ")"
         (when branch (str ", branch " n " " (name (:heading branch)) " " (:steps tunnel 0) " blocks")))
    (str (when (and (pos? (:steps descent 0)) (not= :no-stone-found reason)) (str "; stair down " (:steps descent) " steps"))
         (some-> suggest suggest-text)
         (when (pos? (:steps tunnel 0)) (str "; tunnel " (:steps tunnel) " blocks " (:heading tunnel) " at y " (:y (:origin tunnel))))
         (when (and (zero? (:steps descent 0)) (:stop descent)) (str "; stair stopped: " (name (:stop descent))))
         (when-let [stop (:stop tunnel)] (when (not= :tunnel-length stop) (str "; tunnel stopped: " (name stop))))
         (when (seq unreached) (str "; seen ore out of reach at " (str/join " " (map #(str/join "," %) unreached)))))))

(def facts
  "End reasons that name a fact (condition-failed); the other reasons short of the count are tries that failed
  (nothing-possible)."
  #{:none :wet :refused :own-zone :no-shaft :flooded :no-tool})

(defn wanted
  "The items the call needs for :done: :count, or the call's :least when that is a number."
  [c]
  (let [n (:count (:args c))
        least (:least (ctx/frozen-call c) :all)]
    (if (number? least) (min n least) n)))

(defn end-run
  "The ending of a run with data {:got n :reason r ..}, the old result fields: :done when :got meets wanted and the
  body got home, else by reason a kind: :hungry limit-reached on the food line, a fact (a worn last tool
  is :no-tool) condition-failed, any other nothing-possible. The cause is {:reason r} with :tool and :why when the data has them."
  [c {:keys [got reason] :as data}]
  (ctx/update-mem! c assoc :finished true)
  (let [cause (merge {:reason reason} (select-keys data [:tool :why]))]
    (cond
      (and (not= :not-home reason) (>= got (wanted c))) (ctx/end c :done nil data)
      (= :hungry reason) (ctx/end c :limit-reached (assoc cause :line :food) data)
      (facts reason) (ctx/end c :condition-failed cause data)
      :else (ctx/end c :nothing-possible cause data))))

(defn tunnel-tail
  "How the tunnel ended, for the end texts: \", ended at 1,60,2, walked back to 0,64,0\" (empty without a tunnel)."
  [tunnel]
  (when-let [{:keys [end back-at walked-back?]} (when (pos? (:steps tunnel 0)) tunnel)]
    (str (when end (str ", ended at " (str/join "," end)))
         (if walked-back?
           (str ", walked back to " (str/join "," back-at))
           ", did not get back to its origin"))))

(defn ores-text
  "\"; also picked 2 redstone_ore, 1 gold_ore\" for the ore blocks ore-pick dug on the way (empty for none)."
  [picked]
  (when (seq picked)
    (str "; also picked " (str/join ", " (map (fn [[block n]] (str n " " block)) (frequencies (map :block picked)))))))

(defn finish!
  "End the job (end-run) with the outcome."
  [c]
  (let [{:keys [goal mended dig-reason resumes tunnel left descent wet-skipped] :as m} (ctx/mem c)
        suggest (shaft-suggest m (:args c))
        unreached (into [] (comp (filter (fn [[x y z]] (contains? (kinds (:args c)) (u/seen-name (:primitives c) {:x x :y y :z z})))) (take 10)) (:unreached m))
        m (assoc m :shaft-suggest suggest :unreached unreached)
        got (max 0 (- (carried c) (- goal (:count (:args c)))))
        reason (if (and (zero? got) (pos? (or wet-skipped 0)) (#{:none :no-drops :tunnel-length :tunnel-stopped} (:reason m))) :wet (:reason m))
        wet-text (when (= :wet reason) (str "; " (or wet-skipped 0) " seen blocks beside water skipped, pass :wet true to dig them"))
        why (cond-> {} dig-reason (assoc :dig-reason dig-reason) (or (= :no-stone-found reason) (pos? (:steps descent 0))) (assoc :descent descent) (= :wet reason) (assoc :wet-skipped (or wet-skipped 0)) resumes (assoc :resumes resumes) suggest (assoc :shaft-suggest suggest) (seq unreached) (assoc :unreached unreached) (seq (:holes-open m)) (assoc :holes-open (:holes-open m))
              (pos? (:steps tunnel 0)) (assoc :tunnel (-> tunnel (select-keys [:origin :heading :steps :stop :end :back-at :walked-back?]) (update :origin access/cell)))
              (seq (:legs m)) (assoc :legs (:legs m))
              (seq (ore-pick/picked c)) (assoc :ores (ore-pick/picked c))
              (seq left) (assoc :left (mapv (fn [[pos n]] {:pos pos :count n}) left))
              (seq (:lost m)) (assoc :lost (:lost m)))]
    (end-run c (merge (let [soil (when (= :no-stone-found reason) (str "; dug down " (:steps descent 0) " blocks through soil, found no stone"))
                            tail (str wet-text (dug-text m reason) (tunnel-tail tunnel) (ores-text (ore-pick/picked c)))
                            mended-text (when (pos? (or mended 0)) (str "; mended " mended))]
                        (cond
                          (zero? got) {:text (str "mine got nothing: " (name reason) soil tail mended-text)}
                          (= :not-home reason) {:text (str "mine got " got " but did not get back to its start" mended-text (dug-text m reason) (tunnel-tail tunnel))}
                          (< got (:count (:args c))) {:text (str "mine got " got " of " (:count (:args c)) ": " (name reason) tail mended-text)}
                          :else {:text (str "mine done: " (name reason) ", got " got ", mended " (or mended 0) soil tail)}))
                      {:got got :reason reason :mended (or mended 0)}
                      (:end-cause m)
                      why))))

(defn wrap-up!
  "The normal end: the body walks back to where it started (phase :home), whatever the run did, then finishes."
  [c]
  (ctx/update-mem! c assoc :phase :home)
  :again)

(defn record-shaft-branch!
  "Remember the branch of the shaft this run dug (once), so the next run takes another."
  [c]
  (let [{:keys [shaft tunnel]} (ctx/mem c)
        {:keys [id branch recorded?]} shaft]
    (when (and branch (not recorded?) (pos? (:steps tunnel 0)))
      (shafts/record-branch! c id (assoc branch :length (:steps tunnel)))
      (ctx/update-mem! c assoc-in [:shaft :recorded?] true))))

(defn to-mend!
  "End the dig phase with a reason: mend next, or wrap up when there is nothing to mend."
  [c reason]
  (record-shaft-branch! c)
  (ctx/update-mem! c assoc :phase :mend :reason reason :at-mend (carried c))
  (if (:mend (:args c))
    :again
    (wrap-up! c)))

(defn still-left!
  "Keep in :left only the drops still lying in the world: one a later steer picked up (or that despawned) is
  dropped. Looks as far as the farthest listed drop, from where the body stands now."
  [c]
  (let [left (:left (ctx/mem c))
        here (u/self-pos c)
        radius (inc (apply max (map #(u/dist here (zipmap [:x :y :z] (key %))) left)))
        lying (left-behind c radius)]
    (ctx/update-mem! c assoc :left (into {} (filter #(contains? lying (key %))) left))))

(defn home-done!
  "Record whether the body got back to its start, say so when not, drop the left-behind drops it picked up, end."
  [c arrived?]
  (ctx/update-mem! c update :tunnel assoc :walked-back? (boolean arrived?) :back-at (access/cell (cell-of (u/self-pos c))))
  (when-not arrived?
    (ctx/update-mem! c #(assoc % :reason :not-home :dig-reason (or (:dig-reason %) (:reason %))))
    (ctx/emit! c :mine.not-home :warn {:to (access/cell (:start (ctx/mem c))) :at (access/cell (cell-of (u/self-pos c)))
                                       :text (str "mine did not get back to " (str/join "," (access/cell (:start (ctx/mem c)))))}))
  (when (seq (:left (ctx/mem c))) (still-left! c))
  (if (:cleaning (ctx/mem c))
    (ctx/end c :done nil {:walked-back? (boolean arrived?)})
    (finish! c)))

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
  first, then, if that walk did not arrive, every way (mem :home-pillar). :continue while the walk goes on, else the job ends."
  [c start]
  (let [{:keys [cell range y-margin]} (:home-goal (ctx/mem c))
        arrived? #(let [here (cell-of (u/self-pos c))] (if (zero? range) (= here cell) (beside? here start)))
        walk! (fn ^:async walk [slot ways]
                (await (child/call-walk* c slot
                                         (cond-> {:pos {:x (+ (:x cell) 0.5) :y (:y cell) :z (+ (:z cell) 0.5)} :range range :zone-tolls true
                                                  :ignore-zones? (boolean (:ignore-zones? (:args c)))}
                                           ways (assoc :ways ways)
                                           y-margin (assoc :y-margin y-margin)))))
        r (cond
            (arrived?) :arrived
            (:home-pillar (ctx/mem c)) (await (walk! :home-last nil))
            :else (await (walk! :home home-ways)))]
    (cond
      (= :continue r) :continue
      (or (arrived?) (:home-pillar (ctx/mem c))) (home-done! c (arrived?))
      :else (do (ctx/update-mem! c assoc :home-pillar true)
                (await (home-by-go-to! c start))))))

(defn ^:async home-round!
  "The last step of every normal run: back to the cell it started on through a go-to child (beside it when its floor is open or unseen),
  whatever the digging left open behind it; a warn mine.not-home when the walk did not arrive (the job ends :not-home)."
  [c]
  (let [{:keys [start homing]} (ctx/mem c)]
    (when-not homing
      (ctx/update-mem! c assoc :homing true :home-goal (home-goal c start))
      (ctx/update-mem! c assoc-in [:tunnel :end] (access/cell (cell-of (u/self-pos c)))))
    (await (home-by-go-to! c start))))
