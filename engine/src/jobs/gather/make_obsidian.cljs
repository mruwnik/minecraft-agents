(ns jobs.gather.make-obsidian
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.blocks :as b]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.access :as access]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.look :as look]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.near :as near]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.shore :as shore]
            [jobs.lib.util :as u]))

(def doc
  "Get :count obsidian by pouring water on a seen lava source and mining the result with a diamond pickaxe.
  Takes seen obsidian first (jobs.gather.mine). Else it needs a water bucket carried (an empty bucket is filled at seen water; no bucket at all is fetched, :fetch) and a seen
  lava cell with seen air above it (with none seen, it walks to the cells a sealed mine breach showed lava at, body memory :breach-behind, digs the seal the mine placed there from at least 2 blocks off, looks again and forgets the entry), a clickable air cell to pour at (that one, or one level with it and beside it, the water then runs over) and a stand cell beside the pour cell (a standable cell with no lava beside it, never over lava):
  it walks to the stand (go-to, which keeps off lava), pours the water there, waits for the lava to turn to obsidian,
  scoops the water back up (the fill, pour and scoop are jobs.items.use children; also in the cleanup of a cut run), and mines the obsidian (a mine child, which fetches its pickaxe from a chest or a craft).
  Ends :done {:got n} at :count obsidian gained. Else it ends with :got n in its data: condition-failed :no-bucket (no water bucket and no bucket
  with water seen, or :fetch off); a failed bucket fetch ends with its need cause or :no-lava (no seen lava with open air above and a safe stand); nothing-possible :scoop-failed (the bucket could not be filled at the seen water), :pour-failed (failed sites in a row where
  the pour or the conversion failed: moving lava gives cobblestone) or :unreachable (the stand could not be walked to); the mine child's
  ending (:no-tool ..) passes up; a limit of the call ends limit-reached.
  Quote: time by the walk to the stand and the pour; yields the obsidian.")

(a/defargs args
  {:count {:doc "obsidian to get" :spec (a/int-in 1 nil) :default 1}
   :radius {:doc "how far from the body to look for lava, obsidian and water" :spec (a/num-in 1 nil) :default 24}
   :ignore-zones? :shared
   :fetch {:doc "get a bucket when no bucket is carried, and the mine child's pickaxe, with jobs.lib.need: true, a set of kinds or a map of limits; false waits :no-bucket" :spec need/option? :default true}})

(a/defargs settings
  {::settle-looks {:default 6 :doc "Half-second looks at the poured water before the pour counts as failed." :spec (a/int-in 1 nil)}
   ::max-failed-sites {:default 2 :doc "Failed pour sites in a row before the job ends :pour-failed." :spec (a/int-in 1 nil)}
   ::job-time-s {:default 600 :doc "Seconds making obsidian may take: the walks, the pour, the wait for the conversion and the mining." :spec (a/num-in 0 nil)}})

(def mine-fetch {:what #{:tool} :how #{:chest :craft}})

(defn fetch-opts
  "The limits {:how :explore :minutes} for fetching a bucket, nil when :fetch does not allow items."
  [c]
  (when-let [o (need/opts c 'jobs.gather.make-obsidian)]
    (when (contains? (:what o) :item) (select-keys o [:how :explore :minutes]))))

(defn carried [c name]
  (reduce + 0 (keep #(when (= name (:name %)) (:count %)) (u/inventory (:primitives c)))))

(defn seen-of [c names]
  (look/seen-blocks (:primitives c) {:names names :radius (:radius (:args c)) :max 32 :live? true}))

(defn seen-water-sources [c] (look/seen-water-sources (:primitives c) (:radius (:args c))))

(defn lava-near?
  "Whether a cell beside the stand cell (its four sides, at its level) is lava, which could flow onto it."
  [p {:keys [x y z]}]
  (boolean (some (fn [[dx dy dz]] (= "lava" (u/seen-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})))
                 (for [[dx dz] shore/sides] [dx 0 dz]))))

(defn solid?
  "Whether the seen block at cell can be clicked: not air, fluid or a plant."
  [p cell]
  (let [n (u/seen-name p cell)]
    (boolean (and n (not (bs/air n)) (not (bs/fluids n)) (not (b/clearable n))))))

(defn supported?
  "Whether a bucket can be emptied at cell: a solid block below it or beside it to click."
  [p {:keys [x y z]}]
  (boolean (some #(solid? p %) (cons {:x x :y (dec y) :z z} (for [[dx dz] shore/sides] {:x (+ x dx) :y y :z (+ z dz)})))))

(defn pours
  "The cells to empty the bucket at for the air cell above: the cell itself when it can be clicked, else a clickable air cell level with it
  and beside it (the water runs over and falls onto the lava)."
  [p above]
  (->> (cons above (for [[dx dz] shore/sides] (-> above (update :x + dx) (update :z + dz))))
       (filter #(and (shore/seen-air? p %) (supported? p %)))))

(defn sites
  "The pour sites {:lava :above :pour :stand} for the seen lava: air above the lava, a clickable air cell to pour at (the one above or one
  beside it), a stand beside the pour cell that is standable with no lava beside it; nearest to the body first, those in :tried left out."
  [c]
  (let [p (:primitives c)
        here (u/self-pos c)
        tried (set (:tried (ctx/mem c)))]
    (->> (seen-of c ["lava"])
         (mapcat (fn [{:keys [pos]}]
                   (let [above (update pos :y inc)]
                     (when (and (shore/seen-air? p above) (not (tried pos)))
                       (for [pour (pours p above)
                             :when (not (#{:zone :claim :footprint} (:reason (access/may? c :place pour))))
                             [dx dz] shore/sides
                             :let [stand {:x (+ (:x pour) dx) :y (:y pour) :z (+ (:z pour) dz)}]
                             :when (and (not= stand above) (shore/standable? p stand) (not (lava-near? p stand)))]
                         {:lava pos :above above :pour pour :stand stand})))))
         (sort-by #(u/dist here (:stand %))))))

(defn breach-lava
  "The remembered breach entries (data of body memory :breach-behind, nearest first) of this dimension that showed lava."
  [c]
  (let [p (:primitives c)
        dim (.-dimension (.self p))
        here (u/self-pos c)]
    (->> (ctx/entries c :breach-behind)
         (map :data)
         (filter #(and (= dim (:dim %)) (some #{"lava"} (:names %))))
         (sort-by #(u/dist here (zipmap [:x :y :z] (first (:cells %))))))))

(defn gained [c] (- (carried c "obsidian") (:base (ctx/mem c) (carried c "obsidian"))))

(defn end
  "The ending of kind and cause, with the obsidian got so far and a text."
  [c kind cause text]
  (ctx/end c kind cause {:got (gained c) :text text}))

(defn ^:async walk!
  "Walk to within range of pos (jobs.lib.near/go-near!): :there, :continue (it waits on the world, or a leg that got nearer) or :blocked."
  [c pos range]
  (let [w (await (near/go-near! c pos range))]
    (if (= :partial w) :continue w)))

(defn cell-of [v] (if (map? v) v (zipmap [:x :y :z] v)))

(defn seal-cells
  "The entry's own seal cells (memory :seals, else the breach cell :at) that are solid blocks now, can be dug (zones, hazards beyond lava and water
  beside it taken as known) and lie between lo and 4.5 blocks from the body."
  [c entry lo]
  (let [p (:primitives c)
        here (u/self-pos c)
        in (b/rules-in c)]
    (->> (or (seq (:seals entry)) [(:at entry)])
         (keep #(some-> % cell-of))
         (filter #(and (solid? p %)
                       (<= lo (u/dist here %) 4.5)
                       (= :ok (access/judge (access/may-dig? in %) [:lava-adjacent :fluid-adjacent])))))))

(defn seal-cell
  "The entry's seal cell in reach but not beside the body, so lava let out cannot reach the stand at once; nil when none."
  [c entry]
  (first (seal-cells c entry 2)))

(defn ^:async back-off!
  "The walk can end against the seal: step back to 4 blocks from it, along the line from the seal to the body. True when the body stands there."
  [c entry]
  (when-let [seal (first (seal-cells c entry 0))]
    (let [here (u/self-pos c)
          cx (+ (:x seal) 0.5)
          cz (+ (:z seal) 0.5)
          dx (- (:x here) cx)
          dz (- (:z here) cz)
          n (max 1e-6 (Math/hypot dx dz))
          goal {:x (Math/floor (+ cx (* 4 (/ dx n)))) :y (:y seal) :z (Math/floor (+ cz (* 4 (/ dz n))))}]
      (= :there (await (walk! c goal 1))))))

(defn ^:async open-seal!
  "Dig the seal of a breach entry the body stands near (stepping back first when it stands against it), and look through the hole, so the lava
  behind it is seen. Nothing when no seal qualifies."
  [c entry]
  (when-not (seal-cell c entry)
    (await (back-off! c entry)))
  (when-let [seal (seal-cell c entry)]
    (when (= "dug" (.-status (await (tidy/dig! c seal))))
      (await (dig-look/look-at! c (vec (first (or (seq (:lava entry)) (:cells entry)))))))))

(defn ^:async visit-breach!
  "No lava is seen: walk to within 4 of the nearest remembered breach lava cell and dig the seal the mine placed there, so the lava is
  seen; then forget the entry, whatever the walk did (remembered lava is stale; if it is still there the next step sees it).
  :again, or :continue while the walk waits."
  [c entry]
  (let [target (cell-of (first (or (seq (:lava entry)) (:cells entry))))
        w (await (walk! c target 4))]
    (if (= :continue w)
      :continue
      (do (when (= :there w) (await (open-seal! c entry)))
          (ctx/forget-where! c :breach-behind #(= entry %))
          :again))))

(defn ^:async use-bucket!
  "One bucket use at pos (a jobs.items.use child in slot): :continue while it waits, else its ending."
  [c slot pos item]
  (await (child/run!* c slot 'jobs.items.use
                      {:on pos :item item :ignore-zones? (boolean (:ignore-zones? (:args c)))})))

(defn ^:async fill-bucket!
  "An empty bucket carried and water seen: scoop the water (use-on walks to it). :again when filled, :continue while it waits, else the
  ending of the end: condition-failed :no-bucket with no water seen, nothing-possible :scoop-failed (data :why the use-on reason)."
  [c]
  (let [water (first (seen-water-sources c))]
    (if-not water
      (end c :condition-failed :no-bucket "no water bucket is carried and no seen water to fill the bucket at")
      (let [e (await (use-bucket! c :fill (:pos water) "bucket"))]
        (cond
          (= :continue e) :continue
          (and (= :done (:kind e)) (pos? (carried c "water_bucket"))) :again
          (= :done (:kind e)) (let [_ (ctx/update-mem! c update :fill-fails (fnil inc 0))
                                    fails (:fill-fails (ctx/mem c))]
                                (if (< fails (settings/get settings ::max-failed-sites))
                                  :again
                                  (ctx/end c :nothing-possible :scoop-failed
                                           {:got (gained c) :why :no-water-taken :text (str "the bucket click at " (pr-str (:pos water)) " took no water")})))
          :else (let [why (or (get-in e [:data :reason]) (:cause e) :failed)]
                  (ctx/end c :nothing-possible :scoop-failed
                           {:got (gained c) :why why :text (str "could not fill the bucket at the water at " (pr-str (:pos water)) ": " (name why))})))))))

(defn ^:async pour!
  "Note the pour cell of site in memory (:poured), then pour there (the note first: a cut right after the
  pour still finds its water); settle! waits and scoops. :again, :continue while the pour waits, or :failed."
  [c {:keys [pour] :as site}]
  (ctx/update-mem! c assoc :poured (assoc site :looks 0))
  (let [e (await (use-bucket! c :pour pour "water_bucket"))]
    (cond
      (= :done (:kind e)) :again
      :else (do (ctx/update-mem! c dissoc :poured)
                (if (= :continue e) :continue :failed)))))

(defn ^:async settle!
  "One look at the poured water: the lava turned (or the looks are spent) scoops the water back and books the success
  or the failed site; else half a second is waited
  (a pause child, so the engine's limit and cuts apply between looks). :again, or :continue while the pause waits."
  [c]
  (let [p (:primitives c)
        {:keys [lava pour looks]} (:poured (ctx/mem c))
        made? (= "obsidian" (u/seen-name p lava))]
    (if (or made? (>= looks (settings/get settings ::settle-looks)))
      (let [e (when (= "water" (u/seen-name p pour))
                (await (use-bucket! c :scoop pour "bucket")))]
        (if (= :continue e)
          :continue
          (do (ctx/update-mem! c #(-> (dissoc % :poured)
                                      (cond-> made? (assoc :failed 0)
                                              (not made?) (-> (update :tried (fnil conj []) lava) (update :failed (fnil inc 0))))))
              :again)))
      (let [e (await (child/pause! c 0.5 :waiting-for-conversion))]
        (if (= :continue e)
          :continue
          (do (ctx/update-mem! c update-in [:poured :looks] inc) :again))))))

(defn ^:async cleanup
  "However the run ended: water poured and not yet scooped back is taken up again, from within reach (a go-to child,
  :continue while it waits). Done, or nothing-possible :water-left (data :pos) when the walk or the scoop failed."
  [c _]
  (let [{:keys [pour]} (:poured (ctx/mem c))
        left! (fn [] (ctx/update-mem! c dissoc :poured)
                (ctx/end c :nothing-possible :water-left {:pos pour}))]
    (if-not (and pour (= "water" (u/seen-name (:primitives c) pour)))
      (do (ctx/update-mem! c dissoc :poured)
          (ctx/end c :done nil {}))
      (let [w (await (walk! c pour 3))]
        (case w
          :continue :continue
          :blocked (left!)
          (let [e (await (use-bucket! c :scoop pour "bucket"))]
            (cond
              (= :continue e) :continue
              (= :done (:kind e)) (do (ctx/update-mem! c dissoc :poured)
                                      (ctx/end c :done nil {}))
              :else (left!))))))))

(defn ^:async mine-seen-now!
  "The seen obsidian mined with a mine child, which is kept under way (memory :mining) until it ends: the dug block is no longer seen while the child picks it up. :again, :continue while it works, or its ending when it got none."
  [c got]
  (let [e (await (ctx/call c :mine 'jobs.gather.mine
                           {:block "obsidian" :item "obsidian" :count (+ (carried c "obsidian") (- (:count (:args c)) got)) :fetch (if (contains? (:what (need/opts c 'jobs.gather.make-obsidian)) :tool) mine-fetch false) :mend false :radius (:radius (:args c))
                            :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (ctx/update-mem! c #(cond-> (assoc % :mining (= :continue e)) (not= :continue e) (dissoc :drained)))
    (cond
      (= :continue e) :continue
      (> (gained c) got) :again
      :else (let [done? (= :done (:kind e))
                  why (or (some-> e :cause :reason) (:cause e) :unknown)]
              (ctx/end c (if done? :nothing-possible (:kind e)) (if done? :mine-failed (:cause e))
                       {:got got :text (str "the obsidian could not be mined: " (name why))})))))

(defn ^:async mine-seen!
  "mine-seen-now!, after the scooped water's flow has drained from the body (a few half-second looks): a head in water ends the mine child :flooded."
  [c got]
  (if (and (not (:mining (ctx/mem c))) (.-inWater (.self (:primitives c))) (< (:drained (ctx/mem c) 0) (settings/get settings ::settle-looks)))
    (let [e (await (child/pause! c 0.5 :waiting-for-water-to-drain))]
      (if (= :continue e) :continue (do (ctx/update-mem! c update :drained (fnil inc 0)) :again)))
    (await (mine-seen-now! c got))))

(defn ^:async step [c]
  (let [{:keys [count]} (:args c)
        got (gained c)]
    (cond
      (>= got count) (ctx/end c :done nil {:got got})
      (:poured (ctx/mem c)) (await (settle! c))
      (or (:mining (ctx/mem c)) (seq (seen-of c ["obsidian"]))) (await (mine-seen! c got))
      (>= (:failed (ctx/mem c) 0) (settings/get settings ::max-failed-sites)) (end c :nothing-possible :pour-failed "water on the lava did not make obsidian in a row")
      (zero? (carried c "water_bucket"))
      (if (pos? (carried c "bucket"))
        (let [r (await (fill-bucket! c))]
          r)
        (if (fetch-opts c)
          (let [e (await (need/item! c "bucket" 1 (fetch-opts c)))]
            (cond (= :continue e) :continue
                  (= :done (:kind e)) :again
                  :else e))
          (end c :condition-failed :no-bucket "no water bucket is carried (a bucket is 3 iron plus a seen water source)")))
      :else
      (if-let [site (first (sites c))]
        (let [w (await (walk! c (:stand site) 1))]
          (case w
            :continue :continue
            :there (case (await (pour! c site))
                     :again :again
                     :continue :continue
                     (do (ctx/update-mem! c #(-> % (update :tried (fnil conj []) (:lava site)) (update :failed (fnil inc 0))))
                         :again))
            (end c :nothing-possible :unreachable (str "could not walk to the stand at " (pr-str (:stand site))))))
        (if-let [entry (first (breach-lava c))]
          (await (visit-breach! c entry))
          (end c :condition-failed :no-lava "no seen lava with open air above and a safe stand beside it"))))))

(defn ^:async round [c]
  (ctx/update-mem! c update :base #(or % (carried c "obsidian")))
  (await (pace/steps! c #(step c))))

;; ------------------------------------------------------------------ the contract

(def target [:count])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/safe-if-lit)

(def fit? ready/fit?)

(defn bucket-cause
  "Cause :no-bucket before the start (the round's own ending covers it later): no obsidian seen to mine, no water bucket carried and no bucket with water seen to fill; none when a bucket can be fetched."
  [c]
  (when (and (zero? (carried c "water_bucket"))
             (empty? (seen-of c ["obsidian"]))
             (not (and (pos? (carried c "bucket")) (seq (seen-water-sources c))))
             (not (and (zero? (carried c "bucket")) (fetch-opts c))))
    {:reason :no-bucket}))

(defn lava-cause
  "Cause :no-lava before the start: no obsidian seen to mine and no pour site."
  [c]
  (when (and (empty? (seen-of c ["obsidian"])) (empty? (sites c)) (empty? (breach-lava c)))
    {:reason :no-lava}))

(def can-start?
  (cond/and (checks/refusing :bucket "a water bucket or a bucket and water" bucket-cause)
            (checks/refusing :look :lava "obsidian or lava seen" lava-cause)))

(defn done?
  ":count obsidian gained and no child under way (a mine child still picks up)."
  [c]
  (let [n (:count (:args c))
        got (if (:base (ctx/mem c)) (gained c) 0)
        data [:units :obsidian :done (min got n) :left (max 0 (- n got)) :result {:got got}]]
    (cond
      (< got n) (apply contract/later :short (str "make-obsidian: " got " of " n " obsidian") data)
      (checks/child-under-way? c) (apply contract/later :wrapping-up "make-obsidian: the count is gained, wrapping up" data)
      :else (apply contract/ok :made (str "made " got " obsidian") data))))

(defn quote-of [c]
  (let [n (or (:count (:args c)) 1)]
    {:uses {:job-time [1 (+ 20 (* 6 n)) (max (settings/get settings ::job-time-s) (+ 40 (* 6 n)))]}
     :yields {:items {"obsidian" [0 n n]}}
     :done [0 n n]}))
