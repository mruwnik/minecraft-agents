(ns jobs.gather.make-obsidian
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.blocks :as b]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.shore :as shore]
            [jobs.lib.util :as u]))

(def doc
  "Get :count obsidian by pouring water on a seen lava source and mining the result with a diamond pickaxe.
  Takes seen obsidian first (jobs.gather.mine). Else it needs a water bucket carried (an empty bucket is filled at seen water; no bucket at all is fetched, :fetch) and a seen
  lava cell with seen air above it, a clickable air cell to pour at (that one, or one level with it and beside it, the water then runs over) and a stand cell beside the pour cell (a standable cell with no lava beside it, never over lava):
  it walks to the stand (go-to, which keeps off lava), pours the water there, waits for the lava to turn to obsidian,
  scoops the water back up (also in the cleanup of a cut run), and mines the obsidian (a mine child, which fetches its pickaxe from a chest or a craft).
  Ends :done {:got n} at :count obsidian gained. Else it ends with :got n in its data: condition-failed :no-bucket (no water bucket and no bucket
  with water seen, or :fetch off); a failed bucket fetch ends with its need cause or :no-lava (no seen lava with open air above and a safe stand); nothing-possible :pour-failed (two sites in a row where
  the pour or the conversion failed: moving lava gives cobblestone) or :unreachable (the stand could not be walked to); the mine child's
  ending (:no-tool ..) passes up; a limit of the call ends limit-reached.
  Quote: time by the walk to the stand and the pour; yields the obsidian.")

(a/defargs args
  {:count {:doc "obsidian to get" :spec (a/int-in 1 nil) :default 1}
   :radius {:doc "how far from the body to look for lava, obsidian and water" :spec (a/num-in 1 nil) :default 24}
   :ignore-zones? {:doc "act regardless of zones and claims (the walks and the mine child); the rules of the game allow it" :spec boolean? :default false}
   :fetch {:doc "get a bucket when no bucket is carried, and the mine child's pickaxe, with jobs.lib.need: true, a set of kinds or a map of limits; false waits :no-bucket" :spec need/option? :default true}})

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds making obsidian may take: the walks, the pour, the wait for the conversion and the mining." :spec (a/num-in 0 nil)}})

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

(defn lava-near?
  "Whether a cell beside the stand cell (its four sides, at its level) is lava, which could flow onto it."
  [p {:keys [x y z]}]
  (boolean (some (fn [[dx dy dz]] (= "lava" (u/seen-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})))
                 (for [[dx dz] shore/sides] [dx 0 dz]))))

(defn solid?
  "Whether the seen block at cell can be clicked: not air, fluid or a plant."
  [p cell]
  (let [n (u/seen-name p cell)]
    (boolean (and n (not (b/air n)) (not (b/fluids n)) (not (b/clearable n))))))

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
                             [dx dz] shore/sides
                             :let [stand {:x (+ (:x pour) dx) :y (:y pour) :z (+ (:z pour) dz)}]
                             :when (and (not= stand above) (shore/standable? p stand) (not (lava-near? p stand)))]
                         {:lava pos :above above :pour pour :stand stand})))))
         (sort-by #(u/dist here (:stand %))))))

(defn gained [c] (- (carried c "obsidian") (:base (ctx/mem c) (carried c "obsidian"))))

(defn end
  "The ending of kind and cause, with the obsidian got so far and a text."
  [c kind cause text]
  (ctx/end c kind cause {:got (gained c) :text text}))

(defn ^:async walk!
  "Walk to within range of pos with a go-to child: :there, :continue (it waits on the world, or a leg that got nearer) or
  :blocked (it gave up)."
  [c pos range]
  (let [e (await (child/call-walk* c :walk {:pos pos :range range :one-way :closed :warn false :retry false
                                            :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue e) :continue
      (and (= :done (:kind e)) (:arrived (:data e))) :there
      (and (= :done (:kind e)) (:leg (:data e))) :continue
      :else :blocked)))

(defn ^:async fill-bucket!
  "An empty bucket carried and water seen: walk to the water and scoop it. :again when filled, :blocked when not."
  [c]
  (let [water (first (seen-of c ["water"]))]
    (if-not water
      :blocked
      (let [w (await (walk! c (:pos water) 3))]
        (case w
          :there (if (= "placed" (.-status (await (ctx/act c :place (clj->js {:pos (:pos water) :item "bucket"}))))) :again :blocked)
          :continue :continue
          :blocked)))))

(def settle-looks 6)

(defn ^:async pour!
  "Note the pour cell of site in memory (:poured), then pour there (the note first: a cut right after the
  pour still finds its water); settle! waits and scoops. :again, or :failed."
  [c {:keys [pour] :as site}]
  (ctx/update-mem! c assoc :poured (assoc site :looks 0))
  (if (= "placed" (.-status (await (ctx/act c :place (clj->js {:pos pour :item "water_bucket"})))))
    :again
    (do (ctx/update-mem! c dissoc :poured) :failed)))

(defn ^:async settle!
  "One look at the poured water: the lava turned (or the looks are spent) scoops the water back and books the success
  or the failed site; else half a second is waited
  (a pause child, so the engine's limit and cuts apply between looks). :again, or :continue while the pause waits."
  [c]
  (let [p (:primitives c)
        {:keys [lava pour looks]} (:poured (ctx/mem c))
        made? (= "obsidian" (u/seen-name p lava))]
    (if (or made? (>= looks settle-looks))
      (do (when (= "water" (u/seen-name p pour))
            (await (ctx/act c :place (clj->js {:pos pour :item "bucket"}))))
          (ctx/update-mem! c #(-> (dissoc % :poured)
                                  (cond-> made? (assoc :failed 0)
                                          (not made?) (-> (update :tried (fnil conj []) lava) (update :failed (fnil inc 0))))))
          :again)
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
          (if (= "placed" (.-status (await (ctx/act c :place (clj->js {:pos pour :item "bucket"})))))
            (do (ctx/update-mem! c dissoc :poured)
                (ctx/end c :done nil {}))
            (left!)))))))

(defn ^:async mine-seen!
  "The seen obsidian mined with a mine child: :again, :continue while it works, or its ending when it got none."
  [c got]
  (let [e (await (ctx/call c :mine 'jobs.gather.mine
                           {:block "obsidian" :item "obsidian" :count (- (:count (:args c)) got) :fetch (if (contains? (:what (need/opts c 'jobs.gather.make-obsidian)) :tool) mine-fetch false) :mend false :radius (:radius (:args c))
                            :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue e) :continue
      (> (gained c) got) :again
      :else (ctx/end c (if (= :done (:kind e)) :nothing-possible (:kind e)) (if (= :done (:kind e)) :mine-failed (:cause e))
                     {:got got :text (str "the obsidian could not be mined: " (name (or (some-> e :cause :reason) (:cause e) :unknown)))}))))

(defn ^:async step [c]
  (let [{:keys [count]} (:args c)
        got (gained c)]
    (cond
      (>= got count) (ctx/end c :done nil {:got got})
      (:poured (ctx/mem c)) (await (settle! c))
      (seq (seen-of c ["obsidian"])) (await (mine-seen! c got))
      (>= (:failed (ctx/mem c) 0) 2) (end c :nothing-possible :pour-failed "water on the lava did not make obsidian twice in a row")
      (zero? (carried c "water_bucket"))
      (if (pos? (carried c "bucket"))
        (let [r (await (fill-bucket! c))]
          (case r
            :continue :continue
            :again :again
            (end c :condition-failed :no-bucket "no water bucket is carried and no seen water to fill the bucket at")))
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
            :there (if (= :again (await (pour! c site)))
                     :again
                     (do (ctx/update-mem! c #(-> % (update :tried (fnil conj []) (:lava site)) (update :failed (fnil inc 0))))
                         :again))
            (end c :nothing-possible :unreachable (str "could not walk to the stand at " (pr-str (:stand site))))))
        (end c :condition-failed :no-lava "no seen lava with open air above and a safe stand beside it")))))

(defn ^:async round [c]
  (ctx/update-mem! c update :base #(or % (carried c "obsidian")))
  (await (pace/steps! c #(step c))))

;; ------------------------------------------------------------------ the contract

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/safe-if-lit)

(def fit? ready/fit?)

(defn bucket-cause
  "Cause :no-bucket before the start (the round's own ending covers it later): no obsidian seen to mine, no water bucket carried and no bucket with water seen to fill; none when a bucket can be fetched."
  [c]
  (when (and (zero? (carried c "water_bucket"))
             (empty? (seen-of c ["obsidian"]))
             (not (and (pos? (carried c "bucket")) (seq (seen-of c ["water"]))))
             (not (and (zero? (carried c "bucket")) (fetch-opts c))))
    {:reason :no-bucket}))

(defn lava-cause
  "Cause :no-lava before the start: no obsidian seen to mine and no pour site."
  [c]
  (when (and (empty? (seen-of c ["obsidian"])) (empty? (sites c)))
    {:reason :no-lava}))

(def can-start?
  (cond/and (checks/refusing :bucket "a water bucket or a bucket and water" bucket-cause)
            (checks/refusing :lava "obsidian or lava seen" lava-cause)))

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
