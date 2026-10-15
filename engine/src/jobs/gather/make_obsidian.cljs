(ns jobs.gather.make-obsidian
  (:require [jobs.lib.started :as started]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.shore :as shore]
            [jobs.lib.util :as u]))

(def doc
  "Get :count obsidian by pouring water on a seen lava source and mining the result with a diamond pickaxe.
  Takes seen obsidian first (jobs.gather.mine). Else it needs a water bucket carried (an empty bucket is filled at seen water) and a seen
  lava cell with seen air above it and a stand cell beside that air (a standable cell with no lava beside it, never over lava):
  it walks to the stand (go-to, which keeps off lava), pours the water at the air cell above the lava, waits for the lava to turn to obsidian,
  scoops the water back up, and mines the obsidian (a mine child, which fetches its pickaxe from a chest or a craft).
  Ends :done {:got n} at :count obsidian gained. Else it ends with :got n in its data: condition-failed :no-bucket (no water bucket and no bucket
  with water seen) or :no-lava (no seen lava with open air above and a safe stand); nothing-possible :pour-failed (two sites in a row where
  the pour or the conversion failed: moving lava gives cobblestone) or :unreachable (the stand could not be walked to); the mine child's
  ending (:no-tool ..) passes up; a limit of the call ends limit-reached.
  Quote: time by the walk to the stand and the pour; yields the obsidian.")

(a/defargs args
  {:count {:doc "obsidian to get" :spec (a/int-in 1 nil) :default 1}
   :radius {:doc "how far from the body to look for lava, obsidian and water" :spec (a/num-in 1 nil) :default 24}})

(a/defargs settings
  {::time-s {:default 600 :doc "Seconds making obsidian may take: the walks, the pour, the wait for the conversion and the mining." :spec (a/num-in 0 nil)}})

(def mine-fetch {:what #{:tool} :how #{:chest :craft}})

(defn carried [c name]
  (reduce + 0 (keep #(when (= name (:name %)) (:count %)) (u/inventory (:primitives c)))))

(defn seen-of [c names]
  (look/seen-blocks (:primitives c) {:names names :radius (:radius (:args c)) :max 32 :live? true}))

(defn lava-near?
  "Whether a cell beside the stand cell (its four sides, at its level) is lava, which could flow onto it."
  [p {:keys [x y z]}]
  (boolean (some (fn [[dx dy dz]] (= "lava" (u/seen-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})))
                 (for [[dx dz] shore/sides] [dx 0 dz]))))

(defn sites
  "The pour sites {:lava :above :stand} for the seen lava: air above the lava, a stand beside that air (not the lava's column) that
  is standable with no lava beside it; nearest to the body first, those in :tried left out."
  [c]
  (let [p (:primitives c)
        here (u/self-pos c)
        tried (set (:tried (ctx/mem c)))]
    (->> (seen-of c ["lava"])
         (mapcat (fn [{:keys [pos]}]
                   (let [above (update pos :y inc)]
                     (when (and (shore/seen-air? p above) (not (tried pos)))
                       (for [[dx dz] shore/sides
                             :let [stand {:x (+ (:x above) dx) :y (:y above) :z (+ (:z above) dz)}]
                             :when (and (shore/standable? p stand) (not (lava-near? p stand)))]
                         {:lava pos :above above :stand stand})))))
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

(defn ^:async pour!
  "Pour at the air cell above the lava of site, wait for the conversion, scoop the water back. :again, or :failed."
  [c {:keys [lava above]}]
  (let [p (:primitives c)
        st (.-status (await (ctx/act c :place (clj->js {:pos above :item "water_bucket"}))))]
    (if-not (= "placed" st)
      :failed
      (do (dotimes [_ 6]
            (when (not= "obsidian" (u/seen-name p lava))
              (await (ctx/act c :wait (clj->js {:ms 500})))))
          (let [made? (= "obsidian" (u/seen-name p lava))]
            (when (= "water" (u/seen-name p above))
              (await (ctx/act c :place (clj->js {:pos above :item "bucket"}))))
            (if made? :again :failed))))))

(defn ^:async mine-seen!
  "The seen obsidian mined with a mine child: :again, :continue while it works, or its ending when it got none."
  [c got]
  (let [e (await (ctx/call c :mine 'jobs.gather.mine
                           {:block "obsidian" :item "obsidian" :count (- (:count (:args c)) got) :fetch mine-fetch :mend false :radius (:radius (:args c))}))]
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
      (seq (seen-of c ["obsidian"])) (await (mine-seen! c got))
      (>= (:failed (ctx/mem c) 0) 2) (end c :nothing-possible :pour-failed "water on the lava did not make obsidian twice in a row")
      (zero? (carried c "water_bucket"))
      (if (pos? (carried c "bucket"))
        (let [r (await (fill-bucket! c))]
          (case r
            :continue :continue
            :again :again
            (end c :condition-failed :no-bucket "no water bucket is carried and no seen water to fill the bucket at")))
        (end c :condition-failed :no-bucket "no water bucket is carried (a bucket is 3 iron plus a seen water source)"))
      :else
      (if-let [site (first (sites c))]
        (let [w (await (walk! c (:stand site) 1))]
          (case w
            :continue :continue
            :there (let [r (await (pour! c site))]
                     (if (= :again r)
                       (do (ctx/update-mem! c assoc :failed 0) :again)
                       (do (ctx/update-mem! c #(-> % (update :tried (fnil conj []) (:lava site)) (update :failed (fnil inc 0))))
                           :again)))
            (end c :nothing-possible :unreachable (str "could not walk to the stand at " (pr-str (:stand site))))))
        (end c :condition-failed :no-lava "no seen lava with open air above and a safe stand beside it")))))

(defn ^:async round [c]
  (ctx/update-mem! c update :base #(or % (carried c "obsidian")))
  (await (pace/steps! c #(step c))))

;; ------------------------------------------------------------------ the contract

(def target [])

(def defaults {:spend {:time ::time-s}})

(defn bucket-cause
  "Cause :no-bucket before the start (the round's own ending covers it later): no obsidian seen to mine, no water bucket carried and no bucket with water seen to fill."
  [c]
  (when (and (zero? (carried c "water_bucket"))
             (empty? (seen-of c ["obsidian"]))
             (not (and (pos? (carried c "bucket")) (seq (seen-of c ["water"])))))
    {:reason :no-bucket}))

(defn lava-cause
  "Cause :no-lava before the start: no obsidian seen to mine and no pour site."
  [c]
  (when (and (empty? (seen-of c ["obsidian"])) (empty? (sites c)))
    {:reason :no-lava}))

(def conditions [[:fn (started/at-start bucket-cause)] [:fn (started/at-start lava-cause)]])

(defn done? [c]
  (let [n (:count (:args c))
        got (if (:base (ctx/mem c)) (gained c) 0)]
    {:done? (>= got n) :units :obsidian :done got :left (max 0 (- n got))}))

(defn quote-of [c]
  (let [n (or (:count (:args c)) 1)]
    {:uses {:time [1 (+ 20 (* 6 n)) (max (settings/get settings ::time-s) (+ 40 (* 6 n)))]}
     :yields {:items {"obsidian" [0 n n]}}
     :done [0 n n]}))
