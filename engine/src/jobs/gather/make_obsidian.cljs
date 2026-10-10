(ns jobs.gather.make-obsidian
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.result :as result]
            [jobs.lib.shore :as shore]
            [jobs.lib.util :as u]))

(def doc
  "Get :count obsidian by pouring water on a seen lava source and mining the result with a diamond pickaxe.
  Takes seen obsidian first (jobs.gather.mine). Else it needs a water bucket carried (an empty bucket is filled at seen water) and a seen
  lava cell with seen air above it and a stand cell beside that air (a standable cell with no lava beside it, never over lava):
  it walks to the stand (go-to, which keeps off lava), pours the water at the air cell above the lava, waits for the lava to turn to obsidian,
  scoops the water back up, and mines the obsidian (a mine child, which fetches its pickaxe from a chest or a craft).
  Ends {:status :done :got n} or {:status :stopped :reason r :got n}: :no-bucket (no water bucket and no bucket with water seen),
  :no-lava (no seen lava with open air above and a safe stand), :pour-failed (two sites in a row where the pour or the conversion failed:
  moving lava gives cobblestone), :unreachable (the stand could not be walked to), or the mine child's reason (:no-tool ..).")

(a/defargs args
  {:count {:doc "obsidian to get" :spec (a/int-in 1 nil) :default 1}
   :radius {:doc "how far from the body to look for lava, obsidian and water" :spec (a/num-in 1 nil) :default 24}})

(defn check
  "Passes: the round decides, and ends :stopped with the reason when it cannot make obsidian."
  [_c] true)

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

(defn stop! [c reason text]
  (result/stop! c reason text :got (carried c "obsidian")))

(defn ^:async fill-bucket!
  "An empty bucket carried and water seen: walk to the water and scoop it. :again when filled, :blocked when not."
  [c]
  (let [water (first (seen-of c ["water"]))]
    (if-not water
      :blocked
      (let [w (await (near/go-near! c (:pos water) 3))]
        (case w
          :there (if (= "placed" (.-status (await (ctx/act c :place (clj->js {:pos (:pos water) :item "bucket"}))))) :again :blocked)
          :partial :continue
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

(defn ^:async step [c]
  (let [{:keys [count]} (:args c)
        base (:base (ctx/mem c))
        got (- (carried c "obsidian") base)]
    (cond
      (>= got count) (result/finish! c {:got got})
      (seq (seen-of c ["obsidian"]))
      (let [st (await (child/run! c :mine 'jobs.gather.mine
                                  {:block "obsidian" :item "obsidian" :count (- count got) :fetch mine-fetch :mend false :radius (:radius (:args c))}
                                  {:max-calls 1}))]
        (if (= :continue st)
          :continue
          (let [res (ctx/child-result c :mine)]
            (if (> (- (carried c "obsidian") base) got)
              :again
              (stop! c (or (:reason res) :mine-failed) (str "the obsidian could not be mined: " (name (or (:reason res) :unknown))))))))
      (>= (:failed (ctx/mem c) 0) 2) (stop! c :pour-failed "water on the lava did not make obsidian twice in a row")
      (zero? (carried c "water_bucket"))
      (if (pos? (carried c "bucket"))
        (let [r (await (fill-bucket! c))]
          (case r
            :continue :continue
            :again :again
            (stop! c :no-bucket "no water bucket is carried and no seen water to fill the bucket at")))
        (stop! c :no-bucket "no water bucket is carried (a bucket is 3 iron plus a seen water source)"))
      :else
      (if-let [site (first (sites c))]
        (let [w (await (near/go-near! c (:stand site) 1))]
          (case w
            :partial :continue
            :there (let [r (await (pour! c site))]
                     (if (= :again r)
                       (do (ctx/update-mem! c assoc :failed 0) :again)
                       (do (ctx/update-mem! c #(-> % (update :tried (fnil conj []) (:lava site)) (update :failed (fnil inc 0))))
                           :again)))
            (stop! c :unreachable (str "could not walk to the stand at " (pr-str (:stand site))))))
        (stop! c :no-lava "no seen lava with open air above and a safe stand beside it")))))

(defn ^:async round [c]
  (ctx/update-mem! c update :base #(or % (carried c "obsidian")))
  (await (pace/steps! c #(step c))))
