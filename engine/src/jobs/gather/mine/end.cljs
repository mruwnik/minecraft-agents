(ns jobs.gather.mine.end
  "How jobs.gather.dig-for and jobs.gather.mine end: the end texts, finish! and stop! (the dig), end-run (both)."
  (:require [jobs.gather.mine.way-set :as ways]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.shafts :as shafts]
            [jobs.lib.util :as u]
            [jobs.lib.look :refer [cell-of]]
            [jobs.gather.mine.targets :refer [kinds left-behind owed got]]))

(defn shaft-suggest
  "{:at :heading :to-y} of the stair this run cut with no shaft (the top step's feet cell, the stair's heading, the feet y at
  its bottom), when it could be declared as a mineshaft: not when a shaft was used or :descend-shaft is not a way."
  [{:keys [descent shaft]} args]
  (let [{:keys [top top-heading bottom-y]} descent]
    (when (and top bottom-y (ways/shaft? args) (not= :used (:state shaft)) (< bottom-y (second top)))
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
  #{:none :wet :refused :own-zone :no-shaft :flooded :no-tool :no-torches})

(defn wanted
  "The items more than carried at the start the call needs for :done: up to :count carried, or the call's :least when that is a number."
  [c]
  (let [n (owed c)
        least (:least (ctx/frozen-call c) :all)]
    (if (number? least) (min n least) n)))

(defn end-run
  "The ending of a run with data {:got n :reason r ..}: :done when :got meets wanted, else by reason a kind: a fact (a worn last tool
  is :no-tool) condition-failed, any other nothing-possible. The cause is {:reason r} with :tool and :why when the data has them."
  [c {:keys [got reason] :as data}]
  (ctx/update-mem! c assoc :finished true)
  (let [cause (merge {:reason reason} (select-keys data [:tool :why]))]
    (cond
      (>= got (wanted c)) (ctx/end c :done nil data)
      (facts reason) (ctx/end c :condition-failed cause data)
      :else (ctx/end c :nothing-possible cause data))))

(defn tunnel-tail
  "How the tunnel ended, for the end texts: \", ended at 1,60,2\" (empty without a tunnel)."
  [tunnel]
  (when-let [end (when (pos? (:steps tunnel 0)) (:end tunnel))]
    (str ", ended at " (str/join "," end))))

(defn record-shaft-branch!
  "Remember the branch of the shaft this run dug (once), so the next run takes another."
  [c]
  (let [{:keys [shaft tunnel]} (ctx/mem c)
        {:keys [id branch recorded?]} shaft]
    (when (and branch (not recorded?) (pos? (:steps tunnel 0)))
      (shafts/record-branch! c id (assoc branch :length (:steps tunnel)))
      (ctx/update-mem! c assoc-in [:shaft :recorded?] true))))

(defn still-lying
  "Of left ([{:pos :count}]) the drops still lying in the world: one a later steer picked up (or that despawned) is dropped. Looks as
  far as the farthest listed drop, from where the body stands now."
  [c left]
  (if (empty? left)
    left
    (let [here (u/self-pos c)
          radius (inc (apply max (map #(u/dist here (zipmap [:x :y :z] (:pos %))) left)))
          lying (left-behind c radius)]
      (filterv #(contains? lying (:pos %)) left))))

(defn still-left!
  "Keep in :left (memory, pos -> count) only the drops still lying in the world."
  [c]
  (ctx/update-mem! c assoc :left (into {} (map (juxt :pos :count))
                                       (still-lying c (mapv (fn [[pos n]] {:pos pos :count n}) (:left (ctx/mem c)))))))

(defn finish!
  "End the dig (end-run) with the outcome in memory (:reason, :end-cause)."
  [c]
  (when (seq (:left (ctx/mem c))) (still-left! c))
  (ctx/update-mem! c #(cond-> % (pos? (get-in % [:tunnel :steps] 0)) (assoc-in [:tunnel :end] (access/cell (cell-of (u/self-pos c))))))
  (let [{:keys [dig-reason tunnel left descent wet-skipped ground] :as m} (ctx/mem c)
        suggest (shaft-suggest m (:args c))
        unreached (into [] (comp (filter (fn [[x y z]] (contains? (kinds (:args c)) (u/seen-name (:primitives c) {:x x :y y :z z})))) (take 10)) (:unreached m))
        m (assoc m :shaft-suggest suggest :unreached unreached)
        got (got c)
        reason (if (and (zero? got) (pos? (or wet-skipped 0)) (#{:none :no-drops :tunnel-length :tunnel-stopped} (:reason m))) :wet (:reason m))
        wet-text (when (= :wet reason) (str "; " (or wet-skipped 0) " seen blocks beside water skipped, pass :wet true to dig them"))
        why (cond-> {} dig-reason (assoc :dig-reason dig-reason) (or (= :no-stone-found reason) (pos? (:steps descent 0))) (assoc :descent descent) (= :wet reason) (assoc :wet-skipped (or wet-skipped 0)) suggest (assoc :shaft-suggest suggest) (seq unreached) (assoc :unreached unreached) (seq (:holes-open m)) (assoc :holes-open (:holes-open m))
              (pos? (:steps tunnel 0)) (assoc :tunnel (-> tunnel (select-keys [:origin :heading :steps :stop :end]) (update :origin access/cell)))
              (:start-floor-dug m) (assoc :start-floor-dug true)
              (seq (:legs m)) (assoc :legs (:legs m))
              (seq left) (assoc :left (mapv (fn [[pos n]] {:pos pos :count n}) (:left (ctx/mem c))))
              (seq (:lost m)) (assoc :lost (:lost m))
              (:track-ground (:args c)) (assoc :ground ground))]
    (record-shaft-branch! c)
    (end-run c (merge (let [soil (when (= :no-stone-found reason) (str "; dug down " (:steps descent 0) " blocks through soil, found no stone"))
                            tail (str wet-text (dug-text m reason) (tunnel-tail tunnel))]
                        (cond
                          (zero? got) {:text (str "dig-for got nothing: " (name reason) soil tail)}
                          (< got (owed c)) {:text (str "dig-for got " got " of " (owed c) ": " (name reason) tail)}
                          :else {:text (str "dig-for done: " (name reason) ", got " got soil tail)}))
                      {:got got :reason reason}
                      (:end-cause m)
                      why))))

(defn stop!
  "End the dig with reason (and cause fields: :tool :why :text) from wherever it stands."
  ([c reason] (stop! c reason nil))
  ([c reason cause]
   (ctx/update-mem! c assoc :reason reason :end-cause cause)
   (finish! c)))
