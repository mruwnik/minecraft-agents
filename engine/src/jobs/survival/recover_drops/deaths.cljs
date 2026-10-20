(ns jobs.survival.recover-drops.deaths
  "The deaths a run serves and the trip saved for one: which is open, which is next, the saved :recover-trip."
  (:require [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.memory :as mem]
            [jobs.lib.body :as body]
            [jobs.lib.cost :as cost]
            [jobs.lib.util :as u]
            [jobs.survival.recover-drops.config :as cfg]))

(defn retried-deaths
  "With :retry, the latest death whose pile an earlier run ended :abandoned while its despawn window is still open."
  [c]
  (let [view (ctx/view c)
        died (mem/latest view :died)
        recovered (last (filter #(= (:t died) (:death-t (:data %))) (mem/entries view :recovered)))]
    (when (and (:retry (:args c)) died (= :abandoned (:decision (:data recovered)))
               (< (game/ticks-since died view) game/despawn-ticks))
      [died])))

(defn stale-deaths
  "The open deaths whose despawn window has closed while a newer death is open: their pile is gone and the newer one
  is the one to serve."
  [c]
  (let [view (ctx/view c)
        open (body/open-deaths view)]
    (filterv (fn [d] (and (>= (game/ticks-since d view) game/despawn-ticks)
                          (some #(> (:t %) (:t d)) open)))
             open)))

(defn open-deaths
  "The open deaths to serve: body/actionable-deaths (with :retry every open one, a remembered danger too), plus the
  death :retry takes up again; none whose pile is gone while a newer death is open (stale-deaths)."
  [c]
  (let [view (ctx/view c)
        stale (set (map :t (stale-deaths c)))]
    (vec (remove #(stale (:t %))
                 (sort-by :t (concat (retried-deaths c)
                                     (if (:retry (:args c)) (body/open-deaths view) (body/actionable-deaths view))))))))

(defn danger-wait
  "The wait reason while the only open deaths are ones declined for a remembered danger, else nil."
  [view]
  (when-let [{:keys [until text]} (some #(body/danger-hold view %) (body/open-deaths view))]
    {:reason :danger :until until
     :text (str "the pile was declined for danger (" text "); weighed again in " (js/Math.ceil (/ (- until (:now view)) 1000))
                " s, or now with (jobs.survival.recover-drops {:retry true})")}))

(defn death-score
  "What a death's pile is worth net of the walk to it: its carried items' value less 0.3 per block."
  [c died]
  (let [{:keys [pos inventory]} (:data died)]
    (- (:value (cost/item-value inventory :overrides (:value-overrides (:args c))))
       (* 0.3 (if pos (u/dist (u/self-pos c) pos) 0)))))

(defn current-death
  "The death this run serves: the one its memory is keyed to while still open, else the open death inside its window
  whose pile is best (value net of the walk), else the latest open one (past its window: it ends :abandoned)."
  [c]
  (let [open (open-deaths c)
        keyed (some #(when (= (:t %) (:death-t (ctx/mem c))) %) open)
        fresh (filterv #(< (game/ticks-since % (ctx/view c)) game/despawn-ticks) open)]
    (or keyed
        (when (seq fresh) (apply max-key #(death-score c %) fresh))
        (peek open))))

(defn settling?
  "True while the latest :respawned entry, newer than the death, is younger than settle-ms."
  [c entry]
  (let [respawned (ctx/latest c :respawned)]
    (boolean (and respawned
                  (> (:t respawned) (:t entry))
                  (< (- (ctx/now c) (:t respawned)) cfg/settle-ms)))))

(defn saved-trip
  "The :recover-trip entry's data for the death at death-t, else nil."
  [c death-t]
  (let [trip (:data (ctx/latest c :recover-trip))]
    (when (= death-t (:death-t trip)) trip)))

(defn save-trip!
  "Write the trip so far (job memory :decided :baseline :phase :cuts :blocked :fruitless :hostile-cut) to the :recover-trip entry."
  [c]
  (let [m (ctx/mem c)]
    (ctx/remember! c :recover-trip (select-keys m [:death-t :decided :baseline :phase :cuts :blocked :fruitless :hostile-cut]) cfg/trip-policy)))

(defn hostile-cut?
  "Whether the saved trip was last cut by a hostile: it declined for a danger, or the hostile reflex wrote a :hostile
  entry since the trip was saved. A cut by night, eating or a restart is not."
  [c saved]
  (boolean (or (:hostile-cut saved)
               (seq (ctx/since c :hostile (:t (ctx/latest c :recover-trip)))))))

(defn key-to-death!
  "Reset the job memory when it belongs to another death than entry: start from the saved trip of this death (a run
  cut before, or a restart), else afresh. The hostile cuts are counted whatever :blocked says."
  [c entry]
  (when (not= (:t entry) (:death-t (ctx/mem c)))
    (let [saved (when-not (seq (retried-deaths c)) (saved-trip c (:t entry)))]
      (ctx/update-mem! c (constantly (merge {:death-t (:t entry)}
                                            (select-keys saved [:decided :baseline :phase :blocked :fruitless])
                                            (when saved {:cuts (+ (:cuts saved 0) (if (hostile-cut? c saved) 1 0))}))))
      (when saved (save-trip! c)))))

(defn window-closed?
  "True once the despawn window of the death entry has passed."
  [c entry]
  (>= (game/ticks-since entry (ctx/view c)) game/despawn-ticks))
