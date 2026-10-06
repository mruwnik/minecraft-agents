(ns engine.core.restart
  "Restoring saved state after a restart: reflex jobs dropped, jobs and register entries that no longer
  resolve dropped or stripped of stale args."
  (:require [engine.core.base :refer [emit! empty-state job-of reflex-text remove-listed state trigger-def]]
            [engine.expr :as expr]
            [engine.memory :as mem]
            [clojure.string :as str]))

(defn restore
  "Saved state after a restart: the in-flight round is lost (its job resumes
  first); reflex jobs and the manual slot job (the lease is not saved) are dropped."
  [saved]
  (let [s (merge empty-state saved)
        reflex-ids (keep (fn [[id inst]] (when (:reflex inst) id)) (:instances s))
        slot-ids (set (keep (fn [[id inst]] (when (:slot? inst) id)) (:instances s)))
        current (:current s)]
    (-> s
        (update :list #(filterv (complement slot-ids) %))
        (update :failed select-keys (:list s))
        (dissoc :backoff)
        (assoc :resume (if (and (some #{current} (:list s)) (not (slot-ids current))) current (:resume s))
               :current nil)
        (update :instances #(apply dissoc % (concat reflex-ids slot-ids))))))

(defn drop-leftover-slot-jobs!
  "After a restore: end the manual slot job saved held (restore unlisted it) with one stopped event, reason :restart."
  [eng saved]
  (doseq [[id inst] (:instances saved)
          :when (:slot? inst)]
    (mem/delete-job! (:store eng) id)
    (emit! eng {:source :job :kind :stopped :level :warn :job id :chain [id] :reason :restart
                :text "stopped: restart (manual jobs end with the lease)"})))

(defn unknown-job
  "The message why inst's spec no longer resolves against the registry, or nil."
  [eng inst]
  (try (job-of eng inst) nil
       (catch :default e (ex-message e))))

(defn stale-keys
  "The arg keys of job sym that its registry entry no longer declares, sorted; nil when none."
  [registry sym args]
  (let [entry (get registry sym)]
    (when (contains? entry :args)
      (seq (sort-by str (remove (set (keys (:args entry))) (keys args)))))))

(defn strip-form
  "[form' stale] for a job spec form: every leaf's args without the keys its job no longer declares;
  stale is [[sym [key ...]] ...]. Forms of an unexpected shape pass through (parse refuses them)."
  [registry form]
  (let [head (when (and (seq? form) (seq form)) (first form))
        parts (rest form)
        kids (fn [kids] (reduce (fn [[out stale] k] (let [[k2 s2] (strip-form registry k)] [(conj out k2) (into stale s2)]))
                                [[] []] kids))]
    (cond
      (not (symbol? head)) [form []]
      (#{'seq 'any 'repeat 'hold} head) (let [[out stale] (kids parts)] [(apply list head out) stale])
      (not (map? (first parts))) [form []]
      :else (let [args (first parts)
                  ks (stale-keys registry head args)]
              (if ks
                [(list head (apply dissoc args ks)) [[head (vec ks)]]]
                [form []])))))

(defn strip-node
  "[node' stale] for a parsed node, as strip-form does for a form."
  [registry node]
  (case (:op node)
    :leaf (let [ks (stale-keys registry (:job node) (:args node))]
            [(cond-> node ks (update :args #(apply dissoc % ks))) (if ks [[(:job node) (vec ks)]] [])])
    :repeat (let [[c stale] (strip-node registry (:child node))] [(assoc node :child c) stale])
    (let [rs (mapv #(strip-node registry %) (:children node))]
      [(assoc node :children (mapv first rs)) (into [] (mapcat second) rs)])))

(defn stale-text [stale]
  (str/join "; " (for [[sym ks] stale] (str sym " " (str/join " " ks)))))

(defn drop-unknown-jobs!
  "After a restore: a listed job whose job namespace is gone is dropped with a warn; one whose saved args
  hold keys its job no longer declares keeps running without them (defaults apply) and is returned in a
  vector of notices {:job-id :reason :message :data} to raise once restore is done."
  [eng]
  (vec
   (for [[id inst] (:instances (state eng))
         :let [problem (unknown-job eng inst)]
         :let [[node stale] (when-not problem (strip-node (:jobs eng) (:spec inst)))]
         :when (or problem (seq stale))
         :let [notice (when-not problem
                        (swap! (:state eng) assoc-in [:instances id :spec] node)
                        {:job-id id :reason :stale-args :kind :stale-args
                         :data {:dropped (mapv (fn [[sym ks]] {:job (str sym) :args (mapv str ks)}) stale)}
                         :message (str "job " id " restored without args its job no longer accepts (its defaults apply): " (stale-text stale))})]]
     (do (when problem
           (swap! (:state eng) remove-listed id)
           (mem/delete-job! (:store eng) id)
           (emit! eng {:source :job :kind :failed :level :warn :job id :chain [id]
                       :error problem :text (str "dropped on restore: " problem)}))
         notice))))

(defn drop-leftover-reflex-jobs!
  "After a restore: restore dropped the reflex jobs a crash left in saved;
  say so with one reflex.ended each (a clean shutdown already did)."
  [eng saved]
  (doseq [[id inst] (:instances saved)
          :when (:reflex inst)]
    (emit! eng {:source :reflex :kind :ended :level :info :reflex (:reflex inst) :job id
                :outcome :dropped :how :dropped :by :restart
                :text (str (reflex-text (:reflex inst) (:spec inst)) ": dropped")})))

(defn unresolved-entry
  "The message why register entry e no longer resolves (trigger or job spec), or nil."
  [eng e]
  (try (trigger-def eng (:trigger e))
       (expr/parse (:jobs eng) (:job e))
       nil
       (catch :default err (ex-message err))))

(defn remove-entry [s id]
  (-> s
      (update :register #(filterv (fn [x] (not= id (:id x))) %))
      (update :changes dissoc id)
      (update :reflex-state dissoc id)))

(defn repair-entries!
  "After a restore, fix register entries that no longer resolve.
  An entry whose trigger is gone, or whose job does not parse even without its stale args, is dropped.
  An entry with only stale args stays, minus those args (the job's defaults apply).
  Each case is warned and returned as a notice {:job-id :reason :kind :message :data}
  for the caller to raise as a required attention request. Never silent."
  [eng]
  (vec
   (for [e (:register (state eng))
         :let [problem (unresolved-entry eng e)]
         :when problem
         :let [id (:id e)
               [form stale] (strip-form (:jobs eng) (:job e))
               kept? (and (seq stale)
                          (some? (get-in eng [:triggers (:trigger e)]))
                          (nil? (expr/problem (:jobs eng) form)))]]
     (if kept?
       (let [text (str "reflex " id " restored without args its job no longer accepts (its defaults apply): " (stale-text stale))]
         (swap! (:state eng) update :register (fn [r] (mapv #(if (= id (:id %)) (assoc % :job form) %) r)))
         (emit! eng {:source :system :kind :reflex-repaired :level :warn :reflex id
                     :dropped (mapv (fn [[sym ks]] {:job (str sym) :args (mapv str ks)}) stale) :text text})
         {:job-id (str "reflex:" (name id)) :reason :reflex-repaired :kind :reflex-repaired
          :data {:reflex id :dropped (mapv (fn [[sym ks]] {:job (str sym) :args (mapv str ks)}) stale)}
          :message text})
       (let [text (str "reflex " id " DROPPED on restore, the body no longer has it: " problem)]
         (swap! (:state eng) remove-entry id)
         (emit! eng {:source :system :kind :dropped :level :warn :reflex id :error problem :text text})
         {:job-id (str "reflex:" (name id)) :reason :reflex-dropped :kind :reflex-dropped
          :data {:reflex id :error problem} :message text})))))
