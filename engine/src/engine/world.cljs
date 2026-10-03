(ns engine.world
  "The world's shared knowledge as a body reads it: the plans of its world (state/worlds/<world>/plans/<id>.edn) and
  the blueprints they place (blueprints/<id>.edn at the repo root), parsed with plan.parse and expanded with
  plan.shape. Bodies only read; nothing here writes a file.

  A world is {:state atom :said atom :opts {...}}. Jobs ask through engine.ctx (ctx/plan), which answers from memory:
  the files are stat-ed again at most every :every-ms (asked lazily, by the first read after that), and only files
  whose stamp (modification time and size) changed are read and parsed again. A file that turns unreadable or invalid
  keeps its last good copy and is reported once, by a world.plan-unreadable (or world.blueprint-unreadable) warn; a
  file that was never readable answers as broken, not as absent.

  The state: {:plans {id entry} :blueprints {id entry} :expanded {id expansion} :checked-at ms}, an entry being
  {:stamp [mtime size] :value v :error text}, :value the last good copy and :error the current file's trouble."
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [plan.parse :as parse]
            [plan.shape :as shape]))

(def default-every-ms 3000)

;; ------------------------------------------------------------------ pure bookkeeping

(defn due?
  "Whether the files should be stat-ed again: never checked, or :every-ms has passed."
  [{:keys [checked-at every-ms]} now]
  (or (nil? checked-at) (>= (- now checked-at) every-ms)))

(defn stale-ids
  "Ids of stamps {id stamp} that are new or whose stamp differs from the entry's."
  [entries stamps]
  (keep (fn [[id stamp]] (when (not= stamp (get-in entries [id :stamp])) id)) stamps))

(defn drop-gone
  "Entries without a file any more are forgotten."
  [entries stamps]
  (select-keys entries (keys stamps)))

(defn absorb
  "Fold one parsed file ({:value v} or {:errors [text ..]}) into entries: [entries warn], warn being nil or
  {:id :error :kept} (kept: a last good copy is still used)."
  [entries id stamp {:keys [value errors]}]
  (if (empty? errors)
    [(assoc entries id {:stamp stamp :value value}) nil]
    (let [error (str/join "; " errors)
          old (get-in entries [id :value])]
      [(assoc entries id (cond-> {:stamp stamp :error error} old (assoc :value old)))
       {:id id :error error :kept (some? old)}])))

(defn expand-all
  "state with :expanded rebuilt: each plan's last good copy expanded over the blueprints' last good copies."
  [state]
  (let [blueprints (into {} (keep (fn [[id e]] (when (:value e) [id (:value e)]))) (:blueprints state))]
    (assoc state :expanded (into {} (keep (fn [[id e]] (when (:value e) [id (shape/expand (:value e) blueprints)])))
                                 (:plans state)))))

(defn answer
  "What a job gets for plan id: nil (no such file), {:id :broken text} (never readable), or
  {:id :plan :status :cells :errors} (:cells [{:pos [x y z] :want :part}], :errors those of the expansion), with
  :error when the current file is bad and this is the last good copy."
  [state id]
  (let [{:keys [value error] :as entry} (get-in state [:plans id])
        expansion (get-in state [:expanded id])]
    (cond
      (nil? entry) nil
      (nil? value) {:id id :broken error}
      :else (cond-> {:id id :plan value :status (:status value) :cells (:cells expansion) :errors (:errors expansion)}
              error (assoc :error error)))))

;; ------------------------------------------------------------------ files

(defn stamps
  "{id [mtime size]} of the <id>.edn files of dir; a missing dir has none."
  [dir]
  (let [names (try (vec (.readdirSync fs dir)) (catch :default _ []))]
    (into {} (keep (fn [f]
                     (when (str/ends-with? f ".edn")
                       (when-let [st (try (.statSync fs (path/join dir f)) (catch :default _ nil))]
                         [(str/replace f #"\.edn$" "") [(.-mtimeMs st) (.-size st)]]))))
          names)))

(defn read-parsed
  "The file of id parsed with parse-fn into {:value v} or {:errors [..]}; an unreadable file is an error."
  [dir id parse-fn k]
  (let [text (try (.readFileSync fs (path/join dir (str id ".edn")) "utf8") (catch :default e {:error (ex-message e)}))]
    (if (map? text)
      {:errors [(str "unreadable file: " (:error text))]}
      (let [parsed (parse-fn text id)]
        (if (:errors parsed) parsed {:value (get parsed k)})))))

(def kinds
  {:plans {:dir :plans-dir :parse parse/parse :k :plan :kind :world.plan-unreadable :what :plan}
   :blueprints {:dir :blueprint-dir :parse parse/parse-blueprint :k :blueprint :kind :world.blueprint-unreadable
                :what :blueprint}})

(defn sync-kind
  "[entries warns] for one kind of file, re-reading only what changed."
  [entries dir {:keys [parse k]}]
  (let [now-stamps (stamps dir)]
    (reduce (fn [[entries warns] id]
              (let [[entries warn] (absorb entries id (get now-stamps id) (read-parsed dir id parse k))]
                [entries (cond-> warns warn (conj warn))]))
            [(drop-gone entries now-stamps) []]
            (stale-ids entries now-stamps))))

(defn warn-event [{:keys [kind what]} {:keys [id error kept]}]
  {:source :system :kind kind :level :warn what id :error error :kept kept
   :text (str (name what) " " id " cannot be read (" error ")"
              (if kept "; the last good copy is still used" "; it was never readable"))})

(defn refresh!
  "Stat the files when due and fold in what changed; emits a warn per newly broken file. A world without
  files (of-data) is never refreshed."
  [{:keys [state opts]}]
  (when (:plans-dir opts)
    (let [now ((:now opts))]
      (when (due? (assoc @state :every-ms (:every-ms opts)) now)
        (let [old @state
              results (into {} (map (fn [[kind spec]] [kind (sync-kind (get old kind) (get opts (:dir spec)) spec)]))
                            kinds)
              plans (first (:plans results))
              blueprints (first (:blueprints results))]
          (reset! state (cond-> (assoc old :checked-at now :plans plans :blueprints blueprints)
                          (or (not= (:plans old) plans) (not= (:blueprints old) blueprints)) expand-all))
          (doseq [[kind [_ warns]] results
                  warn warns]
            ((:emit opts) (warn-event (get kinds kind) warn))))))))

;; ------------------------------------------------------------------ worlds

(defn open
  "A world over the files of :plans-dir and :blueprint-dir. opts: :now (ms clock), :emit (an event fn), :every-ms."
  [{:keys [now every-ms] :as opts}]
  {:state (atom {:plans {} :blueprints {}})
   :said (atom #{})
   :opts (assoc opts :now (or now js/Date.now) :every-ms (or every-ms default-every-ms))})

(defn data-state [plans blueprints]
  (expand-all {:plans (update-vals plans (fn [p] {:value p}))
               :blueprints (update-vals blueprints (fn [b] {:value b}))}))

(defn of-data
  "A world over plans {id plan} and blueprints {id blueprint} held in memory, no files (tests)."
  [plans blueprints]
  {:state (atom (data-state plans blueprints)) :said (atom #{}) :opts {}})

(defn set-data!
  "Replace the plans and blueprints of a world made with of-data."
  [w plans blueprints]
  (reset! (:state w) (data-state plans blueprints)))

(defn plan
  "The answer for plan id (see answer); nil for a nil world."
  [w id]
  (when w
    (refresh! w)
    (answer @(:state w) id)))

(defn first-time!
  "True the first time key is given to this world, false after: for warns said once per body process."
  [w key]
  (let [[old _] (swap-vals! (:said w) conj key)]
    (not (contains? old key))))
