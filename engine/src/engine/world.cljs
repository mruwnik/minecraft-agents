(ns engine.world
  "The world's shared knowledge as a body reads it: the plans of its world (worlds/<world>/plans/<id>.edn) and
  the blueprints they place (blueprints/<id>.edn at the repo root), parsed with plan.parse and expanded with
  plan.shape. Bodies only read; nothing here writes a file.

  A world is {:state atom :said atom :opts {...}}. Jobs ask through engine.ctx (ctx/plan), which answers from memory:
  the files are stat-ed again at most every :every-ms (asked lazily, by the first read after that), and only files
  whose stamp (modification time and size) changed are read and parsed again. A file that turns unreadable or invalid
  keeps its last good copy and is reported once, by a world.plan-unreadable (or world.blueprint-unreadable) warn; a
  file that was never readable answers as broken, not as absent.

  The zone file (worlds/<world>/zones.edn, checked by engine.zones) is read the same way, with one difference:
  a missing file answers nil, like one never readable (warned once, by world.zones-missing, each time it goes), so a
  dig or place job declines; an empty vector is a valid \"no zones\". A bad edit keeps the last good copy
  (world.zones-unreadable).

  The claims file (worlds/<world>/claims.edn, checked by engine.zones/parse-claims) is read the same way, but a missing
  file is [] (no claims, no warn); a bad edit keeps the last good copy (world.claims-unreadable). These are the
  shared-map area claims of bodies, held under :area-claims; the :claims of the state are the plans' cells.

  The state: {:plans {id entry} :blueprints {id entry} :zones entry :expanded {id expansion} :claims {id #{cell}}
  :footprints {cell id} :checked-at ms}, an entry being {:stamp [mtime size] :value v :error text}, :value the last
  good copy and :error the current file's trouble; the zone entry is {:missing true} while there is no file.
  :claims holds the cells of each plan, :footprints the same keyed by cell."
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [engine.zones :as zones]
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

(defn claims
  "{id #{[x y z]}}: the expanded cells of each plan that has a last good copy."
  [{:keys [plans expanded]}]
  (into {} (keep (fn [[id e]] (when (:value e)
                                [id (into #{} (map :pos) (get-in expanded [id :cells]))])))
        plans))

(defn footprint-index
  "{[x y z] id} of claims; a cell two plans claim names one of them."
  [claims]
  (into {} (for [[id cells] claims cell cells] [cell id])))

(defn expand-all
  "state with :expanded rebuilt (each plan's last good copy expanded over the blueprints' last good copies), and the
  :claims and :footprints of the active plans with it."
  [state]
  (let [blueprints (into {} (keep (fn [[id e]] (when (:value e) [id (:value e)]))) (:blueprints state))
        state (assoc state :expanded (into {} (keep (fn [[id e]] (when (:value e) [id (shape/expand (:value e) blueprints)])))
                                           (:plans state)))
        cs (claims state)]
    (assoc state :claims cs :footprints (footprint-index cs))))

(defn answer
  "What a job gets for plan id: nil (no such file), {:id :broken text} (never readable), or
  {:id :plan :cells :errors} (:cells [{:pos [x y z] :want :part}], :errors those of the expansion), with
  :error when the current file is bad and this is the last good copy."
  [state id]
  (let [{:keys [value error] :as entry} (get-in state [:plans id])
        expansion (get-in state [:expanded id])]
    (cond
      (nil? entry) nil
      (nil? value) {:id id :broken error}
      :else (cond-> {:id id :plan value :cells (:cells expansion) :errors (:errors expansion)}
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

(defn file-stamp
  "[mtime size] of file, or nil when there is none."
  [file]
  (when-let [st (try (.statSync fs file) (catch :default _ nil))]
    [(.-mtimeMs st) (.-size st)]))

(defn read-zones
  "The zone file parsed into {:value zones} or {:errors [..]}; an unreadable file is an error."
  [file]
  (let [text (try (.readFileSync fs file "utf8") (catch :default e {:error (ex-message e)}))]
    (if (map? text)
      {:errors [(str "unreadable file: " (:error text))]}
      (zones/parse text))))

(defn absorb-zones
  "Fold the zone file's stamp (nil: no file) into its entry: [entry warn]. parsed is a thunk giving the file parsed,
  called only when the stamp changed. No file forgets the zones, warning {:missing true} when it goes; a bad file
  keeps the last good copy and warns {:id :error :kept} like absorb."
  [entry stamp parsed]
  (cond
    (nil? stamp) [{:missing true} (when-not (:missing entry) {:missing true})]
    (= stamp (:stamp entry)) [entry nil]
    :else (let [[entries warn] (absorb {:zones (dissoc entry :missing)} :zones stamp (parsed))]
            [(:zones entries) warn])))

(defn read-claims
  "The claims file parsed into {:value claims} or {:errors [..]}; an unreadable file is an error."
  [file]
  (let [text (try (.readFileSync fs file "utf8") (catch :default e {:error (ex-message e)}))]
    (if (map? text)
      {:errors [(str "unreadable file: " (:error text))]}
      (zones/parse-claims text))))

(defn absorb-claims
  "Fold the claims file's stamp (nil: no file) into its entry: [entry warn]. No file is no claims; a bad file keeps
  the last good copy and warns {:id :error :kept} like absorb. parsed is a thunk, called only when the stamp changed."
  [entry stamp parsed]
  (cond
    (nil? stamp) [{:value []} nil]
    (= stamp (:stamp entry)) [entry nil]
    :else (let [[entries warn] (absorb {:area-claims entry} :area-claims stamp (parsed))]
            [(:area-claims entries) warn])))

(defn claims-warn-event [file {:keys [error kept]}]
  {:source :system :kind :world.claims-unreadable :level :warn :path file :error error :kept kept
   :text (str "claims file " file " cannot be read (" error ")"
              (if kept "; the last good copy is still used" "; it was never readable, no claims are known"))})

(defn zones-warn-event [file {:keys [missing error kept]}]
  (if missing
    {:source :system :kind :world.zones-missing :level :warn :path file
     :text (str "zone file " file " is missing; dig and place jobs decline until it is there")}
    {:source :system :kind :world.zones-unreadable :level :warn :path file :error error :kept kept
     :text (str "zone file " file " cannot be read (" error ")"
                (if kept "; the last good copy is still used" "; it was never readable, dig and place jobs decline"))}))

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
              blueprints (first (:blueprints results))
              file (:zones-file opts)
              [zone-entry zone-warn] (if file
                                       (absorb-zones (:zones old) (file-stamp file) #(read-zones file))
                                       [(:zones old) nil])
              claims-file (:claims-file opts)
              [claims-entry claims-warn] (if claims-file
                                           (absorb-claims (:area-claims old) (file-stamp claims-file) #(read-claims claims-file))
                                           [(:area-claims old) nil])]
          (reset! state (cond-> (assoc old :checked-at now :plans plans :blueprints blueprints :zones zone-entry
                                       :area-claims claims-entry)
                          (or (not= (:plans old) plans) (not= (:blueprints old) blueprints)) expand-all))
          (doseq [[kind [_ warns]] results
                  warn warns]
            ((:emit opts) (warn-event (get kinds kind) warn)))
          (when zone-warn
            ((:emit opts) (zones-warn-event file zone-warn)))
          (when claims-warn
            ((:emit opts) (claims-warn-event claims-file claims-warn))))))))

;; ------------------------------------------------------------------ worlds

(defn open
  "A world over the files of :plans-dir and :blueprint-dir and the zone file :zones-file. opts: :now (ms clock),
  :emit (an event fn), :every-ms."
  [{:keys [now every-ms] :as opts}]
  {:state (atom {:plans {} :blueprints {}})
   :said (atom #{})
   :derived (atom {})
   :opts (assoc opts :now (or now js/Date.now) :every-ms (or every-ms default-every-ms))})

(defn data-state [plans blueprints]
  (expand-all {:plans (update-vals plans (fn [p] {:value p}))
               :blueprints (update-vals blueprints (fn [b] {:value b}))}))

(defn zone-entry [zones] (if (some? zones) {:value zones} {:missing true}))

(defn of-data
  "A world over plans {id plan}, blueprints {id blueprint} and zones (default [], none; nil: never read) held in
  memory, no files (tests)."
  ([plans blueprints] (of-data plans blueprints []))
  ([plans blueprints zones]
   {:state (atom (assoc (data-state plans blueprints) :zones (zone-entry zones) :area-claims {:value []})) :said (atom #{}) :derived (atom {})
    :opts {}}))

(defn set-data!
  "Replace the plans and blueprints of a world made with of-data; its zones stay."
  [w plans blueprints]
  (swap! (:state w) #(assoc (data-state plans blueprints) :zones (:zones %) :area-claims (:area-claims %))))

(defn set-zones!
  "Replace the zones of a world made with of-data (nil: never read)."
  [w zones]
  (swap! (:state w) assoc :zones (zone-entry zones)))

(defn set-area-claims!
  "Replace the claims of a world made with of-data."
  [w claims]
  (swap! (:state w) assoc :area-claims {:value claims}))

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

(defn answers
  "The answers (see answer) of every plan in state, by id."
  [state]
  (keep #(answer state %) (sort (keys (:plans state)))))

(defn derived
  "(f answers) over the answers of all plans, kept under key k until a plan or blueprint changes: for a
  reader asked every tick (a trigger) that must not rebuild its index per tick. nil for a nil world."
  [w k f]
  (when w
    (refresh! w)
    (let [{:keys [plans expanded] :as state} @(:state w)
          kept (get @(:derived w) k)]
      (cond
        (and kept (identical? plans (:plans kept)) (identical? expanded (:expanded kept))) (:value kept)
        (and kept (= plans (:plans kept)) (= expanded (:expanded kept)))
        (do (swap! (:derived w) assoc k (assoc kept :plans plans :expanded expanded)) (:value kept))
        :else (let [value (f (answers state))]
                (swap! (:derived w) assoc k {:plans plans :expanded expanded :value value})
                value)))))

(defn zones
  "The world's zone list, or nil when the zone file is missing or was never readable (nil for a nil world)."
  [w]
  (when w
    (refresh! w)
    (get-in @(:state w) [:zones :value])))

(defn footprints
  "{[x y z] plan-id}: the cells the plans claim, without plan except's own (nil leaves none out); {} for a
  nil world. A cell plan except shares with another plan stays, under the other's id."
  [w except]
  (if-not w
    {}
    (do (refresh! w)
        (let [{:keys [claims footprints]} @(:state w)]
          (if (contains? claims except)
            (footprint-index (dissoc claims except))
            (or footprints {}))))))

(defn area-claims
  "The world's area claims as read from claims.edn ([] when there is no file or no world; the last good copy when it is
  bad). Not filtered by status or expiry: see live-claims."
  [w]
  (if-not w
    []
    (do (refresh! w)
        (or (get-in @(:state w) [:area-claims :value]) []))))

(defn live-claims
  "The claims that are :active (keyword or text) and whose :until is after now (ms)."
  [claims now]
  (filterv #(and (= "active" (some-> (:status %) name)) (> (:until %) now)) claims))
