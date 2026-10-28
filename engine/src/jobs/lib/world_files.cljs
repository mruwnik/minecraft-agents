(ns jobs.lib.world-files
  "The world's shared knowledge as a body reads it. Bodies only read; nothing here writes a file.
  Files:
    worlds/<world>/plans/<id>.edn  plans, parsed with plan.parse and expanded with plan.shape
    blueprints/<id>.edn            the blueprints plans place (repo root)
    worlds/<world>/zones.edn       zones (checked by jobs.lib.zone-file)
    worlds/<world>/claims.edn      shared-map area claims of bodies (jobs.lib.zone-file/parse-claims)
    worlds/<world>/places.json     shared markers {:name :kind :x :y :z :by :note}, written by the agent tools only

  A world is {:state atom :said atom :opts {...}}. Jobs ask through jobs.lib.world (plan, zones, claims), which
  answers from memory.
    A re-read zones.edn, places.json or plan file emits a :debug world.reloaded naming the files.
    The files are stat-ed again at most every :every-ms, by a read or by the timer a real body's world runs; a plan read
    checks its own file at once.
    Only files whose stamp (modification time and size) changed are read and parsed again.
    A file that turns unreadable or invalid keeps its last good copy and warns once
    (world.plan-unreadable, world.blueprint-unreadable, world.zones-unreadable, world.claims-unreadable).
    A file that was never readable answers as broken, not as absent.
  Zones differ: a missing file answers nil (warned once by world.zones-missing, each time it goes), so a dig or
  place job declines. An empty vector is a valid \"no zones\".
  Claims differ: a missing file is [] (no claims, no warn). So are markers (world.markers-unreadable on a bad file).

  The state: {:plans {id entry} :blueprints {id entry} :zones entry :area-claims entry :expanded {id (delay expansion)}
  :claims {id (delay #{cell})} :footprints (delay {cell id}) :checked-at ms}.
    A file's text is read when it changes but parsed by the first reader that needs its value (entry :lazy, see resolved), which
    is why a bad file warns when first used, not when read. Readers that scan every plan skip those whose text lacks their
    needle (mentions?, shaft-ids, plan-authors).
    Expansions are delays, forced by the first reader that needs them (a trigger that asks for gates forces only the plans
    that mention one). A real body's world also forces the footprint index (all expansions) right after each change of the plans.
    An entry is {:stamp [mtime size] :value v :error text}: :value the last good copy, :error the current file's
    trouble. The zone entry is {:missing true} while there is no file.
    :area-claims holds the claims file, :markers the markers file. :claims and :footprints are the plans' cells, by plan and by cell."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [engine.file-sync :as fsync]
            [jobs.lib.zone-file :as zones]
            [plan.parse :as parse]
            [plan.shape :as shape]))

(a/defargs settings
  {::default-every-ms {:default 3000 :doc "How often a world file is re-read, in ms." :spec (a/int-in 0 nil)}
   ::default-marker-limit {:default 10 :doc "Markers a world file listing returns by default." :spec (a/int-in 1 nil)}
   ::max-marker-limit {:default 50 :doc "The most markers a listing may ask for." :spec (a/int-in 1 nil)}})

(defn default-every-ms [] (settings/get settings ::default-every-ms))

;; ------------------------------------------------------------------ pure bookkeeping

(defn footprint-index
  "{[x y z] id} of claims; a cell two plans claim names one of them."
  [claims]
  (into {} (for [[id cells] claims cell cells] [cell id])))

;; ------------------------------------------------------------------ lazy entries

(defn resolved
  "The entry with its :value/:error, parsing the file now if nobody has needed it yet."
  [e]
  (if-let [d (:lazy e)] @d e))

(defn value "The last good copy of the plan or blueprint of entry e (parsed now if need be)." [e] (:value (resolved e)))

(defn value-if
  "(value e), unless the file's text lacks needle (then nil without parsing it)."
  [e needle]
  (let [t (:text e)]
    (when (or (nil? t) (str/includes? t needle)) (value e))))

(defn lazy-entry
  "Entry for a file whose text was read: parsed (with warn on a bad file, via warn!) by the first reader of its value."
  [old id stamp text parse-fn k warn!]
  {:stamp stamp
   :text text
   :lazy (delay
          (let [parsed (parse-fn text id)
                [entries warn] (fsync/absorb (if (:errors parsed) {id (resolved old)} {}) id stamp
                                             (if (:errors parsed) parsed {:value (get parsed k)}))]
            (when warn (warn! warn))
            (get entries id)))})

(defn soil-cells
  "The cells under each cell of expansion x wanting a crop: a field's farmland is no cell of the plan, yet the plan's own."
  [x]
  (into #{} (comp (filter (fn [{:keys [want]}] (and (map? want) (contains? want :crop))))
                  (map #(update (vec (:pos %)) 1 dec)))
        (:cells x)))

(defn expand-all
  "state with :expanded rebuilt (a delay of each plan's last good copy expanded over the blueprints' last good copies), and
  the :claims and :footprints of the active plans with it (delays too). With prior (the state before), a plan whose entry
  is unchanged keeps its expansion and claim, unless a blueprint changed."
  ([state] (expand-all state nil))
  ([state prior]
   (let [blueprints (delay (into {} (keep (fn [[id e]] (when-let [v (value e)] [id v]))) (:blueprints state)))
         reuse? (and prior (= (:blueprints prior) (:blueprints state)))
         same? (fn [id e] (and reuse? (= e (get-in prior [:plans id])) (contains? (:expanded prior) id)))
         active (:plans state)
         expanded (into {} (map (fn [[id e]] [id (if (same? id e)
                                                   (get-in prior [:expanded id])
                                                   (delay (when-let [v (value e)] (shape/expand v @blueprints))))]))
                        active)
         cs (into {} (map (fn [[id e]] [id (if (same? id e)
                                             (get-in prior [:claims id])
                                             (let [x (get expanded id)] (delay (into #{} (map :pos) (:cells @x)))))]))
                  active)]
     (assoc state :expanded expanded :claims cs :footprints (delay (footprint-index (update-vals cs deref)))))))

(defn expansion
  "The expansion of plan id in state (see shape/expand), expanded now if nobody has needed it yet; nil without one."
  [state id]
  (some-> (get-in state [:expanded id]) deref))

(defn answer
  "What a job gets for plan id: nil (no such file), {:id :broken text} (never readable), or
  {:id :plan :cells :spots :errors} (:cells [{:pos [x y z] :want :part}], :spots {\"part/spot\" [x y z]}, :errors those of the expansion), with
  :error when the current file is bad and this is the last good copy."
  [state id]
  (let [entry (some-> (get-in state [:plans id]) resolved)
        {:keys [value error]} entry
        expansion (when value (expansion state id))]
    (cond
      (nil? entry) nil
      (nil? value) {:id id :broken error}
      :else (cond-> {:id id :plan value :cells (:cells expansion) :spots (:spots expansion) :errors (:errors expansion)}
              error (assoc :error error)))))

;; ------------------------------------------------------------------ files

(def kinds
  {:plans {:dir :plans-dir :parse parse/parse :k :plan :kind :world.plan-unreadable :what :plan}
   :blueprints {:dir :blueprint-dir :parse parse/parse-blueprint :k :blueprint :kind :world.blueprint-unreadable
                :what :blueprint}})

(defn sync-kind
  "[entries warns] for one kind of file, re-reading only what changed."
  [entries dir {:keys [parse k]} warn!]
  (let [now-stamps (fsync/stamps dir)]
    (reduce (fn [[entries warns] id]
              (let [text (try (.readFileSync fs (path/join dir (str id ".edn")) "utf8") (catch :default e {:error (ex-message e)}))]
                (if (map? text)
                  (let [[entries warn] (fsync/absorb entries id (get now-stamps id) {:errors [(str "unreadable file: " (:error text))]})]
                    [entries (cond-> warns warn (conj warn))])
                  [(assoc entries id (lazy-entry (get entries id) id (get now-stamps id) text parse k warn!)) warns])))
            [(fsync/drop-gone entries now-stamps) []]
            (fsync/stale-ids entries now-stamps))))

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
    :else (let [[entries warn] (fsync/absorb {:zones (dissoc entry :missing)} :zones stamp (parsed))]
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
    :else (let [[entries warn] (fsync/absorb {:area-claims entry} :area-claims stamp (parsed))]
            [(:area-claims entries) warn])))

(defn marker-problems [i m]
  (cond
    (not (map? m)) [(str "marker " i ": not an object")]
    (not (and (string? (:name m)) (seq (:name m)))) [(str "marker " i ": :name must be a non-empty string")]
    (not-every? #(js/Number.isFinite (get m %)) [:x :y :z]) [(str "marker " (:name m) ": :x :y :z must be numbers")]))

(defn parse-markers
  "Text of the markers file (a JSON array of objects with name, x, y, z) -> {:value [marker ..]} (keyword keys) or
  {:errors [text ..]}; never throws."
  [text]
  (let [read (try {:value (js->clj (js/JSON.parse text) :keywordize-keys true)}
                  (catch :default e {:error (str "unreadable JSON: " (ex-message e))}))
        markers (:value read)]
    (cond
      (:error read) {:errors [(:error read)]}
      (not (vector? markers)) {:errors ["the markers file must hold an array of markers"]}
      :else (let [errors (vec (keep-indexed marker-problems markers))]
              (if (seq errors) {:errors errors} {:value markers})))))

(defn read-markers
  "The markers file parsed into {:value markers} or {:errors [..]}; an unreadable file is an error."
  [file]
  (let [text (try (.readFileSync fs file "utf8") (catch :default e {:error (ex-message e)}))]
    (if (map? text)
      {:errors [(str "unreadable file: " (:error text))]}
      (parse-markers text))))

(defn absorb-markers
  "Fold the markers file's stamp (nil: no file) into its entry: [entry warn]. No file is no markers; a bad file
  keeps the last good copy and warns {:id :error :kept} like absorb. parsed is a thunk, called only when the stamp changed."
  [entry stamp parsed]
  (cond
    (nil? stamp) [{:value []} nil]
    (= stamp (:stamp entry)) [entry nil]
    :else (let [[entries warn] (fsync/absorb {:markers entry} :markers stamp (parsed))]
            [(:markers entries) warn])))

(defn markers-warn-event [file {:keys [error kept]}]
  {:source :system :kind :world.markers-unreadable :level :warn :path file :error error :kept kept
   :text (str "markers file " file " cannot be read (" error ")"
              (if kept "; the last good copy is still used" "; it was never readable, no markers are known"))})

(defn claims-warn-event [file {:keys [error kept]}]
  {:source :system :kind :world.claims-unreadable :level :warn :path file :error error :kept kept
   :text (str "claims file " file " cannot be read (" error ")"
              (if kept "; the last good copy is still used" "; it was never readable, no claims are known"))})

(defn zones-warn-event [file {:keys [missing error kept]}]
  (if missing
    {:source :system :kind :world.zones-missing :level :warn :path file
     :text (str "zone file " file " is missing; dig, place and take jobs decline until it is there unless given :ignore-zones? true")}
    {:source :system :kind :world.zones-unreadable :level :warn :path file :error error :kept kept
     :text (str "zone file " file " cannot be read (" error ")"
                (if kept "; the last good copy is still used" "; it was never readable, dig and place jobs decline"))}))

(defn warn-event [{:keys [kind what]} {:keys [id error kept]}]
  {:source :system :kind kind :level :warn what id :error error :kept kept
   :text (str (name what) " " id " cannot be read (" error ")"
              (if kept "; the last good copy is still used" "; it was never readable"))})

(defn reloaded-event
  "The :debug event for zone or markers files whose stamp changed (so they were read again), or nil. mtimes {file
  mtime-ms} is the modification time of the copy read: a writer waits for the one its own edit made."
  [files mtimes]
  (when (seq files)
    {:source :system :kind :world.reloaded :level :debug :files (vec files) :mtimes mtimes
     :text (str "re-read " (str/join ", " files))}))

(defn changed-files
  "Paths in dir of the entries whose stamp differs between old and new (read again)."
  [dir old new]
  (vec (for [[id {:keys [stamp]}] (sort-by key new)
             :when (not= stamp (:stamp (get old id)))]
         (path/join dir (str id ".edn")))))

(declare warm!)

(defn refresh!
  "Stat the files when due (or at once with force?) and fold in what changed; emits a warn per newly broken file. A world
  without files (of-data) is never refreshed."
  [{:keys [state opts]} & [force?]]
  (when (:plans-dir opts)
    (let [now ((:now opts))]
      (when (or force? (fsync/due? (assoc @state :every-ms (:every-ms opts)) now))
        (let [old @state
              results (into {} (map (fn [[kind spec]] [kind (sync-kind (get old kind) (get opts (:dir spec)) spec #((:emit opts) (warn-event spec %)))]))
                            kinds)
              plans (first (:plans results))
              blueprints (first (:blueprints results))
              file (:zones-file opts)
              [zone-entry zone-warn] (if file
                                       (absorb-zones (:zones old) (file-stamp file) #(read-zones file))
                                       [(:zones old) nil])
              markers-file (or (:markers-file opts) (some-> file (path/join ".." "places.json") path/normalize))
              [markers-entry markers-warn] (if markers-file
                                             (absorb-markers (:markers old) (file-stamp markers-file) #(read-markers markers-file))
                                             [(:markers old) nil])
              claims-file (:claims-file opts)
              [claims-entry claims-warn] (if claims-file
                                           (absorb-claims (:area-claims old) (file-stamp claims-file) #(read-claims claims-file))
                                           [(:area-claims old) nil])]
          (reset! state (cond-> (assoc old :checked-at now :plans plans :blueprints blueprints :zones zone-entry
                                       :area-claims claims-entry :markers markers-entry)
                          (or (not= (:plans old) plans) (not= (:blueprints old) blueprints)) (expand-all old)))
          (when (and (:warm? opts) (not= (:footprints old) (:footprints @state)))
            (js/setTimeout #(warm! {:state state :opts opts}) 0))
          (doseq [[kind [_ warns]] results
                  warn warns]
            ((:emit opts) (warn-event (get kinds kind) warn)))
          (when-let [ev (reloaded-event (cond-> (changed-files (:plans-dir opts) (:plans old) plans)
                                          (and file (not= (:stamp zone-entry) (:stamp (:zones old))) (:stamp zone-entry)) (conj file)
                                          (and markers-file (not= (:stamp markers-entry) (:stamp (:markers old))) (:stamp markers-entry)) (conj markers-file))
                                        (into {} (keep (fn [[f e]] (when-let [[mtime] (and f (:stamp e))] [f mtime])))
                                              [[file zone-entry] [markers-file markers-entry]]))]
            ((:emit opts) ev))
          (when zone-warn
            ((:emit opts) (zones-warn-event file zone-warn)))
          (when markers-warn
            ((:emit opts) (markers-warn-event markers-file markers-warn)))
          (when claims-warn
            ((:emit opts) (claims-warn-event claims-file claims-warn))))))))

;; ------------------------------------------------------------------ worlds

(defn warm!
  "Build the plans' expansions, claims and footprint index now (a real body's world does it after each change of the plans,
  so the first zone check in a danger decision reads them built)."
  [{:keys [state]}]
  (some-> (:footprints @state) deref)
  nil)

(defn open
  "A world over the files of :plans-dir and :blueprint-dir and the zone file :zones-file. opts: :now (ms clock),
  :emit (an event fn), :every-ms. Without :now (a real body) it reads the files right after the open returns (the engine exists by then to take the events) and again every :every-ms by
  a timer, so a file written at any time is logged as world.reloaded even when no job reads the world (close! stops it)."
  [{:keys [now every-ms] :as opts}]
  (let [w {:state (atom {:plans {} :blueprints {}})
           :said (atom #{})
           :derived (atom {})
           :opts (assoc opts :now (or now js/Date.now) :every-ms (or every-ms (default-every-ms)) :warm? (not now))}]
    (if (or now (not (:plans-dir opts)))
      w
      (do (js/setTimeout #(refresh! w) 0)
          (assoc w :timer (doto (js/setInterval #(refresh! w) (get-in w [:opts :every-ms])) (.unref)))))))

(defn close!
  "Stops the world's watch (see open)."
  [w]
  (some-> (:timer w) js/clearInterval))

(defn data-state [plans blueprints]
  (expand-all {:plans (update-vals plans (fn [p] {:value p}))
               :blueprints (update-vals blueprints (fn [b] {:value b}))}))

(defn zone-entry [zones] (if (some? zones) {:value zones} {:missing true}))

(defn of-data
  "A world over plans {id plan}, blueprints {id blueprint} and zones (default [], none; nil: never read) held in
  memory, no files (tests)."
  ([plans blueprints] (of-data plans blueprints []))
  ([plans blueprints zones]
   {:state (atom (assoc (data-state plans blueprints) :zones (zone-entry zones) :area-claims {:value []} :markers {:value []})) :said (atom #{}) :derived (atom {})
    :opts {}}))

(defn blank
  "A world with no plans, blueprints, zones or claims (the engine's when none is given)."
  []
  (of-data {} {}))

(defn set-data!
  "Replace the plans and blueprints of a world made with of-data; its zones stay."
  [w plans blueprints]
  (swap! (:state w) #(assoc (data-state plans blueprints) :zones (:zones %) :area-claims (:area-claims %) :markers (:markers %))))

(defn set-zones!
  "Replace the zones of a world made with of-data (nil: never read)."
  [w zones]
  (swap! (:state w) assoc :zones (zone-entry zones)))

(defn set-area-claims!
  "Replace the claims of a world made with of-data."
  [w claims]
  (swap! (:state w) assoc :area-claims {:value claims}))

(defn set-markers!
  "Replace the markers of a world made with of-data."
  [w markers]
  (swap! (:state w) assoc :markers {:value markers}))

(defn plan
  "The answer for plan id (see answer); nil for a nil world."
  [w id]
  (when w
    (let [file (some-> (get-in w [:opts :plans-dir]) (path/join (str id ".edn")))]
      (refresh! w (and file (not= (file-stamp file) (get-in @(:state w) [:plans id :stamp])))))
    (answer @(:state w) id)))

(defn first-time!
  "True the first time key is given to this world, false after: for warns said once per body process."
  [w key]
  (let [[old _] (swap-vals! (:said w) conj key)]
    (not (contains? old key))))

(defn mentions?
  "Whether plan id, or a blueprint it places, names a block containing one of needles (text): false means the plan has no cell
  wanting such a block, so it need not be expanded."
  [state id needles]
  (let [file-text (get-in state [:plans id :text])]
    (and (not (and file-text (not (str/includes? file-text "blueprint")) (not-any? #(str/includes? file-text %) needles)))
         (let [plan (value (get-in state [:plans id]))
               text (str (pr-str plan)
                         (apply str (for [part (:parts plan)
                                          :let [bp (value (get-in state [:blueprints (:blueprint part)]))]
                                          :when bp]
                                      (pr-str (:key bp)))))]
           (boolean (some #(str/includes? text %) needles))))))

(defn answers
  "The answers (see answer) of every plan in state, by id; with needles only of the plans that mention one (see mentions?)."
  ([state] (answers state nil))
  ([state needles]
   (keep #(when (or (nil? needles) (mentions? state % needles)) (answer state %))
         (sort (keys (:plans state))))))

(defn derived
  "(f answers) over the answers of all plans, kept under key k until a plan or blueprint changes: for a
  reader asked every tick (a trigger) that must not rebuild its index per tick. nil for a nil world. With needles (block
  name parts), only the plans that mention one are answered (and expanded): f must ignore the cells of the others."
  ([w k f] (derived w k f nil))
  ([w k f needles]
  (when w
    (refresh! w)
    (let [{:keys [plans expanded] :as state} @(:state w)
          kept (get @(:derived w) k)]
      (cond
        (and kept (identical? plans (:plans kept)) (identical? expanded (:expanded kept))) (:value kept)
        (and kept (= plans (:plans kept)) (= expanded (:expanded kept)))
        (do (swap! (:derived w) assoc k (assoc kept :plans plans :expanded expanded)) (:value kept))
        :else (let [value (f (answers state needles))]
                (swap! (:derived w) assoc k {:plans plans :expanded expanded :value value})
                value))))))

(defn memo
  "(f) kept under key k until the plans or claims change (f must read only those and k's own inputs): for readers asked
  many times per round. (f) is called without a world's state when w is nil."
  [w k f]
  (if-not w
    (f)
    (do (refresh! w)
        (let [{:keys [plans claims]} @(:state w)
              kept (get @(:derived w) k)]
          (cond
            (and kept (identical? plans (:plans kept)) (identical? claims (:claims kept))) (:value kept)
            (and kept (= plans (:plans kept)) (= claims (:claims kept)))
            (do (swap! (:derived w) assoc k (assoc kept :plans plans :claims claims)) (:value kept))
            :else (let [value (f)
                        current? (fn [e] (and (= plans (:plans e)) (= claims (:claims e))))]
                    (swap! (:derived w)
                           (fn [d] (assoc (into {} (remove (fn [[_ e]] (and (contains? e :claims) (not (current? e))))) d)
                                          k {:plans plans :claims claims :value value})))
                    value))))))

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
            (memo w [:footprints-except except] #(footprint-index (update-vals (dissoc (:claims @(:state w)) except) deref)))
            (if footprints @footprints {}))))))

(defn soil-footprints
  "{[x y z] plan-id}: the soil under each crop cell of the plans but except's (see soil-cells); {} for a nil world."
  [w except]
  (if-not w
    {}
    (do (refresh! w)
        (memo w [:soil-footprints except]
              #(footprint-index (into {} (keep (fn [[id x]] (when (not= id except) [id (soil-cells (some-> x deref))]))) (:expanded @(:state w))))))))

(defn shaft-ids
  "The ids of the plans of :kind :mineshaft, as a set; #{} for a nil world."
  [w]
  (if-not w
    #{}
    (do (refresh! w)
        (memo w :shaft-ids #(into #{} (keep (fn [[id e]] (when (= :mineshaft (:kind (value-if e ":mineshaft"))) id))) (:plans @(:state w)))))))

(defn crop-cells
  "The set of [x y z] cells every plan wants a crop on (a want map with :crop); #{} for a nil world."
  [w]
  (if-not w
    #{}
    (do (refresh! w)
        (memo w :crop-cells
              #(into #{} (comp (keep (fn [[_ x]] (some-> x deref)))
                               (mapcat :cells)
                               (filter (fn [{:keys [want]}] (and (map? want) (contains? want :crop))))
                               (map (comp vec :pos)))
                     (:expanded @(:state w)))))))

(defn plan-authors
  "{plan-id by}: the :metadata :by (the body that made the plan) of each plan that names one; {} for a nil world."
  [w]
  (if-not w
    {}
    (do (refresh! w)
        (memo w :plan-authors #(into {} (keep (fn [[id e]] (when-let [by (shape/author (value-if e ":by"))] [id by]))) (:plans @(:state w)))))))

(defn area-claims
  "The world's area claims as read from claims.edn ([] when there is no file or no world; the last good copy when it is
  bad). Not filtered by status or expiry: see live-claims."
  [w]
  (if-not w
    []
    (do (refresh! w)
        (or (get-in @(:state w) [:area-claims :value]) []))))

(defn markers
  "The shared markers of places.json as read ([] when there is no file or no world; the last good copy when it is bad).
  Bodies only read them; the agent tools write."
  [w]
  (if-not w
    []
    (do (refresh! w)
        (or (get-in @(:state w) [:markers :value]) []))))

(defn marker
  "The marker called name (text), or nil."
  [w name]
  (first (filter #(= name (:name %)) (markers w))))

(defn default-marker-limit [] (settings/get settings ::default-marker-limit))
(defn max-marker-limit [] (settings/get settings ::max-marker-limit))

(defn find-markers
  "A bounded search of markers (a vector of maps, one pass, no file read): {:text :kind :near {:x :y :z} :limit}. :text matches
  name, kind or note as a case-insensitive substring, :kind the kind exactly (case-insensitive). With :near the nearest come
  first (by distance in x z and y; a :near without numeric x y z is ignored). At most :limit (default 10, 50 at most) are
  returned. Marker coordinates are as given (no dimension)."
  [markers {:keys [text kind near limit]}]
  (let [low #(some-> % str str/lower-case)
        t (low text)
        k (low kind)
        has? (fn [m] (some #(some-> (low %) (str/includes? t)) [(:name m) (:kind m) (:note m)]))
        dist (fn [m] (let [dx (- (:x m) (:x near)) dy (- (:y m) (:y near)) dz (- (:z m) (:z near))]
                       (+ (* dx dx) (* dy dy) (* dz dz))))
        near (when (every? #(number? (% near)) [:x :y :z]) near)
        hits (filter #(and (or (nil? t) (has? %)) (or (nil? k) (= k (low (:kind %))))) markers)]
    (vec (take (min (max-marker-limit) (or limit (default-marker-limit))) (if near (sort-by dist hits) hits)))))

(defn live-claims
  "The claims that are :active (keyword or text) and whose :until is after now (ms)."
  [claims now]
  (filterv #(and (= "active" (some-> (:status %) name)) (> (:until %) now)) claims))
