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
    A re-read zones.edn or places.json emits a :debug world.reloaded naming the files.
    The files are stat-ed again at most every :every-ms, lazily, by the first read after that.
    Only files whose stamp (modification time and size) changed are read and parsed again.
    A file that turns unreadable or invalid keeps its last good copy and warns once
    (world.plan-unreadable, world.blueprint-unreadable, world.zones-unreadable, world.claims-unreadable).
    A file that was never readable answers as broken, not as absent.
  Zones differ: a missing file answers nil (warned once by world.zones-missing, each time it goes), so a dig or
  place job declines. An empty vector is a valid \"no zones\".
  Claims differ: a missing file is [] (no claims, no warn). So are markers (world.markers-unreadable on a bad file).

  The state: {:plans {id entry} :blueprints {id entry} :zones entry :area-claims entry :expanded {id expansion}
  :claims {id #{cell}} :footprints {cell id} :checked-at ms}.
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
  (let [now-stamps (fsync/stamps dir)]
    (reduce (fn [[entries warns] id]
              (let [[entries warn] (fsync/absorb entries id (get now-stamps id) (read-parsed dir id parse k))]
                [entries (cond-> warns warn (conj warn))]))
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
  "The :debug event for zone or markers files whose stamp changed (so they were read again), or nil."
  [files]
  (when (seq files)
    {:source :system :kind :world.reloaded :level :debug :files (vec files)
     :text (str "re-read " (str/join ", " files))}))

(defn refresh!
  "Stat the files when due and fold in what changed; emits a warn per newly broken file. A world without
  files (of-data) is never refreshed."
  [{:keys [state opts]}]
  (when (:plans-dir opts)
    (let [now ((:now opts))]
      (when (fsync/due? (assoc @state :every-ms (:every-ms opts)) now)
        (let [old @state
              results (into {} (map (fn [[kind spec]] [kind (sync-kind (get old kind) (get opts (:dir spec)) spec)]))
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
                          (or (not= (:plans old) plans) (not= (:blueprints old) blueprints)) expand-all))
          (doseq [[kind [_ warns]] results
                  warn warns]
            ((:emit opts) (warn-event (get kinds kind) warn)))
          (when-let [ev (reloaded-event (cond-> []
                                          (and file (not= (:stamp zone-entry) (:stamp (:zones old))) (:stamp zone-entry)) (conj file)
                                          (and markers-file (not= (:stamp markers-entry) (:stamp (:markers old))) (:stamp markers-entry)) (conj markers-file)))]
            ((:emit opts) ev))
          (when zone-warn
            ((:emit opts) (zones-warn-event file zone-warn)))
          (when markers-warn
            ((:emit opts) (markers-warn-event markers-file markers-warn)))
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
   :opts (assoc opts :now (or now js/Date.now) :every-ms (or every-ms (default-every-ms)))})

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

(defn plan-authors
  "{plan-id by}: the :metadata :by (the body that made the plan) of each plan that names one; {} for a nil world."
  [w]
  (if-not w
    {}
    (do (refresh! w)
        (into {} (keep (fn [[id e]] (when-let [by (shape/author (:value e))] [id by]))) (:plans @(:state w))))))

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
