(ns dashboard.blueprint-library
  "Canonical blueprint EDN sources and bounded, native previews. No legacy build parser or allocation."
  (:require ["fs" :as fs]
            ["path" :as path]
            ["crypto" :as crypto]
            [dashboard.edn :as edn]
            [clojure.string :as str]
            [plan.parse :as parse]
            [plan.shape :as shape]))

(def max-source-bytes (* 2 1024 1024))
(def max-preview-cells 100000)
(def max-library-files 1000)
(def max-library-bytes (* 32 1024 1024))
(def unit [[0 0 0 1 1 1]])

(defn source-hash [source]
  (.digest (.update (.createHash crypto "sha256") source) "hex"))

(defn state-values [want]
  (into {} (map (fn [[k v]] [k (shape/state-text v)])) (when (map? want) (dissoc want :block))))

(defn display-shapes
  "Schematic block geometry, independent of Minecraft collision data."
  [block states]
  (cond
    (str/ends-with? block "_slab") (case (:type states) "top" [[0 0.5 0 1 1 1]] "double" unit [[0 0 0 1 0.5 1]])
    (str/ends-with? block "_stairs")
    (let [top? (= "top" (:half states))
          [a b] (if top? [0 0.5] [0.5 1])
          base (if top? [0 0.5 0 1 1 1] [0 0 0 1 0.5 1])
          tread (case (:facing states)
                  "north" [0 a 0 1 b 0.5]
                  "south" [0 a 0.5 1 b 1]
                  "west" [0 a 0 0.5 b 1]
                  [0.5 a 0 1 b 1])]
      [base tread])
    (str/ends-with? block "_carpet") [[0 0 0 1 0.0625 1]]
    (str/ends-with? block "_bed") [[0 0 0 1 0.5625 1]]
    (str/ends-with? block "_pane") [[0.4375 0 0 0.5625 1 1] [0 0 0.4375 1 1 0.5625]]
    (str/ends-with? block "_fence") [[0.375 0 0.375 0.625 1 0.625]]
    (str/ends-with? block "_door") (if (#{"east" "west"} (:facing states)) [[0 0 0 0.1875 1 1]] [[0 0 0 1 1 0.1875]])
    (#{"torch" "wall_torch" "soul_torch" "soul_wall_torch"} block) [[0.4375 0 0.4375 0.5625 0.625 0.5625]]
    :else unit))

(defn exact-block [want]
  (cond (string? want) want (map? want) (:block want)))

(defn bill [bp]
  (let [by-y (group-by #(second (:offset %)) (shape/blueprint-cells bp))
        layers (mapv (fn [y]
                       {:y y :items (frequencies (keep #(let [block (exact-block (:want %))]
                                                         (when (and block (not (shape/air? block))) block)) (get by-y y)))})
                     (range (count (:layers bp))))]
    {:total (apply merge-with + {} (map :items layers)) :layers layers :tools []}))

(defn preview-cells [bp]
  (vec (keep (fn [{:keys [offset want]}]
               (let [block (exact-block want) states (state-values want) [x y z] offset]
                 (when (and block (not (shape/air? block)))
                   {:x x :y y :z z :name block :states states :shapes (display-shapes block states)})))
             (shape/blueprint-cells bp))))

(defn invalid-detail [source id errors]
  {:name id :source source :hash (source-hash source) :bp nil :bill nil :preview []
   :lint {:errors [] :warnings []} :errors (vec errors)
   :validation-errors (mapv #(hash-map :error %) errors) :builds [] :palette "native-edn"})

(defn checked-detail [source id remaining-cells]
  (if-not (string? source)
    (invalid-detail "" id ["blueprint source must be EDN text"])
    (if (> (.byteLength js/Buffer source "utf8") max-source-bytes)
      (invalid-detail "" id ["blueprint source exceeds 2 MiB"])
      (let [_ (edn/one-form source)
            {:keys [blueprint errors]} (parse/parse-blueprint source id)
            [w d] (when blueprint (shape/layer-size blueprint))
            cells (when blueprint (* w d (count (:layers blueprint))))]
        (cond
          (not (shape/part-id? id)) (invalid-detail source id ["blueprint needs a valid :id"])
          (seq errors) (invalid-detail source id errors)
          (> cells max-preview-cells) (invalid-detail source id ["blueprint preview exceeds 100000 cells"])
          (> cells remaining-cells) (invalid-detail source id ["aggregate blueprint library preview exceeds 100000 cells"])
          :else {:name id :source source :hash (source-hash source) :bp blueprint
                 :bill (bill blueprint) :preview (preview-cells blueprint)
                 :errors [] :validation-errors [] :lint {:errors [] :warnings []}
                 :builds [] :palette "native-edn"})))))

(defn detail
  ([source id] (detail source id max-preview-cells))
  ([source id remaining-cells]
   (try (checked-detail source id remaining-cells)
        (catch :default e
          (invalid-detail (if (string? source) source "") id
                          [(str "invalid blueprint EDN: " (ex-message e))])))))

(defn stock-errors [stock]
  (when-not (and (map? stock) (every? string? (keys stock))
                 (every? #(and (integer? %) (<= 0 %)) (vals stock)))
    ["declared stock must be an EDN map of block names to nonnegative integer counts"]))

(defn preview [source stock-source]
  (try
    (let [_ (when (or (not (string? source)) (> (.byteLength js/Buffer source "utf8") max-source-bytes))
              (throw (js/Error. (if (string? source) "blueprint source exceeds 2 MiB" "blueprint source must be EDN text"))))
          document (edn/one-form source)
          result (detail source (:id document))]
      (if (or (seq (:errors result)) (str/blank? stock-source))
        result
        (let [stock (edn/one-form stock-source) errors (stock-errors stock)]
          (if (seq errors)
            (invalid-detail source (:id document) errors)
            (assoc result :stock {:basis "block-names" :counts stock
                                 :shortage (into {} (keep (fn [[block n]]
                                                           (let [missing (max 0 (- n (get stock block 0)))]
                                                             (when (pos? missing) [block missing]))))
                                                (get-in result [:bill :total]))})))))
    (catch :default e (invalid-detail (if (string? source) source "") nil [(str "unreadable EDN: " (ex-message e))]))))

(defn library [dir]
  (let [files (try (->> (.readdirSync fs dir) (filter #(str/ends-with? % ".edn")) sort vec)
                   (catch :default e (if (= "ENOENT" (.-code e)) [] (throw e))))]
    (when (> (count files) max-library-files)
      (throw (js/Error. "blueprint library exceeds 1000 EDN files")))
    (let [remaining (atom max-preview-cells) bytes (atom 0)
          details (mapv (fn [file]
                          (let [id (str/replace file #"\.edn$" "") full-path (.join path dir file)]
                            (try
                              (let [stat (.lstatSync fs full-path) size (.-size stat)]
                                (cond
                                  (not (.isFile stat)) (invalid-detail "" id ["blueprint is not a regular file"])
                                  (> size max-source-bytes) (invalid-detail "" id ["blueprint source exceeds 2 MiB"])
                                  (> (+ @bytes size) max-library-bytes) (invalid-detail "" id ["aggregate blueprint library source exceeds 32 MiB"])
                                  :else (do (swap! bytes + size)
                                            (let [result (detail (.readFileSync fs full-path "utf8") id @remaining)]
                                              (when-let [bp (:bp result)]
                                                (let [[w d] (shape/layer-size bp)]
                                                  (swap! remaining - (* w d (count (:layers bp))))))
                                              result))))
                              (catch :default e (invalid-detail "" id [(str "cannot read blueprint: " (ex-message e))])))))
                        files)]
      {:blueprints details :errors (mapv #(select-keys % [:name :errors]) (filter #(seq (:errors %)) details))})))
