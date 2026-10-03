(ns dashboard.worlds)

(defn parse-json [text]
  (try
    (js->clj (js/JSON.parse text) :keywordize-keys true)
    (catch :default _ nil)))

;; world.json is {host, port}; a world whose file cannot be read is still listed, so it can be picked and seen empty
(defn parse-world-list [entries]
  (->> entries
       (map (fn [{:keys [name text]}]
              (let [info (parse-json text)]
                {:name name
                 :host (when (map? info) (:host info))
                 :port (when (map? info) (:port info))})))
       (sort-by :name)
       vec))

;; The only way a requested world name becomes a world: strict equality against the directory listing, never a path.
;; Nothing asked -> the first world (nil when there are none); anything not listed -> :unknown.
(defn resolve-world [names requested]
  (if (or (nil? requested) (= "" requested))
    (first names)
    (or (some #(when (= % requested) %) names) :unknown)))

(defn world-choice [names requested]
  (let [resolved (resolve-world names requested)]
    (if (= :unknown resolved)
      {:error (str "no world called " requested) :worlds (vec names)}
      {:name resolved})))

(defn scope-snapshot [snapshot world-name]
  (let [bodies (filterv #(= world-name (:world %)) (:bodies snapshot))
        mine? (fn [n] (some #(or (= n (:name %)) (= n (:username %))) bodies))]
    (assoc snapshot
           :agents (filterv mine? (:agents snapshot))
           :bodies bodies
           :worlds (filterv #(= world-name (:name %)) (:worlds snapshot)))))

(defn agent-in-world? [agents name world-name]
  (or (nil? world-name)
      (boolean (some #(and (= name (:name %)) (= world-name (:world %))) agents))))

;; JSON legend keys are single characters such as "~" or "#"; keywordized they
;; print as :~ and the browser's cljs.reader cannot read them back
(defn readable-place [place]
  (let [legend (get-in place [:structure :legend])]
    (if (map? legend)
      (assoc-in place [:structure :legend]
                (into {} (map (fn [[k v]] [(name k) v])) legend))
      place)))

(defn readable-places [places]
  (mapv readable-place places))
