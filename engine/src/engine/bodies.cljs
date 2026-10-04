(ns engine.bodies
  "Where a body's folder is: worlds/<world>/agents/<name>. A name is unique only within a world, so the world
  is always given; there is no search over worlds and no default. A real account's login cache is per account, shared
  by every world: worlds/.accounts/<name>. The JS twin is engine/js/bodies.mjs; keep the two alike."
  (:require ["fs" :as fs]
            ["path" :as path]))

(def name-re #"^[A-Za-z0-9_-]{1,64}$")

(defn- checked [what value]
  (if (and (string? value) (re-matches name-re value))
    value
    (throw (js/Error. (str "a body folder needs a " what " of letters, digits, _ and -, got " (pr-str value))))))

(defn missing-world-error [flag]
  (str "missing " flag " <world>: the world the body plays in (a folder under worlds/)"))

(defn storage-root
  "Canonical worlds directory, or an explicit legacy state parent. Both flags are mutually exclusive."
  [{:keys [state worlds]} repo-root]
  (when (and state worlds) (throw (js/Error. "choose --worlds or legacy --state, not both")))
  (if state
    (path/resolve state)
    {:worldsDir (path/resolve (or worlds (path/join repo-root "worlds")))}))

(defn worlds-dir [state-dir]
  (if (map? state-dir) (:worldsDir state-dir)
      (if (and state-dir (.-worldsDir state-dir)) (.-worldsDir state-dir)
          (path/join state-dir "worlds"))))

(defn body-dir [state-dir world name]
  (path/join (worlds-dir state-dir) (checked "world" world) "agents" (checked "name" name)))

(defn account-dir [state-dir name]
  (path/join (if (string? state-dir) (path/join state-dir "accounts")
                (path/join (worlds-dir state-dir) ".accounts")) (checked "name" name)))

(defn world-of-body-dir
  "The world of a body folder: the name of the folder two levels up."
  [dir]
  (path/basename (path/dirname (path/dirname (path/resolve dir)))))

(defn- dir-names [dir]
  (if-not (fs/existsSync dir)
    []
    (->> (fs/readdirSync dir #js {:withFileTypes true})
         (filter #(and (.isDirectory %) (re-matches name-re (.-name %))))
         (map #(.-name %))
         sort)))

(defn list-bodies
  "Every body folder, of one world or of all: [{:world :name :dir}] sorted by world then name."
  ([state-dir] (vec (mapcat #(list-bodies state-dir %) (dir-names (worlds-dir state-dir)))))
  ([state-dir world]
   (mapv (fn [n] {:world world :name n :dir (body-dir state-dir world n)})
         (dir-names (path/join (worlds-dir state-dir) world "agents")))))
