(ns dashboard.edn-data
  "Bounded, one-form EDN reads for shared dashboard snapshots. No JSON fallback."
  (:require ["fs" :as fs]
            [dashboard.edn :as edn]))

(def max-bytes (* 8 1024 1024))
(def max-files 1000)

(def one-form edn/one-form)

(defn read-text [file]
  (let [stat (.lstatSync fs file)]
    (when-not (.isFile stat) (throw (js/Error. (str "not a regular file: " file))))
    (when (> (.-size stat) max-bytes) (throw (js/Error. (str "file exceeds 8 MiB: " file))))
    (.readFileSync fs file "utf8")))

(defn read-file [file] (one-form (read-text file)))

(defn files [dir pattern]
  (let [names (if (.existsSync fs dir) (vec (sort (filter #(re-matches pattern %) (.readdirSync fs dir)))) [])]
    (when (> (count names) max-files) (throw (js/Error. (str "directory exceeds 1000 records: " dir))))
    names))
