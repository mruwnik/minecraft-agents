(ns plan.drop-status-cli
  "node out/drop-plan-status.cjs <plans-dir> [--dry-run]: runs plan.drop-status over the <id>.edn files of one plans
  directory (worlds/<world>/plans): deletes the :retired plans, strips :status from the others in place (formatting
  kept) and prints the counts. A file that does not read as one map is left alone and named."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [plan.drop-status :as drop-status]
            ["fs" :as fs]
            ["path" :as path]))

(defn read-plan [file]
  (try (let [v (reader/read-string (fs/readFileSync file "utf8"))] (when (map? v) v))
       (catch :default _ nil)))

(defn edn-files [dir]
  (sort (filter #(str/ends-with? % ".edn") (vec (fs/readdirSync dir)))))

(defn stripped-text
  "The text of plan id's file without :status; throws when that would change more than the key."
  [dir plans id]
  (let [text (drop-status/strip-status-text (fs/readFileSync (path/join dir (str id ".edn")) "utf8"))]
    (when-not (= (dissoc (get plans id) :status) (reader/read-string text))
      (throw (ex-info (str "stripping " id " changed more than :status") {:id id})))
    text))

(defn migrate-dir!
  "Returns {:deleted [ids] :stripped [ids] :skipped [files]}; touches the files unless dry-run?. Every stripped text
  is computed and checked before the first delete or write, so a failure leaves the directory as it was."
  [dir dry-run?]
  (let [files (edn-files dir)
        read (into {} (map (fn [f] [f (read-plan (path/join dir f))])) files)
        skipped (vec (for [[f v] read :when (nil? v)] f))
        plans (into {} (for [[f v] read :when v] [(str/replace f #"\.edn$" "") v]))
        {:keys [deleted stripped]} (drop-status/migrate plans)
        texts (into {} (map (fn [id] [id (stripped-text dir plans id)])) stripped)]
    (when-not dry-run?
      (doseq [id deleted] (fs/unlinkSync (path/join dir (str id ".edn"))))
      (doseq [[id text] texts] (fs/writeFileSync (path/join dir (str id ".edn")) text)))
    {:deleted deleted :stripped stripped :skipped skipped}))

(defn main [& args]
  (let [dry-run? (some #{"--dry-run"} args)
        dir (first (remove #(str/starts-with? % "--") args))]
    (if-not dir
      (do (println "usage: node out/drop-plan-status.cjs <plans-dir> [--dry-run]") (js/process.exit 2))
      (let [{:keys [deleted stripped skipped]} (migrate-dir! dir dry-run?)]
        (println (str (if dry-run? "dry run: " "") "deleted " (count deleted) " " (pr-str deleted)
                      "; stripped " (count stripped) " " (pr-str stripped)
                      (when (seq skipped) (str "; skipped unreadable " (pr-str skipped)))))))))
