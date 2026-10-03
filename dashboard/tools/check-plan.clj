;; Checks plan and blueprint files with plan.shape on the JVM, no shadow-cljs compile:
;;   cd engine && clojure -Sdeps '{:paths ["src"]}' -M ../dashboard/tools/check-plan.clj <blueprint-dir> <plan.edn> ...
;; Prints every problem (blueprints first), then per valid plan its cell and spot counts. Exits 1 on any problem.
(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[plan.shape :as shape])

(defn file-id [f] (str/replace (.getName (io/file f)) #"\.edn$" ""))

(defn read-edn [f] (edn/read-string (slurp f)))

(defn check-blueprints [dir]
  (let [files (sort-by #(.getName %) (filter #(str/ends-with? (.getName %) ".edn") (or (.listFiles (io/file dir)) [])))]
    (for [f files :let [bp (read-edn f) id (file-id f)]]
      {:id id :blueprint bp :errors (shape/blueprint-errors bp id)})))

(defn check-plan [blueprints f]
  (let [plan (read-edn f)
        errors (shape/plan-errors plan (file-id f))]
    (if (seq errors)
      {:errors errors}
      (let [{:keys [cells spots] :as e} (shape/expand plan blueprints)]
        {:errors (:errors e) :cells (count cells) :spots (count spots)}))))

(let [[dir & plan-files] *command-line-args*
      bps (check-blueprints dir)
      blueprints (into {} (for [{:keys [id blueprint errors]} bps :when (empty? errors)] [id blueprint]))
      plans (for [f plan-files] (assoc (check-plan blueprints f) :file f))]
  (doseq [{:keys [id errors]} bps e errors] (println "blueprint" id ":" (:error e)))
  (doseq [{:keys [file errors cells spots]} plans]
    (doseq [e errors] (println file ":" (pr-str e)))
    (when cells (println file ":" cells "cells," spots "spots")))
  (System/exit (if (some (comp seq :errors) (concat bps plans)) 1 0)))
