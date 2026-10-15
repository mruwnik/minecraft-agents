(ns engine.registry-test-macros
  "Compile-time probes of engine.registry's file checks: the registry is JVM code, so a cljs test reads its answer
  from a macro."
  (:require [clojure.java.io :as io]
            [engine.registry :as registry]))

(defmacro describe-message
  "The message describe throws for a job file with source, or nil when it does not throw."
  [source]
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir") (str "registry-describe-" (System/nanoTime) "/jobs")) .mkdirs)
        file (io/file dir "probe.cljs")]
    (spit file source)
    (try
      (registry/describe dir file)
      nil
      (catch Exception e (ex-message e))
      (finally (io/delete-file file) (io/delete-file dir) (io/delete-file (.getParentFile dir))))))

(defmacro describes-job?
  "Whether describe takes the job file with source as a job (not a helper)."
  [source]
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir") (str "registry-describe-" (System/nanoTime) "/jobs")) .mkdirs)
        file (io/file dir "probe.cljs")]
    (spit file source)
    (try
      (some? (registry/describe dir file))
      (finally (io/delete-file file) (io/delete-file dir) (io/delete-file (.getParentFile dir))))))
