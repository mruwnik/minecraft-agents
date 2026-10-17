(ns engine.single-child
  "The child process of engine.single-test's cross-process lock tests. test-support/lean-child.cjs loads only this and
  engine.single, not the whole test bundle, so a child starts fast."
  (:require [engine.single :as single]
            ["fs" :as fs]))

(def child-env "SINGLE_LOCK_CHILD")

(defn sleep [ms] (js/Promise. (fn [r] (js/setTimeout r ms))))

(defn ^:async inside-alone?
  "Hold the lock's inside for ms behind marker; false when another holder's marker was already there."
  [marker ms]
  (if (try (fs/mkdirSync marker) true (catch :default e (if (= "EEXIST" (.-code e)) false (throw e))))
    (do (await (sleep ms)) (fs/rmdirSync marker) true)
    false))

(defn stdin-closed [] (js/Promise. (fn [r] (.on js/process.stdin "end" r) (.resume js/process.stdin))))

(defn ^:async run-child
  "One child's part: :loop takes the lock iterations times (dying inside on the crash-at'th), :hold keeps it until
  stdin closes, :die dies inside it."
  [{:keys [mode sock marker iterations crash-at hold-ms]}]
  (case mode
    "loop" (do (loop [i 0]
                 (when (< i iterations)
                   (await (single/with-replace-lock
                            sock (fn ^:async inside []
                                   (when-not (await (inside-alone? marker hold-ms)) (println "LOCK:overlap"))
                                   (when (= i crash-at) (.kill js/process js/process.pid "SIGKILL")))))
                   (recur (inc i))))
               (println "LOCK:done"))
    "hold" (await (single/with-replace-lock
                    sock (fn ^:async inside []
                           (fs/mkdirSync marker)
                           (println "LOCK:in")
                           (await (stdin-closed))
                           (fs/rmdirSync marker))))
    "die" (await (single/with-replace-lock
                   sock (fn [] (println "LOCK:in") (.kill js/process js/process.pid "SIGKILL"))))))

(defn main []
  (enable-console-print!)
  (run-child (js->clj (js/JSON.parse (aget js/process.env child-env)) :keywordize-keys true)))
