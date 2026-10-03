(ns engine.events
  "The body's event stream: JSON lines on stdout and appended to a file."
  (:require [clojure.string :as str]
            ["fs" :as fs]
            ["path" :as path]))

(def levels {:debug 0 :info 1 :warn 2 :error 3})

(defn at-least? [floor level]
  (>= (levels level) (levels floor)))

(defn last-seq
  "The seq of the last event in file, or 0. Reads only the file's tail."
  [file]
  (if-not (and file (fs/existsSync file))
    0
    (let [size (.-size (fs/statSync file))
          n (min size 65536)
          buf (js/Buffer.alloc n)
          fd (fs/openSync file "r")]
      (fs/readSync fd buf 0 n (- size n))
      (fs/closeSync fd)
      (let [line (->> (str/split-lines (.toString buf "utf8")) (remove str/blank?) last)]
        (if line (.-seq (js/JSON.parse line)) 0)))))

(defn json-line [event]
  (str (js/JSON.stringify (clj->js event)) "\n"))

(defn file-sink [file]
  (fs/mkdirSync (path/dirname file) #js {:recursive true})
  (fn [event] (fs/appendFileSync file (json-line event))))

(defn stdout-sink [min-level]
  (fn [event]
    (when (at-least? min-level (:level event))
      (.write js/process.stdout (json-line event)))))

(defn make
  "An event stream. Options: :body, :file (appended JSON lines, also where the
  seq resumes from), :stdout? with :stdout-level, :sinks (extra fns of the
  event), :pos-fn (current position or nil), :now (ms clock)."
  [{:keys [body file stdout? stdout-level sinks pos-fn now]
    :or {stdout-level :debug now js/Date.now pos-fn (constantly nil)}}]
  (atom {:seq (last-seq file)
         :body body
         :pos-fn pos-fn
         :now now
         :sinks (cond-> (vec sinks)
                  file (conj (file-sink file))
                  stdout? (conj (stdout-sink stdout-level)))}))

(defn cell [p]
  (when p
    {:x (js/Math.floor (:x p)) :y (js/Math.floor (:y p)) :z (js/Math.floor (:z p))}))

(defn emit!
  "Send an event. `event` holds :source :kind :level plus any envelope or
  kind-specific fields; seq, t, body and pos are filled in. Returns the seq."
  [stream event]
  (let [{:keys [body pos-fn now sinks]} (swap! stream update :seq inc)
        n (:seq @stream)
        full (merge {:seq n :t (now) :body body}
                    (when-let [p (cell (pos-fn))] {:pos p})
                    (into {} (remove (comp nil? val) event)))]
    (doseq [sink sinks] (sink full))
    n))
