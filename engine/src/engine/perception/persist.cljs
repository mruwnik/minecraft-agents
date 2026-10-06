(ns engine.perception.persist
  "Block memory to and from a file (io: engine/js/seen-file.mjs)."
  (:require [engine.perception.store :refer [evict-oldest! section-key store-of]]))

(defn all-sections
  "Every remembered section, least recently seen first, as the file module takes them."
  [^js st]
  (->> (for [[dim ^js m] (es6-iterator-seq (.entries (.-stores st)))
             ^js sec (es6-iterator-seq (.values m))]
         #js {:dim dim :cx (.-cx sec) :sy (.-sy sec) :cz (.-cz sec) :seen (.-seen sec) :base (.-base sec)
              :ids (.-ids sec) :times (.-times sec)})
       (sort-by #(.-seen ^js %))
       to-array))

(defn save!
  "Writes memory to file. Returns a promise."
  [{:keys [raw st]} ^js io file]
  (.saveSeen io file #js {:version (.version ^js raw) :sections (all-sections st)}))

(defn load!
  "Reads memory from file, unless it is missing, damaged or from another game version. Returns the sections loaded."
  [{:keys [raw st]} ^js io file]
  (let [^js st st ^js data (.loadSeen io file)]
    (if-not (and data (= (.-version data) (.version ^js raw)))
      0
      (do (doseq [^js s (.-sections data)]
            (while (>= (.-count st) (.-cap st)) (evict-oldest! st))
            (let [store (store-of st (.-dim s))
                  key (section-key (.-cx s) (.-sy s) (.-cz s))]
              (when-not (.has store key) (set! (.-count st) (inc (.-count st))))
              (.set store key #js {:ids (.-ids s) :times (.-times s) :base (.-base s) :seen (.-seen s) :stamp -1 :cx (.-cx s) :sy (.-sy s) :cz (.-cz s)})))
          (set! (.-lastKey st) -1)
          (count (.-sections data))))))
