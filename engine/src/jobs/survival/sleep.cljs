(ns jobs.survival.sleep
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.memory :as mem]))

(def doc
  "Walk to the known :bed place and sleep. Done once asleep, or when it turns
  out not to be night. A taken bed or a monster nearby is retried three
  times; a missing bed warns and ends.")

(defn bed-of [c]
  (mem/place (ctx/view c) :bed))

(defn check
  "A bed is known and it is night."
  [c]
  (and (some? (bed-of c))
       (not (.-isDay (.self (:primitives c))))))

(defn ^:async round [c]
  (let [bed (bed-of c)
        w (if bed (await (u/walk-near! c bed 2)) :blocked)]
    (case w
      :partial :continue
      :blocked (u/fail! c :bed_unreachable "cannot reach the bed")
      (let [r (await (ctx/act c :sleep (clj->js {:pos bed})))]
        (case (.-status r)
          ("sleeping" "not-night") :done
          "missing" (do (ctx/emit! c :bed_missing :warn {:pos bed :text "no bed at the remembered place"})
                        :done)
          (u/fail! c :bed_unusable (str "cannot sleep: " (.-status r))))))))
