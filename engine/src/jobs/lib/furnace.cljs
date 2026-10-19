(ns jobs.lib.furnace
  "What a smelt call's memory says about its furnace load, and putting a carried furnace down."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.places :as places]
            [jobs.lib.spot :as free-spot]))

(defn loaded?
  "Whether a smelt call's memory m holds a load put in the furnace."
  [m]
  (some? (:owed m)))

(defn loaded-count
  "The count a smelt call's memory m has put in its furnace and still owes while the cook is not due at now, else nil."
  [m now]
  (when (and (loaded? m) (some? (:ready-at m)) (< now (:ready-at m)))
    (let [{:keys [count base] :or {count 0 base 0}} (:owed m)]
      (- count base))))

(defn ^:async place-furnace!
  "Put the carried furnace down beside the body (jobs.blocks.place child; free-spot/spot! finds the cell) and make it the
  furnace of this job. :again once it stands, :continue while a child waits, nil when it could not be put down (memory
  :furnace-refusal says why)."
  [c]
  (let [spot (or (:furnace-spot (ctx/mem c)) (await (free-spot/spot! c)))]
    (cond
      (= :continue spot) :continue
      (nil? spot) (do (ctx/update-mem! c assoc :furnace-refusal "no free cell beside the body to put it on")
                      nil)
      :else
      (do (ctx/update-mem! c assoc :furnace-spot spot)
          (let [e (await (ctx/call c :place 'jobs.blocks.place {:item "furnace" :pos spot :fetch false}))]
            (if (= :continue e)
              :continue
              (do (ctx/update-mem! c dissoc :furnace-spot)
                  (if (and (= :done (:kind e)) (get-in e [:data :placed]))
                    (do (ctx/update-mem! c assoc :furnace spot)
                        (places/offer! c :furnace spot)
                        (ctx/emit! c :smelt.furnace :info {:furnace spot :text (str "put a furnace down at " (:x spot) " " (:y spot) " " (:z spot))})
                        :again)
                    (do (ctx/update-mem! c assoc :furnace-refusal (str (or (get-in e [:data :why]) (some-> (contract/cause-key e) name) "it was not placed")))
                        nil)))))))))
