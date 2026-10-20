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
  "Put the carried furnace down beside the body (free-spot/place-beside!) and make it the furnace of this job. :again once
  it stands, :continue while a child waits, nil when it could not be put down (memory :furnace-refusal says why)."
  [c]
  (let [r (await (free-spot/place-beside! c "furnace" [:furnace-spot] {}))]
    (cond
      (= :continue r) :continue
      (:placed r) (let [spot (:placed r)]
                    (ctx/update-mem! c assoc :furnace spot)
                    (places/offer! c :furnace spot)
                    (ctx/emit! c :smelt.furnace :info {:furnace spot :text (str "put a furnace down at " (:x spot) " " (:y spot) " " (:z spot))})
                    :again)
      :else (do (ctx/update-mem! c assoc :furnace-refusal (:why r)) nil))))
