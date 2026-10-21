(ns jobs.lib.furnace
  "What a smelt call's memory says about its furnace load, and putting a carried furnace down."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.places :as places]
            [jobs.lib.spot :as free-spot]
            [jobs.lib.util :as u]))

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

(def cooking-kind :smelt/cooking)

(defn note-cooking!
  "Remember in body memory that item is cooking in the furnace at pos (a cut smelt leaves its load there)."
  [c pos item count]
  (ctx/remember! c cooking-kind {:pos pos :item item :count count} {:cap 5 :ttl (* 60 60 1000)}))

(defn forget-cooking!
  "Drop the cooking notes of the furnace at pos: it was taken empty."
  [c pos]
  (ctx/forget-where! c cooking-kind #(= pos (:pos %))))

(defn cooking-near
  "The newest cooking note {:pos :item :count} within radius of the body whose item passes item?, else nil."
  [c radius item?]
  (->> (ctx/entries c cooking-kind) (sort-by :t >) (map :data)
       (filter #(and (item? (:item %)) (<= (u/dist (u/self-pos c) (:pos %)) radius)))
       first))

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
