(ns agent-tools.observe.missed
  "What happened on a body since an observer last looked (its checkpoint's cursor), read without moving the checkpoint."
  (:require [agent-tools.world-data :as data]
            [agent-tools.http :as http]
            [agent-tools.observe.lock :refer [saved-checkpoint]]
            [agent-tools.observe.status :refer [missed-summary]]
            [shadow.cljs.modern :refer [js-await]]))

(def max-pages 8)

(defn events-query [stream-id after]
  (str "/events?stream-id=" (js/encodeURIComponent stream-id) "&after=" after "&limit=256"))

(defn missed!
  "A promise of {:count :items} for the news after the cursor saved in `file`, or nil when there is no checkpoint,
  nothing happened, or the events cannot be read. get! is (get! socket-path path options)."
  [get! socket-path file]
  (let [cursor (try (:cursor (saved-checkpoint file)) (catch :default _ nil))]
    (if-not (:stream-id cursor)
      (js/Promise.resolve nil)
      (-> (letfn [(from [after events pages]
                    (js-await [response (get! socket-path (events-query (:stream-id cursor) after) {})]
                      (if-not (and (= 200 (:status response)) (http/edn-response? (:content-type response)))
                        events
                        (let [page (data/read-edn (:text response))
                              events (into events (:events page))]
                          (if (or (:gap? page) (empty? (:events page)) (zero? pages)
                                  (>= (:seq (last (:events page))) (:latest-seq page)))
                            events
                            (from (:seq (last (:events page))) events (dec pages)))))))]
            (from (:seq cursor) [] max-pages))
          (.then #(missed-summary % (js/Date.now)))
          (.catch (fn [_] nil))))))
