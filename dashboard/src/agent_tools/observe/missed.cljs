(ns agent-tools.observe.missed
  "What happened on a body since an observer last looked (its checkpoint's cursor), read without moving the checkpoint."
  (:require [agent-tools.world-data :as data]
            [agent-tools.http :as http]
            [agent-tools.observe.lock :refer [saved-checkpoint]]
            [agent-tools.observe.status :refer [missed-summary missed?]]
            [shadow.cljs.modern :refer [js-await]]))

(def page-size 1000)
(def max-pages 40)

(defn events-query [stream-id after]
  (str "/events?stream-id=" (js/encodeURIComponent stream-id) "&after=" after "&limit=" page-size))

(defn missed!
  "A promise of {:count :items} for the news after the cursor saved in `file`, or nil when there is no checkpoint,
  nothing happened, or the events cannot be read. It reads up to max-pages pages of page-size events, keeping only the news
  of each; further back than that it starts at the latest max-pages pages and says how many events it skipped (:skipped).
  get! is (get! socket-path path options)."
  [get! socket-path file]
  (let [cursor (try (:cursor (saved-checkpoint file)) (catch :default _ nil))]
    (if-not (:stream-id cursor)
      (js/Promise.resolve nil)
      (-> (letfn [(from [after events pages skipped]
                    (js-await [response (get! socket-path (events-query (:stream-id cursor) after) {})]
                      (if-not (and (= 200 (:status response)) (http/edn-response? (:content-type response)))
                        [events skipped]
                        (let [page (data/read-edn (:text response))
                              reach (- (:latest-seq page 0) after)
                              jump (- (:latest-seq page 0) (* max-pages page-size))]
                          (if (and (zero? skipped) (= pages max-pages) (> reach (* max-pages page-size)))
                            (from jump events pages (- jump after))
                            (let [events (into events (filter missed?) (:events page))]
                              (if (or (:gap? page) (empty? (:events page)) (zero? pages)
                                      (>= (:seq (last (:events page))) (:latest-seq page)))
                                [events skipped]
                                (from (:seq (last (:events page))) events (dec pages) skipped))))))))]
            (from (:seq cursor) [] max-pages 0))
          (.then (fn [[events skipped]] (missed-summary events (js/Date.now) skipped)))
          (.catch (fn [_] nil))))))
