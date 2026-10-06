(ns dashboard.ui.api
  (:require [dashboard.edn :as edn]
            [re-frame.core :as rf]))

;; [key url] of requests still in flight: a slow response never piles up behind the next poll tick,
;; but a request for another url (new world, new body) is not dropped behind the old one
(defonce in-flight (atom #{}))

(defn fetch-json! [{:keys [key url on-ok on-err]}]
  (when-not (contains? @in-flight [key url])
    (swap! in-flight conj [key url])
    (-> (js/fetch url)
        (.then (fn [res]
                 (-> (.json res)
                     (.then (fn [data]
                              (if (.-ok res)
                                (rf/dispatch (conj on-ok (js->clj data :keywordize-keys true)))
                                (rf/dispatch (conj on-err (or (.-error data) (str "http " (.-status res))))))))))) 
        (.catch (fn [e] (rf/dispatch (conj on-err (str e)))))
        (.finally (fn [] (swap! in-flight disj [key url]))))))

(rf/reg-fx :fetch-json fetch-json!)

(defn edn-outcome
  "[:ok data] or [:err message] for an HTTP reply: an unparseable body is an error, whatever the status."
  [ok? status text]
  (let [parsed (try {:data (edn/one-form text)} (catch :default e {:parse-error (str e)}))
        data (:data parsed)]
    (cond
      (and ok? (:parse-error parsed)) [:err (str "bad reply: " (:parse-error parsed))]
      ok? [:ok data]
      :else [:err (or (when (map? data) (:error data)) (str "http " status))])))

(defn fetch-edn! [{:keys [key url on-ok on-err]}]
  (when-not (contains? @in-flight [key url])
    (swap! in-flight conj [key url])
    (-> (js/fetch url)
        (.then (fn [res]
                 (-> (.text res)
                     (.then (fn [text]
                              (let [[outcome value] (edn-outcome (.-ok res) (.-status res) text)]
                                (rf/dispatch (conj (if (= :ok outcome) on-ok on-err) value))))))))
        (.catch (fn [e] (rf/dispatch (conj on-err (str e)))))
        (.finally (fn [] (swap! in-flight disj [key url]))))))

(rf/reg-fx :fetch-edn fetch-edn!)

(defn post-json! [{:keys [url body on-ok on-err on-unsupported]}]
  (-> (js/fetch url #js {:method "POST" :headers #js {"content-type" "application/json"} :body (js/JSON.stringify body)})
      (.then (fn [res]
               (if (= 501 (.-status res))
                 (rf/dispatch on-unsupported)
                 (-> (.json res)
                     (.then (fn [data]
                              (if (or (not (.-ok res)) (.-error data))
                                (rf/dispatch (conj on-err (or (.-error data) (str "http " (.-status res)))))
                                (rf/dispatch (conj on-ok (js->clj data :keywordize-keys true))))))))))
      (.catch (fn [e] (rf/dispatch (conj on-err (str e)))))))

(rf/reg-fx :post-json post-json!)

(defn post-edn! [{:keys [url body on-ok on-err]}]
  (-> (js/fetch url #js {:method "POST" :headers #js {"content-type" "application/edn"} :body (pr-str body)})
      (.then (fn [res]
               (.then (.text res)
                      (fn [text]
                        (let [data (try (edn/one-form text) (catch :default e {:error (str e)}))]
                          (if (and (.-ok res) (not (:error data)))
                            (rf/dispatch (conj on-ok data))
                            (rf/dispatch (conj on-err (or (:error data) (str "http " (.-status res)))))))))))
      (.catch (fn [e] (rf/dispatch (conj on-err (str e)))))))

(rf/reg-fx :post-edn post-edn!)

(rf/reg-fx :download-json
           (fn [{:keys [filename text]}]
             (let [url (js/URL.createObjectURL (js/Blob. #js [text] #js {:type "application/json"}))
                   link (js/document.createElement "a")]
               (set! (.-href link) url)
               (set! (.-download link) filename)
               (.click link)
               (js/URL.revokeObjectURL url))))

(rf/reg-fx :download-edn
           (fn [{:keys [filename text]}]
             (let [url (js/URL.createObjectURL (js/Blob. #js [text] #js {:type "application/edn"}))
                   link (js/document.createElement "a")]
               (set! (.-href link) url)
               (set! (.-download link) filename)
               (.click link)
               (js/URL.revokeObjectURL url))))

(rf/reg-fx :push-url
           (fn [url] (.pushState js/history nil "" url)))

(rf/reg-fx :replace-url
           (fn [url] (.replaceState js/history nil "" url)))

;; the server was rebuilt and restarted (dashboard.ui.buildid)
(rf/reg-fx :reload-page
           (fn [_] (.reload js/location)))

(rf/reg-fx :console-log
           (fn [text] (js/console.log text)))

(defonce timers (atom {}))

(defn start-timer! [k ms event]
  (some-> (get @timers k) js/clearInterval)
  (swap! timers assoc k (js/setInterval #(rf/dispatch event) ms)))

(rf/reg-fx :stop-timers
           (fn [ks] (doseq [k ks]
                      (some-> (get @timers k) js/clearInterval)
                      (swap! timers dissoc k))))

(rf/reg-fx :start-timers
           (fn [specs] (doseq [[k ms event] specs] (start-timer! k ms event))))
