(ns dashboard.ui.api
  (:require [dashboard.edn :as edn]
            [re-frame.core :as rf]))

;; [key url] of GET polls still in flight: a slow response never piles up behind the next poll tick, but a request for
;; another url (new world, new body) is not dropped behind the old one. POSTs are user actions: never deduped, never timed out
(defonce in-flight (atom #{}))

;; a hung GET is aborted after this, so in-flight clears and the error is reported
(def request-timeout-ms 20000)

(defn request!
  "One guarded request (a POST is neither guarded nor timed out). read turns the response into [:ok data] or
  [:err message] (a promise of it or a value); a failed fetch or read is [:err]. The handler is dispatched outside
  that chain, so a handler throw is not reported as a fetch error."
  [{:keys [key url opts read on-ok on-err on-unsupported]}]
  (let [post? (= "POST" (some-> opts (aget "method")))]
    (when (or post? (not (contains? @in-flight [key url])))
      (when-not post? (swap! in-flight conj [key url]))
      (let [controller (js/AbortController.)
            timer (when-not post? (js/setTimeout #(.abort controller) request-timeout-ms))]
        ;; the timeout covers the body read too, so a stalled body aborts as well
        (-> (js/fetch url (if post? opts (doto (or opts (js-obj)) (aset "signal" (.-signal controller)))))
            (.then read)
            (.catch (fn [e] [:err (if (= "AbortError" (.-name e)) "request timed out" (str e))]))
            (.then (fn [[outcome value]] (rf/dispatch (case outcome
                                                          :ok (conj on-ok value)
                                                          :unsupported on-unsupported
                                                          (conj on-err value)))))
            (.finally (fn [] (js/clearTimeout timer) (swap! in-flight disj [key url]))))))))
  
(defn json-outcome
  "An error field in a 2xx body counts as an error when error-field? is set."
  [res error-field?]
  (-> (.json res)
      (.then (fn [data]
               (if (and (.-ok res) (not (and error-field? (.-error data))))
                 [:ok (js->clj data :keywordize-keys true)]
                 [:err (or (.-error data) (str "http " (.-status res)))])))))

(defn fetch-json! [{:keys [key url on-ok on-err]}]
  (request! {:key key :url url :on-ok on-ok :on-err on-err :read #(json-outcome % false)}))

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
  (request! {:key key :url url :on-ok on-ok :on-err on-err
             :read (fn [res] (-> (.text res) (.then #(edn-outcome (.-ok res) (.-status res) %))))}))

(rf/reg-fx :fetch-edn fetch-edn!)

(defn post-json! [{:keys [url body on-ok on-err on-unsupported]}]
  (request! {:key [:post (js/JSON.stringify body)] :url url
             :opts #js {:method "POST" :headers #js {"content-type" "application/json"} :body (js/JSON.stringify body)}
             :read (fn [res]
                     (if (= 501 (.-status res))
                       [:unsupported nil]
                       (json-outcome res true)))
             :on-ok on-ok :on-err on-err :on-unsupported on-unsupported}))

(rf/reg-fx :post-json post-json!)

(defn post-edn! [{:keys [url body on-ok on-err]}]
  (request! {:key [:post (pr-str body)] :url url
             :opts #js {:method "POST" :headers #js {"content-type" "application/edn"} :body (pr-str body)}
             :read (fn [res]
                     (-> (.text res)
                         (.then (fn [text]
                                  (let [data (try (edn/one-form text) (catch :default e {:error (str e)}))]
                                    (if (and (.-ok res) (not (:error data)))
                                      [:ok data]
                                      [:err (or (:error data) (str "http " (.-status res)))]))))))
             :on-ok on-ok :on-err on-err}))

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
