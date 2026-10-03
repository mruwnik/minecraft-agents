(ns dashboard.ui.api
  (:require [re-frame.core :as rf]))

;; keys of requests still in flight: a slow response never piles up behind the next poll tick
(defonce in-flight (atom #{}))

(defn fetch-json! [{:keys [key url on-ok on-err]}]
  (when-not (contains? @in-flight key)
    (swap! in-flight conj key)
    (-> (js/fetch url)
        (.then (fn [res]
                 (-> (.json res)
                     (.then (fn [data]
                              (if (.-ok res)
                                (rf/dispatch (conj on-ok (js->clj data :keywordize-keys true)))
                                (rf/dispatch (conj on-err (or (.-error data) (str "http " (.-status res))))))))))) 
        (.catch (fn [e] (rf/dispatch (conj on-err (str e)))))
        (.finally (fn [] (swap! in-flight disj key))))))

(rf/reg-fx :fetch-json fetch-json!)

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

(rf/reg-fx :download-json
           (fn [{:keys [filename text]}]
             (let [url (js/URL.createObjectURL (js/Blob. #js [text] #js {:type "application/json"}))
                   link (js/document.createElement "a")]
               (set! (.-href link) url)
               (set! (.-download link) filename)
               (.click link)
               (js/URL.revokeObjectURL url))))

(rf/reg-fx :push-url
           (fn [url] (.pushState js/history nil "" url)))

(rf/reg-fx :replace-url
           (fn [url] (.replaceState js/history nil "" url)))

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
