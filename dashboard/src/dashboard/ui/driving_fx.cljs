(ns dashboard.ui.driving-fx
  "The effects of dashboard.ui.driving, kept thin: fetch with a timeout, one serial queue per body, interval timers."
  (:require [re-frame.core :as rf]
            [re-frame.db :as rf-db]
            [dashboard.ui.db :as db]))

(def request-timeout-ms 1500)
(def ping-ms 500)
(def look-ms 50)
(def poll-ms 1000)

;; a body is <world>/<name>. The driven body is the popup's, always of the page's world, read when the request goes out.
(defn drive-url [world name] (str "/drive/" (js/encodeURIComponent world) "/" (js/encodeURIComponent name)))
(defn page-drive-url [name] (drive-url (db/current-world @rf-db/app-db) name))

(defn post-init [body keepalive?]
  #js {:method "POST" :headers #js {"Content-Type" "application/json"} :body (js/JSON.stringify (clj->js body)) :keepalive keepalive?})

(defn fetch-reply
  "Promise of the keywordized JSON reply, nil when the request failed or timed out (so a hung one cannot hold a queue)."
  [url init]
  (let [controller (js/AbortController.)
        timer (js/setTimeout #(.abort controller) request-timeout-ms)]
    (-> (js/fetch url (doto init (aset "signal" (.-signal controller))))
        (.then #(.json %))
        (.then #(js->clj % :keywordize-keys true))
        (.catch (fn [_] nil))
        (.finally #(js/clearTimeout timer)))))

;; per body: the tail of its request chain, so a keyup never overtakes its keydown
(defonce queues (atom {}))

(defn enqueue! [name f]
  (let [result (-> (or (get @queues name) (js/Promise.resolve nil)) (.then f))]
    (swap! queues assoc name (.catch result (fn [_] nil)))
    result))

(defn run-post! [{:keys [name gen body]} keepalive?]
  (-> (fetch-reply (page-drive-url name) (post-init body keepalive?))
      (.then (fn [reply] (rf/dispatch (if reply [:dashboard.ui.driving/reply name gen (:op body) reply]
                                          [:dashboard.ui.driving/request-failed name gen (:op body)]))))))

(rf/reg-fx
 :drive/post
 (fn [{:keys [name keepalive?] :as request}]
   (if keepalive?
     (run-post! request true)
     (enqueue! name #(run-post! request false)))))

(rf/reg-fx
 :drive/poll-get
 (fn [{:keys [name gen]}]
   (-> (fetch-reply (page-drive-url name) #js {})
       (.then (fn [reply] (rf/dispatch (if reply [:dashboard.ui.driving/poll-reply name gen reply]
                                           [:dashboard.ui.driving/request-failed name gen "poll"])))))))

(defonce timers (atom {}))

(defn set-timer! [k ms event on?]
  (some-> (get @timers k) js/clearInterval)
  (swap! timers dissoc k)
  (when on? (swap! timers assoc k (js/setInterval #(rf/dispatch event) ms))))

(rf/reg-fx :drive/ping-timer (fn [{:keys [name on?]}] (set-timer! [:ping name] ping-ms [:dashboard.ui.driving/ping-tick name] on?)))
(rf/reg-fx :drive/look-timer (fn [{:keys [name on?]}] (set-timer! [:look name] look-ms [:dashboard.ui.driving/look-flush name] on?)))
(rf/reg-fx :drive/poll (fn [{:keys [name on?]}] (set-timer! [:poll name] poll-ms [:dashboard.ui.driving/poll-tick name] on?)))
