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

(rf/reg-fx :push-url
           (fn [url] (.pushState js/history nil "" url)))

(rf/reg-fx :console-log
           (fn [text] (js/console.log text)))

(defonce timers (atom {}))

(defn start-timer! [k ms event]
  (some-> (get @timers k) js/clearInterval)
  (swap! timers assoc k (js/setInterval #(rf/dispatch event) ms)))

(rf/reg-fx :start-timers
           (fn [specs] (doseq [[k ms event] specs] (start-timer! k ms event))))
