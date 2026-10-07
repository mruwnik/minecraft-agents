(ns view.drive
  "JS-value adapters over drive.keys for view.takeover and the page: the /drive reply as drive.keys wants it, the banner
   text, the who parameter. Also the page's request plumbing: a serial queue and a fetch with a timeout."
  (:require [drive.keys :as k]))

(set! *warn-on-infer* true)

(defn manual-of
  "The /drive reply's manual ({who, why, expiresAt}) as drive.keys wants it; nil for null or undefined."
  [^js manual]
  (when (some? manual) {:who (.-who manual) :why (.-why manual)}))

(defn reply-of [^js reply]
  (when (some? reply)
    {:ok (.-ok reply) :offline (.-offline reply) :reason (.-reason reply) :manual (manual-of (.-manual reply))}))

(defn banner-text [manual me] (k/banner-text (manual-of manual) me))

(defn who-from
  ([search] (k/who-from search))
  ([search fallback] (k/who-from search (if (some? fallback) fallback "view"))))

(defn serial-queue
  "A function that runs the functions given to it one at a time in call order: each starts after the previous one settled,
   failed or not. Returns the promise of the function's result."
  []
  (let [tail (volatile! (js/Promise.resolve))]
    (fn [f]
      (let [result (.then ^js @tail (fn [_] (f)))]
        (vreset! tail (.catch result (fn [_] nil)))
        result))))

(defn with-timeout
  "fetch-fn that aborts after ms, so a hung request cannot hold the serial queue."
  [fetch-fn ms]
  (fn timed
    ([url] (timed url #js {}))
    ([url ^js init]
     (let [controller (js/AbortController.)
           timer (js/setTimeout #(.abort controller) ms)]
       (-> (js/Promise.resolve)
           (.then (fn [_] (fetch-fn url (js/Object.assign #js {} init #js {:signal (.-signal controller)}))))
           (.finally #(js/clearTimeout timer)))))))
