(ns view.drive
  "The view page's drive rules as drive.mjs (tools/view/web) calls them: JS values in, JS values out, over drive.keys.
   Controls and leave actions are strings, look steps {dyaw, dpitch} objects, the /drive reply the server's JSON.
   Also the page's request plumbing: a serial queue and a fetch with a timeout."
  (:require [drive.keys :as k]))

(set! *warn-on-infer* true)

(defn manual-of
  "The /drive reply's manual ({who, why, expiresAt}) as drive.keys wants it; nil for null or undefined."
  [^js manual]
  (when (some? manual) {:who (.-who manual) :why (.-why manual)}))

(defn reply-of [^js reply]
  (when (some? reply)
    {:ok (.-ok reply) :offline (.-offline reply) :reason (.-reason reply) :manual (manual-of (.-manual reply))}))

(defn control-for [code] (some-> (k/control-for code) name))
(defn look-step-for [code] (some-> (k/look-step-for code) clj->js))

(defn mouse-look
  ([movement-x movement-y] (clj->js (k/mouse-look movement-x movement-y)))
  ([movement-x movement-y sensitivity]
   (clj->js (k/mouse-look movement-x movement-y (if (some? sensitivity) sensitivity 0.15)))))

(defn merge-look [^js a ^js b]
  (clj->js (k/merge-look {:dyaw (.-dyaw a) :dpitch (.-dpitch a)} {:dyaw (.-dyaw b) :dpitch (.-dpitch b)})))

(defn banner-text [manual me] (k/banner-text (manual-of manual) me))

(defn who-from
  ([search] (k/who-from search))
  ([search fallback] (k/who-from search (if (some? fallback) fallback "view"))))

(defn should-take-on-click [^js args]
  (k/should-take-on-click? {:embed? (.-embed args) :driving? (.-driving args) :manual (manual-of (.-manual args)) :me (.-me args)}))

(defn should-release-on-escape [^js args]
  (k/should-release-on-escape? {:code (.-code args) :driving? (.-driving args) :pointer-locked? (.-pointerLocked args)}))

(defn leave-action [event] (some-> (when (string? event) (k/leave-action (keyword event))) name))

(defn holds-body [^js args]
  (k/holds-body? {:driving? (.-driving args) :reply (reply-of (.-reply args)) :me (.-me args)}))

(defn is-stale [^js args] (k/stale? {:started-gen (.-startedGen args) :current-gen (.-currentGen args)}))

(defn should-drop [^js args]
  (k/should-drop? {:driving? (.-driving args) :reply (reply-of (.-reply args)) :me (.-me args)
                   :started-gen (.-startedGen args) :current-gen (.-currentGen args)}))

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
