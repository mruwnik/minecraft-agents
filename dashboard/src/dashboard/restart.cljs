(ns dashboard.restart
  "POST /api/restart: the launcher (start.mjs) rebuilds and replaces this server. Loopback peers only.
  The server tells the launcher over the IPC channel it was spawned with (process.send)."
  (:require [clojure.string :as str]))

(defn loopback-address?
  "True for 127.0.0.0/8, ::1 and the IPv4-mapped forms of 127.0.0.0/8, as http's remoteAddress prints them."
  [addr]
  (let [v4 (if (str/starts-with? (or addr "") "::ffff:") (subs addr 7) addr)]
    (boolean (or (= "::1" addr)
                 (and v4 (re-matches #"127(?:\.\d{1,3}){3}" v4))))))

(def build-id
  "Names this server process; the page reloads when /api/state reports another one."
  (str (js/Date.now) "-" (.-pid js/process)))

(defn ask-launcher!
  "True when a launcher is listening on the IPC channel and was told; false when the server runs on its own."
  []
  (if-not (fn? (.-send js/process))
    false
    (do (.send js/process #js {:type "restart"}) true)))
