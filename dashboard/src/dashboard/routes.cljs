(ns dashboard.routes
  (:require [clojure.string :as str]))

(def blueprint-re #"^/api/blueprint/([a-z0-9]+(?:-[a-z0-9]+)*)$")
(def unsupported-re #"^/api/(?:look|screen|actions|icon)/[A-Za-z0-9_]{1,64}(?:/live)?$")
(def whisper-send-re #"^/api/whisper/([A-Za-z0-9_-]{1,64})$")
(def thumb-re #"^/api/thumb/([A-Za-z0-9_-]+)\.png$")
(def tile-re #"^/api/tile/([A-Za-z0-9_-]+)/(-?\d+)\.(-?\d+)\.png$")
(def tiles-re #"^/api/tiles/([A-Za-z0-9_-]+)$")
(def item-icon-re #"^/api/item-icon/([a-z0-9_]+)\.png$")
(def plan-re #"^/api/plan/([A-Za-z0-9_-]+)$")
(def events-re #"^/api/events/([A-Za-z0-9_-]+)$")
(def attention-resolve-re #"^/api/attention/([A-Za-z0-9_-]+)/resolve$")
(def static-re #"^/[A-Za-z0-9_./-]+\.(?:js|css|map|png|svg|ico|json|html|txt|woff2?)$")

(def exact
  {"/" {:kind :page}
   "/index.html" {:kind :page}
   "/villagers" {:kind :page}
   "/villages" {:kind :page}
   "/blueprints" {:kind :page}
   "/map" {:kind :page}
   "/plans" {:kind :page}
   "/jobs" {:kind :page}
   "/api/thumbs/stats" {:kind :thumbs-stats}
   "/api/tile-stats" {:kind :tile-stats}
   "/api/worlds" {:kind :worlds}
   "/api/state" {:kind :state}
   "/api/plans" {:kind :plans-api}
   "/api/villagers" {:kind :villagers-api}
   "/api/villages" {:kind :villages-api}
   "/api/chat" {:kind :chat}
   "/api/chat/send" {:kind :chat-send}
   "/api/jobs" {:kind :jobs-api}
   "/api/world" {:kind :world}
   "/api/blueprints" {:kind :blueprints}
   "/api/blueprint-preview" {:kind :blueprint-preview}})

(defn pathname [url]
  (.-pathname (js/URL. url "http://dashboard")))

(defn route [url]
  (let [path (pathname url)]
    (or (exact path)
        (some->> (re-find blueprint-re path) second (assoc {:kind :blueprint} :name))
        (some->> (re-find thumb-re path) second (assoc {:kind :thumb} :name))
        (when-let [[_ world cx cz] (re-find tile-re path)]
          {:kind :tile :world world :cx (js/parseInt cx 10) :cz (js/parseInt cz 10)})
        (some->> (re-find tiles-re path) second (assoc {:kind :tiles} :world))
        (some->> (re-find item-icon-re path) second (assoc {:kind :item-icon} :name))
        (some->> (re-find plan-re path) second (assoc {:kind :plan-api} :name))
        (some->> (re-find attention-resolve-re path) second (assoc {:kind :attention-resolve} :name))
        (some->> (re-find events-re path) second (assoc {:kind :events} :name))
        (some->> (re-find whisper-send-re path) second (assoc {:kind :whisper-send} :name))
        (when (re-find unsupported-re path) {:kind :unsupported})
        (when (and (re-find static-re path) (not (str/starts-with? path "/api/")) (not (str/includes? path ".."))) {:kind :static :path path})
        {:kind :unknown})))
