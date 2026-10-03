(ns dashboard.routes
  (:require [clojure.string :as str]))

(def blueprint-re #"^/api/blueprint/([a-z0-9]+(?:-[a-z0-9]+)*)$")
(def unsupported-re #"^/api/(?:look|screen|actions|whisper|icon)/[A-Za-z0-9_]{1,64}(?:/live)?$")
(def static-re #"^/[A-Za-z0-9_./-]+\.(?:js|css|map|png|svg|ico|json|html|txt|woff2?)$")

(def exact
  {"/" {:kind :page}
   "/index.html" {:kind :page}
   "/villagers" {:kind :page}
   "/villages" {:kind :page}
   "/blueprints" {:kind :page}
   "/api/worlds" {:kind :worlds}
   "/api/state" {:kind :state}
   "/api/villagers" {:kind :villagers-api}
   "/api/villages" {:kind :villages-api}
   "/api/chat" {:kind :chat}
   "/api/world" {:kind :world}
   "/api/blueprints" {:kind :blueprints}
   "/api/blueprint-preview" {:kind :blueprint-preview}})

(defn pathname [url]
  (.-pathname (js/URL. url "http://dashboard")))

(defn route [url]
  (let [path (pathname url)]
    (or (exact path)
        (some->> (re-find blueprint-re path) second (assoc {:kind :blueprint} :name))
        (when (re-find unsupported-re path) {:kind :unsupported})
        (when (and (re-find static-re path) (not (str/includes? path ".."))) {:kind :static :path path})
        {:kind :unknown})))
