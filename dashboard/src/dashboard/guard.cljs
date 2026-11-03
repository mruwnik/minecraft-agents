(ns dashboard.guard
  "The guard every state-changing route goes through (a browser page on another origin must not be able to POST here).
  The same rules as guard() in tools/view/drive-proxy.mjs, which is not exported, plus the method and body limits."
  (:require [clojure.string :as str]))

(def max-body-bytes 4096)

(def bind-host (or (.-DASHBOARD_HOST js/process.env) "127.0.0.1"))

(def loopback-bind? (contains? #{"127.0.0.1" "localhost" "::1"} bind-host))

(defn local-host?
  "Loopback Host names only; when the owner bound another address on purpose (DASHBOARD_HOST), any Host with the port."
  ([host port] (local-host? host port loopback-bind?))
  ([host port loopback-only?]
   (boolean (if loopback-only?
              (some #(= % host) (map #(str % ":" port) ["127.0.0.1" "localhost" "[::1]"]))
              (and host (str/ends-with? host (str ":" port)))))))

(defn refusal
  "nil when the request may proceed, else {:status :error}. Origin must equal http://<Host> when present."
  [{:keys [host origin content-type port content-types loopback-only?] :or {loopback-only? loopback-bind?}}]
  (let [content-types (or content-types ["application/json"])]
  (cond
    (not (local-host? host port loopback-only?)) {:status 403 :error "bad Host"}
    (and (some? origin) (not= origin (str "http://" host))) {:status 403 :error "bad Origin"}
    (not (some #(str/starts-with? (or content-type "") %) content-types))
    {:status 415 :error (str "Content-Type must be " (str/join " or " content-types))})))

(defn method-refusal [method]
  (when-not (= "POST" method) {:status 405 :error "POST only"}))
