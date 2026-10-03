(ns dashboard.guard
  "The guard every state-changing route goes through (a browser page on another origin must not be able to POST here).
  The same rules as guard() in tools/view/drive-proxy.mjs, which is not exported, plus the method and body limits."
  (:require [clojure.string :as str]))

(def max-body-bytes 4096)

(defn local-host? [host port]
  (boolean (some #(= % host) (map #(str % ":" port) ["127.0.0.1" "localhost" "[::1]"]))))

(defn refusal
  "nil when the request may proceed, else {:status :error}. Origin must equal http://<Host> when present."
  [{:keys [host origin content-type port]}]
  (cond
    (not (local-host? host port)) {:status 403 :error "bad Host"}
    (and (some? origin) (not= origin (str "http://" host))) {:status 403 :error "bad Origin"}
    (not (str/starts-with? (or content-type "") "application/json")) {:status 415 :error "Content-Type must be application/json"}))

(defn method-refusal [method]
  (when-not (= "POST" method) {:status 405 :error "POST only"}))
