(ns dashboard.server.responses
  "HTTP plumbing: json and edn senders, the printed-state memo, static files, and bounded POST bodies behind dashboard.guard."
  (:require ["fs" :as fs]
            [dashboard.guard :as guard]
            ["path" :as path]
            [clojure.string :as str]
            [dashboard.server.files :refer [js-dir port public-dir]]))

;; ---------------------------------------------------------------- responses
(defn to-js [x] (clj->js x :keyword-fn #(subs (str %) 1)))

(def content-types
  {".html" "text/html; charset=utf-8" ".js" "text/javascript; charset=utf-8" ".css" "text/css; charset=utf-8"
   ".json" "application/json" ".map" "application/json" ".png" "image/png" ".svg" "image/svg+xml"
   ".ico" "image/x-icon" ".txt" "text/plain; charset=utf-8" ".woff" "font/woff" ".woff2" "font/woff2"})

(defn send! [res code type payload]
  (.writeHead res code #js {"content-type" type "cache-control" "no-store"})
  (.end res payload))

(defn send-json-js! [res code value]
  (send! res code "application/json" (js/JSON.stringify value)))

(defn send-json! [res code value]
  (send-json-js! res code (to-js value)))

(defn pr-body [body] (pr-str body))
(defn pr-part [part] (pr-str part))

(def print-memo (atom {}))

(defn memo-print
  "print of value, kept under key: an equal value (freshly built or not) is printed once."
  [k print value]
  (let [[held text] (get @print-memo k)]
    (if (and text (= held value))
      text
      (let [text (print value)] (swap! print-memo assoc k [value text]) text))))

(def live-world-keys [:entities :entity-sources :entity-truncated?])

(defn worlds-text
  "Text of the :worlds vector. Each world's static part (places, villages ...) is printed once while it stays equal;
  its live entity keys change every poll and are printed each time."
  [worlds]
  (str "[" (str/join " " (map (fn [world]
                                (let [fixed (memo-print [:world (:name world)] pr-part (apply dissoc world live-world-keys))
                                      live (pr-str (select-keys world live-world-keys))]
                                  (cond (= "{}" live) fixed
                                        (= "{}" fixed) live
                                        :else (str (subs fixed 0 (dec (count fixed))) ", " (subs live 1)))))
                              worlds)) "]"))

(defn state-text
  "EDN text of an /api/state snapshot; the bodies and the static part of each world are printed once while they stay equal."
  [snapshot]
  (let [body-key-of (fn [b] [:body (:world b) (:name b)])
        used (cond-> #{} (:worlds snapshot) (into (map (fn [w] [:world (:name w)])) (:worlds snapshot))
                    (vector? (:bodies snapshot)) (into (map body-key-of) (:bodies snapshot)))
        text (str "{" (str/join ", " (map (fn [[k v]]
                                            (str (pr-str k) " "
                                                 (cond
                                                   (and (= k :bodies) (vector? v))
                                                   (str "[" (str/join " " (map #(memo-print (body-key-of %) pr-body %) v)) "]")
                                                   (and (= k :worlds) (vector? v)) (worlds-text v)
                                                   :else (pr-str v))))
                                          snapshot))
                  "}")]
    (swap! print-memo select-keys used)
    text))

(defn send-edn! [res code value]
  (send! res code "application/edn; charset=utf-8" (if (:bodies value) (state-text value) (pr-str value))))

(defn regular-file? [file]
  (try (.isFile (.statSync fs file)) (catch :default _ false)))

(defn send-file! [res file]
  (let [type (get content-types (str/lower-case (.extname path file)) "application/octet-stream")]
    (if (regular-file? file)
      (send! res 200 type (.readFileSync fs file))
      (send-json! res 404 {:error (str "no such file: " (.basename path file))}))))

;; a file under dir only when the normalised path stays inside it
(defn safe-join [dir relative]
  (let [full (.resolve path dir (str/replace relative #"^/+" ""))]
    (when (str/starts-with? full (str dir (.-sep path))) full)))

(defn serve-static! [res request-path]
  (let [js? (str/starts-with? request-path "/js/")
        file (if js?
               (safe-join js-dir (subs request-path 4))
               (safe-join public-dir request-path))]
    (if file
      (send-file! res file)
      (send-json! res 404 {:error "not found"}))))


(defn read-body
  "Calls on-done once with the body text, or nil when it is over the limit or the request broke off."
  [req limit on-done]
  (let [chunks (atom []) size (atom 0) done? (atom false)
        finish! (fn [text] (when-not @done? (reset! done? true) (on-done text)))
        refuse! (fn []
                  (finish! nil)
                  (some-> (.-socket req) (.destroySoon)))
        declared (js/Number (or (some-> (.-headers req) (aget "content-length")) 0))]
    (if (> declared limit)
      (refuse!)
      (do (.on req "data" (fn [chunk]
                            (when-not @done?
                              (swap! size + (.-length chunk))
                              (if (> @size limit) (refuse!) (swap! chunks conj chunk)))))
          (.on req "error" #(finish! nil))
          (.on req "aborted" #(finish! nil))
          (.on req "end" #(finish! (.toString (js/Buffer.concat (to-array @chunks)) "utf8")))))))

;; ---------------------------------------------------------------- state-changing routes
;; Every POST route goes through dashboard.guard (Host, Origin, Content-Type, method) and a body limit.


(defn guarded-post!
  "Refuses with the guard's status, else reads the bounded body and calls on-body."
  ([req res limit-bytes on-body]
   (guarded-post! req res limit-bytes on-body {}))
  ([req res limit-bytes on-body {:keys [content-types send-error] :or {send-error send-json!}}]
   (let [headers (.-headers req)
         refused (or (guard/method-refusal (.-method req))
                     (guard/refusal {:host (.-host headers) :origin (.-origin headers)
                                     :content-type (aget headers "content-type") :port port
                                     :content-types content-types}))]
     (if refused
       (send-error res (:status refused) {:error (:error refused)})
       (read-body req limit-bytes on-body)))))
