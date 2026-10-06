(ns dashboard.server.pictures
  "Thumbnails (js/thumbs.mjs) and item pictures."
  (:require ["fs" :as fs]
            [dashboard.items :as items]
            ["path" :as path]
            [dashboard.thumbs :as thumbs]
            ["url" :as url]
            [dashboard.server.files :refer [dashboard-dir file-exists? repo-root state-dir]]
            [dashboard.server.responses :refer [send-json!]]
            [dashboard.server.engine-state :refer [body-key view-file]]
            [dashboard.server.esm :refer [import-esm]]))

;; ---------------------------------------------------------------- thumbnails
;; The fallback for browsers without WebGL2: dashboard.thumbs decides, js/thumbs.mjs renders one still on request.
;; thumbs.mjs is ESM and this build is CJS. A literal import() in the bundle does not work (Closure; and
;; new Function("return import(..)") fails in the bundle: "A dynamic import callback was not specified"), so js/import-esm.cjs,
;; a native CommonJS file, makes the call. THUMBS_MODULE overrides the module path (tests).
(def thumbs-module (or (.-THUMBS_MODULE js/process.env) (.join path dashboard-dir "js" "thumbs.mjs")))



(def no-thumbnails
  {:get (fn [_] (js/Promise.resolve nil))
   :stats (fn [] {:error "thumbnailer unavailable"})
   :close (fn [])})

(defn pose-mtime [body]
  (try (.-mtimeMs (.statSync fs (view-file body "pose.json")))
       (catch :default _ nil)))

(defn load-thumbnailer []
  (-> (import-esm (.-href (.pathToFileURL url thumbs-module)))
      (.then (fn [m]
               (let [r ((.-createRenderer m) #js {:stateDir state-dir})
                     t (thumbs/make {:render (fn [{:keys [world name]}] (-> (.render r world name)
                                                            (.then (fn [x] (when x {:png (.-png x) :ms (.-ms x) :loaded (.-loaded x)})))))
                                     :pose-mtime pose-mtime
                                     :recycle #(.recycle r)
                                     :now #(js/Date.now)
                                     :min-interval-ms thumbs/min-interval-ms
                                     :column-cap thumbs/column-cap
                                     :idle-ms thumbs/idle-ms
                                     :set-timer (fn [f ms] (doto (js/setTimeout f ms) .unref))
                                     :clear-timer js/clearTimeout})]
                 (assoc t :close #(.close r)))))
      (.catch (fn [e]
                (js/console.error (str "thumbnails disabled: " (ex-message e)))
                no-thumbnails))))

;; one thumbnailer for the process, created on the first request
(defonce thumbnailer (delay (load-thumbnailer)))

(defn send-thumb! [res body]
  (-> @thumbnailer
      (.then (fn [t] ((:get t) (body-key body))))
      (.then (fn [thumb]
               (if-not thumb
                 (send-json! res 404 {:error (str "no view for " (:name body) " in world " (:world body))})
                 (do (.writeHead res 200 #js {"content-type" "image/png" "cache-control" "no-store"
                                              "x-pose-mtime" (str (:pose-mtime-ms thumb))})
                     (.end res (:png thumb))))))
      (.catch (fn [e] (when-not (.-headersSent res) (send-json! res 500 {:error (str (ex-message e))}))))))

(defn send-thumbs-stats! [res]
  (-> @thumbnailer
      (.then (fn [t] (send-json! res 200 ((:stats t)))))
      (.catch (fn [e] (when-not (.-headersSent res) (send-json! res 500 {:error (str (ex-message e))}))))))


;; ---------------------------------------------------------------- item pictures
;; The textures the view uses (repo textures/: blocks at the top, items under item/): the first candidate that exists.
(def textures-dir (.join path repo-root "textures"))

(defn send-item-icon! [res name]
  (let [file (->> (items/icon-candidates name)
                  (map #(.join path textures-dir %))
                  (filter file-exists?)
                  first)]
    (if-not file
      (send-json! res 404 {:error (str "no picture for " name)})
      (do (.writeHead res 200 #js {"content-type" "image/png" "cache-control" "public, max-age=3600"})
          (.end res (.readFileSync fs file))))))
