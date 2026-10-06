(ns dashboard.server.esm
  "Loading ES modules into the CJS bundle, and the live view mounted on this origin."
  (:require ["path" :as path]
            ["url" :as url]
            [dashboard.server.files :refer [dashboard-dir repo-root state-dir]]))

(def import-esm (js/require (.join path dashboard-dir "js" "import-esm.cjs")))

;; ---------------------------------------------------------------- the live view, on this origin
;; js/viewmount.mjs builds tools/view/serve.mjs's request handler without listening; its paths (/view, /pose/, /hud/,
;; /drive/, /web/ ...) are forwarded to it. Loaded once at startup, like the thumbnailer; absent when it fails to load.
(def viewmount-module (or (.-VIEWMOUNT_MODULE js/process.env) (.join path dashboard-dir "js" "viewmount.mjs")))

(defonce view-mount (atom nil))

(defn load-view-mount []
  (-> (import-esm (.-href (.pathToFileURL url viewmount-module)))
      (.then (fn [m] (reset! view-mount ((.-mountView m) #js {:repo repo-root :stateDir state-dir}))))
      (.catch (fn [e] (js/console.error (str "live view disabled: " (ex-message e)))))))

(defn view-request? [req]
  (when-let [m @view-mount]
    (.handles m (.-pathname (js/URL. (.-url req) "http://dashboard")))))
