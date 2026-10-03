(ns drive.keys
  "Pure key, look and lease-reply rules for driving a body, ported one to one from tools/view/web/drive-keys.mjs
  (Minecraft degrees: positive dyaw turns right, negative dpitch looks up). Ctrl is not a control: Ctrl+W closes the tab.
  Controls are keywords (:forward ...); the JSON the server wants is the same names as strings.
  Replies are keywordized maps of the server's /drive reply."
  (:require [clojure.string :as str]))

(def controls
  {"KeyW" :forward "KeyS" :back "KeyA" :left "KeyD" :right "Space" :jump
   "ShiftLeft" :sneak "ShiftRight" :sneak "KeyR" :sprint})

(def look-steps
  {"ArrowLeft" {:dyaw -15} "ArrowRight" {:dyaw 15} "ArrowUp" {:dpitch -10} "ArrowDown" {:dpitch 10}})

(defn control-for [code] (get controls code))
(defn look-step-for [code] (get look-steps code))

(defn mouse-look
  ([movement-x movement-y] (mouse-look movement-x movement-y 0.15))
  ([movement-x movement-y sensitivity]
   {:dyaw (* movement-x sensitivity) :dpitch (* movement-y sensitivity)}))

(defn merge-look [a b]
  {:dyaw (+ (or (:dyaw a) 0) (or (:dyaw b) 0))
   :dpitch (+ (or (:dpitch a) 0) (or (:dpitch b) 0))})

(defn banner-text [manual me]
  (when manual
    (if (= (:who manual) me)
      "MANUAL CONTROL (you) — WASD move, space jump, shift sneak, R sprint, arrows/mouse look, G release"
      (str "MANUAL CONTROL by " (:who manual) ": " (:why manual)))))

(def who-pattern #"^[A-Za-z0-9:_-]{1,40}$")

(defn decode-component
  "URL-decoded s (+ is a space), nil when s has a malformed escape."
  [s]
  (let [s (str/replace s "+" " ")]
    #?(:cljs (try (js/decodeURIComponent s) (catch :default _ nil))
       :clj (try (java.net.URLDecoder/decode s "UTF-8") (catch Exception _ nil)))))

(defn query-param
  "The first value of `name` in a query string like \"?a=1&b=2\", nil when absent or malformed."
  [search name]
  (some (fn [pair]
          (let [[k v] (str/split pair #"=" 2)]
            (when (= name (decode-component k)) (decode-component (or v "")))))
        (str/split (str/replace (or search "") #"^\?" "") #"&")))

(defn who-from
  ([search] (who-from search "view"))
  ([search fallback]
   (let [who (query-param search "who")]
     (if (and who (re-matches who-pattern who)) who fallback))))

(defn should-take-on-click?
  "Embedded in the dashboard a click on the canvas takes over, unless someone else holds the body."
  [{:keys [embed? driving? manual me]}]
  (boolean (and embed? (not driving?) (or (nil? manual) (= (:who manual) me)))))

(defn should-release-on-escape?
  "The first Esc only exits pointer lock (the browser eats it); an Esc with no lock left releases."
  [{:keys [code driving? pointer-locked?]}]
  (boolean (and (= code "Escape") driving? (not pointer-locked?))))

(def leave-actions {:pointerlock-lost :stop-release :hidden :stop :blur :stop :pagehide :stop})

(defn leave-action
  "What the view page does when it may be leaving. Note: the dashboard slice keeps the lease on :pointerlock-lost
  (stop only), unlike :stop-release here."
  [event]
  (get leave-actions event))

(defn holds-body?
  "Whether this page still holds the body, from the latest GET/POST /drive reply (nil: the request failed)."
  [{:keys [driving? reply me]}]
  (boolean (and driving? reply (not (:offline reply))
                (not (#{"not-taken" "not-driver"} (:reason reply)))
                (= (get-in reply [:manual :who]) me))))

(defn stale?
  "A reply to a request that started before the latest take says nothing about the current takeover."
  [{:keys [started-gen current-gen]}]
  (< started-gen current-gen))

(defn should-drop? [{:keys [driving? reply me started-gen current-gen]}]
  (boolean (and driving?
                (not (stale? {:started-gen started-gen :current-gen current-gen}))
                (not (holds-body? {:driving? driving? :reply reply :me me})))))
