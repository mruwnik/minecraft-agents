(ns dashboard.ui.drive
  "Manual takeover as the dashboard sees it: who drives the body, the banner and countdown text, and the requests it sends.
  Pure; the polling, messages and requests live in dashboard.ui.detail-events."
  (:require [clojure.string :as str]))

(def who-prefix "dashboard-")
(def who-chars 6)

(defn new-who
  "This page load's name: \"dashboard-\" and 6 random base36 chars. `rand-fn` returns a float in [0, 1)."
  [rand-fn]
  (str who-prefix (apply str (repeatedly who-chars #(.toString (js/Math.floor (* 36 (rand-fn))) 36)))))

(defn ours-who?
  "Is the driver this page (its own who, nothing else)?"
  [who driver] (and (some? who) (= who driver)))

(defn driving-now?
  "True when we drive: the page told us so, or the body's lease is held by our who."
  [{:keys [driving? manual]} who]
  (boolean (or driving? (ours-who? who (:who manual)))))

(defn parse-message
  "{:driving? :manual :expires-at} from a postMessage {type: 'drive', driving, manual, expiresAt} (keywordized), else nil."
  [data]
  (when (and (map? data) (= "drive" (:type data)))
    {:driving? (boolean (:driving data)) :manual (:manual data) :expires-at (:expiresAt data)}))

(defn apply-message [drive msg now]
  (assoc (merge drive msg) :at now))

(defn apply-poll
  "A GET /drive reply's :manual (nil when nobody drives), seen at now."
  [drive manual now who]
  (cond
    (nil? manual) (assoc drive :manual nil :expires-at nil :driving? false :at now)
    (ours-who? who (:who manual)) (assoc drive :manual manual :expires-at (if (number? (:expiresAt manual)) (:expiresAt manual) (:expires-at drive)) :at now)
    :else (assoc drive :manual manual :driving? false :expires-at nil :at now)))

(defn countdown-text [expires-at now]
  (when (number? expires-at)
    (let [ms (- expires-at now)]
      (if (pos? ms) (str "auto-release in " (js/Math.ceil (/ ms 1000)) " s") "auto-release now"))))

(defn banner [{:keys [manual expires-at] :as drive} now who]
  (cond
    (and (nil? manual) (not (:driving? drive))) {:kind :none}
    (driving-now? drive who) {:kind :ours :text (str "MANUAL: you are driving (" (or (:who manual) who) ")") :countdown (countdown-text expires-at now)}
    :else {:kind :other :text (str "driven by " (:who manual) (when-not (str/blank? (:why manual)) (str ": " (:why manual))))}))

(defn take-request [who] {:op "take" :who who :why "dashboard"})

(defn release-request [_drive who] {:op "release" :who who})

(def hint "click the view to take control (WASD, space, shift, mouse; Esc releases)")
