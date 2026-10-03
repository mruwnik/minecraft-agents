(ns dashboard.ui.drive
  "Manual takeover as the dashboard sees it: who drives the body, the banner and countdown text, and the requests it sends.
  Pure; the polling, messages and requests live in dashboard.ui.detail-events."
  (:require [clojure.string :as str]))

(def me "dashboard")
;; until the view page honours ?who=, the page inside our iframe drives as "view"
(def ours #{"dashboard" "view"})

(defn ours-who? [who] (contains? ours who))

(defn driving-now?
  "True when we drive: the page told us so, or the body's lease is held by one of our names."
  [{:keys [driving? manual]}]
  (boolean (or driving? (ours-who? (:who manual)))))

(defn parse-message
  "{:driving? :manual :expires-at} from a postMessage {type: 'drive', driving, manual, expiresAt} (keywordized), else nil."
  [data]
  (when (and (map? data) (= "drive" (:type data)))
    {:driving? (boolean (:driving data)) :manual (:manual data) :expires-at (:expiresAt data)}))

(defn apply-message [drive msg now]
  (assoc (merge drive msg) :at now))

(defn apply-poll
  "A GET /drive reply's :manual (nil when nobody drives), seen at now."
  [drive manual now]
  (cond
    (nil? manual) (assoc drive :manual nil :expires-at nil :driving? false :at now)
    (ours-who? (:who manual)) (assoc drive :manual manual :expires-at (if (number? (:expiresAt manual)) (:expiresAt manual) (:expires-at drive)) :at now)
    :else (assoc drive :manual manual :driving? false :expires-at nil :at now)))

(defn countdown-text [expires-at now]
  (when (number? expires-at)
    (let [ms (- expires-at now)]
      (if (pos? ms) (str "auto-release in " (js/Math.ceil (/ ms 1000)) " s") "auto-release now"))))

(defn banner [{:keys [manual expires-at] :as drive} now]
  (cond
    (and (nil? manual) (not (:driving? drive))) {:kind :none}
    (driving-now? drive) {:kind :ours :text (str "MANUAL: you are driving (" (or (:who manual) me) ")") :countdown (countdown-text expires-at now)}
    :else {:kind :other :text (str "driven by " (:who manual) (when-not (str/blank? (:why manual)) (str ": " (:why manual))))}))

(defn take-request [] {:op "take" :who me :why "dashboard"})

(defn release-request [{:keys [manual]}]
  {:op "release" :who (if (ours-who? (:who manual)) (:who manual) me)})

(def hint "click the view to take control (WASD, space, shift, mouse; Esc releases)")
