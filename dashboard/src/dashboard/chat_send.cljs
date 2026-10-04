(ns dashboard.chat-send
  "Chat send, pure part: validate the request body, build the one fixed command (`tellraw @a <json>`), rate limit.
  Engine bodies hear it as a mineflayer chat event because mineflayer parses system messages shaped like `<name> text`."
  (:require [clojure.string :as str]
            [dashboard.guard :as guard]))

(def max-text 256)
(def sender-re #"^[A-Za-z0-9_]{1,16}$")
(def target-re #"^[A-Za-z0-9_]{3,16}$")
(def min-gap-ms 1000)
(def window-ms 30000)
(def window-max 5)

(def default-sender "dashboard")

(defn configured-sender
  "The sender the owner configured (env DASHBOARD_CHAT_AS), else the neutral label."
  [configured]
  (if (str/blank? configured) default-sender configured))

(defn valid-sender? [s] (and (string? s) (boolean (re-matches sender-re s))))

(defn valid-target? [s] (and (string? s) (boolean (re-matches target-re s))))

(def section-code (js/RegExp. "§[\\s\\S]" "gu"))

(defn clean-text [text]
  (->> (-> (str text)
           (.replace section-code "")
           (str/replace #"[\r\n\t]+" " ")
           (str/replace #"[\u0000-\u001f\u007f]" "")
           str/trim
           js/Array.from)
       (take max-text)
       (apply str)
       str/trim))

(defn command
  "The only command the dashboard sends. The text component is JSON, never concatenated."
  [sender text]
  (str "tellraw @a " (js/JSON.stringify (clj->js {:text (str "<" sender "> " (clean-text text))}))))

(defn whisper-command
  "tellraw to one named player, shaped as the vanilla whisper line so the body hears it from the sender."
  [sender target text]
  (str "tellraw " target " "
       (js/JSON.stringify (clj->js {:translate "commands.message.display.incoming"
                                    :with [{:text sender} {:text (clean-text text)}]
                                    :color "gray"
                                    :italic true}))))

(defn parse-body [body]
  (try (js->clj (js/JSON.parse body))
       (catch :default _ ::bad-json)))

(defn validate [body]
  (let [input (parse-body body)
        extras (when (map? input) (dissoc input "text"))]
    (cond
      (= ::bad-json input) {:error "body must be JSON"}
      (not (map? input)) {:error "body must be a JSON object"}
      (contains? input "target") {:error "target is not accepted"}
      (seq extras) {:error (str "unexpected field: " (first (sort (keys extras))))}
      (or (not (string? (get input "text"))) (str/blank? (clean-text (get input "text")))) {:error "text must be a non-empty string"}
      :else {:text (get input "text")})))

(defn within-window [stamps now] (filterv #(< (- now %) window-ms) stamps))

(defn rate-limited? [stamps now]
  (let [recent (within-window stamps now)]
    (or (>= (count recent) window-max)
        (boolean (some #(< (- now %) min-gap-ms) recent)))))

(defn plan
  "body is nil when over the limit. Returns {:status :json :stamps} for a refusal, {:command :stamps} to run."
  [body {:keys [sender stamps now]}]
  (let [refuse (fn [status error] {:status status :json {:error error} :stamps stamps})
        {:keys [text error]} (when (some? body) (validate body))]
    (cond
      (nil? body) (refuse 413 (str "body exceeds " guard/max-body-bytes " bytes"))
      error (refuse 400 error)
      (rate-limited? stamps now) (refuse 429 "rate limit: 1 per second, 5 per 30 seconds")
      :else {:command (command sender text) :stamps (conj (within-window stamps now) now)})))

(defn plan-whisper
  "Like plan, for one named body: known and online are sets of body names. Refuses with 400, 404 or 409 before the rate limit."
  [target body {:keys [sender stamps now known online]}]
  (let [refuse (fn [status error] {:status status :json {:error error} :stamps stamps})
        {:keys [text error]} (when (some? body) (validate body))]
    (cond
      (nil? body) (refuse 413 (str "body exceeds " guard/max-body-bytes " bytes"))
      error (refuse 400 error)
      (not (valid-target? target)) (refuse 400 "target is not a valid name")
      (not (contains? known target)) (refuse 404 (str "no body called " target))
      (not (contains? online target)) (refuse 409 (str target " is offline"))
      (rate-limited? stamps now) (refuse 429 "rate limit: 1 per second, 5 per 30 seconds")
      :else {:command (whisper-command sender target text) :stamps (conj (within-window stamps now) now)})))
