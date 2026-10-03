(ns jobs.debug.notify
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]))

(def doc
  "For testing triggers: does nothing but report. Emits an info event of kind
  job.notify with the text and a snapshot of sensing (health, food, oxygen,
  on-fire, position, time of day, nearby hostiles), writes a :notify entry to
  body memory, and sends the text with the chat primitive when :chat? is true
  (it refuses commands and rate-limits to 1 line/s and 5 per 30 s, returning
  blocked or rate past that; a chat that is not sent is reported as an info
  notify.chat-failed and does not fail the job). Register it against a trigger
  to see that the trigger fires without running the real job.")

(def args
  {:text {:doc "text to report" :default "notify"}
   :chat? {:doc "also send the text to game chat, when the primitives can" :default false}})

(def hostile-radius 16)

(def notify-policy {:cap 20 :ttl 600000})

(defn snapshot
  "Compact sensing text for debugging triggers."
  [p]
  (let [s (.self p)
        pos (.-pos s)
        hostiles (count (.entities p #js {:radius hostile-radius :kind "hostile"}))]
    (str "health " (.-health s) ", food " (.-food s) ", oxygen " (.-oxygen s)
         ", on-fire " (boolean (.-onFire s))
         ", pos " (js/Math.round (.-x pos)) " " (js/Math.round (.-y pos)) " " (js/Math.round (.-z pos))
         ", time " (.-timeOfDay s) ", hostiles " hostiles)))

(defn check [_c] true)

(defn ^:async round [c]
  (let [{:keys [text chat?]} (:args c)
        p (:primitives c)]
    (ctx/emit! c :job.notify :info {:text (str/join " | " [text (snapshot p)])})
    (ctx/remember! c :notify {:text text} notify-policy)
    (when (and chat? (some? (.-chat p)))
      (let [r (await (ctx/act c :chat (clj->js {:message text})))
            status (.-status r)]
        (when (not= "sent" status)
          (ctx/emit! c :notify.chat-failed :info {:text (str "chat " status (when (.-reason r) (str ": " (.-reason r))))
                                                  :status status :reason (.-reason r)}))))
    :done))
