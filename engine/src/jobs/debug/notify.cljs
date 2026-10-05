(ns jobs.debug.notify
  (:require [clojure.string :as str]
            [engine.chat :as chat]
            [engine.ctx :as ctx]))

(def doc
  "For testing triggers: does nothing but report. Register it against a trigger to see that the trigger fires
  without running the real job.
  Emits an info event job.notify with the text and a snapshot of sensing (health, food, oxygen, on-fire, position,
  time of day, nearby hostiles), and writes a :notify entry to body memory.
  With :chat? true it also sends the text with engine.chat/say! (commands are refused; the engine rate-limits
  chat). A chat that is not sent is reported as info notify.chat-failed and does not fail the job.")

(def args
  {:text {:doc "text to report" :default "notify"}
   :chat? {:doc "also send the text to game chat, when the primitives can" :default false}
   :to {:doc "whisper the text to this player instead of saying it to all" :default nil}})

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
  (let [{:keys [text chat? to]} (:args c)
        p (:primitives c)]
    (ctx/emit! c :job.notify :info {:text (str/join " | " [text (snapshot p)])
                                    :attention :notice})
    (ctx/remember! c :notify {:text text} notify-policy)
    (when (and chat? (some? (.-chat p)))
      (let [{:keys [status reason]} (await (chat/say! c text {:to to}))]
        (when (not= "sent" status)
          (ctx/emit! c :notify.chat-failed :info {:text (str "chat " status (when reason (str ": " reason)))
                                                  :status status :reason reason
                                                  :attention :notice}))))
    :done))
