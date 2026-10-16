(ns jobs.debug.notify
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string :as str]
            [engine.chat :as chat]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.ready :as ready]
            [jobs.lib.combat :as combat]))

(def doc
  "For testing triggers: does nothing but report. Register it against a trigger to see that the trigger fires
  without running the real job.
  Emits an info event job.notify with the text and a snapshot of sensing (health, food, oxygen, on-fire, position,
  time of day, nearby hostiles), and writes a :notify entry to body memory.
  With :chat? true it also sends the text with engine.chat/say! (commands are refused; the engine rate-limits
  chat). A chat that is not sent is reported as info notify.chat-failed and does not fail the job.")

(a/defargs args
  {:text {:doc "text to report" :spec string? :default "notify"}
   :chat? {:doc "also send the text to game chat, when the primitives can" :spec boolean? :default false}
   :to {:doc "whisper the text to this player instead of saying it to all" :spec a/name? :default nil}})

(a/defargs settings
  {::job-time-s {:default 10 :doc "Seconds one report may take." :spec (a/int-in 1 nil)}
   ::hostile-radius {:default 16 :doc "Blocks within which a hostile mob is reported." :spec (a/int-in 0 nil)}})
(defn hostile-radius [] (settings/get settings ::hostile-radius))

(def notify-policy {:cap 20 :ttl 600000})

(defn snapshot
  "Compact sensing text for debugging triggers."
  [p]
  (let [s (.self p)
        pos (.-pos s)
        hostiles (count (combat/known-or-raw p (hostile-radius)))]
    (str "health " (.-health s) ", food " (.-food s) ", oxygen " (.-oxygen s)
         ", on-fire " (boolean (.-onFire s))
         ", pos " (js/Math.round (.-x pos)) " " (js/Math.round (.-y pos)) " " (js/Math.round (.-z pos))
         ", time " (.-timeOfDay s) ", hostiles " hostiles)))

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(defn done?
  "Never done before it runs: a report has no state that can hold before it."
  [_c]
  (checks/goal false nil [:unsent "the report is not sent"] {:units :reports :done 0 :left 1}))


(defn quote-of
  "Time of the chat, when it will be sent (the chat act's own bound is 3 s)."
  [c]
  {:uses {:job-time [0 0 (if (:chat? (:args c)) 3 1)]}})

(def safe? ready/ok)
(def fit? ready/ok)

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
    (ctx/end c :done nil)))
