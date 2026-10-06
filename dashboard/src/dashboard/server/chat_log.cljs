(ns dashboard.server.chat-log
  "The chat lines read from each body's event files, for /api/chat."
  (:require [dashboard.chat :as chat]
            [dashboard.chat-send :as chat-send]
            [dashboard.engine-events :as ee]
            ["fs" :as fs]
            [dashboard.server.files :refer [chat-keep chat-tail-bytes reduce-lines]]
            [dashboard.server.engine-state :refer [agent-entries agent-names body-key canonical-events-file engine-folder? tails]]))

;; ---------------------------------------------------------------- chat
(defn edn-talk-lines-of [text]
  (into [] (comp (filter chat/maybe-edn-talk-line?) (keep chat/edn-talk-line)) (.split text "\n")))

;; First sight of a file: its last chat-tail-bytes. After that only the bytes appended since (a line still being
;; written is carried to the next read); a file that shrank starts over. Position events flood the file, so a small
;; tail would forget a chat line within minutes. Each chunk is reduced to its chat lines at once.
(defn read-chat-tail [file lines-of cached]
  (let [size (.-size (.statSync fs file))
        fresh? (or (nil? cached) (< size (:size cached)))
        start (if fresh? (max 0 (- size chat-tail-bytes)) (:size cached))
        keep-last (fn [lines text] (vec (take-last chat-keep (into lines (lines-of text)))))
        {:keys [acc rest]} (reduce-lines file start size (and fresh? (pos? start))
                                         (if fresh? (js/Uint8Array. 0) (:rest cached))
                                         keep-last (if fresh? [] (:lines cached)))]
    {:size size :rest rest :lines acc}))

(defn chat-lines [body]
  (let [file (canonical-events-file body)
        size (.-size (.statSync fs file))
        k (body-key body)
        cached (get @tails k)]
    (if (= size (:size cached))
      (:lines cached)
      (let [tail (read-chat-tail file edn-talk-lines-of cached)]
        (swap! tails assoc k tail)
        (:lines tail)))))

(def chat-sender (chat-send/configured-sender (.-DASHBOARD_CHAT_AS js/process.env)))

(defn chat-log [limit world-name]
  (let [agents (ee/parse-engine-agents (filter #(and (= world-name (:world %)) (engine-folder? %)) (agent-entries)))]
    {:at (js/Date.now)
     :sender chat-sender
     :agents (agent-names agents)
     :messages (chat/merge-chat (mapv (fn [a] {:agent (:name a) :lines (try (chat-lines a) (catch :default _ []))}) agents) limit)}))
