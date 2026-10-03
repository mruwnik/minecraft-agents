(ns dashboard.chat)

(def same-line-ms 2000)
(def chat-default 200)
(def chat-cap 1000)

(defn epoch-ms [t]
  (if (string? t) (js/Date.parse t) t))

;; Chat shapes: a body's engine event {source "body" kind "chat" from message} (what others said, heard by that
;; body), the older {source "chat" kind "said"|"whisper"}, {type "chat"|"whisper" t iso}, and the body's own
;; chat order {source "action" kind "started" name "chat" args {message to}} (from = the body itself).
(defn own-chat? [e]
  (and (= "action" (:source e)) (= "started" (:kind e)) (= "chat" (:name e))))

(defn talk-kind [e]
  (cond
    (and (= "body" (:source e)) (= "chat" (:kind e))) "chat"
    (and (= "chat" (:source e)) (= "said" (:kind e))) "chat"
    (and (= "chat" (:source e)) (= "whisper" (:kind e))) "whisper"
    (own-chat? e) (if (get-in e [:args :to]) "whisper" "chat")
    (#{"chat" "whisper"} (:type e)) (:type e)))

(defn talk-from [agent e] (if (own-chat? e) agent (:from e)))
(defn talk-to [agent e]
  (cond
    (own-chat? e) (get-in e [:args :to])
    (= "whisper" (talk-kind e)) agent))
(defn talk-message [e] (str (if (own-chat? e) (get-in e [:args :message]) (:message e))))

(defn talk? [e]
  (and (map? e) (talk-kind e)
       (let [t (epoch-ms (:t e))] (and (number? t) (not (js/Number.isNaN t))))
       (or (own-chat? e) (string? (:from e)))))

;; A cheap test on the raw line, run before JSON.parse: true for every line talk? could accept (and some others).
;; Position events flood the file; only lines naming chat or whisper in kind, source, type or name are parsed.
(def talk-marker #"\"(?:kind|source|type|name)\"\s*:\s*\"(?:chat|whisper)\"")
(defn maybe-talk-line? [line] (re-find talk-marker line))

(defn talk-line [agent e]
  {:t (epoch-ms (:t e))
   :from (talk-from agent e)
   :to (talk-to agent e)
   :kind (talk-kind e)
   :message (talk-message e)})

(defn identity-of [m] [(:from m) (:to m) (:message m)])

(defn by-time [a b]
  (let [c (compare (:t a) (:t b))]
    (if (zero? c) (compare (or (:to a) "") (or (:to b) "")) c)))

;; A body logs what OTHERS say: the same chat sits in every online body's file, while a whisper is only in its
;; recipient's file. Two lines with the same identity within same-line-ms are one line (kept at its earliest stamp).
(defn merge-chat [per-agent limit]
  (let [lines (->> per-agent
                   (mapcat (fn [{:keys [agent lines]}] (map #(talk-line agent %) (filter talk? lines))))
                   (sort by-time))
        step (fn [{:keys [seen out]} m]
               (let [before (get seen (identity-of m))
                     dup? (and before (<= (- (:t m) before) same-line-ms))]
                 {:seen (assoc seen (identity-of m) (:t m))
                  :out (if dup? out (conj out m))}))]
    (vec (take-last limit (:out (reduce step {:seen {} :out []} lines))))))

(defn chat-limit [raw]
  (let [n (js/Number raw)]
    (if (and (js/Number.isInteger n) (pos? n)) (min n chat-cap) chat-default)))
