(ns dashboard.chat)

(def same-line-ms 2000)
(def chat-default 200)
(def chat-cap 1000)

(defn epoch-ms [t]
  (if (string? t) (js/Date.parse t) t))

;; An engine event {source "chat" kind "said"|"whisper" from message} or an old-style {type "chat"|"whisper" t iso}.
(defn talk-kind [e]
  (cond
    (and (= "chat" (:source e)) (= "said" (:kind e))) "chat"
    (and (= "chat" (:source e)) (= "whisper" (:kind e))) "whisper"
    (#{"chat" "whisper"} (:type e)) (:type e)))

(defn talk? [e]
  (and (map? e) (talk-kind e)
       (let [t (epoch-ms (:t e))] (and (number? t) (not (js/Number.isNaN t))))
       (string? (:from e))))

(defn talk-line [agent e]
  (let [kind (talk-kind e)]
    {:t (epoch-ms (:t e))
     :from (:from e)
     :to (when (= "whisper" kind) agent)
     :kind kind
     :message (str (:message e))}))

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
