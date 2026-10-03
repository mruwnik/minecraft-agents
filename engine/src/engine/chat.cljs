(ns engine.chat
  "Chat limits as data, enforced in engine.core/act! for every :chat act (gate!),
  plus the pure splitting and the say! convenience for jobs. See README.md, ctx.

  Vanilla kicks for spam above 200 points (20 per line or command, 1 off per
  tick): 10 lines in a burst, or over 1 line/s sustained. The limits stay far
  below that: a gap between lines and a cap per window."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]))

(def limits {:gap-ms 1000 :max 5 :window-ms 30000 :max-chars 256})

(def sentence-ends [". " "! " "? " "; "])

;; ------------------------------------------------------------------ limits

(defn recent
  "The timestamps of sent-times still inside the window."
  [{:keys [window-ms]} sent-times now]
  (filterv #(< (- now %) window-ms) sent-times))

(defn allow
  "{:ok true}, or {:ok false :status :reason ...} when n more lines do not fit:
  cannot/too-long when n exceeds the window's capacity, blocked/rate with
  :retry-ms (until n lines fit) otherwise."
  [{:keys [max window-ms] :as lim} sent-times now n]
  (let [kept (recent lim sent-times now)
        overflow (- (+ (count kept) n) max)]
    (cond
      (> n max) {:ok false :status "cannot" :reason "too-long"}
      (<= overflow 0) {:ok true}
      :else {:ok false :status "blocked" :reason "rate"
             :retry-ms (- (+ (nth kept (dec overflow)) window-ms) now)})))

(defn gap-left
  "Ms until the gap since the last line has passed."
  [{:keys [gap-ms]} sent-times now]
  (if (empty? sent-times)
    0
    (js/Math.max 0 (- (+ (peek sent-times) gap-ms) now))))

;; ------------------------------------------------------------------ text

(defn clean
  "Control characters become spaces, the section sign (formatting code) is dropped."
  [text]
  (-> (str text)
      (str/replace #"[\x00-\x1f\x7f]" " ")
      (str/replace "§" "")
      str/trim))

(defn cut-at
  "How many characters of text make the next piece under budget."
  [text budget]
  (if (<= (count text) budget)
    (count text)
    (let [head (subs text 0 (inc budget))
          sentence (apply max (map #(.lastIndexOf head %) sentence-ends))
          space (.lastIndexOf head " ")]
      (cond
        (pos? sentence) (inc sentence)
        (pos? space) space
        :else budget))))

(defn split
  "text in pieces of at most budget characters, cut at a sentence end, else the
  last space, else hard; trimmed, none empty."
  [text budget]
  (loop [rest (str/trim (str text)) out []]
    (if (empty? rest)
      out
      (let [n (cut-at rest budget)]
        (recur (str/trim (subs rest n)) (conj out (str/trim (subs rest 0 n))))))))

(defn parts
  "{:parts [line ...]} for text, to a player (a whisper) or all; or
  {:status \"cannot\" :reason \"command\"} when the text or any part starts with a slash."
  [text to]
  (let [cleaned (clean text)
        budget (if to (- (:max-chars limits) (count (str "/tell " to " "))) (:max-chars limits))
        ps (split cleaned budget)]
    (if (or (str/starts-with? cleaned "/") (some #(str/starts-with? % "/") ps))
      {:status "cannot" :reason "command"}
      {:parts ps})))

;; ------------------------------------------------------------------ enforcement

(defn sleep [ms] (js/Promise. (fn [resolve _] (js/setTimeout resolve ms))))

(defn ^:async gate!
  "The act-boundary check for one :chat line; args is the JS object handed to the
  act. Blocked lines never reach the primitive. Waits for the gap, calls the
  primitive, records the time only when it was sent. Returns the primitive's result."
  [eng p token args]
  (let [lim (get eng :chat-limits limits)
        now (:now eng)
        said (:said eng)
        _ (swap! said #(recent lim % (now)))
        verdict (allow lim @said (now) 1)]
    (if-not (:ok verdict)
      #js {:status (:status verdict) :reason (:reason verdict) :retryMs (:retry-ms verdict)}
      (let [wait (gap-left lim @said (now))
            _ (when (pos? wait) (await (sleep wait)))
            r (await (.call (aget p "chat") p token args))]
        (when (= "sent" (.-status r)) (swap! said conj (now)))
        r))))

(defn ^:async say!
  "Say message to all, or to (:to opts): split into lines, one :chat act each with
  a wait between. Stops at the first line not sent. {:status \"sent\" :parts n},
  or {:status :reason :parts sent-so-far}. Holds no state; gate! enforces the limits."
  [c message opts]
  (let [to (:to opts)
        {ps :parts :as split-result} (parts message to)]
    (if-not ps
      split-result
      (loop [todo ps n 0]
        (if (empty? todo)
          {:status "sent" :parts n}
          (let [_ (when (pos? n) (await (ctx/act c :wait #js {:ms 1000})))
                r (await (ctx/act c :chat #js {:message (first todo) :to to}))]
            (if (= "sent" (.-status r))
              (recur (rest todo) (inc n))
              {:status (.-status r) :reason (.-reason r) :parts n})))))))
