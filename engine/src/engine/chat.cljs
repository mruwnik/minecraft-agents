(ns engine.chat
  "Chat limits as data, enforced for job and direct chat by one serialized gate!,
  plus splitting and say! for jobs. See README.md, ctx.

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

(defonce chat-states (js/WeakMap.))
(defonce direct-busy (js/WeakSet.))

(defn- enqueue!
  "Serialize every sender for one engine so rate checks and sends are atomic."
  [eng f]
  (let [state (or (.get chat-states eng) #js {:tail (js/Promise.resolve nil) :pending 0})
        _ (.set chat-states eng state)
        _ (set! (.-pending state) (inc (.-pending state)))
        result (-> (.-tail state)
                   (.catch (fn [_] nil))
                   (.then (fn [_] (f)))
                   (.finally (fn [] (set! (.-pending state) (dec (.-pending state))))))]
    (set! (.-tail state) result)
    result))

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
(declare send-gated!)

(defn ^:async gate!
  "The act-boundary check for one :chat line; args is the JS object handed to the
  act. Blocked lines never reach the primitive. Waits for the gap, calls the
  primitive, records the time only when it was sent. Returns the primitive's result."
  [eng p token args]
  (await (enqueue! eng #(send-gated! eng (fn [t a] (.call (aget p "chat") p t a)) token args))))

(defn ^:async send-gated!
  "One serialized, rate-limited send through `send-line`, shared by jobs and the direct chat API."
  [eng send-line token args]
  (let [lim (get eng :chat-limits limits)
        now (:now eng)
        said (:said eng)
        _ (swap! said #(recent lim % (now)))
        verdict (allow lim @said (now) 1)]
    (if-not (:ok verdict)
      #js {:status (:status verdict) :reason (:reason verdict) :retryMs (:retry-ms verdict)}
      (let [wait (gap-left lim @said (now))
            _ (when (pos? wait) (await (sleep wait)))
            r (await (send-line token args))]
        (when (contains? #{"sent" "failed"} (.-status r)) (swap! said conj (now)))
        r))))

(defn ^:async direct!
  "Fast control-adjacent chat; shares limits with job chat and never acquires the body lease."
  [eng message to]
  (let [cleaned (clean message)
        to-valid? (or (nil? to) (and (string? to) (re-matches #"[A-Za-z0-9_]{3,16}" to)))
        budget (if to (- (:max-chars limits) (count (str "/tell " to " "))) (:max-chars limits))
        invalid (cond
                  (not to-valid?) {:status "cannot" :reason "bad-name"}
                  (empty? cleaned) {:status "cannot" :reason "empty"}
                  (str/starts-with? cleaned "/") {:status "cannot" :reason "command"}
                  (> (count cleaned) budget) {:status "cannot" :reason "too-long"})]
    (if invalid
      invalid
      (let [state (.get chat-states eng)
            pending (if state (.-pending state) 0)]
        (if (or (pos? pending) (.has direct-busy eng))
          {:status "blocked" :reason "busy"}
          (do
            (.add direct-busy eng)
            (try
              (await
               (enqueue! eng
                 (fn []
                   (let [p (:primitives eng)]
                     (send-gated! eng (fn [_ a] (.call (aget p "chatDirect") p a))
                                  nil #js {:message cleaned :to to})))))
              (finally (.delete direct-busy eng)))))))))

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
