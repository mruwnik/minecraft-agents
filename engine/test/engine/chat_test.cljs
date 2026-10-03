(ns engine.chat-test
  "engine.chat: limits as data, the pure allow/split, gate! at the act boundary, say! for jobs."
  (:require [cljs.test :refer [deftest is are async]]
            [clojure.string :as str]
            [engine.chat :as chat]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def sentence "This is a sentence of moderate length. ")
(def long-text (str/trim (apply str (repeat 20 sentence))))
(def no-gap (assoc chat/limits :gap-ms 0))

(defn setup [limits]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake {:entities [{:kind "player" :username "Steve" :name "Steve" :pos {:x 2 :y 64 :z 0}}]})
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng (assoc eng :chat-limits limits) :p p :seen seen :clock clock}))

(defn chat-lines [p] (mapv #(.-message %) (.. p -world -state -chat)))

(defn ^:async run-job
  "Run job-def as a one-round top-level job; the value its round returns."
  [{:keys [eng]} round]
  (let [out (atom nil)
        job {:check (constantly true)
             :round (fn ^:async r [c] (reset! out (await (round c))) :done)}
        eng (assoc eng :jobs (assoc (:jobs eng) 'chat-job job))]
    (core/submit! eng '(chat-job) {})
    (await (core/tick! eng))
    @out))

;; ------------------------------------------------------------------ split / clean

(deftest split-cases
  (are [name text budget want] (= want (chat/split text budget))
    "short" "hello there" 50 ["hello there"]
    "sentence end" "One two. Three four five" 15 ["One two." "Three four five"]
    "space" "aaa bbb ccc ddd" 8 ["aaa bbb" "ccc ddd"]
    "hard" "abcdefghij" 4 ["abcd" "efgh" "ij"]
    "semicolon" "ab; cd ef gh" 6 ["ab;" "cd ef" "gh"]))

(deftest split-pieces-are-within-budget-and-never-empty
  (doseq [budget [10 37 100]]
    (let [ps (chat/split long-text budget)]
      (is (every? #(and (pos? (count %)) (<= (count %) budget) (= % (str/trim %))) ps) (str "budget " budget)))))

(deftest clean-strips-control-characters-and-the-section-sign
  (are [text want] (= want (chat/clean text))
    "a\nb" "a b"
    "  hi\u0000there\u007f " "hi there"
    "§cred" "cred"
    "\n\n" ""
    "/say x" "/say x"))

(deftest parts-refuse-commands
  (are [text to] (= {:status "cannot" :reason "command"} (chat/parts text to))
    "/op me" nil
    "/op me" "Steve"
    "\n/stop" nil
    "§/stop" "Steve"
    (str (apply str (repeat 254 "a")) " /tell Someone hi") nil
    (str (apply str (repeat 244 "a")) " /op me") "Steve"))

(deftest newlines-in-the-message-are-spaces
  (is (= {:parts ["a /b"]} (chat/parts "a\n/b" nil))))

(deftest no-public-part-ever-begins-with-a-slash-across-split-messages
  (doseq [m [long-text (apply str (repeat 120 "word ")) (apply str (repeat 500 "x"))
             (str (apply str (repeat 100 "a")) ". " (apply str (repeat 200 "b")))]]
    (is (every? #(not (str/starts-with? % "/")) (:parts (chat/parts m nil))))))

(deftest a-long-text-gives-several-parts-cut-at-sentence-ends
  (let [ps (:parts (chat/parts long-text nil))]
    (is (> (count ps) 1))
    (is (every? #(and (<= (count %) 256) (str/ends-with? % ".")) ps))))

(deftest the-whisper-budget-is-smaller-than-the-chat-budget
  (let [text (apply str (repeat 256 "x"))]
    (is (= 1 (count (:parts (chat/parts text nil)))))
    (let [ps (:parts (chat/parts text "Steve"))]
      (is (= 2 (count ps)))
      (is (every? #(<= (count %) (- 256 (count "/tell Steve "))) ps)))))

(deftest every-whisper-part-fits-the-budget-with-a-long-name
  (let [ps (:parts (chat/parts (apply str (repeat 100 "word ")) "Steve_1234567890"))]
    (is (> (count ps) 1))
    (is (every? #(<= (count (str "/tell Steve_1234567890 " %)) 256) ps))))

;; ------------------------------------------------------------------ allow

(deftest five-single-lines-in-30-s-pass-and-the-sixth-is-blocked-with-retry-ms
  (let [times (mapv #(+ 1000 (* 1000 %)) (range 5))]
    (is (= {:ok true} (chat/allow chat/limits (subvec times 0 4) 5000 1)))
    (is (= {:ok false :status "blocked" :reason "rate" :retry-ms 25000} (chat/allow chat/limits times 6000 1)))
    (is (= {:ok true} (chat/allow chat/limits times 31000 1)))))

(deftest more-lines-than-the-window-holds-is-too-long
  (is (= {:ok false :status "cannot" :reason "too-long"} (chat/allow chat/limits [] 0 6))))

(deftest lines-that-do-not-fit-the-window-right-now-are-blocked
  (is (= {:ok false :status "blocked" :reason "rate" :retry-ms 30000} (chat/allow chat/limits [0 0 0] 0 3))))

(deftest allow-counts-only-lines-inside-the-window
  (is (= [true false] (mapv #(:ok (chat/allow chat/limits [0 0 0 0] 0 %)) [1 2])))
  (is (true? (:ok (chat/allow chat/limits [0 0 0 0] 30000 5)))))

(deftest gap-left-is-time-until-the-gap-since-the-last-line-has-passed
  (are [times now want] (= want (chat/gap-left chat/limits times now))
    [] 5 0
    [100 400] 500 900
    [100 400] 1400 0))

;; ------------------------------------------------------------------ gate!

(deftest gate-blocks-the-sixth-line-without-calling-the-primitive
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup no-gap)
              _ (.setOwner p 1)
              rs (loop [i 0 acc []]
                   (if (= i 6) acc (recur (inc i) (conj acc (await (chat/gate! eng p 1 #js {:message (str "m" i)}))))))]
          (is (= ["sent" "sent" "sent" "sent" "sent" "blocked"] (mapv #(.-status %) rs)))
          (is (= "rate" (.-reason (peek rs))))
          (is (pos? (.-retryMs (peek rs))))
          (is (= 5 (count (chat-lines p))))
          (is (= 5 (count @(:said eng)))))))))

(deftest gate-does-not-record-a-line-that-was-not-sent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup no-gap)
              _ (.setOwner p 1)
              r (await (chat/gate! eng p 1 #js {:message "/op me"}))]
          (is (= "cannot" (.-status r)))
          (is (empty? @(:said eng))))))))

(deftest a-job-acting-chat-directly-six-times-is-blocked-on-the-sixth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup no-gap)
              rs (await (run-job s (fn ^:async burst [c]
                                     (loop [i 0 acc []]
                                       (if (= i 6)
                                         acc
                                         (recur (inc i) (conj acc (await (ctx/act c :chat #js {:message (str "m" i)})))))))))]
          (is (= ["sent" "sent" "sent" "sent" "sent" "blocked"] (mapv #(.-status %) rs)))
          (is (= "rate" (.-reason (peek rs))))
          (is (= 5 (count (chat-lines (:p s))))))))))

(deftest gate-waits-out-the-gap-between-lines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc chat/limits :gap-ms 80))
              clock (atom (js/Date.now))
              _ (.setOwner p 1)
              eng (assoc eng :now #(deref clock))
              _ (await (chat/gate! eng p 1 #js {:message "a"}))
              t0 (js/Date.now)
              _ (await (chat/gate! eng p 1 #js {:message "b"}))]
          (is (>= (- (js/Date.now) t0) 70)))))))

;; ------------------------------------------------------------------ say!

(deftest say-splits-a-long-message-with-waits-between-parts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup no-gap)
              r (await (run-job s (fn ^:async f [c] (chat/say! c long-text {}))))
              lines (chat-lines (:p s))
              waits (filterv #(= "wait" (.-name %)) (.-calls (.. (:p s) -world)))]
          (is (= {:status "sent" :parts (count lines)} r))
          (is (> (count lines) 1))
          (is (= (dec (count lines)) (count waits))))))))

(deftest say-whispers-to-a-player
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup no-gap)
              r (await (run-job s (fn ^:async f [c] (chat/say! c "psst" {:to "Steve"}))))]
          (is (= "sent" (:status r)))
          (is (= ["psst"] (chat-lines (:p s)))))))))

(deftest say-refuses-a-later-part-starting-with-a-slash-and-acts-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup no-gap)
              r (await (run-job s (fn ^:async f [c] (chat/say! c (str (apply str (repeat 254 "a")) " /tell Someone hi") {}))))]
          (is (= {:status "cannot" :reason "command"} r))
          (is (empty? (chat-lines (:p s)))))))))

(deftest say-stops-at-the-first-line-that-is-not-sent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup no-gap)
              _ (doseq [i (range 4)] (swap! (:said (:eng s)) conj 1000000))
              r (await (run-job s (fn ^:async f [c] (chat/say! c (apply str (repeat 600 "x")) {}))))]
          (is (= "blocked" (:status r)))
          (is (= "rate" (:reason r)))
          (is (= 1 (:parts r)))
          (is (= 1 (count (chat-lines (:p s))))))))))
