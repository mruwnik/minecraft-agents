(ns dashboard.chat-test
  (:require [cljs.test :refer [are deftest is testing]]
            [dashboard.chat :as chat]))

(def t0 1790000000000)

(defn said
  ([t from message] (said t from message "said"))
  ([t from message kind]
   {:seq 1 :t t :source "chat" :kind kind :from from :message message :level "info"}))
(defn heard [agent & lines] {:agent agent :lines (vec lines)})

(def hello (said t0 "Jizo" "hello all"))
(def reply (said (+ t0 3000) "Steve" "hi Jizo"))

(deftest chat-heard-by-three-is-one-line
  (is (= [{:t t0 :from "Jizo" :to nil :kind "chat" :message "hello all"}]
         (chat/merge-chat [(heard "Chani" hello) (heard "Perrin" hello) (heard "Mariel" hello)] 200))))

(deftest copies-a-few-ms-apart-are-one-line-at-earliest-stamp
  (is (= [t0] (mapv :t (chat/merge-chat [(heard "Chani" (assoc hello :t (+ t0 3))) (heard "Perrin" hello)] 200)))))

(deftest same-words-a-minute-later-are-two-lines
  (let [again (assoc hello :t (+ t0 60000))]
    (is (= [t0 (+ t0 60000)] (mapv :t (chat/merge-chat [(heard "Chani" hello again)] 200))))))

(deftest whisper-is-addressed-to-the-holding-body
  (is (= [{:t t0 :from "Jizo" :to "Pacer" :kind "whisper" :message "done"}]
         (chat/merge-chat [(heard "Pacer" (said t0 "Jizo" "done" "whisper"))] 200))))

(deftest whisper-to-two-bodies-is-two-lines
  (let [psst (said t0 "Jizo" "done" "whisper")]
    (is (= ["Chani" "Pacer"] (mapv :to (chat/merge-chat [(heard "Pacer" psst) (heard "Chani" psst)] 200))))))

(deftest ordered-by-time-across-files
  (is (= ["Jizo" "Steve"] (mapv :from (chat/merge-chat [(heard "Chani" reply) (heard "Perrin" hello reply)] 200)))))

(deftest limit-keeps-the-newest
  (let [many (mapv #(said (+ t0 (* 1000 %)) "Jizo" (str "line " %)) (range 5))]
    (is (= ["line 3" "line 4"] (mapv :message (chat/merge-chat [{:agent "Chani" :lines many}] 2))))))

(deftest noise-is-skipped
  (doseq [[what line]
          [["not talk" {:seq 2 :t t0 :source "body" :kind "hurt"}]
           ["a chat refusal" {:seq 2 :t t0 :source "chat" :kind "refused" :from "Jizo" :message "x"}]
           ["no time" {:seq 2 :source "chat" :kind "said" :from "Jizo" :message "when?"}]
           ["no sender" {:seq 2 :t t0 :source "chat" :kind "said" :message "who?"}]
           ["not a map" "garbage"]
           ["nil" nil]]]
    (testing what
      (is (= ["hello all"] (mapv :message (chat/merge-chat [(heard "Chani" line hello)] 200)))))))

(deftest nothing-heard-is-empty
  (is (= [] (chat/merge-chat [(heard "Chani") (heard "Perrin")] 200))))

(deftest legacy-lines-are-normalised-to-epoch-ms
  (is (= [{:t (js/Date.parse "2026-09-24T16:36:49.053Z") :from "Jizo" :to nil :kind "chat" :message "hello all"}]
         (chat/merge-chat [(heard "Chani" {:t "2026-09-24T16:36:49.053Z" :type "chat" :from "Jizo" :message "hello all"})] 200))))

(deftest chat-limit-cases
  (doseq [[what raw expected]
          [["default when nothing asked" nil 200]
           ["what is asked" "50" 50]
           ["capped at 1000" "5000" 1000]
           ["default for nonsense" "lots" 200]
           ["default for zero" "0" 200]
           ["default for negative" "-3" 200]]]
    (testing what
      (is (= expected (chat/chat-limit raw))))))

(defn heard-real [t from message] {:source "body" :kind "chat" :from from :message message :t t :body "ProbeWater" :level "info" :seq 3})
(defn said-by-body [t args] {:source "action" :kind "started" :name "chat" :args args :t t :body "ProbeWater" :level "debug" :seq 4})

(deftest real-body-chat-events
  (is (= [{:t t0 :from "Ann" :to nil :kind "chat" :message "hi"}]
         (chat/merge-chat [(heard "A" (heard-real t0 "Ann" "hi")) (heard "B" (heard-real (+ t0 3) "Ann" "hi"))] 200))))

(deftest body-own-chat-is-from-the-body
  (is (= [{:t t0 :from "Pacer" :to nil :kind "chat" :message "yo"}
          {:t (+ t0 1000) :from "Pacer" :to "Ann" :kind "whisper" :message "psst"}]
         (chat/merge-chat [(heard "Pacer" (said-by-body t0 {:message "yo"})
                                  (said-by-body (+ t0 1000) {:message "psst" :to "Ann"}))] 200))))

(deftest other-body-events-are-not-chat
  (is (= [] (chat/merge-chat [(heard "A" {:source "body" :kind "spawned" :from "Ann" :t t0}
                                     {:source "action" :kind "started" :name "moveTo" :args {} :t t0}
                                     {:source "action" :kind "done" :name "chat" :t t0})] 200))))

(deftest maybe-talk-line-keeps-every-line-talk-accepts
  (are [line expected] (= expected (boolean (chat/maybe-talk-line? line)))
    "{\"source\":\"body\",\"kind\":\"chat\",\"from\":\"a\"}" true
    "{\"source\":\"chat\",\"kind\":\"said\"}" true
    "{\"source\": \"chat\", \"kind\": \"whisper\"}" true
    "{\"type\":\"chat\",\"t\":\"2026-01-01\"}" true
    "{\"type\":\"whisper\"}" true
    "{\"source\":\"action\",\"kind\":\"started\",\"name\":\"chat\"}" true
    "{\"source\":\"body\",\"kind\":\"position\",\"text\":\"chat\"}" false
    "{\"source\":\"job\",\"kind\":\"round_started\"}" false))
