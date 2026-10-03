(ns dashboard.chat-send-test
  (:require [cljs.test :refer [deftest are is]]
            [dashboard.chat-send :as cs]))

(defn payload [command] (js->clj (js/JSON.parse (subs command (count "tellraw @a ")))))

(deftest sender-names
  (are [s ok] (= ok (cs/valid-sender? s))
    "Dan" true, "a_b9" true, "" false, "a b" false, (apply str (repeat 17 "x")) false, "Dan\"}" false, nil false))

(deftest clean-text
  (are [in out] (= out (cs/clean-text in))
    "hi there" "hi there"
    "a\nb\r\nc" "a b c"
    "§cred§r text" "red text"
    "  spaced  " "spaced"
    "a\u0000b\u007fc\u001b" "abc"
    (apply str (repeat 300 "x")) (apply str (repeat 256 "x"))
    "こんにちは" "こんにちは"
    nil ""))

(deftest command-shape
  (are [text expected] (= expected (payload (cs/command "Dan" text)))
    "hi" {"text" "<Dan> hi"}
    "say \"hi\"" {"text" "<Dan> say \"hi\""}
    "a\\b\\" {"text" "<Dan> a\\b\\"}
    "\"},{\"text\":\"x" {"text" "<Dan> \"},{\"text\":\"x"})
  (is (= "tellraw @a " (subs (cs/command "Bob" "x") 0 11))))

(deftest plan-validation
  (are [body expected] (= expected (select-keys (cs/plan body {:sender "Dan" :stamps [] :now 1000}) [:status :json]))
    nil {:status 413 :json {:error "body exceeds 4096 bytes"}}
    "nope" {:status 400 :json {:error "body must be JSON"}}
    "[1]" {:status 400 :json {:error "body must be a JSON object"}}
    "{}" {:status 400 :json {:error "text must be a non-empty string"}}
    "{\"text\":\"  \"}" {:status 400 :json {:error "text must be a non-empty string"}}
    "{\"text\":\"§c\"}" {:status 400 :json {:error "text must be a non-empty string"}}
    "{\"text\":5}" {:status 400 :json {:error "text must be a non-empty string"}}
    "{\"text\":\"x\",\"target\":\"@a\"}" {:status 400 :json {:error "target is not accepted"}}
    "{\"text\":\"x\",\"target\":\"@e\"}" {:status 400 :json {:error "target is not accepted"}}
    "{\"text\":\"x\",\"from\":\"Root\"}" {:status 400 :json {:error "unexpected field: from"}}
    "{\"text\":\"hi\"}" {}))

(deftest plan-accepts
  (is (= {:command "tellraw @a {\"text\":\"<Dan> hi\"}" :stamps [1000]}
         (select-keys (cs/plan "{\"text\":\"hi\"}" {:sender "Dan" :stamps [] :now 1000}) [:command :stamps]))))

(deftest rate-limit
  (are [stamps now ok] (= ok (nil? (:status (cs/plan "{\"text\":\"hi\"}" {:sender "Dan" :stamps stamps :now now}))))
    [] 100000 true
    [99500] 100000 false            ; under 1 s since the last
    [99000] 100000 true             ; exactly 1 s
    [90000 91000 92000 93000 94000] 100000 false   ; 5 in 30 s
    [60000 91000 92000 93000 94000] 100000 true    ; one fell out of the window
    [] 0 true)
  (is (= {:status 429 :json {:error "rate limit: 1 per second, 5 per 30 seconds"}}
         (select-keys (cs/plan "{\"text\":\"hi\"}" {:sender "Dan" :stamps [99900] :now 100000}) [:status :json])))
  (is (= [99900] (:stamps (cs/plan "{\"text\":\"hi\"}" {:sender "Dan" :stamps [99900] :now 100000})))))

(deftest rejected-requests-do-not-use-the-budget
  (is (= [99000] (:stamps (cs/plan "{}" {:sender "Dan" :stamps [99000] :now 100000})))))
