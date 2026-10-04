(ns dashboard.chat-send-test
  (:require [cljs.test :refer [deftest are is]]
            [dashboard.chat-send :as cs]))

(defn payload [command] (js->clj (js/JSON.parse (subs command (count "tellraw @a ")))))

(deftest sender-names
  (are [s ok] (= ok (cs/valid-sender? s))
    "Ann" true, "a_b9" true, "" false, "a b" false, (apply str (repeat 17 "x")) false, "Ann\"}" false, nil false))

(deftest default-sender
  (are [configured expected] (= expected (cs/configured-sender configured))
    nil "dashboard", "" "dashboard", "Ann" "Ann")
  (is (cs/valid-sender? (cs/configured-sender nil))))

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
  (are [text expected] (= expected (payload (cs/command "Ann" text)))
    "hi" {"text" "<Ann> hi"}
    "say \"hi\"" {"text" "<Ann> say \"hi\""}
    "a\\b\\" {"text" "<Ann> a\\b\\"}
    "\"},{\"text\":\"x" {"text" "<Ann> \"},{\"text\":\"x"})
  (is (= "tellraw @a " (subs (cs/command "Bob" "x") 0 11))))

(deftest plan-validation
  (are [body expected] (= expected (select-keys (cs/plan body {:sender "Ann" :stamps [] :now 1000}) [:status :json]))
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
  (is (= {:command "tellraw @a {\"text\":\"<Ann> hi\"}" :stamps [1000]}
         (select-keys (cs/plan "{\"text\":\"hi\"}" {:sender "Ann" :stamps [] :now 1000}) [:command :stamps]))))

(deftest rate-limit
  (are [stamps now ok] (= ok (nil? (:status (cs/plan "{\"text\":\"hi\"}" {:sender "Ann" :stamps stamps :now now}))))
    [] 100000 true
    [99500] 100000 false            ; under 1 s since the last
    [99000] 100000 true             ; exactly 1 s
    [90000 91000 92000 93000 94000] 100000 false   ; 5 in 30 s
    [60000 91000 92000 93000 94000] 100000 true    ; one fell out of the window
    [] 0 true)
  (is (= {:status 429 :json {:error "rate limit: 1 per second, 5 per 30 seconds"}}
         (select-keys (cs/plan "{\"text\":\"hi\"}" {:sender "Ann" :stamps [99900] :now 100000}) [:status :json])))
  (is (= [99900] (:stamps (cs/plan "{\"text\":\"hi\"}" {:sender "Ann" :stamps [99900] :now 100000})))))

(deftest rejected-requests-do-not-use-the-budget
  (is (= [99000] (:stamps (cs/plan "{}" {:sender "Ann" :stamps [99000] :now 100000})))))

(defn whisper-payload [command target] (js->clj (js/JSON.parse (subs command (count (str "tellraw " target " "))))))

(deftest whisper-command-shape
  (are [text expected] (= expected (whisper-payload (cs/whisper-command "Ann" "Bob_1" text) "Bob_1"))
    "hi" {"translate" "commands.message.display.incoming" "with" [{"text" "Ann"} {"text" "hi"}] "color" "gray" "italic" true}
    "say \"hi\"" {"translate" "commands.message.display.incoming" "with" [{"text" "Ann"} {"text" "say \"hi\""}] "color" "gray" "italic" true}
    "a\\b\\" {"translate" "commands.message.display.incoming" "with" [{"text" "Ann"} {"text" "a\\b\\"}] "color" "gray" "italic" true}
    "§cred\nx" {"translate" "commands.message.display.incoming" "with" [{"text" "Ann"} {"text" "red x"}] "color" "gray" "italic" true})
  (is (= "tellraw Bob_1 " (subs (cs/whisper-command "Ann" "Bob_1" "x") 0 14))))

(deftest target-names
  (are [s ok] (= ok (cs/valid-target? s))
    "Ann" true, "a_b9" true, "" false, "ab" false, (apply str (repeat 16 "x")) true, (apply str (repeat 17 "x")) false,
    "a b" false, "@a" false, "@e[type=player]" false, "Ann\"}" false, nil false))

(def whisper-opts {:sender "Ann" :stamps [] :now 1000 :known #{"Bob" "Cy_1"} :online #{"Bob"}})

(deftest plan-whisper-refusals
  (are [target body expected] (= expected (select-keys (cs/plan-whisper target body whisper-opts) [:status :json]))
    "" "{\"text\":\"hi\"}" {:status 400 :json {:error "target is not a valid name"}}
    "ab" "{\"text\":\"hi\"}" {:status 400 :json {:error "target is not a valid name"}}
    (apply str (repeat 17 "x")) "{\"text\":\"hi\"}" {:status 400 :json {:error "target is not a valid name"}}
    "a b" "{\"text\":\"hi\"}" {:status 400 :json {:error "target is not a valid name"}}
    "@a" "{\"text\":\"hi\"}" {:status 400 :json {:error "target is not a valid name"}}
    "@e[type=player]" "{\"text\":\"hi\"}" {:status 400 :json {:error "target is not a valid name"}}
    "Ann\"}" "{\"text\":\"hi\"}" {:status 400 :json {:error "target is not a valid name"}}
    nil "{\"text\":\"hi\"}" {:status 400 :json {:error "target is not a valid name"}}
    "Zed" "{\"text\":\"hi\"}" {:status 404 :json {:error "no body called Zed"}}
    "Cy_1" "{\"text\":\"hi\"}" {:status 409 :json {:error "Cy_1 is offline"}}
    "Bob" "{}" {:status 400 :json {:error "text must be a non-empty string"}}
    "Bob" "{\"text\":\"§c\"}" {:status 400 :json {:error "text must be a non-empty string"}}
    "Bob" "nope" {:status 400 :json {:error "body must be JSON"}}
    "Bob" "{\"text\":\"x\",\"target\":\"@a\"}" {:status 400 :json {:error "target is not accepted"}}
    "Bob" nil {:status 413 :json {:error "body exceeds 4096 bytes"}}
    "Bob" "{\"text\":\"hi\"}" {}))

(deftest plan-whisper-rate-limit
  (is (= {:status 429 :json {:error "rate limit: 1 per second, 5 per 30 seconds"}}
         (select-keys (cs/plan-whisper "Bob" "{\"text\":\"hi\"}" (assoc whisper-opts :stamps [999])) [:status :json])))
  (are [target body] (= [99000] (:stamps (cs/plan-whisper target body (assoc whisper-opts :stamps [99000] :now 100000))))
    "Zed" "{\"text\":\"hi\"}"
    "Cy_1" "{\"text\":\"hi\"}"
    "Bob" "{}"
    "@a" "{\"text\":\"hi\"}"))

(deftest plan-whisper-accepts
  (let [planned (cs/plan-whisper "Bob" "{\"text\":\"hi\"}" whisper-opts)]
    (is (= [1000] (:stamps planned)))
    (is (= (cs/whisper-command "Ann" "Bob" "hi") (:command planned)))
    (is (nil? (:status planned))))
  (is (= (apply str (repeat 256 "x"))
         (get-in (whisper-payload (:command (cs/plan-whisper "Bob" (js/JSON.stringify #js {:text (apply str (repeat 300 "x"))}) whisper-opts)) "Bob")
                 ["with" 1 "text"]))))
