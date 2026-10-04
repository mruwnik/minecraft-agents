(ns dashboard.rcon-test
  (:require ["net" :as net]
            [cljs.test :refer [deftest is async]]
            [dashboard.rcon :as rcon]))

(deftest encode-packet-layout-is-little-endian-length-id-type-body-two-zeros
  (is (= [12 0 0 0 7 0 0 0 2 0 0 0 104 105 0 0]
         (vec (rcon/encode-packet 7 2 "hi")))))

(deftest decode-packet-round-trips-and-reports-bytes-used
  (let [body "Added Aviendha to the whitelist"
        buf (js/Buffer.concat #js [(rcon/encode-packet 3 0 body) (js/Buffer.from #js [9 9])])]
    (is (= {:id 3 :type 0 :body body :size (- (.-length buf) 2)}
           (rcon/decode-packet buf)))))

(deftest decode-packet-incomplete-data-is-nil
  (is (nil? (rcon/decode-packet (.subarray (rcon/encode-packet 1 2 "list") 0 9))))
  (is (nil? (rcon/decode-packet (js/Buffer.alloc 3)))))

;; The client against a local fake server (127.0.0.1, random port); never the real server.

(defn listen!
  "Starts a fake server whose connections run (on-connection socket); resolves to {:port :server}."
  [on-connection]
  (js/Promise.
   (fn [resolve _]
     (let [server (.createServer net on-connection)]
       (.listen server 0 "127.0.0.1" #(resolve {:port (.-port (.address server)) :server server}))))))

(defn ok-auth [socket] (.write socket (rcon/encode-packet 1 2 "")))

(defn reply-to
  "A connection handler: authenticates, then answers every command packet with (replies-fn id body) -> a Buffer to write."
  [replies-fn]
  (fn [socket]
    (.on socket "error" identity)
    (let [buffer (atom (js/Buffer.alloc 0))]
      (.on socket "data"
           (fn [chunk]
             (swap! buffer #(js/Buffer.concat #js [% chunk]))
             (when-let [packet (rcon/take-packet! buffer)]
               (if (= rcon/auth-type (:type packet))
                 (ok-auth socket)
                 (.write socket (replies-fn (:id packet) (:body packet))))))))))

(defn run-against
  "Runs (rcon/send-commands! opts commands) against a fake server, calls (check result) with {:ok replies} or {:error message}, then done."
  [on-connection opts commands check done]
  (.then (listen! on-connection)
         (fn [{:keys [port server]}]
           (-> (rcon/send-commands! (merge {:port port :password "pw"} opts) commands)
               (.then (fn [replies] {:ok (vec replies)})
                      (fn [e] {:error (.-message e)}))
               (.then (fn [result]
                        (.close server)
                        (check result)
                        (done)))))))

(deftest send-commands-returns-each-reply-in-order
  (async done
         (run-against (reply-to (fn [id body] (rcon/encode-packet id 0 (str "re:" body))))
                      {} ["list" "time"]
                      #(is (= {:ok ["re:list" "re:time"]} %))
                      done)))

(deftest a-reply-split-over-several-packets-is-assembled
  (let [chunk-a (apply str (repeat 4096 "a"))
        chunk-b (apply str (repeat 4096 "b"))]
    (async done
           (run-against (reply-to (fn [id _]
                                    (js/Buffer.concat #js [(rcon/encode-packet id 0 chunk-a)
                                                           (rcon/encode-packet id 0 chunk-b)
                                                           (rcon/encode-packet id 0 "end")])))
                        {} ["list"]
                        #(is (= {:ok [(str chunk-a chunk-b "end")]} %))
                        done))))

(deftest take-packet-keeps-the-bytes-after-it
  (let [buffer (atom (js/Buffer.concat #js [(rcon/encode-packet 1 0 "one") (rcon/encode-packet 2 0 "two") (js/Buffer.from #js [7])]))]
    (is (= ["one" "two" nil] [(:body (rcon/take-packet! buffer)) (:body (rcon/take-packet! buffer)) (:body (rcon/take-packet! buffer))]))
    (is (= [7] (vec @buffer)))))

(deftest no-reply-rejects-with-a-timeout
  (async done
         (run-against (fn [socket] (.on socket "error" identity) (.on socket "data" identity))
                      {:timeout-ms 100} ["list"]
                      #(is (re-find #"timed out" (:error %)))
                      done)))

(deftest a-close-mid-reply-rejects
  (async done
         (run-against (fn [socket]
                        (.on socket "error" identity)
                        (.on socket "data"
                             (fn [_] (let [reply (rcon/encode-packet 1 2 "")]
                                       (.end socket (.subarray reply 0 6))))))
                      {} ["list"]
                      #(is (re-find #"closed before the reply" (:error %)))
                      done)))

(deftest a-socket-reset-mid-reply-rejects
  (async done
         (run-against (fn [socket]
                        (.on socket "error" identity)
                        (.on socket "data" (fn [_] (.write socket (js/Buffer.from #js [1 2]) (fn [] (.resetAndDestroy socket))))))
                      {} ["list"]
                      #(is (some? (:error %)))
                      done)))

(deftest a-refused-password-rejects
  (async done
         (run-against (fn [socket] (.on socket "error" identity) (.on socket "data" (fn [_] (.write socket (rcon/encode-packet -1 2 "")))))
                      {} ["list"]
                      #(is (= {:error "RCON refused the password"} %))
                      done)))
