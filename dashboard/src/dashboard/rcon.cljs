(ns dashboard.rcon
  "The one RCON packet codec and client (node net), shared by the dashboard server and the tools/rcon*.mjs entry points via dashboard.rcon-tools. The password is read in-process only."
  (:require ["fs" :as fs]
            ["net" :as net]
            ["os" :as os]
            ["path" :as path]))

(def host "127.0.0.1")
(def default-port 25575)
(def auth-type 3)
(def command-type 2)

(defn password-file [] (.join path (.homedir os) ".config" "minecraft-claude" "rcon-password"))

(defn encode-packet [id type body]
  (let [text (js/Buffer.from body "utf8")
        buf (js/Buffer.alloc (+ 14 (.-length text)))]
    (.writeInt32LE buf (+ 10 (.-length text)) 0)
    (.writeInt32LE buf id 4)
    (.writeInt32LE buf type 8)
    (.copy text buf 12)
    buf))

(defn decode-packet [buf]
  (when (>= (.-length buf) 4)
    (let [size (+ 4 (.readInt32LE buf 0))]
      (when (>= (.-length buf) size)
        {:id (.readInt32LE buf 4) :type (.readInt32LE buf 8) :body (.toString buf "utf8" 12 (- size 2)) :size size}))))

(def default-timeout-ms 10000)
(def max-payload "A reply body this long (bytes) is followed by more packets of the same reply." 4096)

(defn take-packet!
  "Removes and returns the first complete packet in the buffer atom, keeping any bytes after it."
  [buffer]
  (when-let [packet (decode-packet @buffer)]
    (swap! buffer #(.subarray % (:size packet)))
    packet))

(defn exchange
  "Sends one packet and resolves to the reply {:id :type :body :size}; a reply split over several packets (each body
  at max-payload bytes until the last) is assembled into one body. Bytes after the reply stay in `buffer` (an atom
  holding the connection's unread bytes). Rejects on a socket error, a close before the reply is complete, or no
  complete reply within timeout-ms."
  [socket buffer id type body timeout-ms]
  (js/Promise.
   (fn [resolve reject]
     (let [parts (atom [])
           timer (atom nil)
           listeners (atom {})
           finish! (fn [settle value]
                     (js/clearTimeout @timer)
                     (doseq [[event f] @listeners] (.off socket event f))
                     (settle value))
           on-data (fn [chunk]
                     (swap! buffer #(js/Buffer.concat #js [% chunk]))
                     (loop []
                       (when-let [packet (take-packet! buffer)]
                         (swap! parts conj (:body packet))
                         (if (< (.byteLength js/Buffer (:body packet)) max-payload)
                           (finish! resolve (assoc packet :body (apply str @parts)))
                           (recur)))))
           on-error (fn [e] (finish! reject e))
           on-close (fn [] (finish! reject (js/Error. "RCON connection closed before the reply was complete")))]
       (reset! listeners {"data" on-data "error" on-error "close" on-close})
       (doseq [[event f] @listeners] (.on socket event f))
       (reset! timer (js/setTimeout #(finish! reject (js/Error. (str "RCON timed out after " timeout-ms " ms waiting for a reply")))
                                    timeout-ms))
       (.write socket (encode-packet id type body))))))

(defn connect!
  "Resolves to the connected socket; rejects on a connection error or after timeout-ms."
  [port timeout-ms]
  (js/Promise.
   (fn [resolve reject]
     (let [socket (.connect net #js {:host host :port port})
           timer (js/setTimeout (fn []
                                  (.destroy socket)
                                  (reject (js/Error. (str "RCON connect timed out after " timeout-ms " ms"))))
                                timeout-ms)]
       ;; a late socket error (reset during .end) must not become an uncaught exception
       (.on socket "error" identity)
       (.once socket "error" (fn [e] (js/clearTimeout timer) (reject e)))
       (.once socket "connect" (fn [] (js/clearTimeout timer) (resolve socket)))))))

(defn send-commands!
  "Connects, authenticates, sends each command in order on one connection (ids 2, 3, ...), resolves to the vector of
  reply bodies. opts: :port (default 25575), :password (default: the password file), :timeout-ms (default 10000,
  for the connect and for each reply)."
  [{:keys [port password timeout-ms] :or {port default-port timeout-ms default-timeout-ms}} commands]
  (-> (js/Promise.resolve nil)
      (.then (fn [_]
               (let [secret (or password (.trim (.readFileSync fs (password-file) "utf8")))
                     buffer (atom (js/Buffer.alloc 0))]
                 (-> (connect! port timeout-ms)
                     (.then (fn [socket]
                              (-> (exchange socket buffer 1 auth-type secret timeout-ms)
                                  (.then (fn [auth]
                                           (when (= -1 (:id auth)) (throw (js/Error. "RCON refused the password")))
                                           (reduce (fn [chain [i command]]
                                                     (.then chain (fn [replies]
                                                                    (.then (exchange socket buffer (+ i 2) command-type command timeout-ms)
                                                                           #(conj replies (:body %))))))
                                                   (js/Promise.resolve [])
                                                   (map-indexed vector commands))))
                                  (.finally #(.destroy socket)))))))))))

(defn send-command!
  "Sends one command, resolves to the reply body."
  [command]
  (.then (send-commands! {} [command]) first))
