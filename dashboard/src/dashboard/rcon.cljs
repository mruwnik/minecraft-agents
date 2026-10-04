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

(defn exchange [socket id type body]
  (js/Promise.
   (fn [resolve reject]
     (let [pending (atom (js/Buffer.alloc 0))
           on-data (fn on-data [chunk]
                     (swap! pending #(js/Buffer.concat #js [% chunk]))
                     (when-let [packet (decode-packet @pending)]
                       (.off socket "data" on-data)
                       (resolve packet)))]
       (.on socket "data" on-data)
       (.once socket "error" reject)
       (.write socket (encode-packet id type body))))))

(defn send-commands!
  "Connects, authenticates, sends each command in order on one connection (ids 2, 3, ...), resolves to the vector of
  reply bodies. opts: :port (default 25575)."
  [{:keys [port] :or {port default-port}} commands]
  (-> (js/Promise.resolve nil)
      (.then (fn [_]
               (let [secret (.trim (.readFileSync fs (password-file) "utf8"))
                     socket (.connect net #js {:host host :port port})]
                 (-> (js/Promise. (fn [resolve reject] (.once socket "connect" resolve) (.once socket "error" reject)))
                     (.then #(exchange socket 1 auth-type secret))
                     (.then (fn [auth]
                              (when (= -1 (:id auth)) (throw (js/Error. "RCON refused the password")))
                              (reduce (fn [chain [i command]]
                                        (.then chain (fn [replies]
                                                       (.then (exchange socket (+ i 2) command-type command)
                                                              #(conj replies (:body %))))))
                                      (js/Promise.resolve [])
                                      (map-indexed vector commands))))
                     (.finally #(.end socket))))))))

(defn send-command!
  "Sends one command, resolves to the reply body."
  [command]
  (.then (send-commands! {} [command]) first))
