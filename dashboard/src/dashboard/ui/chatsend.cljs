(ns dashboard.ui.chatsend
  "The chat composer's state: the draft, whether a send is in flight, the 3 s ack, the error. Pure transitions."
  (:require [clojure.string :as str]))

(def initial {:draft "" :status :idle :error nil :ack-id 0})

(defn sendable? [{:keys [draft status]}]
  (and (not= :pending status) (not (str/blank? draft))))

(defn request-body [{:keys [draft]}] {:text (str/trim draft)})

(defn edited [state text] (assoc state :draft text :status :idle :error nil))

(defn begin [state] (assoc state :status :pending :error nil))

(defn succeeded [state ack-id] (assoc state :draft "" :status :sent :error nil :ack-id ack-id))

(defn failed [state message] (assoc state :status :failed :error message))

(defn clear-ack
  "The ack goes away after its timer, unless a later send replaced it."
  [state ack-id]
  (if (and (= :sent (:status state)) (= ack-id (:ack-id state)))
    (assoc state :status :idle)
    state))

(defn caption
  "The line under the composer. `sender` is what the server sends as, nil until the page has heard it."
  [{:keys [status error]} sender]
  (case status
    :pending "sending..."
    :sent "sent"
    :failed error
    (if (str/blank? sender) "to everyone" (str "to everyone as " sender))))

;; ---------------------------------------------------------------- whispers to one body (the popup's box)
(def minecraft-name #"[A-Za-z0-9_]{3,16}")

(defn whisper-url
  "POST target for a whisper to `name`, nil unless it is a well-formed Minecraft name."
  [name]
  (when (and (string? name) (re-matches minecraft-name name))
    (str "/api/whisper/" name)))

(defn whisper-block-reason [online? _state]
  (when-not online? "offline: nobody would hear it"))

(defn whisper-sendable? [online? state name]
  (boolean (and (sendable? state) (nil? (whisper-block-reason online? state)) (whisper-url name))))

(defn whisper-caption [{:keys [status] :as state} online? name]
  (or (whisper-block-reason online? state)
      (case status
        :pending "sending..."
        :sent "sent"
        :failed (:error state)
        (str "whisper to " name))))
