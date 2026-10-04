(ns view.takeover
  "The view page's manual-takeover state machine: whether this page drives the body, which takeover it is on (a generation
   bumped on every successful take, so replies to older requests are ignored), the last /drive reply, the pending mouse
   look, the error banner, the serial request queue and what to send on each key, click, mouse move, leave and timer tick.
   The rules are drive.keys (through view.drive's JS-value adapters); the page's drive.mjs only forwards DOM events and
   timers here and applies what this asks it to show.

   (create-driver env) takes a JS object {agent, me, embed, fetch, timedFetch, render, exitPointerLock, requestPointerLock}:
   timedFetch carries the POSTs (it aborts a hung request so the queue cannot stall), fetch the polls, render receives the
   view {text, hidden, driving, buttonText, disabled, title, message} to put on the page, and the pointer-lock functions act
   on the canvas. It returns an object of functions the adapter calls; see the end of this file."
  (:require [clojure.string :as str]
            [drive.keys :as k]
            [view.drive :as d]))

(set! *warn-on-infer* true)

(def error-ms 3000)
(def sent-keep 200)

(defn agent-key?
  "Whether ?agent= names a body: <world>/<name>, each of letters, digits, _ and - (the view server's rule, tools/view/serve.mjs)."
  [value]
  (and (string? value) (boolean (re-matches #"[A-Za-z0-9_-]+/[A-Za-z0-9_-]+" value))))

(defn view-model
  "What the page shows for a state: the banner (an error beats the manual-control text), the button, and the message the
   dashboard iframe's parent gets."
  [{:keys [driving manual error-text]} me]
  (let [text (or error-text (d/banner-text manual me))
        other? (boolean (and manual (not= (.-who ^js manual) me)))]
    #js {:text (or text "")
         :hidden (nil? text)
         :driving driving
         :buttonText (if driving "release (G)" "take over (G)")
         :disabled other?
         :title (if other? (str "driven by " (.-who ^js manual)) "")
         :message #js {:type "drive" :driving driving :manual manual :expiresAt (some-> ^js manual .-expiresAt)}}))

(defn handled? [code]
  (or (some? (k/control-for code)) (some? (k/look-step-for code)) (= code "KeyF")))

(defn json-reply
  "The /drive reply (a JS object) of a request, nil when the request failed or the body is not JSON."
  [fetch-fn url init]
  (.then (fetch-fn url init)
         (fn [^js response]
           (when (some-> (.. response -headers (get "content-type")) (str/includes? "json")) (.json response)))
         (fn [_] nil)))

(defn create-driver [^js env]
  (let [agent (.-agent env)
        me (.-me env)
        embed? (boolean (.-embed env))
        url (str "/drive/" agent)
        st (atom {:driving false :take-gen 0 :manual nil :last-reply nil :error-text nil :pending-look nil :had-lock false})
        sent (array)
        enqueue (d/serial-queue)
        error-timer (volatile! nil)
        render! (fn [] (.render env (view-model @st me)))
        show-error (fn [text]
                     (swap! st assoc :error-text text)
                     (js/clearTimeout @error-timer)
                     (vreset! error-timer (js/setTimeout (fn [] (swap! st assoc :error-text nil) (render!)) error-ms))
                     (render!))
        drop-driving! (fn []
                        (swap! st assoc :driving false :pending-look nil)
                        (.exitPointerLock env))
        ;; any reply (or a failed request) that shows this page no longer holds the body clears every marker of control
        check-hold! (fn [reply started-gen]
                      (when (k/should-drop? {:driving? (:driving @st) :reply (d/reply-of reply) :me me
                                             :started-gen started-gen :current-gen (:take-gen @st)})
                        (drop-driving!)
                        (render!)))
        stale? (fn [started-gen] (k/stale? {:started-gen started-gen :current-gen (:take-gen @st)}))
        post! (fn [msg init]
                (let [started-gen (:take-gen @st)]
                  (.push sent #js {:t (js/performance.now) :op (:op msg)})
                  (when (> (.-length sent) sent-keep) (.shift sent))
                  (.then (json-reply (.-timedFetch env) url
                                     (js/Object.assign #js {:method "POST" :headers #js {"Content-Type" "application/json"}
                                                            :body (js/JSON.stringify (clj->js msg))}
                                                       init))
                         (fn [^js reply]
                           (if (stale? started-gen)
                             reply
                             (do (when reply
                                   (swap! st assoc :last-reply reply)
                                   (when (not= js/undefined (.-manual reply)) (swap! st assoc :manual (.-manual reply))))
                                 (check-hold! reply started-gen)
                                 (when reply (render!))
                                 reply))))))
        send! (fn [msg] (enqueue #(post! msg #js {})))
        poll! (fn []
                (let [started-gen (:take-gen @st)]
                  (.then (json-reply (.-fetch env) url #js {})
                         (fn [^js reply]
                           (when-not (stale? started-gen)
                             (when reply (swap! st assoc :last-reply reply :manual (or (.-manual reply) nil)))
                             (check-hold! reply started-gen)
                             (when reply (render!)))))))
        take! (fn []
                (.then (send! {:op "take" :who me :why "driven from the view page"})
                       (fn [^js reply]
                         (cond
                           (nil? reply) (show-error (str "no running body " agent))
                           (not (.-ok reply)) (show-error (str "cannot take over: " (.-reason reply)))
                           :else (do (swap! st #(-> % (update :take-gen inc) (assoc :driving true)))
                                     (render!))))))
        release! (fn []
                   (drop-driving!)
                   (.then (send! {:op "release" :who me}) (fn [_] (render!))))
        toggle! (fn [] (if (:driving @st) (release!) (take!)))
        ours? (fn [] (let [manual (:manual @st)] (or (nil? manual) (= (.-who ^js manual) me))))
        leave! (fn [event]
                 (when (:driving @st)
                   (when-let [action (k/leave-action (keyword event))]
                     (if (= event "pagehide")
                       (post! {:op "stop" :who me} #js {:keepalive true}) ; not queued: the page is going away; no release
                       (do (send! {:op "stop" :who me})
                           (when (= action :stop-release) (release!)))))))
        keydown! (fn [^js e]
                   (let [code (.-code e)]
                     (cond
                       (and (= code "KeyG") (not (.-repeat e)) (not (.-ctrlKey e)) (not (.-metaKey e)) (not (.-altKey e)))
                       (do (when (ours?) (toggle!)) false)

                       (k/should-release-on-escape? {:code code :driving? (:driving @st) :pointer-locked? (.-pointerLocked e)})
                       (do (release!) false)

                       (not (and (:driving @st) (handled? code))) false

                       :else
                       (do (when-let [control (k/control-for code)]
                             (when-not (.-repeat e) (send! {:op "set" :who me :controls {control true}})))
                           (when-let [step (k/look-step-for code)]
                             (send! {:op "set" :who me :look step}))
                           true))))
        keyup! (fn [^js e]
                 (let [code (.-code e)]
                   (if-not (and (:driving @st) (handled? code))
                     false
                     (do (when-let [control (k/control-for code)]
                           (send! {:op "set" :who me :controls {control false}}))
                         true))))]
    #js {:take take!
         :release release!
         :toggle toggle!
         :poll poll!
         :keydown keydown!
         :keyup keyup!
         :click (fn []
                  (let [{:keys [driving manual]} @st]
                    (cond
                      driving (.requestPointerLock env)
                      (k/should-take-on-click? {:embed? embed? :driving? driving :manual (d/manual-of manual) :me me}) (take!))))
         :mousemove (fn [movement-x movement-y pointer-locked?]
                      (when (and (:driving @st) pointer-locked?)
                        (swap! st update :pending-look #(k/merge-look % (k/mouse-look movement-x movement-y)))))
         :flushLook (fn []
                      (let [{:keys [driving pending-look]} @st]
                        (when (and driving pending-look)
                          (swap! st assoc :pending-look nil)
                          (send! {:op "set" :who me :look pending-look}))))
         :ping (fn [] (when (:driving @st) (send! {:op "ping" :who me})))
         :leave leave!
         :pointerLockChange (fn [locked?]
                              (cond
                                locked? (swap! st assoc :had-lock true)
                                (:had-lock @st) (do (swap! st assoc :had-lock false) (leave! "pointerlock-lost"))))
         :visibilityChange (fn [hidden?] (if hidden? (leave! "hidden") (poll!)))
         :start (fn [] (render!) (poll!))
         :state (fn [] (let [{:keys [driving manual last-reply]} @st]
                         #js {:driving driving :manual manual :lastReply last-reply :expiresAt (some-> ^js manual .-expiresAt)}))
         :sent sent}))
