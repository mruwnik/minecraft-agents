(ns dashboard.ui.rail
  "The one narrow bar at the right edge: a toggle per list the page has (chat everywhere,
  places and players beside the map). Each list's shown state lives in the db."
  (:require [re-frame.core :as rf]))

(def lists
  {:chat {:title "chat" :open-key :chat-open? :event :toggle-chat}
   :places {:title "places" :open-key :places-open? :event :toggle-places}
   :players {:title "players" :open-key :players-open? :event :toggle-players}})

(defn page-lists [page]
  (if (= page :map) [:chat :places :players] [:chat]))

(defn toggles
  "One entry per list of the page, in bar order; `open` is a map of the db's *-open? flags."
  [page open]
  (mapv (fn [id]
          (let [{:keys [title open-key event]} (lists id)
                pressed? (boolean (open open-key))]
            {:id id :label title :event event :pressed? pressed?
             :title (str (if pressed? "hide " "show ") title)}))
        (page-lists page)))

(defn rail-bar [page]
  (let [open {:chat-open? @(rf/subscribe [:chat-open?])
              :places-open? @(rf/subscribe [:places-open?])
              :players-open? @(rf/subscribe [:players-open?])}]
    (into [:nav#listbar {:aria-label "lists"}]
          (map (fn [{:keys [id label event pressed? title]}]
                 ^{:key id}
                 [:button.listtoggle {:class (when pressed? "on") :title title :aria-pressed pressed?
                                      :on-click #(rf/dispatch [event])}
                  label]))
          (toggles page open))))
