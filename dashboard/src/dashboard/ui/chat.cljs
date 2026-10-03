(ns dashboard.ui.chat
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [dashboard.ui.chatsend :as cs]
            [dashboard.ui.logic :as logic]))

(defn message-row [i {:keys [t from to message]}]
  ^{:key i}
  [:div.msg {:class (when to "whisper")}
   [:span.when (logic/clock-ms-text t)]
   [:span.who from (when to [:span.muted (str " -> " to " (whisper)")])]
   [:span.text message]])

(defn chat-log []
  (let [el (atom nil)
        scroll! #(when-let [e @el] (set! (.-scrollTop e) (.-scrollHeight e)))]
    (r/create-class
     {:component-did-mount scroll!
      :component-did-update scroll!
      :reagent-render
      (fn [messages]
        [:div#chatlog {:ref #(reset! el %)}
         (if (empty? messages)
           [:div#chatempty "no messages"]
           (into [:<>] (map-indexed message-row messages)))])})))

(defn composer []
  (let [{:keys [draft status] :as state} @(rf/subscribe [:chat-send])]
    [:div#composer {:class (name status)}
     [:input#whisper {:type "text" :placeholder "say something..." :spell-check false :max-length 256
                      :value draft :read-only (= :pending status)
                      :on-change #(rf/dispatch [:chat-draft (.. % -target -value)])
                      :on-key-down #(when (and (= "Enter" (.-key %)) (not (.-isComposing (.-nativeEvent %))))
                                      (rf/dispatch [:chat-send]))}]
     [:div.caption (cs/caption state @(rf/subscribe [:chat-sender]))]]))

(defn chat-panel []
  (let [open? @(rf/subscribe [:chat-open?])]
    (when open?
      (let [all @(rf/subscribe [:chat])
            visible @(rf/subscribe [:visible-chat])]
        [:aside#chat
         [:div#chatbar
          [:h2 "chat"]
          [:span.dim (if (= (count all) (count visible)) (str (count all)) (str (count visible) " of " (count all)))]]
         [:div#chattools
          [:input#chatfilter {:type "search" :placeholder "filter by name or text" :spell-check false
                              :value @(rf/subscribe [:chat-filter])
                              :on-change #(rf/dispatch [:chat-filter (.. % -target -value)])}]
          [:label [:input {:type "checkbox" :checked @(rf/subscribe [:hide-whispers?])
                           :on-change #(rf/dispatch [:hide-whispers (.. % -target -checked)])}]
           " hide whispers"]]
         [chat-log visible]
         [composer]]))))
