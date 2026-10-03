(ns dashboard.ui.chat
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
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

(defn chat-drawer []
  (let [open? @(rf/subscribe [:chat-open?])
        all @(rf/subscribe [:chat])
        visible @(rf/subscribe [:visible-chat])
        notice (get @(rf/subscribe [:notices]) "whisper")]
    [:div#chat {:class (when-not open? "closed")}
     [:div#chatbar
      [:h2 "chat"]
      [:span.muted (if (= (count all) (count visible)) (str (count all)) (str (count visible) " of " (count all)))]
      [:span.spacer]
      [:input#chatfilter.chatTools {:type "search" :placeholder "filter by name or text" :spellcheck false
                                    :value @(rf/subscribe [:chat-filter])
                                    :on-change #(rf/dispatch [:chat-filter (.. % -target -value)])}]
      [:label.chatTools [:input {:type "checkbox" :checked @(rf/subscribe [:hide-whispers?])
                                 :on-change #(rf/dispatch [:hide-whispers (.. % -target -checked)])}]
       " hide whispers"]
      [:input#whisper.chatTools {:type "text" :placeholder "whisper (Enter)"
                                 :on-key-down #(when (= "Enter" (.-key %)) (rf/dispatch [:unsupported "whisper"]))}]
      [:button#chattoggle {:on-click #(rf/dispatch [:toggle-chat])} (if open? "hide ▾" "show ▴")]]
     (when notice [:div.muted.notice.chatTools notice])
     (when open? [chat-log visible])]))
