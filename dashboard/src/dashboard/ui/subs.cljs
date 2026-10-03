(ns dashboard.ui.subs
  (:require [re-frame.core :as rf]
            [dashboard.ui.db :as db]
            [dashboard.ui.detail-model :as detail-model]
            [dashboard.ui.drive :as drive]
            [dashboard.ui.eventlog :as eventlog]
            [dashboard.ui.logic :as logic]
            [dashboard.ui.trouble :as trouble]))

(defn reg-key-sub [k] (rf/reg-sub k (fn [d _] (get d k))))

(doseq [k [:status :selected :chat-open? :chat-filter :hide-whispers? :detail-body :detail-events :detail-chip :detail-text :drive :notices :chat :worlds :detail-stats?]]
  (reg-key-sub k))

(rf/reg-sub :current-world (fn [d _] (db/current-world d)))
(rf/reg-sub :view (fn [d _] (db/effective-view d)))
(rf/reg-sub :canvas (fn [d _] (:canvas d)))
(rf/reg-sub :now (fn [d _] (get-in d [:state :at])))
(rf/reg-sub :bodies (fn [d _] (db/all-bodies d)))
(rf/reg-sub :places (fn [d _] (:places (db/world-of d))))
(rf/reg-sub :zones (fn [d _] (:zones (db/world-of d))))
(rf/reg-sub :humans (fn [d _] (:humans (db/world-of d))))
(rf/reg-sub :clock-text (fn [d _] (logic/clock-text (:clock (db/world-of d)))))

(rf/reg-sub
 :world-names
 :<- [:worlds]
 :<- [:current-world]
 (fn [[worlds current] _]
   (let [names (mapv :name worlds)]
     (if (or (empty? names) (some #{current} names)) names (cons current names)))))

(rf/reg-sub
 :split-bodies
 :<- [:bodies]
 (fn [bodies _] (logic/split-bodies bodies)))

;; bodies the engine runs: sorted for the cards, counted for the top bar
(rf/reg-sub
 :cards
 :<- [:split-bodies]
 :<- [:now]
 (fn [[{:keys [engine]} now] _]
   (mapv #(trouble/card-model % now) (trouble/sort-bodies engine now))))

(rf/reg-sub
 :status-counts
 :<- [:split-bodies]
 :<- [:now]
 (fn [[{:keys [engine]} now] _] (trouble/counts engine now)))

(rf/reg-sub
 :foreign-count
 :<- [:split-bodies]
 (fn [{:keys [foreign]} _] (count foreign)))

(rf/reg-sub
 :visible-chat
 :<- [:chat]
 :<- [:chat-filter]
 :<- [:hide-whispers?]
 (fn [[chat needle hide] _] (logic/filter-chat chat needle hide)))

(rf/reg-sub
 :map-model
 :<- [:view]
 :<- [:canvas]
 :<- [:bodies]
 :<- [:places]
 :<- [:zones]
 :<- [:humans]
 :<- [:selected]
 :<- [:now]
 :<- [:plan-items]
 (fn [[view canvas bodies places zones humans selected now plans] _]
   {:view view :canvas canvas :bodies bodies :places places :zones zones :humans humans :selected selected
    :now now :plans plans}))

(rf/reg-sub
 :detail-model
 :<- [:detail-body]
 :<- [:split-bodies]
 :<- [:detail-events]
 :<- [:now]
 (fn [[name {:keys [engine]} events now] _]
   (detail-model/detail-model (first (filter #(= name (:name %)) engine)) (or now 0) (eventlog/current-action events))))

(rf/reg-sub
 :detail-rows
 :<- [:detail-events]
 :<- [:detail-chip]
 :<- [:detail-text]
 (fn [[events chip text] _] (eventlog/rows events {:chip chip :text text})))

(rf/reg-sub
 :drive-banner
 :<- [:drive]
 (fn [d _] (drive/banner d (or (:at d) 0))))

(rf/reg-sub
 :driving-now?
 :<- [:drive]
 (fn [d _] (drive/driving-now? d)))
