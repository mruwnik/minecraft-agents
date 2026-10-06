(ns dashboard.ui.subs
  (:require [re-frame.core :as rf]
            [dashboard.ui.chatsend :as cs]
            [dashboard.ui.db :as db]
            [dashboard.ui.detail-model :as detail-model]
            [dashboard.ui.drive :as drive]
            [dashboard.ui.eventlog :as eventlog]
            [dashboard.ui.logic :as logic]
            [dashboard.ui.mapmodel :as mm]
            [dashboard.ui.statusfilter :as statusfilter]
            [dashboard.ui.trouble :as trouble]))

(defn reg-key-sub [k] (rf/reg-sub k (fn [d _] (get d k))))

(doseq [k [:status :selected :chat-open? :places-open? :players-open? :chat-filter :hide-whispers? :detail-body :detail-events :detail-stream :detail-chip :detail-text :detail-debug? :attention-outstanding :attention-error :detail-notices :drive :notices :chat :worlds :detail-stats? :chat-send :chat-sender :who :status-filter]]
  (reg-key-sub k))

(rf/reg-sub :whisper-send (fn [d [_ name]] (get-in d [:whisper-send name] cs/initial)))

(rf/reg-sub :current-world (fn [d _] (db/current-world d)))
(rf/reg-sub :view (fn [d _] (db/effective-view d)))
(rf/reg-sub :canvas (fn [d _] (:canvas d)))
(rf/reg-sub :now (fn [d _] (get-in d [:state :at])))
(rf/reg-sub :bodies (fn [d _] (db/all-bodies d)))
(rf/reg-sub :shown-bodies (fn [d _] (db/shown-bodies d)))
(rf/reg-sub :places (fn [d _] (:places (db/world-of d))))
(rf/reg-sub :zones (fn [d _] (:zones (db/world-of d))))
(rf/reg-sub :entity-status (fn [d _] (select-keys (db/world-of d) [:entity-sources :entity-truncated?])))
(rf/reg-sub :entities (fn [d _] (:entities (db/world-of d))))
(rf/reg-sub :entity-now (fn [d _] (or (:entity-clock d) (get-in d [:state :at]) 0)))
(rf/reg-sub :clock-text (fn [d _] (logic/clock-text (:clock (db/world-of d)))))

(rf/reg-sub
 :world-names
 :<- [:worlds]
 :<- [:current-world]
 (fn [[worlds current] _]
   (let [names (mapv :name worlds)]
     (if (or (empty? names) (nil? current) (some #{current} names)) names (cons current names)))))

(rf/reg-sub
 :split-bodies
 :<- [:bodies]
 (fn [bodies _] (logic/split-bodies bodies)))

;; bodies the engine runs: sorted for the cards, counted for the top bar
(rf/reg-sub
 :cards
 :<- [:split-bodies]
 :<- [:now]
 :<- [:who]
 :<- [:status-filter]
 (fn [[{:keys [engine]} now who pressed] _]
   (mapv #(trouble/card-model % now who) (trouble/sort-bodies (statusfilter/only-states engine pressed now) now))))

(rf/reg-sub
 :status-counts
 :<- [:split-bodies]
 :<- [:now]
 (fn [[{:keys [engine]} now] _] (trouble/counts engine now)))

(rf/reg-sub
 :status-chips
 :<- [:status-counts]
 :<- [:status-filter]
 (fn [[counts pressed] _] (statusfilter/chips counts pressed)))

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
 :entity-sightings
 :<- [:bodies]
 :<- [:entity-now]
 :<- [:entities]
 (fn [[bodies now observations] _] (mm/entity-sightings observations bodies now)))

(rf/reg-sub :humans :<- [:entity-sightings]
 (fn [observations _] (mm/human-sightings observations)))

(rf/reg-sub
 :player-rows
 :<- [:shown-bodies]
 :<- [:humans]
 :<- [:now]
 (fn [[bodies humans now] _] (mm/player-rows bodies humans #(trouble/status % (or now 0)))))

;; where each body is, by name, for the cards' "show on map"
(rf/reg-sub
 :body-positions
 :<- [:bodies]
 (fn [bodies _] (into {} (keep (fn [b] (when-let [p (mm/body-pos b)] [(:name b) p]))) bodies)))

(rf/reg-sub
 :map-model
 :<- [:view]
 :<- [:canvas]
 :<- [:shown-bodies]
 :<- [:places]
 :<- [:zones]
 :<- [:humans]
 :<- [:selected]
 :<- [:now]
 :<- [:plan-items]
 :<- [:plan-conflicts]
 :<- [:terrain?]
 :<- [:tiles]
 :<- [:current-world]
 :<- [:entity-sightings]
 :<- [:entity-now]
 (fn [[view canvas bodies places zones humans selected now plans conflicts terrain? tiles world observations entity-now] _]
   {:view view :canvas canvas :bodies bodies :places places :zones zones :entities observations :entity-now entity-now :selected selected
    :now now :plans plans :conflicts conflicts
    :terrain? terrain? :tile-world (:world tiles)
    :tile-index (when (= (:requested tiles) world) (:index tiles))}))

(rf/reg-sub
 :detail-model
 :<- [:detail-body]
 :<- [:split-bodies]
 :<- [:detail-events]
 :<- [:now]
 :<- [:who]
 (fn [[name {:keys [engine]} events now who] _]
   (detail-model/detail-model (first (filter #(= name (:name %)) engine)) (or now 0) (eventlog/current-action events) who)))

(rf/reg-sub
 :detail-rows
 :<- [:detail-events]
 :<- [:detail-chip]
 :<- [:detail-text]
 :<- [:detail-debug?]
 :<- [:attention-outstanding]
 (fn [[events chip text debug? outstanding] _] (eventlog/rows events outstanding {:chip chip :text text :debug? debug?})))

(rf/reg-sub
 :drive-banner
 :<- [:drive]
 :<- [:who]
 (fn [[d who] _] (drive/banner d (or (:at d) 0) who)))

(rf/reg-sub
 :driving-now?
 :<- [:drive]
 :<- [:who]
 (fn [[d who] _] (drive/driving-now? d who)))
