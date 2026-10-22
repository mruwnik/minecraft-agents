(ns jobs.items.pick-up-loose
  (:require [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [jobs.lib.checks :as checks]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.loose :as loose]))

(def doc
  "Pick up the loose items the body sees nearby that are worth it (jobs.lib.loose: seen, within the :radius setting, worth the
  detour or on :always, not thrown, owed to another live job or in another's zone or claim). The :loose-items reflex runs it.
  One jobs.items.collect-drops child takes them by entity id; its go-to leaves what it cannot reach. Items not
  picked up are remembered (:loose-left) for :left-s; so are the seen items when it finds none worth it, so a
  reflex that judged otherwise cannot fire it again for them. Tuning is the jobs.lib.loose settings (a call's :settings overrides).
  Ends :done with data {:collected n :items [names] :left [ids] :text}. With nothing worth it a listed call waits
  (condition :nothing-worth-it); a started one ends condition-failed :nothing-worth-it, the text naming the radius, what within it was left
  and why, and the nearest item seen beyond it; with items picked at but none taken, nothing-possible :none-collected.
  Quote: the items worth it in sight, a second of walking each.")

(a/defargs args {})

(def target [])

(def meta {:category :reflex})

(defn place
  "The cell of the nearest loose item worth taking, nil for none."
  [c]
  (some-> (first (loose/candidates (:primitives c) (ctx/view c) (:world (:engine c)) {} (:root c)))
          loose/item-pos
          (update-vals js/Math.floor)))

(def safe? (price/staying :work (ready/safe-at-or-roaming place)))
(def fit? ready/ok)

(def defaults {:least 1})


(def s-per-item "Seconds a quote counts to fetch one item." 2)

(defn done?
  "Never done before the round has run: the items are not known to be worth it."
  [c]
  (contract/later :collecting "pick-up-loose: the round ends the call" :units :items :done (:collected (ctx/mem c) 0)
                  :result {:collected (:collected (ctx/mem c) 0)}))

(defn names-of [items] (vec (distinct (keep #(some-> (.-item %) .-name) items))))

(defn stop-text
  "The words of a stop with nothing taken: the radius, what was left and why, the nearest item beyond the radius."
  [{:keys [radius left beyond]}]
  (str "pick-up-loose stopped: "
       (if (seq left)
         (str (count left) " loose item kind" (when (> (count left) 1) "s") " within " radius " blocks, none taken: "
              (str/join ", " (map (fn [[n why]] (str n " " (if (= :not-worth why) "not worth the detour" (name why)))) left)))
         (str "no loose item in sight within " radius " blocks"))
       (when beyond (str "; nearest beyond: " (:name beyond) " " (:distance beyond) " blocks away (a larger :radius takes it)"))))

(defn nothing-worth-it
  "The cause :nothing-worth-it while no loose item in sight is worth picking up (a listed call waits for loot)."
  [c]
  (let [p (:primitives c) view (ctx/view c) w (:world (:engine c))]
    (when (empty? (loose/candidates p view w {} (:root c)))
      {:reason :nothing-worth-it :text (stop-text (loose/explain p view w {} (:root c)))})))

(defn can-start? [c] (checks/cause-status :later (nothing-worth-it c)))

(defn quote-of
  [c]
  (let [items (loose/candidates (:primitives c) (ctx/view c) (:world (:engine c)) {} (:root c))
        n (count items)
        t (* s-per-item n)]
    {:uses {:job-time [0 t (* 3 t)]}
     :yields {:items (update-vals (frequencies (keep #(some-> (.-item %) .-name) items)) (fn [k] [0 k k]))}
     :done [0 n n]}))

(defn ^:async round
  [c]
  (let [p (:primitives c)
        view (ctx/view c)
        o (loose/options {})
        items (loose/candidates p view (:world (:engine c)) {} (:root c))
        ids (mapv #(.-id %) items)]
    (if (empty? ids)
      (do (loose/left! c (into [] (comp (map #(.-id %)) (remove (loose/left-ids view))) (loose/seen p o)) o)
          (ctx/end c :condition-failed :nothing-worth-it
                   {:collected 0 :text (stop-text (loose/explain p view (:world (:engine c)) {} (:root c)))}))
      (let [r (await (child/run!* c :collect 'jobs.items.collect-drops {:ids ids :radius (+ (:radius o) (:collect-pad o))}))
            n (or (:collected (:data r)) 0)
            still (set (map #(.-id %) (look/seen-items p {:radius (:check-radius o) :ids ids})))
            left (filterv still ids)
            out {:collected n :items (names-of items) :left left
                 :text (str "picked up " n " loose item" (when (not= 1 n) "s") " (" (str/join ", " (names-of items)) ")"
                            (when (seq left) (str "; left " (count left))))}]
        (loose/left! c left o)
        (if (pos? n)
          (ctx/end c :done nil out)
          (ctx/end c :nothing-possible :none-collected out))))))
