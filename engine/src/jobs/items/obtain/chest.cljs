(ns jobs.items.obtain.chest
  "The chest source: the seen chests worth a look or a withdraw, and the steps that do either."
  (:require [engine.ctx :as ctx]
            [clojure.string]
            [jobs.items.obtain.base :refer [holds names-of]]
            [jobs.lib.child :as child]
            [jobs.lib.solid :as solid]
            [jobs.lib.stock :as stock]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(defn candidates
  "Usable seen chests not yet withdrawn from or given up: [{:pos :stock items-or-nil}]."
  [c names]
  (let [stock (stock/stock-of (ctx/view c))
        done (:done-chests (ctx/mem c) #{})]
    (keep (fn [pos]
            (let [cell (stock/cell-of pos)
                  items (get stock cell)]
              (when-not (or (done cell) (and items (not (holds items names))))
                {:pos pos :stock items})))
          (stock/usable-chests c))))

(defn say-chest!
  "One info event per chest the walk goes to, naming the item wanted there."
  [c pos what]
  (when-not (= pos (:said-chest (ctx/mem c)))
    (ctx/update-mem! c assoc :said-chest pos)
    (ctx/emit! c :obtain.chest :info
               {:message (str what " chest at " (:x pos) " " (:y pos) " " (:z pos))
                :at pos})))

(defn ^:async withdraw! [c pos items names have]
  (say-chest! c pos (str "taking " (holds items names) " from the"))
  (let [name (holds items names)
        need (- (:target (:start (ctx/mem c))) have)
        e (await (ctx/call c :take 'jobs.storage.withdraw
                           {:chest pos :items {name (+ (storage/carried (u/inventory (:primitives c)) name) need)}}))]
    (if (= :continue e)
      :continue
      (do (ctx/update-mem! c update :done-chests (fnil conj #{}) (stock/cell-of pos))
          :again))))

(defn ^:async inspect!
  "Walk to within 3 of the chest (a go-to child), look into it and book its stock. A chest that will not open under a
  seen solid block gets the block dug (a jobs.blocks.dig child, which keeps to zones and claims) and is looked into once
  more. :continue while a child waits, else :again."
  [c pos]
  (say-chest! c pos (str "looking for " (clojure.string/join "/" (names-of (:args c))) " in the"))
  (let [cell (stock/cell-of pos)
        above (update pos :y inc)
        mark! (fn [why]
                (ctx/update-mem! c (fn [m] (-> m (update :done-chests (fnil conj #{}) cell) (update :inspected (fnil inc 0))
                                               (assoc-in [:blocked-chests pos] why)))))
        w (when-not (u/within? (u/self-pos c) pos 3)
            (await (child/call-walk* c :walk {:pos pos :range 3 :ways #{:walk :clear-seal} :warn false :retry false :zone-tolls true})))]
    (cond
      (= :continue w) :continue
      (and w (not (:arrived (:data w)))) (do (mark! "unreachable") :again)
      (= :clearing (get-in (ctx/mem c) [:lid cell]))
      (let [e (await (ctx/call c :clear 'jobs.blocks.dig {:pos above}))]
        (when-not (= :continue e) (ctx/update-mem! c assoc-in [:lid cell] :cleared))
        (if (= :continue e) :continue :again))
      :else
      (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos pos})))]
        (cond
          (= "ok" (.-status seen))
          (do (stock/note-stock! c pos (.-items seen))
              (ctx/update-mem! c update :inspected (fnil inc 0)))
          (and (nil? (get-in (ctx/mem c) [:lid cell])) (solid/solid? (u/seen-name (:primitives c) above)))
          (ctx/update-mem! c assoc-in [:lid cell] :clearing)
          :else (mark! (str (.-status seen))))
        :again))))

(defn blocked-text
  "\"; chest at x y z could not be opened (status)\" for each chest given up unopened, or nil."
  [blocked]
  (not-empty
   (apply str (for [[p why] blocked]
                (str "; chest at " (:x p) " " (:y p) " " (:z p) " could not be " (if (= "unreachable" why) "reached" (str "opened (" why ")")))))))
