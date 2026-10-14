(ns jobs.items.obtain.chest
  "The chest source: the seen chests worth a look or a withdraw, and the steps that do either."
  (:require [engine.ctx :as ctx]
            [jobs.items.obtain.base :refer [holds]]
            [jobs.lib.child :as child]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(defn candidates
  "Usable seen chests not yet withdrawn from or given up: [{:pos :stock items-or-nil}]."
  [c names]
  (let [stock (fetch/stock-of (ctx/view c))
        done (:done-chests (ctx/mem c) #{})]
    (keep (fn [pos]
            (let [cell (fetch/cell-of pos)
                  items (get stock cell)]
              (when-not (or (done cell) (and items (not (holds items names))))
                {:pos pos :stock items})))
          (fetch/usable-chests c))))

(defn ^:async withdraw! [c pos items names have]
  (let [name (holds items names)
        need (- (:target (:start (ctx/mem c))) have)
        e (await (ctx/call c :take 'jobs.storage.withdraw
                           {:chest pos :items {name (+ (storage/carried (u/inventory (:primitives c)) name) need)}}))]
    (if (= :continue e)
      :continue
      (do (ctx/update-mem! c update :done-chests (fnil conj #{}) (fetch/cell-of pos))
          :again))))

(defn ^:async inspect!
  "Walk to within 3 of the chest (a go-to child), look into it and book its stock. :continue while go-to waits, else :again."
  [c pos]
  (let [cell (fetch/cell-of pos)
        mark! #(ctx/update-mem! c (fn [m] (-> m (update :done-chests (fnil conj #{}) cell) (update :inspected (fnil inc 0)))))
        w (when-not (u/within? (u/self-pos c) pos 3)
            (await (child/call-walk* c :walk {:pos pos :range 3 :escalate false :warn false :retry false :zone-tolls true})))]
    (cond
      (= :continue w) :continue
      (and w (not (:arrived (:data w)))) (do (mark!) :again)
      :else
      (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos pos})))]
        (if (= "ok" (.-status seen))
          (do (fetch/note-stock! c pos (.-items seen))
              (ctx/update-mem! c update :inspected (fnil inc 0)))
          (mark!))
        :again))))
