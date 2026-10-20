(ns jobs.items.obtain.chest
  "The chest source: the seen chests worth a look or a withdraw, and the steps that do either."
  (:require [engine.ctx :as ctx]
            [clojure.string]
            [jobs.items.obtain.base :refer [holds names-of]]
            [jobs.lib.access :as access]
            [jobs.lib.waits :as waits]
            [jobs.lib.chests :as chests]
            [jobs.lib.stock :as stock]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(defn remembered-chests
  "Cells of chests booked in stock as holding one of names, however far, whose zone or claim allows :take: a player walks
  back to a chest he knows holds the item."
  [c stock names]
  (->> stock
       (filter (fn [[_ items]] (holds items names)))
       (map (fn [[[x y z]]] {:x x :y y :z z}))
       (remove #(access/container-refusal c :take %))))

(defn candidates
  "Usable chests not yet withdrawn from or given up, the seen ones and the remembered ones holding a name at any
  distance, nearest first: [{:pos :stock items-or-nil}]."
  [c names]
  (let [stock (stock/stock-of (ctx/view c))
        done (:done-chests (ctx/mem c) #{})
        me (u/self-pos c)
        seen (stock/usable-chests c)
        seen-cells (set (map stock/cell-of seen))
        far (remove #(seen-cells (stock/cell-of %)) (remembered-chests c stock names))]
    (->> (concat seen far)
         (keep (fn [pos]
                 (let [cell (stock/cell-of pos)
                       items (get stock cell)]
                   (when-not (or (done cell) (and items (not (holds items names))))
                     {:pos pos :stock items}))))
         (sort-by #(u/dist me (:pos %))))))

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
        e (waits/waiting-out c (await (ctx/call c :take 'jobs.storage.withdraw
                           {:chest pos :items {name (+ (storage/carried (u/inventory (:primitives c)) name) need)}})))]
    (if (= :continue e)
      :continue
      (do (ctx/update-mem! c update :done-chests (fnil conj #{}) (stock/cell-of pos))
          :again))))

(defn ^:async inspect!
  "Look into the chest (chests/look-in!) and book its stock; one that cannot be reached or opened is given up
  (:blocked-chests). :continue while a child waits, else :again."
  [c pos]
  (say-chest! c pos (str "looking for " (clojure.string/join "/" (names-of (:args c))) " in the"))
  (let [seen (await (chests/look-in! c pos))]
    (if (= :continue seen)
      :continue
      (do (ctx/update-mem! c
                           (fn [m] (let [m (update m :inspected (fnil inc 0))]
                                     (if (= "ok" (:status seen))
                                       m
                                       (-> m (update :done-chests (fnil conj #{}) (stock/cell-of pos))
                                           (assoc-in [:blocked-chests pos] (:status seen)))))))
          :again))))

(defn blocked-text
  "\"; chest at x y z could not be opened (status)\" for each chest given up unopened, or nil."
  [blocked]
  (not-empty
   (apply str (for [[p why] blocked]
                (str "; chest at " (:x p) " " (:y p) " " (:z p) " could not be " (if (= "unreachable" why) "reached" (str "opened (" why ")")))))))
