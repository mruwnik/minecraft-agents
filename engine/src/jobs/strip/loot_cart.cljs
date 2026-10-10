(ns jobs.strip.loot-cart
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.child :as child]
            [jobs.lib.danger :as danger]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.util :as u]))

(def doc
  "Empty a chest minecart and take the cart: walk within reach (jobs.lib.near/go-near!), open it and move every stack into the bag
  (jobs.storage.withdraw's primitives, entity form), then break it (a jobs.combat.attack child) and pick up what falls (a
  jobs.forestry.collect-drops child, radius 4 round the cart): the cart item, and the stacks the bag had no room for.
  The cart is :id (an entity id) or the nearest chest_minecart within :radius of :near (default the body). It only
  declines (check waits :danger) while a real danger is near (jobs.lib.danger). A cart in another owner's zone or claim is refused
  (:ignore-zones? true skips that). Broken-and-gone carts are not looked for again: a restart re-reads the world and goes on from there.
  Ends {:collected n :items {name n}} (what came into the bag, counted from the inventory at the start), with :full true when
  the bag had no room for a stack. Or stopped {:status :stopped :reason r :why text :collected n :items {..}}, r being :gone (no such cart in sight),
  :unreachable (go-near! cannot arrive, or the cart is out of reach), :refused (zones) or :cannot-break (the attack gave up, the cart is still there).")

(a/defargs args
  {:id {:doc "entity id of the chest minecart; nil: the nearest one to :near" :spec number? :default nil}
   :near {:doc "[x y z] or {:x :y :z} the search for the cart is centred on; the body when nil" :spec ::a/pos :default nil}
   :radius {:doc "blocks round :near the cart is searched for, and round the cart that drops are collected" :spec (a/num-in 0 nil) :default 4}
   :ignore-zones? {:doc "act regardless of zones and claims" :spec boolean? :default false}})

(defn check
  "Declines while a real danger is near."
  [c]
  (or (not (danger/danger-near? (:primitives c) (danger/hostile-radius) (danger/ranged-radius)))
      (ctx/wait c {:reason :danger})))

(defn carts
  "The chest minecarts in sight within radius of the anchor (:near, else the body), nearest first."
  [c]
  (let [{:keys [near radius id]} (:args c)
        anchor (if near (a/cell near) (u/self-pos c))
        found (look/seen-entities (:primitives c) {:names ["chest_minecart"] :radius (+ (if id 32 radius) (u/dist anchor (u/self-pos c))) :max 16})]
    (->> found
         (filter #(if id (= id (:id %)) (<= (u/dist anchor (:pos %)) radius)))
         (sort-by #(u/dist anchor (:pos %))))))

(defn gained
  "{name n} the bag holds more of than at the start of the job."
  [c]
  (let [now (fetch/stacks->items (u/inventory (:primitives c)))]
    (into {} (keep (fn [[k n]] (let [d (- n (get (:before (ctx/mem c)) k 0))] (when (pos? d) [k d])))) now)))

(defn finish!
  "End the job with the gained items; extra is merged in (a stop's :status and :reason)."
  [c extra]
  (let [items (gained c)]
    (ctx/result! c (merge {:collected (reduce + 0 (vals items)) :items items} (when (:full (ctx/mem c)) {:full true}) extra))
    :done))

(defn stop!
  "End stopped with a reason keyword and a :why in words."
  [c reason why]
  (ctx/emit! c :loot-cart.stopped :warn {:reason reason :text (str "loot-cart stopped: " why)})
  (finish! c {:status :stopped :reason reason :why why}))

(defn ^:async empty-cart!
  "Move every stack of the cart into the bag. A transfer that moves nothing (no room) ends it with :full."
  [c id]
  (let [seen (await (ctx/act c :inspectContainer (clj->js {:entity id})))]
    (cond
      (not= "ok" (.-status seen)) (.-status seen)
      :else (loop [stacks (vec (fetch/stacks->items (.-items seen)))]
              (if-let [[name n] (first stacks)]
                (let [r (await (ctx/act c :transfer (clj->js {:entity id :direction "withdraw" :item name :count n})))]
                  (if (and (= "ok" (.-status r)) (pos? (.-moved r)))
                    (recur (rest stacks))
                    (do (ctx/update-mem! c assoc :full true) "ok")))
                "ok")))))

(defn ^:async break-cart!
  "Attack the cart until it is gone: :broken, :continue (the child waits) or :cannot-break."
  [c id]
  (let [r (await (child/run! c :break 'jobs.combat.attack {:targets [id] :radius 8 :lost-s 0}))]
    (cond
      (= :continue r) :continue
      (empty? (filter #(= id (:id %)) (carts c))) :broken
      :else :cannot-break)))

(defn ^:async collect!
  "Pick up the drops round the cart's place: :done, or :continue while the child waits."
  [c pos]
  (let [r (await (child/run! c :collect 'jobs.forestry.collect-drops {:radius (:radius (:args c)) :near pos}))]
    (if (= :continue r) :continue :done)))

(defn ^:async round
  "One call is the whole job: find the cart, walk to it, empty it, break it, collect. The phase is in job memory; the cart is
  looked for again on a restart, and one already broken goes straight to the collect."
  [c]
  (when-not (:before (ctx/mem c))
    (ctx/update-mem! c assoc :before (fetch/stacks->items (u/inventory (:primitives c)))))
  (let [mem (ctx/mem c)
        cart (first (carts c))
        pos (or (:pos mem) (:pos cart))]
    (cond
      (:broken mem) (let [r (await (collect! c pos))] (if (= :continue r) :continue (finish! c nil)))
      (nil? cart) (stop! c :gone "no chest minecart in sight")
      :else
      (let [id (:id cart)
            pos (:pos cart)
            verdict (access/container-refusal c :take pos)]
        (ctx/update-mem! c assoc :pos pos)
        (if verdict
          (finish! c (assoc (access/refused-result verdict) :status :stopped))
          (let [w (await (near/go-near! c pos 2))]
            (cond
              (= :blocked w) (stop! c :unreachable "cannot walk within reach of the cart")
              (= :partial w) :continue
              :else
              (let [e (if (:emptied mem) "ok" (await (empty-cart! c id)))]
                (cond
                  (= "missing" e) (stop! c :gone "the cart is gone")
                  (not= "ok" e) (stop! c :unreachable (str "cannot open the cart: " e))
                  :else
                  (do (ctx/update-mem! c assoc :emptied true)
                      (let [b (await (break-cart! c id))]
                        (cond
                          (= :continue b) :continue
                          (= :cannot-break b) (stop! c :cannot-break "the cart did not break")
                          :else (do (ctx/update-mem! c assoc :broken true)
                                    (let [r (await (collect! c pos))] (if (= :continue r) :continue (finish! c nil))))))))))))))))
