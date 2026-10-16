(ns jobs.strip.loot-cart
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.child :as child]
            [jobs.lib.stock :as stock]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]
            [engine.settings :as settings]))

(def doc
  "Empty a chest minecart and take the cart: walk within reach (jobs.lib.near/go-near!), open it and move every stack into the bag
  (a jobs.storage.withdraw-entity child), then break it (a jobs.combat.attack child) and pick up what falls (a
  jobs.forestry.collect-drops child, radius 4 round the cart): the cart item, and the stacks the bag had no room for.
  The cart is :id (an entity id) or the nearest chest_minecart within :radius of :near (default the body). It starts
  only when a cart is in sight (condition :gone; start only). Its safe? is ready/safe-at :near, else ready/safe-if-lit. A cart in
  another owner's zone or claim is refused (:ignore-zones? true skips that). Broken-and-gone carts are not looked for again:
  a restart re-reads the world and goes on from there.
  Ends :done {:collected n :items {name n}} (what came into the bag, counted from the inventory at the start), with :full true
  when the bag had no room for a stack. Else condition-failed :gone (the cart vanished), :refused (zones) or :bag-full (no
  room for anything: nothing gained), nothing-possible
  :unreachable (go-near! cannot arrive, or the cart is out of reach) or :cannot-break (the attack gave up, the cart is still
  there), with :why, :collected and :items as data. Spends :time (default ::job-time-s).")

(a/defargs args
  {:id {:doc "entity id of the chest minecart; nil: the nearest one to :near" :spec number? :default nil}
   :near {:doc "[x y z] or {:x :y :z} the search for the cart is centred on; the body when nil" :spec ::a/pos :default nil}
   :radius {:doc "blocks round :near the cart is searched for, and round the cart that drops are collected" :spec (a/num-in 0 nil) :default 4}
   :ignore-zones? {:doc "act regardless of zones and claims" :spec boolean? :default false}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a loot may take before it ends." :spec (a/num-in 0 nil)}})

(def target [:id :near])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (ready/safe-at-or-if-lit #(a/cell (:near (:args %)))))

(def fit? ready/fit?)

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
  (let [now (stock/stacks->items (u/inventory (:primitives c)))]
    (into {} (keep (fn [[k n]] (let [d (- n (get (:before (ctx/mem c)) k 0))] (when (pos? d) [k d])))) now)))

(def kinds "The kind of ending each stop reason is." {:bag-full :condition-failed :gone :condition-failed :refused :condition-failed :unreachable :nothing-possible :cannot-break :nothing-possible})

(defn items-data
  "The gained items as the job's data; extra is merged in."
  [c extra]
  (let [items (gained c)]
    (merge {:collected (reduce + 0 (vals items)) :items items} (when (:full (ctx/mem c)) {:full true}) extra)))

(defn stop!
  "End with the kind of reason and a :why in words."
  [c reason why & [extra]]
  (ctx/emit! c :loot-cart.stopped :warn {:reason reason :text (str "loot-cart stopped: " why)})
  (ctx/end c (kinds reason) reason (items-data c (merge {:why why} extra))))

(defn looted!
  "The cart is broken and its drops collected: done? holds, so the engine ends the call; a full bag that gained nothing
  ends :bag-full."
  [c]
  (if (and (:full (ctx/mem c)) (empty? (gained c)))
    (stop! c :bag-full "the bag had no room for anything in the cart")
    (do (ctx/update-mem! c assoc :looted true)
        (or (ctx/done c) :continue))))

(def can-start? (checks/refusing :cart "a cart in sight" (fn [c] (when (empty? (carts c)) :gone))))

(defn done?
  "The cart is broken and its drops collected; the items counted are the gain since the first round."
  [c]
  (let [{:keys [collected] :as result} (if (:before (ctx/mem c)) (items-data c nil) {:collected 0 :items {}})]
    (if (:looted (ctx/mem c))
      (contract/ok :looted (str "loot-cart: " collected " items") :units :items :done collected :result result)
      (contract/later :looting (str "loot-cart: " collected " items, cart still to loot") :units :items :done collected
                      :result result))))

(defn quote-of [_c]
  {:uses {:job-time [5 20 (settings/get settings ::job-time-s)]}})

(defn ^:async empty-cart!
  "Move every stack of the cart into the bag (a jobs.storage.withdraw-entity child): \"ok\", :continue while it waits, else
  the status that stopped it. A bag too full for a stack is noted as :full."
  [c id]
  (let [r (await (child/run!* c :empty 'jobs.storage.withdraw-entity {:id id}))]
    (cond
      (= :continue r) :continue
      (= :done (:kind r)) (do (when (:full (:data r)) (ctx/update-mem! c assoc :full true)) "ok")
      :else (name (contract/cause-key r)))))

(defn ^:async break-cart!
  "Attack the cart until it is gone: :broken, :continue (the child waits) or :cannot-break."
  [c id]
  (let [r (await (child/run!* c :break 'jobs.combat.attack {:targets [id] :radius 8 :lost-s 0}))]
    (cond
      (= :continue r) :continue
      (empty? (filter #(= id (:id %)) (carts c))) :broken
      :else :cannot-break)))

(defn ^:async collect!
  "Pick up the drops round the cart's place: :done, or :continue while the child waits."
  [c pos]
  (let [r (await (child/run!* c :collect 'jobs.forestry.collect-drops {:radius (:radius (:args c)) :near pos}))]
    (if (= :continue r) :continue :done)))

(defn ^:async round
  "One call is the whole job: find the cart, walk to it, empty it, break it, collect. The phase is in job memory; the cart is
  looked for again on a restart, and one already broken goes straight to the collect."
  [c]
  (when-not (:before (ctx/mem c))
    (ctx/update-mem! c assoc :before (stock/stacks->items (u/inventory (:primitives c)))))
  (let [mem (ctx/mem c)
        cart (first (carts c))
        pos (or (:pos mem) (:pos cart))]
    (cond
      (:broken mem) (let [r (await (collect! c pos))] (if (= :continue r) :continue (looted! c)))
      (nil? cart) (stop! c :gone "no chest minecart in sight")
      :else
      (let [id (:id cart)
            pos (:pos cart)
            verdict (access/container-refusal c :take pos)]
        (ctx/update-mem! c assoc :pos pos)
        (if verdict
          (stop! c :refused "the cart is in another's zone or claim" (dissoc (access/refused-result verdict) :reason :gave-up))
          (let [w (await (near/go-near! c pos 2))]
            (cond
              (= :blocked w) (stop! c :unreachable "cannot walk within reach of the cart")
              (= :partial w) :continue
              :else
              (let [e (if (:emptied mem) "ok" (await (empty-cart! c id)))]
                (cond
                  (= :continue e) :continue
                  (= "missing" e) (stop! c :gone "the cart is gone")
                  (not= "ok" e) (stop! c :unreachable (str "cannot open the cart: " e))
                  :else
                  (do (ctx/update-mem! c assoc :emptied true)
                      (let [b (await (break-cart! c id))]
                        (cond
                          (= :continue b) :continue
                          (= :cannot-break b) (stop! c :cannot-break "the cart did not break")
                          :else (do (ctx/update-mem! c assoc :broken true)
                                    (let [r (await (collect! c pos))] (if (= :continue r) :continue (looted! c))))))))))))))))
