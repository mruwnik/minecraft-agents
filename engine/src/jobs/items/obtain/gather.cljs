(ns jobs.items.obtain.gather
  "The gather source: the child for a raw need, its tool, and the search for what is not seen."
  (:require [clojure.string :as str]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.items.obtain.base :refer [sheep-radius tried! ways-of]]
            [jobs.items.obtain.need :refer [explore-opts gather-viable? pickaxe-first? search-names tools-wanted trees-unreachable?]]
            [jobs.items.obtain.plan :refer [gather-plan]]
            [jobs.items.obtain.seen :refer [gather-carried gather-slot seen-of shears-source with-avoid with-block with-count]]
            [jobs.items.obtain.table :refer [animal? gather-need]]
            [jobs.lib.child :as child]
            [jobs.lib.need :as fetch-need]))

(defn declined?
  "Whether child ending e is a refusal before it ran: a failed condition with no result of its own."
  [e]
  (and (= :condition-failed (:kind e)) (empty? (:data e))))

(defn spared-why
  "The text of a hunt ending that left animals alone (its :spared-by, a count per rule: pen, keep ..), else nil."
  [res]
  (when-let [by (not-empty (:spared-by res))]
    (str "left alone: " (str/join ", " (map (fn [[k n]] (str n " " (name k))) by)))))

(defn fruitless-limit
  "Fruitless runs that end the gather source: the need's :max-runs, but only for a run that happened; a child that
  was declined (it could not start) broke nothing and counts against the default."
  [need e]
  (if (declined? e) (child/max-fruitless) (:max-runs need (child/max-fruitless))))

(defn gather-failed!
  "A gather run that brought nothing. Trees seen but unreachable (:no-tree) with :explore on send the body to look
  elsewhere (bounded by [:explored key]); a mine that lost its pickaxe (:no-tool) that the pickaxe step can replace
  goes to that step; neither counts as a fruitless run. A stair that found no stone while stone is seen (behind
  glass, bedrock) is :unreachable, not a missing source."
  [c names need why e]
  (cond
    (and (= :no-tree why) (explore-opts c) (< (get-in (ctx/mem c) [:explored (:key need)] 0) (child/max-fruitless)))
    (do (ctx/update-mem! c assoc-in [:gather :unreachable (:key need)] true) :again)

    (and (= :no-tool why) (pickaxe-first? c names need)) :again

    :else
    (let [why (if (and (= :no-stone-found why) (seen-of c need)) :unreachable why)]
      (when (>= (child/fruitless! c :gather) (fruitless-limit need e)) (tried! c :gather (or why :failed)))
      :again)))

(defn pickaxe-args
  "The args of the obtain child that gets a tool for need."
  [c names need]
  (let [a (:args c)]
    (into {} (remove (comp nil? val))
          {:any-of (tools-wanted c names need) :count 1 :ways (ways-of c) :fail-minutes (:fail-minutes a) :explore (:explore a)})))

(defn ^:async pickaxe-step!
  "Get the cheapest tool that harvests the need's block (a jobs.items.obtain child): :again once it is carried, :continue
  while it runs, else the gather source fails."
  [c names need]
  (let [args (pickaxe-args c names need)
        e (await (fetch-need/call! c :pickaxe 'jobs.items.obtain args))]
    (cond
      (= :continue e) :continue
      (pickaxe-first? c names need) (tried! c :gather {:pickaxe (contract/cause-key e)})
      :else :again)))

(defn ^:async explore-walk!
  "Walk and look for need (a jobs.explore.search child): :again once it is seen; a search that finds none stops the
  gather source, as does the third search for one need."
  [c need]
  (let [targets (search-names need)
        o (explore-opts c)
        here (.-pos (.self (:primitives c)))
        _ (ctx/emit! c :obtain.exploring :info
                     {:message (str "nothing seen to get " (str/join "/" targets) ": walking to look for it, up to " (:max-distance o)
                                    " blocks out and " (:max-legs o) " legs, from " (js/Math.floor (.-x here)) " "
                                    (js/Math.floor (.-y here)) " " (js/Math.floor (.-z here)))
                      :attention :notice
                      :for targets :max-distance (:max-distance o) :max-legs (:max-legs o)
                      :from (mapv #(js/Math.floor %) [(.-x here) (.-y here) (.-z here)])})
        e (await (ctx/call c :explore 'jobs.explore.search
                           (merge (explore-opts c) {:target targets :count 1 :scan-radius 16 :use-notes false :takeable true :avoid (vec (:cut-off (ctx/mem c)))})))]
    (if (= :continue e)
      :continue
      (let [res (:data e)
            k (inc (get-in (ctx/mem c) [:explored (:key need)] 0))]
        (ctx/update-mem! c assoc-in [:explored (:key need)] k)
        (ctx/update-mem! c update :gather update :unreachable dissoc (:key need))
        (if (and (= :found (:reason res)) (seen-of c need))
          :again
          (tried! c :gather (cond-> {:searched (:key need) :for targets :result (or (:reason res) (contract/cause-key e))}
                              (:why res) (assoc :why (:why res))
                              (:coverage res) (assoc :legs (:legs (:coverage res)) :farthest (:farthest (:coverage res))))))))))

(defn ^:async gather-step!
  "One round of the gather source: the child for the first raw item the chain lacks, until it ends."
  [c names have target]
  (let [p (:primitives c)
        pl (gather-plan c names (- target have))
        need (some->> (some-> pl :gather (gather-need (game/version-of p))) (with-block c) (with-count p) (with-avoid c))]
    (cond
      (nil? need) (tried! c :gather :no-plan)
      (and (not (get-in (ctx/mem c) [:gather :before])) (pickaxe-first? c names need) (or (explore-opts c) (seen-of c need)))
      (await (pickaxe-step! c names need))
      (and (not (get-in (ctx/mem c) [:gather :before])) (explore-opts c) (not (animal? need)) (not (:tunnel? need))
           (or (not (seen-of c need)) (get-in (ctx/mem c) [:gather :unreachable (:key need)]) (trees-unreachable? c need))
           (< (get-in (ctx/mem c) [:explored (:key need)] 0) (child/max-fruitless)))
      (await (explore-walk! c need))
      (and (not (get-in (ctx/mem c) [:gather :before]))
           (not (if (explore-opts c)
                  (and (or (:tunnel? need) (seen-of c need)) (= :ok (first (ctx/child-status c (gather-slot need) (:job need) (:args need)))))
                  (gather-viable? c names (- target have)))))
      (tried! c :gather :none-seen)
      :else
      (do (when-not (get-in (ctx/mem c) [:gather :before])
            (ctx/update-mem! c assoc-in [:gather :before] (gather-carried p need))
            (ctx/update-mem! c assoc-in [:gather :item] (:key need))
            (when (= 'jobs.forestry.harvest-wood (:job need))
              (ctx/update-mem! c assoc-in [:gather :tree] (select-keys need [:key :job :args])))
            (let [at (:pos (seen-of c need))]
              (ctx/emit! c :obtain.gathering :info
                         {:message (str "getting " (:key need) " with " (:job need)
                                        (when at (str " from " (:x at) " " (:y at) " " (:z at))))
                          :item (:key need) :via (:job need) :at at}))
            (when (:sheep need)
              (let [how (shears-source c)]
                (ctx/emit! c :obtain.wool-source :info
                           {:message (case how
                                       :carried "wool by shearing: shears carried"
                                       :craft "wool by shearing: shears will be crafted from 2 iron ingots"
                                       :chest "wool by shearing: shears or iron fetched from a chest"
                                       "wool by killing sheep: no shears and no iron for shears (the last 2 adults of a colour live)")
                            :how (or how :kill)})))
            (when-let [{:keys [wanted taking]} (:wool-fallback pl)]
              (ctx/emit! c :obtain.wool-colour :info
                         {:message (str "no " wanted " sheep within " (sheep-radius) " blocks; taking " taking " wool instead")
                          :wanted wanted :taking taking})))
          (let [e (await (ctx/call c (gather-slot need) (:job need) (:args need)))]
            (if (= :continue e)
              :continue
              (let [res (merge (when (map? (:cause e)) (:cause e)) (:data e))
                    before (get-in (ctx/mem c) [:gather :before])]
                (ctx/update-mem! c update :gather dissoc :before :item :tree)
                (if (> (gather-carried p need) before)
                  (do (child/progress! c :gather) :again)
                  (do (when-let [w (or (:why res) (spared-why res))] (ctx/update-mem! c assoc-in [:gather :why] w))
                      (when-let [cols (seq (:columns res))] (ctx/update-mem! c update :cut-off (fnil into #{}) (map vec) cols))
                      (gather-failed! c names need (if (= :done (:kind e)) :nothing-gathered (contract/cause-key e)) e))))))))))

(defn ^:async finish-tree!
  "The log child of an earlier round was cut before it ended: runs it to its end even when the logs carried are enough
  (a tree it started is felled whole). :continue while it waits, else :again."
  [c]
  (let [{:keys [key job args]} (get-in (ctx/mem c) [:gather :tree])
        slot (gather-slot {:key key})
        r (await (ctx/call c slot job args))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c update :gather dissoc :before :item :tree) :again))))
