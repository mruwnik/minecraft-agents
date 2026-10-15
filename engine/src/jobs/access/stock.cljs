(ns jobs.access.stock
  "What a dig stocks before it sets out, through jobs.lib.need: torches when few are carried, a spare for the last worn
  pickaxe. Both are optional: a get that fails only warns, and the dig goes on (dark, or with the worn tool)."
  (:require [clojure.string :as str]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.need :as need]
            [jobs.lib.torch :as torch]
            [jobs.lib.trip :as trip]))

(defn fetch-allows?
  "Whether the call's :fetch lets it get a thing of kind (:item or :tool)."
  [c kind]
  (let [f (fetch/parse-arg (:fetch (:args c)))]
    (boolean (and (map? f) (not (:error f)) (or (nil? (:what f)) (contains? (:what f) kind))))))

(defn cause-why
  "The reason of the get that failed (its own, which need keeps as :why), else the cause's."
  [e]
  (or (get-in e [:cause :why]) (contract/cause-key e)))

(defn no-stock-text [held e]
  (let [tried (get-in e [:data :tried])]
    (str "carries " (if (zero? held) "no torches" (str "only " held " torches"))
         (if (zero? held)
           (str ", none made" (when (seq tried) (str " (" (str/join ", " (map (fn [[k v]] (str (name k) " " (if (keyword? v) (name v) v))) tried)) ")")))
           " and could not get more")
         ": the dig goes on " (if (zero? held) "dark" "with them")
         (when-let [w (get-in e [:data :why])] (str " (" w ")")))))

(defn ^:async torches!
  "Before a dig down or a tunnel sets out: with fewer than trip/torch-low torches carried, get them up to trip/torch-stock
  with a jobs.items.obtain child (slot :torches; chest, craft from coal or charcoal, charcoal by smelt, gather), and with
  coal or charcoal carried only what that fuel makes (4 each). Resolves nil to go on, :continue while the get waits.
  Once per call (:torches-tried): info torch.stocked, or warn torch.no-stock with the get's cause. The body walks back to
  where it began (need :return?). Skipped when :fetch lacks :item or the body is short of food."
  [c]
  (let [p (:primitives c)
        held (torch/torches-held p)
        fuel (torch/fuel-held p)
        want (- (trip/torch-stock) held)
        n (if (pos? fuel) (min want (* 4 fuel)) want)]
    (when (and (fetch-allows? c :item) (< held (trip/torch-low)) (not (:torches-tried (ctx/mem c))) (not (trip/food-short? c)))
      (let [e (await (need/fetch! c 'jobs.items.obtain {:item "torch" :count n :avoid (trip/line-ahead c)} :need {:item "torch" :count n}
                                  {:slot :torches :return? true}))]
        (if (= :continue e)
          :continue
          (let [now (torch/torches-held p)]
            (ctx/update-mem! c assoc :torches-tried true)
            (when (and (pos? fuel) (<= now held)) (ctx/update-mem! c assoc :craft-failed true))
            (if (> now held)
              (ctx/emit! c :torch.stocked :info {:held now :text (str "carrying " now " torches for the dig")})
              (ctx/emit! c :torch.no-stock :warn {:held now :reason (cause-why e) :tried (get-in e [:data :tried]) :text (no-stock-text now e)}))
            nil))))))

(defn ^:async spare!
  "Before a dig of block-name that makes many cuts: when the pickaxe-kind tool for it is the last that harvests it and is
  worn (trip/spare-need; amount, the cuts to make, optional), get the cheapest lasting tool of that kind with a
  jobs.items.obtain child (slot :spare). Resolves nil to go on, :continue while the get waits. Once per tool name
  (:spare-tried): info tool.spare, or warn tool.no-spare with the cause. Skipped when :fetch lacks :tool or under a
  spare get (no spare of a spare)."
  [c block-name amount]
  (let [p (:primitives c)
        worn (when (and block-name (not (trip/in-spare? c)) (fetch-allows? c :tool)) (trip/spare-need c block-name amount))
        tool-name (when worn (trip/spare-name p block-name worn))]
    (when (and worn (not (contains? (:spare-tried (ctx/mem c)) tool-name)))
      (let [e (await (need/item! c tool-name 1 {:slot :spare}))]
        (if (= :continue e)
          :continue
          (let [why (cause-why e)]
            (ctx/update-mem! c update :spare-tried (fnil conj #{}) tool-name)
            (if (= :done (:kind e))
              (ctx/emit! c :tool.spare :info {:tool tool-name :text (str "carrying a spare " tool-name)})
              (ctx/emit! c :tool.no-spare :warn {:tool tool-name :durability (:uses-left worn) :reason why :tried (get-in e [:data :tried])
                                                 :text (str "the last " tool-name " is worn (" (:uses-left worn) " left) and no spare could be had"
                                                            (when why (str ": " (name why))))}))
            nil))))))
