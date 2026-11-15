(ns jobs.lib.torch-stock
  "What a dig stocks before it sets out, through jobs.lib.need: torches when few are carried, a spare for the last worn
  pickaxe. Both are optional: a get that fails only warns, and the dig goes on (while its cells are lit, or with the worn tool)."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.fetch-time :as fetch-time]
            [jobs.lib.need :as need]
            [jobs.lib.torch :as torch]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]))

(defn fetch-allows?
  "Whether the call lets it get torches. They need coal, so only when the ways include :gather (a mine run by obtain never
  starts a gather of its own)."
  [c]
  (contains? (need/fetch-how c) :gather))

(defn torch-way
  "The torch way (ready/dark-near-ok) of a job that hangs torches along `blocks` blocks: {:blocks :carried :item (the torch kind carried)} while it has torches
  carried or craftable, or can get them (fetch?: its own need/ get, or its caller's trip on :no-torches), else nil."
  [c blocks fetch?]
  (let [p (:primitives c)]
    (when (or fetch? (torch/lightable? p)) {:blocks blocks :carried (torch/torches-held p) :item (torch/carried-torch p)})))

(defn no-stock-text [held e]
  (let [tried (get-in e [:data :tried])]
    (str "carries " (if (zero? held) "no torches" (str "only " held " torches"))
         (if (zero? held)
           (str ", none made" (when (seq tried) (str " (" (str/join ", " (map (fn [[k v]] (str (name k) " " (if (keyword? v) (name v) v))) tried)) ")")))
           " and could not get more")
         (if (zero? held)
           ": the dig goes on over lit cells and stops :no-torches at the first dark one it must stair or tunnel through"
           ": the dig goes on with them")
         (when-let [w (get-in e [:data :why])] (str " (" w ")")))))

(defn ^:async torches!
  "Before a dig down or a tunnel sets out: with fewer than trip/torch-low torches carried, get them up to trip/torch-stock
  with a jobs.items.obtain child (slot :torches; chest, craft from coal or charcoal, charcoal by smelt, gather), and with
  coal or charcoal carried only what that fuel makes (4 each). Resolves nil to go on, :continue while the get waits.
  A get that ended :job-time (its chain ran longer than the quote its budget is, the logs and stone it made still carried) goes on as a fresh get,
  :continue, while another try is worth its price (cost/try-again? :child-round, a try priced at the get's own quote, fetch-time/items-s); any other end sets :torches-tried: info torch.stocked, or warn torch.no-stock with the get's cause (nothing when a caller is already getting torches: :duplicate-call). The body walks back to
  where it began (need :return?). Skipped without :gather in the call's ways."
  [c]
  (let [p (:primitives c)
        held (torch/torches-held p)
        fuel (torch/fuel-held p)
        want (- (trip/torch-stock) held)
        n (if (pos? fuel) (min want (* 4 fuel)) want)]
    (when (and (fetch-allows? c) (< held (trip/torch-low)) (not (:torches-tried (ctx/mem c))))
      (let [e (await (need/fetch! c 'jobs.items.obtain {:item "torch" :count n :avoid (trip/line-ahead c)} :need {:item "torch" :count n}
                                  {:slot :torches :return? true}))]
        (cond
          (= :continue e) :continue
          (and (= :job-time (need/cause-why e)) (<= (torch/torches-held p) held) (do (u/failed! c) (cost/try-again? c :child-round {:try-s (fetch-time/items-s ["torch"] n)})))
          :continue
          :else
          (let [now (torch/torches-held p)]
            (ctx/update-mem! c assoc :torches-tried true)
            (when (and (pos? fuel) (<= now held)) (ctx/update-mem! c assoc :craft-failed true))
            (cond
              (> now held)
              (ctx/emit! c :torch.stocked :info {:held now :text (str "carrying " now " torches for the dig")})
              (= :duplicate-call (need/cause-why e)) nil
              :else
              (ctx/emit! c :torch.no-stock :warn {:held now :reason (need/cause-why e) :tried (get-in e [:data :tried]) :text (no-stock-text now e)}))
            nil))))))
