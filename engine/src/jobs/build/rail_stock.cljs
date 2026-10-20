(ns jobs.build.rail-stock
  "How much of what a rail line lacks one fetch brings: a line may need more than the bag holds, so the body fetches a
  share of every item, builds with it, and restocks (jobs.build.rail-line). Pure.")

(def stack "Items to a slot of everything a rail line is made of (rails, torches, fill blocks)." 64)

(defn shares
  "The counts to fetch now for shortages [[key n choices]] (n still lacking each) with free empty slots: the same share of
  each item's need, the largest that fits, so the line grows station by station and no item runs out first. A share
  is whole (every n) when everything fits; at least 1 of each. -> [[key m choices]] with m <= n."
  [shortages free]
  (let [kinds (count shortages)
        room (* stack (max 1 (quot free (max 1 kinds))))
        share (min 1 (reduce min 1 (map (fn [[_ n _]] (/ room n)) shortages)))]
    (mapv (fn [[k n names]] [k (max 1 (min n (js/Math.ceil (* share n)))) names]) shortages)))
