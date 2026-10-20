(ns jobs.items.obtain.loose
  "The :loose way: pick up what lies in sight."
  (:require [engine.ctx :as ctx]
            [jobs.items.obtain.base :refer [loose-radius tried!]]
            [jobs.lib.loose :as loose]))

(defn seen
  "The loose item entities of names in sight, nearest first, that jobs.lib.loose lets a body take (zones, others' throws)."
  [c names]
  (let [wanted (set names)]
    (filterv #(contains? wanted (some-> (.-item %) .-name))
             (loose/candidates (:primitives c) (ctx/view c) (:world (:engine c)) {:always names :radius (loose-radius)} (:root c)))))

(defn ^:async step!
  "One jobs.items.collect-drops child for names: :continue while it waits, else the way is tried."
  [c names]
  (let [e (await (ctx/call c :loose 'jobs.items.collect-drops {:items names :radius (loose-radius)}))]
    (if (= :continue e) :continue (tried! c :loose (if (= :done (:kind e)) :done :none)))))
