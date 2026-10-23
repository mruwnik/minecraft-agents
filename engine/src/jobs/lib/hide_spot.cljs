(ns jobs.lib.hide-spot
  "Where the body shut itself in or pillared up lately (a :hid memory entry per hide, written by the flight): a stream of
  mobs that keeps coming back means hiding does not end it. A hide counts for the cells the field of one of its mobs from
  its spot holds (jobs.lib.threat-field). repeated? tells the flight to stop hiding from that kind of mob here and fight or
  leave; near marks the area for mining and digging errands, which look elsewhere first."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.threat-field :as field]))

(a/defargs settings
  {::memory-ms {:default 600000 :doc "Milliseconds a hide is remembered as a spot to stay away from." :spec (a/num-in 1 nil)}
   ::repeat-hides {:default 3 :doc "Hides from one kind of mob in one place within ::memory-ms after which the flight no longer hides from it there (it fights or leaves)." :spec (a/int-in 1 nil)}})

(def policy {:cap 10 :ttl (* 10 60 1000)})

(defn remember!
  "Write a :hid entry for a hide at pos {:x :y :z} from the mob names mobs."
  [c pos mobs]
  (ctx/remember! c :hid {:pos pos :mobs (vec (distinct mobs))} policy))

(defn recent
  "The hides of c still within ::memory-ms, as {:pos :mobs}."
  [c]
  (let [now (ctx/now c) ttl (settings/get settings ::memory-ms)]
    (vec (keep #(when (< (- now (:t %)) ttl) (:data %)) (ctx/entries c :hid)))))

(defn near
  "The hides among hs that count for the cell of pos: the field of one of their mobs from the hide's spot holds it."
  [c hs pos]
  (filterv (fn [{:keys [mobs] :as h}] (some #(field/reaches? c {:mob % :pos (:pos h)} pos) mobs)) hs))

(defn repeated?
  "Whether the body hid from one of mobs (names of mobs seen; a heard one counts for nothing) at least ::repeat-hides times
  lately in hides that count for pos (near)."
  [c pos mobs]
  (let [ms (set mobs)]
    (and (seq ms)
         (<= (settings/get settings ::repeat-hides)
             (count (filter #(some ms (:mobs %)) (near c (recent c) pos)))))))
