(ns jobs.lib.hide-spot
  "Where the body shut itself in or pillared up lately (a :hid memory entry per hide, written by the flight): a stream of
  mobs that keeps coming back means hiding does not end it. repeated? tells the flight to stop hiding from that kind of mob
  here and fight or leave; near? marks the area for mining and digging errands, which look elsewhere first."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::memory-ms {:default 600000 :doc "Milliseconds a hide is remembered as a spot to stay away from." :spec (a/num-in 1 nil)}
   ::radius {:default 40 :doc "Blocks around a remembered hide that count as the same place." :spec (a/num-in 0 nil)}
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
  "The hides among hs within ::radius of pos."
  [hs pos]
  (filterv #(<= (u/dist pos (:pos %)) (settings/get settings ::radius)) hs))

(defn repeated?
  "Whether the body hid from one of mobs (names of mobs seen; a heard one counts for nothing) within ::radius of pos at least ::repeat-hides times lately."
  [c pos mobs]
  (let [ms (set mobs)]
    (and (seq ms)
         (<= (settings/get settings ::repeat-hides)
             (count (filter #(some ms (:mobs %)) (near (recent c) pos)))))))
