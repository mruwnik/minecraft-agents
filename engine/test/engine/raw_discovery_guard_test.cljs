(ns engine.raw-discovery-guard-test
  "Jobs decide only on what the body has seen or heard (jobs.lib.look). A direct primitive read of the world
  (.entities, .blocks, .findBlocks) sees through walls and past the horizon, so each one is listed here:
  `legit` with the reason the raw read is right, `owed` for the ones still to move onto perception (card 8a976e90)."
  (:require [cljs.test :refer [deftest is]]
            [clojure.string :as str]
            ["fs" :as fs]
            ["path" :as path]))

(def legit
  {"items/use.cljs" [1 "re-finds the one entity the call already named by id"]
   "items/give.cljs" [1 "finds the player the order names"]
   "movement/follow.cljs" [1 "finds the player the order names"]
   "movement/leave_vehicle.cljs" [1 "re-finds the body's own vehicle by id"]
   "movement/boat_drive.cljs" [1 "re-finds the body's own boat by id"]
   "lib/threats.cljs" [1 "only compares ids against hostiles perception already listed"]})

(def owed
  (into {} (map (fn [[f n]] [f [n "raw read to move onto jobs.lib.look (card 8a976e90)"]]))
        {"lib/shelter/beds.cljs" 1 "lib/shelter/sleepers.cljs" 1}))

(def raw-read #"\(\.(?:entities|blocks|findBlocks)[\s)]|\.findBlocks")

(defn files-under [dir]
  (mapcat (fn [n]
            (let [p (path/join dir n)]
              (if (.isDirectory (.statSync fs p)) (files-under p) [p])))
          (array-seq (.readdirSync fs dir))))

(defn raw-reads
  "{file-under-src/jobs read-count} for every jobs file reading the world raw, jobs.lib.look (the perception layer) aside."
  []
  (into {}
        (for [f (files-under "src/jobs")
              :let [rel (subs f (count "src/jobs/"))
                    n (count (re-seq raw-read (.readFileSync fs f "utf8")))]
              :when (and (str/ends-with? rel ".cljs") (not= rel "lib/look.cljs") (pos? n))]
          [rel n])))

(deftest raw-world-reads-in-jobs-are-listed
  (let [listed (merge-with (fn [a _] (throw (js/Error. (str "listed twice: " a)))) (update-vals legit first) (update-vals owed first))]
    (is (= listed (raw-reads))
        "a job reads the world raw: go through jobs.lib.look (seen-entities, seen-block), or list the file in legit with a reason; delete entries that no longer read")))

(deftest nothing-calls-blockAt-but-util
  (is (= []
         (vec (for [dir ["src/jobs" "src/triggers"]
                    f (files-under dir)
                    :when (and (str/ends-with? f ".cljs") (not= f "src/jobs/lib/util.cljs")
                               (re-find #"\.blockAt\b" (.readFileSync fs f "utf8")))]
                f)))
      "p.blockAt is the body's sensed answer: read cells through jobs.lib.util (block-at, seen-block, feel), never .blockAt"))
