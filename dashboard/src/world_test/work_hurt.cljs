(ns world-test.work-hurt
  "tools/event-counts.mjs work-hurt: recounts jobs.lib.cost.risk ::work-hurt, the hurt and the hits of a job's own work by its fit? category
  (:long, :work), from the bodies' event logs (main and debug, each body's lines by time).
  Each event that names a job (:job-id) makes that job the innermost running one until the next does; a root's :completed, :stopped,
  :cancelled or :failed ends it. The seconds to the next event (gaps over max-gap-s dropped) and a :hurt meanwhile go to its category:
  that of the innermost job on its path that binds one (long-jobs, work-jobs; a path's job named by its root's :queued or an obtain's
  :obtain.gathering :via, else by its slot, long-slots, work-slots). Not counted: reflex jobs (fights, flights, eating), paths with a
  walk, search, flight or fight slot (moves), paths whose innermost named job or plumbing slot (an obtain's or a fetch's need) comes
  before a binding one, and seconds after a :holding, :waiting, :idle or :queued event (held still or not started).
  A hurt's :amount is a hit; one that left health 0 killed, so the hit was at least that (a lower bound).
  :at-least is the hits of at least k hp, k 1 to 20. A stay is the run of one outermost binding job (gen and path): every
  second its path is innermost or under it, children and walks too, reflexes and holds not; :stay-s their mean.
  Pure parts: scan-line, category, tally."
  (:require [clojure.string :as str]))

(def max-gap-s "Seconds between two events over which the gap is not counted (the body offline or idle)." 30)

(def long-jobs #{"jobs.gather.mine" "jobs.gather.dig-for" "jobs.gather.shaft-descent" "jobs.gather.strip-tunnel" "jobs.access.tunnel"
                 "jobs.build.pen" "jobs.build.path" "jobs.build.rail-line"})
(def work-jobs #{"jobs.access.stair" "jobs.access.bridge" "jobs.access.pillar" "jobs.access.clear-path" "jobs.build.from-plan"
                 "jobs.build.rail" "jobs.blocks.clear-cells" "jobs.animals.herd" "jobs.village.breed" "jobs.village.roll"
                 "jobs.gather.get-seeds" "jobs.gather.cast-obsidian" "jobs.survival.restore-broken" "jobs.movement.boat-launch"
                 "jobs.movement.boat-recover" "jobs.movement.mount"})
(def long-slots #{"tunnel" "strip" "shaft"})
(def work-slots #{"stair" "build" "pillar" "bridge" "trees" "clear"})
(def move-slot? #(or (str/includes? % "walk") (#{"go" "goto" "escalation" "step-off" "flee" "fight" "attack" "cornered" "back-off" "quote" "pause" "leg" "search" "explore"} %)))
(def plumbing-slot? "Slots of an obtain, a fetch or a bill: another job's need, not the binding job's own work." #(re-find #"^(need|fetch|obtain|bill|materials|spare|full|loose|torches|food)(-|$)" %))
(def ends #{"completed" "stopped" "cancelled" "failed"})
(def holds "Kinds after which the job is not working: held still, or queued and not started." #{"holding" "waiting" "idle" "queued"})

(defn- num-of [re line] (some-> (re-find re line) second js/parseFloat))

(defn scan-line
  "One log line -> {:t :gen :kind :job :reflex? :name :via :amount :health}, or nil without a :time-ms."
  [line]
  (when-let [t (num-of #":time-ms (\d+)" line)]
    (let [kind (second (re-find #":kind :([\w.-]+)" line))]
      (cond-> {:t t :gen (second (re-find #":generation-id \"([^\"]*)\"" line)) :kind kind
               :job (second (re-find #":job-id \"([^\"]*)\"" line)) :reflex? (str/includes? line ":reflex-id ")}
        (= "queued" kind) (assoc :name (second (re-find #":name \"([^\"]*)\"" line)))
        (= "obtain.gathering" kind) (assoc :via (second (re-find #":via ([\w.-]+)" line)) :item (second (re-find #":item \"([^\"]*)\"" line)))
        (and (= "hurt" kind) (str/includes? line ":source :body")) (assoc :amount (num-of #":amount (-?[\d.]+)" line) :health (num-of #":health (-?[\d.]+)" line))))))

(defn category
  "[category binding-path] of the innermost job path (a job-id), names a fn of a path to its job name; nil when not counted."
  [path names]
  (let [segs (str/split path #"/")
        paths (map #(str/join "/" (take % segs)) (range 1 (inc (count segs))))]
    (when-not (some move-slot? (rest segs))
      (reduce (fn [_ [p seg]]
                (let [n (names p)]
                  (cond (or (long-jobs n) (long-slots seg)) (reduced [:long p])
                        (or (work-jobs n) (some #(str/starts-with? (str n) %) ["jobs.farm." "jobs.forestry." "jobs.apiary."]) (work-slots seg)) (reduced [:work p])
                        (or n (plumbing-slot? (str seg))) (reduced nil))))
              nil (reverse (map vector paths (cons nil (rest segs))))))))

(defn outermost
  "[category binding-path] of the outermost job on path that binds a category (the job being started, its children and
  walks its run), names as category's; nil with none."
  [path names]
  (let [segs (str/split path #"/")]
    (some (fn [i]
            (let [p (str/join "/" (take i segs)) seg (when (> i 1) (nth segs (dec i))) n (names p)]
              (cond (or (long-jobs n) (long-slots seg)) [:long p]
                    (or (work-jobs n) (some #(str/starts-with? (str n) %) ["jobs.farm." "jobs.forestry." "jobs.apiary."]) (work-slots seg)) [:work p])))
          (range 1 (inc (count segs))))))

(defn- name-of [names {:keys [gen kind job name via item]}]
  (cond (and (= "queued" kind) job name) (assoc names [gen job] name)
        (and (= "obtain.gathering" kind) job via item) (assoc names [gen (str job "/gather-" item)] via)
        :else names))

(def blank {:s 0 :hp 0 :hits 0 :lethal 0 :at-least (vec (repeat 20 0)) :stays {}})

(defn- add-hit [row amount lethal?]
  (-> row
      (update :hp + amount)
      (update :hits inc)
      (update :lethal + (if lethal? 1 0))
      (update :at-least (fn [v] (mapv #(+ %1 (if (>= amount (inc %2)) 1 0)) v (range 20))))))

(defn tally
  "{category row} of one body's records (scan-line's, sorted by :t); row as blank, :stays {[gen path] seconds} of each
  outermost binding job's run (its children and walks too)."
  [records]
  (loop [[r & more] records names {} cur nil counts {}]
    (if (nil? r)
      counts
      (let [names (name-of names r)
            working? (and cur (not (:reflex? cur)) (not (holds (:kind cur))) (:job cur))
            named #(names [(:gen cur) %])
            cat (when working? (first (category (:job cur) named)))
            [outer at] (when working? (outermost (:job cur) named))
            gap-s (when cur (/ (- (:t r) (:t cur)) 1000))
            counted? (and cur (= (:gen cur) (:gen r)) (<= gap-s max-gap-s))
            counts (cond-> counts
                     (and cat counted?) (update cat #(update (or % blank) :s + gap-s))
                     (and outer counted?) (update outer #(update-in (or % blank) [:stays [(:gen cur) at]] (fnil + 0) gap-s))
                     (and cat (:amount r) (pos? (:amount r)))
                     (update cat #(add-hit (or % blank) (:amount r) (<= (or (:health r) 1) 0))))
            next (cond (and (:job r) (ends (:kind r)) (not (str/includes? (:job r) "/"))) (assoc r :job nil)
                       (:job r) r
                       cur (assoc cur :t (:t r) :gen (:gen r))
                       :else nil)]
        (recur more names next counts)))))

(defn report
  "The tallies of every body ({category row}) merged: per category {:s :hp :hits :lethal :at-least :hp-per-s :stays :stay-s :median-stay-s}."
  [tallies]
  (let [merged (apply merge-with (fn [a b] (-> (merge-with + (dissoc a :at-least :stays) (dissoc b :at-least :stays))
                                                (assoc :at-least (mapv + (:at-least a) (:at-least b)) :stays (merge (:stays a) (:stays b)))))
                      tallies)]
    (update-vals merged (fn [{:keys [s hp stays] :as row}]
                          (let [secs (sort (vals stays))]
                            (-> (dissoc row :stays)
                                (assoc :hp-per-s (when (pos? s) (/ hp s)) :stays (count secs)
                                       :stay-s (when (seq secs) (/ (reduce + secs) (count secs)))
                                       :median-stay-s (when (seq secs) (nth secs (quot (count secs) 2))))))))))
