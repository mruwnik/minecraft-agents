(ns triggers.survival.needs-bed
  "The needs-bed trigger: by day, a body whose respawn bed is lost (reset, found gone, or seen not a bed) or overdue for sleep (sh-beds/max-days-awake in-game days since it last rested:
  sh-beds/rest-since, its sleep or respawn, else the first thing it remembers) with
  no bed carried and none remembered within the urgent radius gets one (jobs.items.obtain: wool from sheep, planks), so
  the night job can put it down and sleep before phantoms come. A when-convenient errand: it holds only in the last
  :lead-ticks of the day (early on, the body's own work goes first), and its default job is a :priced obtain: each
  walk of its search is priced against what the bed is still worth times the chance a look there finds sheep. Not in the first light or with a hostile known near (jobs.lib.dawn),
  nor while a listed job will bring a bed before dusk for less than getting one now (jobs.lib.makers/waiting-pays?: a
  from-plan whose plan has one, an obtain of one; a bed after dusk misses the night), nor while the body knows no way to
  the wool that pays (wool-way?): a body whose land shows no sheep gets on with its work until it sees one or walks on."
  (:require [clojure.string :as str]
            [engine.notes :as notes]
            [engine.settings :as settings]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.body :as body]
            [jobs.lib.cost :as cost]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.look :as look]
            [jobs.lib.makers :as makers]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(def defaults {:lead-ticks 6000})

(defn why
  "Why a body by day needs a bed: {:why :spawn-lost :pos p} (sh-spawn/spawn-lost-pos), {:why :unslept :days d} (sh-beds/max-days-awake
  passed and sh-spawn/standing-bed finds none within (sh-beds/urgent-bed-radius)), else nil."
  [p view]
  (if-let [pos (sh-spawn/spawn-lost-pos p view)]
    {:why :spawn-lost :pos pos}
    (let [days (or (sh-beds/days-awake-in view) 0)]
      (when (and (>= days (sh-beds/max-days-awake))
                 (nil? (sh-spawn/standing-bed p view (sh-beds/urgent-bed-radius))))
        {:why :unslept :days days}))))

(def bed-wool "Wool of one colour the game's bed recipe takes." 3)

(defn wool-carried
  "Wool carried by colour (\"white\"), every 4 string counting a white one (the game's string recipe)."
  [items]
  (let [t (storage/carried-totals items)]
    (merge-with + {"white" (quot (get t "string" 0) 4)}
                (into {} (keep (fn [[k v]] (when (str/ends-with? k "_wool") [(subs k 0 (- (count k) 5)) v]))) t))))

(defn wool-way?
  "Whether the body knows a way to a bed's wool that pays: a colour whose wool carried and the adult unsheared sheep of
  it in sight a hunt or shear takes (animal-spare/takeable-adults: a hunt spares the last wool-keep, shears none) make
  bed-wool, or a look round where it stands worth a trip: cost/look-chance over the ground the notes (kn's store)
  searched for sheep in vain round here, times what the bed is worth. Ground nobody searched for sheep pays; every look
  that found none lowers the chance, so the errand comes again only on new ground or with sheep enough seen."
  [p view kn]
  (let [at (u/self-pos {:primitives p})
        inv (u/inventory p)
        spared (if (spare/shears-at-hand inv) 0 spare/wool-keep)
        sheep (frequencies (keep #(when-not (or (true? (.-baby %)) (true? (.-sheared %))) (.-color %))
                                 (look/seen-objs p {:names ["sheep"]})))
        wool (merge-with + (wool-carried inv) (into {} (map (fn [[k n]] [k (spare/takeable-adults spared n)])) sheep))
        ns (some-> (:notes kn) (notes/all (:now view)))
        r (some #(when (cost/searched-for? ["sheep"] %) (:r %)) ns)]
    (boolean
     (or (some #(>= % bed-wool) (vals wool))
         (nil? r)
         (> (* (cost/look-chance ns ["sheep"] [(:x at) (:z at)] r) (cost/worth-to (cost/death-body p view) ["white_bed"] at))
            (cost/trip))))))

(defn needs-bed
  "Holds by day, in the last :lead-ticks before dusk, awake, on the surface (wool and planks are not underground), with no bed item carried, no listed job worth waiting for and a way to the wool (wool-way?), when why names a reason."
  [p view args kn live]
  (boolean
   (and (< 0 (- sh-clock/dusk-tick (.-timeOfDay (.self p))) (:lead-ticks (merge defaults args)))
        (not (body/recovery-pending? view :needs-bed))
        (not (sh-sense/night? p))
        (not (sh-sense/sleeping? p))
        (not (sh-sense/buried? p))
        (dawn/calm? p)
        (nil? (sh-beds/carried-bed p))
        (not (makers/waiting-pays? p view kn live cost/bed?
                                   (/ (settings/ticks->ms (- sh-clock/dusk-tick (.-timeOfDay (.self p)))) 1000)))
        (some? (why p view))
        (wool-way? p view kn))))
