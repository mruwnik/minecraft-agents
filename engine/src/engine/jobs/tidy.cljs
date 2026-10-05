(ns engine.jobs.tidy
  "Tidying up after trespassing. A job that breaks or places a block in another's zone or claim (a survival job's last
  resort, or any job run with :ignore-zones?) notes what it did: before the act, `refusal` asks the rules (zones
  ignored by no opt-out) whether the cell is another's; after the act, `record!` writes a :tidy entry to body memory
  {:cell [x y z] :action :dig|:place :was block-before :now block-after :zone/:claim/:plan :tries n :job id}, :job the
  top-level job that recorded it (the trigger :tidy-pending waits until that job has ended). The
  jobs.survival.restore-broken job puts the cells back when the body is safe. Best effort, never at the cost of safety."
  (:require [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.combat :as combat]
            [engine.jobs.util :as u]))

(def tidy-policy
  "Body memory policy of the :tidy entries: they outlive a restart for six hours."
  {:cap 100 :ttl (* 6 60 60 1000)})

(def max-tries 3)

(def reported-policy
  "Body memory policy of :tidy-reported, the cells still waiting when restore-broken last ended."
  {:cap 1 :ttl (* 6 60 60 1000)})

(defn unsafe?
  "Whether the body (primitives p) should not be busy with other people's blocks now: health under :min-health or a
  hostile within :danger-radius."
  [p {:keys [min-health danger-radius]}]
  (or (< (.-health (.self p)) min-health)
      (boolean (seq (combat/hostiles p danger-radius)))))

(defn carried? [p item] (boolean (some #(= item (:name %)) (u/inventory p))))

(defn block-now [p cell] (u/block-name p (zipmap [:x :y :z] cell)))

(defn in-body?
  "Whether cell [x y z] is one the body stands in (feet or head)."
  [p [x y z]]
  (let [pos (.-pos (.self p))
        feet [(js/Math.floor (.-x pos)) (js/Math.floor (.-y pos)) (js/Math.floor (.-z pos))]]
    (boolean (some #{[x y z]} [feet (update feet 1 inc)]))))

(defn why-not
  "Why entry cannot be restored now (:changed :not-carried :occupied: a block put back would be placed into the
  body), or nil."
  [p {:keys [cell now action was]}]
  (cond
    (not= now (block-now p cell)) :changed
    (and (= :dig action) (not (carried? p was))) :not-carried
    (and (= :dig action) (in-body? p cell)) :occupied))

(defn refusal
  "The refusing verdict (a zone, claim or plan footprint of another's) for action at pos ({:x :y :z}), judged as if
  the job's :ignore-zones? were off; nil when the cell is permitted or no zone list is read. Call before the act."
  [c action pos]
  (access/trespass-refusal (access/rules-input c {:ignore-zones? false}) action pos))

(defn pending
  "What to remember about acting (:dig or :place) on pos: {:cell :action :was :zone :claim :plan} when the cell is
  another's, else nil. Reads the block, so call it before the act."
  [c action pos]
  (when-let [v (refusal c action pos)]
    (merge {:cell (access/cell pos) :action action :was (u/block-name (:primitives c) pos)}
           (select-keys v [:zone :claim :plan]))))

(defn record!
  "Remember pending entry p (see pending) with the block now at its cell; nil p does nothing."
  [c p now]
  (when p
    (ctx/remember! c :tidy (assoc p :now now :tries 0 :job (:root c)) tidy-policy)))

(defn entries
  "The :tidy data maps, oldest first."
  [c]
  (mapv :data (ctx/entries c :tidy)))

(defn forget-cell! [c cell]
  (ctx/forget-where! c :tidy #(= cell (:cell %))))

(defn count-try!
  "Replace the entry for cell with one counting another try."
  [c cell]
  (let [e (first (filter #(= cell (:cell %)) (entries c)))]
    (forget-cell! c cell)
    (ctx/remember! c :tidy (update e :tries inc) tidy-policy)))

(defn noting?
  "Whether acts of this job are noted: a job run with :ignore-zones?, or any act with last-resort? set (a survival
  job's trespass, chosen because no permitted option existed)."
  [c last-resort?]
  (boolean (or last-resort? (:ignore-zones? (:args c)))))

(defn ^:async dig!
  "ctx/act :dig at pos, noting a dug block of another's (see noting?). Resolves to the act's result."
  ([c pos] (dig! c pos false))
  ([c pos last-resort?]
   (let [pend (when (noting? c last-resort?) (pending c :dig pos))
         r (await (ctx/act c :dig (clj->js {:pos pos})))]
     (when (= "dug" (.-status r)) (record! c pend "air"))
     r)))

(defn ^:async place!
  "ctx/act :place of item at pos, noting a placed block of another's (see noting?). Resolves to the act's result."
  ([c pos item] (place! c pos item false))
  ([c pos item last-resort?]
   (let [pend (when (noting? c last-resort?) (pending c :place pos))
         r (await (ctx/act c :place (clj->js {:pos pos :item item})))]
     (when (= "placed" (.-status r)) (record! c pend item))
     r)))
