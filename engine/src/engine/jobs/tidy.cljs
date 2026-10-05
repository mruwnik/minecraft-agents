(ns engine.jobs.tidy
  "Tidying up after trespassing. A job that breaks or places a block in another's zone or claim (a survival job's last
  resort, or any job run with :ignore-zones?) notes what it did.
  Before the act, `refusal` asks the rules (ignoring the :ignore-zones? opt-out) whether the cell is another's.
  After the act, `record!` writes a :tidy entry to body memory:
  {:cell [x y z] :action :dig|:place :was block-before :now block-after :zone/:claim/:plan :tries n :job id}.
  :job is the top-level job that recorded it; the :tidy-pending trigger waits until that job has ended.
  jobs.survival.restore-broken puts the cells back when the body is safe. Best effort, never at the cost of safety."
  (:require [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.combat :as combat]
            [engine.jobs.reach :as reach]
            [engine.jobs.tools :as tools]
            [engine.jobs.util :as u]))

(def tidy-policy
  "Body memory policy of the :tidy entries: they outlive a restart for six hours."
  {:cap 100 :ttl (* 6 60 60 1000)})

(def max-tries 3)

(def reported-policy
  "Body memory policy of :tidy-reported, the cells still waiting when restore-broken last ended."
  {:cap 1 :ttl (* 6 60 60 1000)})

(defn unsafe?
  "Whether the body should not be busy with other people's blocks now: health under :min-health or a hostile within
  :danger-radius."
  [p {:keys [min-health danger-radius]}]
  (or (< (.-health (.self p)) min-health)
      (boolean (seq (combat/hostiles p danger-radius)))))

(defn carried? [p item] (boolean (some #(= item (:name %)) (u/inventory p))))

(defn block-now [p cell] (u/block-name p (zipmap [:x :y :z] cell)))

(def half-width 0.3)
(def body-height 1.8)

(defn body-cells
  "The cells [x y z] the body's hitbox (0.6 wide, 1.8 tall, at pos {:x :y :z} its feet) intersects."
  [{:keys [x y z]}]
  (let [span (fn [lo hi] (range (js/Math.floor lo) (inc (js/Math.floor (- hi 1e-9)))))]
    (set (for [i (span (- x half-width) (+ x half-width))
               j (span y (+ y body-height))
               k (span (- z half-width) (+ z half-width))]
           [i j k]))))

(defn in-body?
  "Whether cell [x y z] is one the body's hitbox intersects (a block placed there would be refused)."
  [p cell]
  (let [pos (.-pos (.self p))]
    (contains? (body-cells {:x (.-x pos) :y (.-y pos) :z (.-z pos)}) (vec cell))))

(defn place-item
  "The carried item that puts back dug entry e: its :was block, else the first carried of :any-of (what the block
  drops, go-to escalation holes), or nil."
  [p {:keys [was any-of]}]
  (first (filter #(carried? p %) (distinct (cons was any-of)))))

(defn why-not
  "Why entry cannot be restored now, or nil: :changed (the cell no longer holds what was recorded), :not-carried (no
  place-item carried), :shut-in (a go-to escalation hole, :escalation, while the body is shut in: the hole is its way
  on) or :occupied (the block would be placed into the body)."
  [p {:keys [cell now action escalation] :as e}]
  (cond
    (not= now (block-now p cell)) :changed
    (and (= :dig action) (nil? (place-item p e))) :not-carried
    (and (= :dig action) escalation (reach/enclosed? p)) :shut-in
    (and (= :dig action) (in-body? p cell)) :occupied))

(defn unreachable?
  "Whether the body cannot get within reach blocks of entry's cell: it is farther off and has no walkable way to the
  cell (a sealed body). An unknown answer counts as reachable."
  [p {:keys [cell]} reach]
  (let [here (u/self-pos {:primitives p})
        at (zipmap [:x :y :z] cell)]
    (and (> (u/dist here {:x (+ 0.5 (:x at)) :y (:y at) :z (+ 0.5 (:z at))}) (inc reach))
         (not (reach/walkable-way? p here at)))))

(defn refusal
  "The verdict refusing action at pos ({:x :y :z}) for another's zone, claim or plan footprint, judged as if
  :ignore-zones? were off. nil when permitted or no zone list is read. Call before the act."
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
         r (await (ctx/act c :dig (clj->js {:pos pos})))
         _ (await (tools/note-wear! c))]
     (when (= "dug" (.-status r)) (record! c pend "air"))
     r)))

(defn ^:async place!
  "ctx/act :place of item at pos, noting a placed block of another's (see noting?). A click ({:against :cursor :yaw
  :pitch}) is passed on to the place act. Resolves to the act's result."
  ([c pos item] (place! c pos item false))
  ([c pos item last-resort?] (place! c pos item last-resort? nil))
  ([c pos item last-resort? click]
   (let [pend (when (noting? c last-resort?) (pending c :place pos))
         r (await (ctx/act c :place (clj->js (cond-> {:pos pos :item item} click (assoc :click click)))))]
     (when (= "placed" (.-status r)) (record! c pend item))
     r)))
