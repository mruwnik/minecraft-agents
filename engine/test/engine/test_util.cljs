(ns engine.test-util
  "Helpers shared by the cljs tests: temp dirs, the fake world, async tests."
  (:require [cljs.test :refer [is]]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake-world]
            [engine.fake.node :as node]
            [engine.fast-pace]
            [engine.memory :as mem]
            [jobs.lib.near :as near]))

(def require-here node/require-here)

(defonce made-dirs (atom []))

(defonce remove-dirs-on-exit
  (.on js/process "exit"
       (fn [] (run! #(fs/rmSync % #js {:recursive true :force true}) @made-dirs))))

(defn tmp-dir
  "A fresh temp dir, removed when the test process exits."
  []
  (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "engine-test-"))]
    (swap! made-dirs conj dir)
    dir))

(defn box
  "Blocks named name filling x0..x1, y0..y1, z0..z1, as a fake world's :blocks map."
  [x0 y0 z0 x1 y1 z1 name]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(defn floor
  "Stone one block thick under the body's usual feet height (y 63 unless given first), x0..x1, z0..z1: the ground a
  walk over the path planner needs (the planner reads blocks; the old moveTo teleported)."
  ([x0 z0 x1 z1] (floor 63 x0 z0 x1 z1))
  ([y x0 z0 x1 z1] (box x0 y z0 x1 y z1 "stone")))

(defn fake
  "A fake primitives object (engine.fake/create) built from a cljs spec map. The spec's :floor, [x0 z0 x1 z1], is stone at
  y 63 over that rectangle (floor) added under the spec's :blocks, for walks over the path planner."
  ([] (fake {}))
  ([spec]
   (let [spec (if-let [rect (:floor spec)]
                (-> spec (dissoc :floor) (update :blocks #(merge (apply floor rect) %)))
                spec)]
     (fake-world/create spec))))

(def walk-floor
  "The :floor rectangle most go-to callers' tests use: every spot they walk between."
  [-30 -10 40 10])

(defn fake-on-floor
  "fake with stone at y 63 under the walk-floor rectangle (or the spec's :floor), in the columns the spec's :blocks leave
  empty: the ground the walks of jobs that use jobs.lib.near/go-near! need, without touching ground a test built.
  The spec's :floor-block lays another block than stone (a test that digs stone needs a floor it does not dig)."
  [spec]
  (let [built (into #{} (map (fn [k] (let [[x _ z] (.split (name k) ",")] [x z]))) (keys (:blocks spec)))
        block (:floor-block spec "stone")
        ground (into {} (comp (remove (fn [[k _]] (let [[x _ z] (.split k ",")] (built [x z]))))
                              (map (fn [[k _]] [k block])))
                     (apply floor (:floor spec walk-floor)))]
    (fake (-> spec (dissoc :floor :floor-block) (assoc :blocks (merge ground (:blocks spec)))))))

(defn act-clock
  "A now fn for an engine over fake p: the clock atom's ms plus ms for every primitive call p has recorded, so time
  moves with the body's acts (a whole flight ends by time: out of line, its bound)."
  [clock p ms]
  #(+ @clock (* ms (.-length (.-calls (.-world p))))))

(defn seeing-all
  "p with seenBlocks answering every block in range as seen (stands in for perception's memory); leave it off to
  test a body that has seen nothing."
  [p]
  (aset p "seenBlocks" (fn [q] (.blocks p q)))
  (aset p "seenBlockAt" (fn [pos] (let [b (.blockAt p pos)]
                                    #js {:name (.-name b) :properties (.-properties b) :pos pos :age-ms 0})))
  p)

(defn blind
  "p with no seenBlocks: a body that has seen nothing, whatever the world holds."
  [p]
  (js-delete p "seenBlocks")
  (js-delete p "seenBlockAt")
  p)

(defn short-walks!
  "Make the fake's walks end early, as a steer that timed out after ticks ticks (the fake walks about 0.2 blocks a tick):
  a go-near! toward a target farther than that ends :partial. Only the first n walks when n is given."
  ([p ticks] (short-walks! p ticks js/Infinity))
  ([p ticks n]
   (let [walks (atom 0)]
     (.override (.-world p) "steer"
                (fn [token args impl]
                  (when (< (dec (swap! walks inc)) n) (set! (.-timeoutS args) (/ ticks 20)))
                  (impl token args))))))

(defn walk-calls
  "The calls the fake's world recorded that walk the body: the old moveTo and the planner's steer."
  [p]
  (filterv #(#{"moveTo" "steer"} (.-name %)) (.-calls (.-world p))))

(defn walked-to
  "The targets go-to walked to, in order (it writes a :moved entry per walk), over engine eng."
  [eng]
  (mapv (comp :target :data) (mem/entries (mem/view (:store eng)) :moved)))

(defn pos [x y z] #js {:x x :y y :z z})

(defn ^:async tick-until-idle!
  "Tick eng until its job list is empty, at most max-ticks times."
  [eng max-ticks]
  (loop [i 0]
    (when (and (< i max-ticks) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

(defn ^:async each-async
  "Call the promise-returning (f row) on each of rows in turn: a table of cases without a loop in the test."
  [rows f]
  (doseq [row rows] (await (f row))))

(defn run-async
  "Run the promise-returning thunk f inside a cljs.test async block."
  [done f]
  (-> (js/Promise.resolve)
      (.then f)
      (.catch (fn [e] (is (nil? e) (str "async test threw: " e "\n" (.-stack e)))))
      (.finally done)))

(defn read-json [file]
  (js->clj (js/JSON.parse (fs/readFileSync file "utf8")) :keywordize-keys true))

(defn capture-sink
  "An events sink and the atom it collects into."
  []
  (let [seen (atom [])]
    [seen (fn [e] (swap! seen conj e))]))

(defn legacy-event
  "Test-only adapter for older scheduler/job assertions. Production consumers
  should inspect canonical :context and :data instead."
  [event]
  (let [context (:context event)
        projected (merge {:seq (:seq event) :t (:time-ms event) :body "Fake"}
                         (dissoc context :job-id :reflex-id :cause-seq)
                         (:data event)
                         (select-keys event [:source :kind :level :request-id :attention]))]
    (cond-> projected
      (:job-id context) (assoc :job (:job-id context))
      (:reflex-id context) (assoc :reflex (:reflex-id context))
      (:cause-seq context) (assoc :cause (:cause-seq context))
      (some? (:message event)) (assoc :text (:message event)))))

(defn legacy-capture-sink
  "Legacy-shaped test sink. Kept separate so direct appender tests exercise
  the canonical event API without a compatibility projection."
  []
  (let [seen (atom [])]
    [seen (fn [e] (swap! seen conj (legacy-event e)))]))

(defn kinds
  "The \"source.kind\" names of collected events, in order."
  [seen]
  (mapv #(str (name (:source %)) "." (name (:kind %))) @seen))

(defn ^:async with-near-stub
  "Run the async f with jobs.lib.near/go-near! replaced by stub, restored after."
  [stub f]
  (let [k "cljs$core$IFn$_invoke$arity$4" ; callers use the 4-arity, which the compiler calls directly
        orig (aget near/go-near! k)]
    (aset near/go-near! k stub)
    (try (await (f))
         (finally (aset near/go-near! k orig)))))

(defn ^:async with-near-spy
  "Run the async f with every jobs.lib.near/go-near! call recorded as [pos range opts] in the atom calls*, the walk done."
  [calls* f]
  (let [k "cljs$core$IFn$_invoke$arity$4"
        orig (aget near/go-near! k)]
    (aset near/go-near! k (fn ^:async spy [c pos range opts]
                            (swap! calls* conj [pos range opts])
                            (await (orig c pos range opts))))
    (try (await (f))
         (finally (aset near/go-near! k orig)))))

(defn as-near
  "The go-near! arguments [pos range opts] for walk-style args [pos range opts]: :timeout-s becomes :leg-s, no escalation."
  [[pos range opts]]
  [pos range (cond-> (merge {:escalate false} (dissoc opts :timeout-s))
               (:timeout-s opts) (assoc :leg-s (:timeout-s opts)))])

(defn shutdown-at!
  "On the nth call of the fake act named act, the engine shuts down (the round is cut, the act undone): the job stays
  listed with its memory, for a restart over the same dir."
  [{:keys [p eng]} act n]
  (let [k (atom 0)]
    (.override (.-world p) act
               (fn [token a impl]
                 (if (= n (swap! k inc))
                   (do (core/shutdown! eng)
                       (js/Promise.reject (core/cut-error)))
                   (impl token a))))))

(defn ^:async run-until-empty
  "Tick eng until its job list is empty, at most n ticks; the ticks run."
  [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome
  "Run job with args as the child of a recording parent until the list is empty, at most n ticks; the child's result."
  [eng job args n]
  (let [out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out))
