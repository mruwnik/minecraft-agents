(ns engine.primitive-jobs-test
  "The primitive table (jobs.primitive) and the act check: before an act runs its most is checked against what is left
  of every frame of the chain; a crossing act is refused and ends the outermost crossed frame :limit-reached."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.expr :as expr]
            [engine.fake :as fake]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.primitive :as primitive]
            [jobs.primitive :as primitive-job]))

(def world (atom {}))
(def clock (atom 0))

(defn setup [jobs]
  (reset! world {})
  (reset! clock 1000000)
  (let [[seen sink] (tu/capture-sink)
        p (tu/fake {})
        eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen :p p}))

(defn job-ev [seen kind] (filterv #(= [:job kind] [(:source %) (:kind %)]) @seen))
(defn stopped [seen] (select-keys (:data (first (job-ev seen :stopped))) [:kind :cause]))
(defn calls [p method] (count (filter #(= method (.-name %)) (.-calls (.-world p)))))
(defn quote-of [k p args] ((get-in primitive/table [k :quote-of]) p args))

(deftest every-primitive-has-a-target-a-quote-and-an-ending
  (let [p (tu/fake {})]
    (is (= 29 (count primitive/table)))
    (doseq [[k {:keys [target args quote-of ending]}] primitive/table]
      (is (vector? target) k)
      (is (and (vector? args) (every? (set args) target)) (str k ": its args name the act's keys, the target's among them"))
      (is (nil? (contract/quote-problem (quote-of p {}))) k)
      (is (every? #(< (nth % 2) ##Inf) (keep #(get-in (quote-of p {}) [:uses %]) [:job-time :health :food]))
          (str k ": an act cannot be stopped midway, so its most is finite"))
      (is (contract/ending? (ending #js {:status "failed"})) k))))

(deftest a-walk-is-quoted-by-the-drops-its-path-could-take
  (let [p (tu/fake {})
        hp #(get-in (quote-of :moveTo p %) [:uses :health])]
    (fake/swap-self! p assoc :pos [0 70 0])
    (is (= [0 0 0] (hp {:pos {:x 0 :y 64 :z 6}})) "an uncapped walk drops at most 3 blocks, which hurts nothing")
    (is (= (hp {:pos {:x 0 :y 64 :z 6} :maxDropDown 3}) (hp {:pos {:x 0 :y 64 :z 6}})) "the default cap is 3 blocks fallen")
    (is (= [0 0 2] (hp {:pos {:x 0 :y 64 :z 6} :maxDropDown 4})) "6 down over 6: at most two 4-block drops, 1 hp each")
    (is (= [0 0 1] (hp {:pos {:x 0 :y 70 :z 5} :maxDropDown 4})) "level 5 away: a drop and the climb back")
    (is (= [0 0 0] (hp {:pos {:x 0 :y 70 :z 4} :maxDropDown 4})) "too near to drop 4 and climb back")
    (is (= [0 0 3] (hp {:pos {:x 0 :y 64 :z 6} :maxDropDown 6})) "one 6-block drop, 3 hp")
    (is (= [0 1 20] (get-in (quote-of :moveTo p {:pos {:x 0 :y 64 :z 0} :timeoutS 20}) [:uses :job-time])))))

(deftest a-dig-is-quoted-past-its-dig-time-and-wears-the-held-tool
  (let [p (tu/fake {})]
    (swap! (fake/state p) assoc :dig-ms 3000)
    (fake/swap-self! p assoc :held "iron_pickaxe")
    (let [q (quote-of :dig p {:pos {:x 0 :y 63 :z 0}})]
      (is (= 9 (get-in q [:uses :job-time 2])) "3 s of digging, the dig margin 5 and the drop wait 1")
      (is (= [0 1 1] (get-in q [:uses :wear "iron_pickaxe"]))))))

(deftest a-wait-is-quoted-its-time-and-offline-its-reconnect-too
  (let [p (tu/fake {})]
    (is (= [4 4 4] (get-in (quote-of :wait p {:ms 4000}) [:uses :job-time])))
    (is (= [10 10 10] (get-in (quote-of :wait p {:ms 60000}) [:uses :job-time])) "the wait's own cap")
    (is (= [0 20 240] (get-in (quote-of :offline p {:ms 20000}) [:uses :job-time]))
        "20 s away, then up to 3 reconnects of a 60 s spawn and a 10 s world load, 5 s apart")))

(deftest endings-read-the-primitives-status
  (let [end (get-in primitive/table [:dig :ending])]
    (is (= {:kind :done :cause :dug} (end #js {:status "dug"})))
    (is (= {:kind :limit-reached :cause {:line :job-time}} (end #js {:status "timeout"})))
    (is (= {:kind :nothing-possible :cause :failed} (end #js {:status "failed"})))
    (is (= {:kind :condition-failed :cause :unreachable} (end #js {:status "unreachable"})))))

(deftest an-act-that-ran-and-failed-is-nothing-possible-a-false-precondition-a-failed-condition
  (let [end #(primitive-job/read-ending ((get-in primitive/table [:dig :ending]) #js {:status %}))]
    (are [status kind] (= kind (:kind (end status)))
      "unreachable" :nothing-possible
      "partial" :nothing-possible
      "blocked" :nothing-possible
      "out-of-reach" :nothing-possible
      "cannot" :nothing-possible
      "already-mounted" :condition-failed
      "missing" :condition-failed
      "occupied" :condition-failed
      "gone" :condition-failed
      "not-night" :condition-failed
      "monsters-near" :condition-failed
      "unsupported" :impossible
      "failed" :nothing-possible)
    (is (= :unreachable (:cause (end "unreachable"))))))

(defn drops-to [y]
  {:defaults {:floor {:health 15}}
   :round (fn ^:async drop-round [c]
            (swap! world assoc :moved (.-status (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y y :z 9} :maxDropDown 4}))))
            :continue)})

(defn high-up [p]
  (fake/swap-self! p assoc :pos [0 70 0] :health 17))

(deftest a-drop-past-the-floor-is-refused-before-it-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'down (drops-to 64)})]
          (high-up p)
          (core/submit! eng '(down) {})
          (await (core/tick! eng))
          (is (zero? (calls p "moveTo")) "the act did not run")
          (is (= {:kind :limit-reached :cause {:line :health :floor 15 :act :moveTo}} (stopped seen))))))))

(deftest a-drop-within-the-floor-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'down (drops-to 66)})]
          (high-up p)
          (core/submit! eng '(down) {})
          (await (core/tick! eng))
          (is (= 1 (calls p "moveTo")))
          (is (empty? (job-ev seen :stopped))))))))

(deftest a-refusal-tells-the-acts-most-and-what-was-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'down (drops-to 64)})]
          (high-up p)
          (core/submit! eng '(down) {})
          (await (core/tick! eng))
          (is (= [{:cause {:line :health :floor 15 :act :moveTo} :most 3 :left 2}]
                 (mapv #(select-keys (:data %) [:cause :most :left]) (job-ev seen :limit_reached)))))))))

(defn places-in-a-section [n]
  {:defaults {:spend {:items {"cobblestone" 1}}}
   :round (fn ^:async place-round [c]
            (await (ctx/atomic c :comfort :wall
                               (fn ^:async wall []
                                 (dotimes [i n]
                                   (await (ctx/act c :place #js {:pos #js {:x i :y 64 :z 2} :item "cobblestone"}))))))
            :continue)})

(deftest the-refusal-is-not-held-by-an-atomic-section
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'wall (places-in-a-section 2)})]
          (fake/swap-self! p assoc :pos [0 64 0])
          (swap! (fake/state p) assoc :inventory [{:name "cobblestone" :count 5 :slot 36}])
          (core/submit! eng '(wall) {})
          (await (core/tick! eng))
          (is (= 1 (calls p "place")) "the second place would use a second cobblestone")
          (is (= {:kind :limit-reached :cause {:line :items :item "cobblestone" :spend 1 :act :place}}
                 (stopped seen))))))))

(defn completed [seen] (job-ev seen :completed))

(deftest a-primitive-is-called-by-name-as-a-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup registry/jobs)]
          (core/submit! eng '(jobs.primitive.look {:pos [1 64 1]}) {})
          (await (core/tick! eng))
          (is (= 1 (calls p "look")))
          (is (= 1 (count (completed seen)))))))))

(deftest a-primitive-job-ends-with-the-primitives-status
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup registry/jobs)]
          (core/submit! eng '(jobs.primitive.dig {:pos [0 60 0]}) {})
          (await (core/tick! eng))
          (is (= 1 (calls p "dig")))
          (is (= {:kind :condition-failed :cause :missing} (stopped seen))))))))

(deftest a-failed-act-says-what-it-met
  (are [r text] (= text (primitive-job/failure-text :place (js->clj r :keywordize-keys true)))
    #js {:status "occupied" :block "white_bed"} "place: occupied by white_bed"
    #js {:status "failed" :reason "Server refused (entities near the cell: player#3)"} "place: failed (Server refused (entities near the cell: player#3))"
    #js {:status "no-support"} "place: no-support"))

(deftest wait-and-offline-run-by-name-as-jobs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [form ['(jobs.primitive.wait {:ms 10}) '(jobs.primitive.offline {:ms 10 :why "test"})]]
          (let [{:keys [eng seen p]} (setup registry/jobs)
                k (name (first form))]
            (core/submit! eng form {})
            (await (core/tick! eng))
            (is (= 1 (calls p (subs k (inc (.lastIndexOf k "."))))) k)
            (is (= 1 (count (completed seen))) k)))))))

(def quoter
  {:defaults {:spend {:job-time 100}} :args {}
   :round (fn ^:async quoter-round [c]
            (swap! world assoc :quote (await (ctx/quote c 'jobs.primitive.eat {:item "bread"})))
            (ctx/end c :done nil))})

(deftest a-primitive-is-quoted-by-name
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc registry/jobs 'quoter quoter))]
          (core/submit! eng '(quoter) {})
          (await (core/tick! eng))
          (is (= (quote-of :eat p {:item "bread"}) (:quote @world)))
          (is (zero? (calls p "eat")) "a quote does not act"))))))

(deftest a-primitive-job-without-what-the-act-needs-is-impossible
  (let [can-start? (:can-start? (primitive-job/job :place 'jobs.primitive.place))]
    (is (= [:never {:reason :bad-args :missing [:pos :item]}] ((juxt first #(dissoc (nth % 2) :text)) (can-start? {:args {:pos nil}}))))
    (is (= :ok (first (can-start? {:args {:pos {:x 0 :y 64 :z 0} :item "dirt"}}))))))

(deftest a-primitive-job-with-a-wrong-valued-pos-is-refused-before-it-starts
  (is (re-find #"jobs.primitive.place :pos must be" (expr/problem registry/jobs '(jobs.primitive.place {:pos "x" :item "dirt"}))))
  (is (nil? (expr/problem registry/jobs '(jobs.primitive.place {:pos [0 64 0] :item "dirt"})))))

(defn waits-for [ms]
  {:round (fn ^:async wait-round [c]
            (await (ctx/act c :wait #js {:ms ms}))
            (swap! world update :waited (fnil inc 0))
            :continue)})

(defn ^:async run-one [jobs form check]
  (let [{:keys [eng seen p]} (setup jobs)]
    (fake/swap-self! p assoc :pos [0 64 0])
    (core/submit! eng form {})
    (await (core/tick! eng))
    (check seen p)))

(deftest an-act-past-the-time-left-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (run-one {'waits (assoc (waits-for 5000) :defaults {:spend {:job-time 3}})} '(waits)
                        (fn [seen p]
                          (is (zero? (calls p "wait")))
                          (is (= {:kind :limit-reached :cause {:line :job-time :spend 3 :act :wait}} (stopped seen))))))))))

(deftest an-act-past-the-deadline-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (run-one {'waits (waits-for 5000)} '(waits {:deadline 1002000})
                        (fn [seen p]
                          (is (zero? (calls p "wait")))
                          (is (= {:kind :limit-reached :cause {:line :deadline :deadline 1002000 :act :wait}}
                                 (stopped seen))))))))))

(def digs
  {:defaults {:spend {:wear {"iron_pickaxe" 0}}}
   :round (fn ^:async dig-round [c] (await (ctx/act c :dig #js {:pos #js {:x 0 :y 63 :z 0}})) :continue)})

(deftest a-dig-past-the-tool-wear-left-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'digs digs})]
          (fake/swap-self! p assoc :pos [0 64 0] :held "iron_pickaxe")
          (core/submit! eng '(digs) {})
          (await (core/tick! eng))
          (is (zero? (calls p "dig")))
          (is (= {:kind :limit-reached :cause {:line :wear :tool "iron_pickaxe" :spend 0 :act :dig}}
                 (stopped seen))))))))

(defn calls-kid [kid defaults]
  {:defaults defaults
   :round (fn ^:async parent-round [c]
            (await (ctx/call c :kid kid {:spend {:job-time 100}}))
            (swap! world assoc :parent-went-on true)
            :continue)})

(deftest a-clamped-chain-ends-its-outermost-crossed-frame
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (run-one {'top (calls-kid (waits-for 5000) {:spend {:job-time 4}})} '(top)
                        (fn [seen p]
                          (is (zero? (calls p "wait")))
                          (is (nil? (:parent-went-on @world)))
                          (is (= {:kind :limit-reached :cause {:line :job-time :spend 4 :act :wait}}
                                 (stopped seen))))))))))

(defn cleans-up-with [defaults]
  {:defaults defaults
   :round (fn ^:async cleanup-round [c]
            (fake/swap-self! (:primitives c) assoc :health 11)
            (try
              (await (ctx/act c :look #js {:yaw 0 :pitch 0}))
              (catch :default e
                (await (ctx/act c :wait #js {:ms 5000}))
                (swap! world assoc :cleaned-up true)
                (throw e)))
            :continue)})

(deftest an-unwinding-frame-may-act-past-its-own-lines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (run-one {'job (cleans-up-with {:floor {:health 12} :spend {:job-time 3}})} '(job)
                        (fn [seen p]
                          (is (= 1 (calls p "wait")) "the cleanup act ran")
                          (is (true? (:cleaned-up @world)))
                          (is (= {:kind :limit-reached :cause {:line :health :floor 12}} (stopped seen))))))))))

(deftest a-cleanup-act-past-an-outer-frames-line-ends-that-frame
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (run-one {'top (calls-kid (cleans-up-with {:floor {:health 12}}) {:spend {:job-time 4}})} '(top)
                        (fn [seen p]
                          (is (zero? (calls p "wait")) "refused: the outer frame has 4 s left")
                          (is (nil? (:cleaned-up @world)))
                          (is (nil? (:parent-went-on @world)))
                          (is (= {:kind :limit-reached :cause {:line :job-time :spend 4 :act :wait}}
                                 (stopped seen))))))))))
