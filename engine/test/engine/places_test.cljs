(ns engine.places-test
  "Named places: jobs.lib.places (pure), jobs.memory.set-place and forget-place against the fake world, and the
  self-recording of the sleep and deposit jobs."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.job-api :as api]
            [engine.memory :as mem]
            [jobs.lib.places :as places]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]))

;; ------------------------------------------------------------------ pure

(deftest names-are-short-lowercase-keywords
  (are [in out] (= out (:name (places/parse-name in)))
    :home :home
    "home" :home
    "old-farm-2" :old-farm-2
    :bed :bed))

(deftest bad-names-are-refused-with-a-reason
  (are [in] (= :bad-name (:reason (places/parse-name in)))
    nil "" "Home" "two words" "9lives" :job/j1 :a/b "-x" 42
    (apply str (repeat 33 "a"))))

(deftest names-the-engine-uses-for-its-own-memory-are-reserved
  (are [in] (= :reserved-name (:reason (places/parse-name in)))
    :hurt :slept :moved :notify :bed-unreachable :chest-unusable :needs-bed :restart :picked-up :chat :log-out :weather-changed))

(deftest positions-take-a-vector-or-a-map-and-floor
  (are [in out] (= out (:pos (places/parse-pos in)))
    [1 64 2] {:x 1 :y 64 :z 2}
    {:x 1 :y 64 :z 2} {:x 1 :y 64 :z 2}
    [1.7 64.0 -2.2] {:x 1 :y 64 :z -3}
    {:x -0.5 :y 70.9 :z 3} {:x -1 :y 70 :z 3}))

(deftest bad-positions-are-refused-with-a-reason
  (are [in] (= :bad-pos (:reason (places/parse-pos in)))
    [1 2] [1 2 3 4] "1 2 3" {:x 1 :y 2} {:x 1 :y "2" :z 3} [1 js/NaN 3] [1 js/Infinity 3]
    [0 -65 0] [0 321 0] [30000001 64 0]))

(def world-blocks {"5,64,5" "white_bed" "5,64,6" "red_bed" "9,64,9" "chest" "9,64,10" "chest"
                   "20,64,20" "chest"})

(defn block-at [pos] (get world-blocks (str (:x pos) "," (:y pos) "," (:z pos)) "air"))

(deftest a-block-is-found-at-the-position-first
  (are [wanted at found] (= found (:pos (places/find-block block-at at wanted)))
    "white_bed" {:x 5 :y 64 :z 5} {:x 5 :y 64 :z 5}
    "bed" {:x 5 :y 64 :z 5} {:x 5 :y 64 :z 5}
    "chest" {:x 9 :y 64 :z 10} {:x 9 :y 64 :z 10}))

(deftest a-block-one-off-is-found-when-it-is-the-only-one
  (are [wanted at found] (= found (:pos (places/find-block block-at at wanted)))
    "chest" {:x 21 :y 64 :z 20} {:x 20 :y 64 :z 20}
    "chest" {:x 20 :y 65 :z 21} {:x 20 :y 64 :z 20}
    "white_bed" {:x 4 :y 64 :z 4} {:x 5 :y 64 :z 5}))

(deftest two-candidates-near-the-position-are-ambiguous
  (let [both (places/find-block block-at {:x 8 :y 64 :z 10} "chest")]
    (is (= :ambiguous (:reason both)))
    (is (= [{:x 9 :y 64 :z 9} {:x 9 :y 64 :z 10}] (:candidates both))))
  (is (= :ambiguous (:reason (places/find-block block-at {:x 5 :y 65 :z 5} "bed")))
      "bed matches both halves of different colours within one"))

(deftest a-block-that-is-not-there-says-what-is
  (are [wanted at reason] (= reason (:reason (places/find-block block-at at wanted)))
    "chest" {:x 0 :y 64 :z 0} :no-such-block
    "white_bed" {:x 5 :y 64 :z 7} :no-such-block
    "bed" {:x 9 :y 64 :z 9} :no-such-block))

(deftest an-unloaded-position-is-not-loaded
  (is (= :not-loaded (:reason (places/find-block (constantly nil) {:x 0 :y 64 :z 0} "chest")))))

(deftest a-place-is-gone-when-its-block-is-loaded-and-not-of-its-kind
  (are [kind block gone] (= gone (places/gone? kind block))
    :bed "red_bed" false
    :bed "air" true
    :bed nil false
    :chest "chest" false
    :chest "barrel" false
    :chest "air" true
    :chest nil false
    :home "air" false
    :home nil false))

;; ------------------------------------------------------------------ fixtures

(defn setup
  ([world] (setup world (tu/tmp-dir)))
  ([world dir]
   (let [clock (atom 1000000)
         [seen sink] (tu/legacy-capture-sink)
         p (tu/fake (merge {:floor tu/walk-floor} world))
         eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir dir :now #(deref clock)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     {:eng eng :p p :seen seen :clock clock :dir dir})))

(defn ^:async run-job
  "Submit spec and tick until the list is empty (at most 8 ticks)."
  [eng spec]
  (core/submit! eng spec {})
  (await (run-until-empty eng 8)))

(defn events-of [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn place [eng kind] (mem/place (mem/view (:store eng)) kind))

(defn entries [eng kind] (mapv :data (mem/entries (mem/view (:store eng)) kind)))

(defn know-place! [eng kind pos] (mem/write! (:store eng) kind {:pos pos} mem/place-policy))

(defn run-world
  "Run (f {:eng :p :seen ...}) over a fresh fake world built from spec, inside an async test."
  [done spec f]
  (tu/run-async done (fn ^:async t [] (await (f (setup spec))))))

(def set-place 'jobs.memory.set-place)
(def forget-place 'jobs.memory.forget-place)

;; ------------------------------------------------------------------ set-place

(deftest set-place-records-a-position
  (async done (run-world done {} (fn ^:async t [{:keys [eng seen]}]
    (await (run-job eng (list set-place {:name :home :pos [3 64 5]})))
    (is (= {:x 3 :y 64 :z 5} (place eng :home)))
    (is (= [{:name :home :pos {:x 3 :y 64 :z 5}}]
           (mapv #(select-keys % [:name :pos]) (events-of seen :place.set))))
    (is (= {:cap 1 :ttl :forever} (mem/policy (mem/view (:store eng)) :home)))))))

(deftest set-place-takes-a-map-position-and-a-string-name
  (async done (run-world done {} (fn ^:async t [{:keys [eng]}]
    (await (run-job eng (list set-place {:name "old-farm" :pos {:x 1.5 :y 70 :z -3.5}})))
    (is (= {:x 1 :y 70 :z -4} (place eng :old-farm)))))))

(deftest set-place-without-a-position-records-the-cell-the-body-stands-in
  (async done (run-world done {:self {:pos {:x 12.6 :y 64.0 :z -7.2}}} (fn ^:async t [{:keys [eng]}]
    (await (run-job eng (list set-place {:name :home})))
    (is (= {:x 12 :y 64 :z -8} (place eng :home)))))))

(deftest set-place-moves-a-recorded-place
  (async done (run-world done {} (fn ^:async t [{:keys [eng]}]
    (know-place! eng :home {:x 0 :y 64 :z 0})
    (await (run-job eng (list set-place {:name :home :pos [4 64 4]})))
    (is (= {:x 4 :y 64 :z 4} (place eng :home)))
    (is (= 1 (count (entries eng :home))))))))

(deftest set-place-replaces-a-gone-place
  (async done (run-world done {} (fn ^:async t [{:keys [eng]}]
    (mem/write! (:store eng) :bed {:gone true :was {:x 0 :y 64 :z 0}} mem/place-policy)
    (await (run-job eng (list set-place {:name :bed :pos [4 64 4]})))
    (is (= {:x 4 :y 64 :z 4} (place eng :bed)))))))

(deftest set-place-with-a-block-records-the-blocks-own-cell
  (async done (run-world done {:blocks {"5,64,5" "white_bed" "9,64,9" "chest"}} (fn ^:async t [{:keys [eng seen]}]
    (await (run-job eng (list set-place {:name :bed :pos [5 64 5] :block "white_bed"})))
    (is (= {:x 5 :y 64 :z 5} (place eng :bed)))
    (await (run-job eng (list set-place {:name :chest :pos [10 64 9] :block "chest"})))
    (is (= {:x 9 :y 64 :z 9} (place eng :chest)) "one off the given position")
    (is (= ["white_bed" "chest"] (mapv :block (events-of seen :place.set))) "the event names the block found")))))

(deftest set-place-with-the-bed-shorthand-takes-any-colour
  (async done (run-world done {:blocks {"5,64,5" "red_bed"}} (fn ^:async t [{:keys [eng]}]
    (await (run-job eng (list set-place {:name :bed :pos [5 64 5] :block "bed"})))
    (is (= {:x 5 :y 64 :z 5} (place eng :bed)))))))

(deftest set-place-with-a-block-that-is-not-there-gives-up-and-records-nothing
  (async done (run-world done {:blocks {"5,64,5" "white_bed"}} (fn ^:async t [{:keys [eng seen]}]
    (know-place! eng :bed {:x 1 :y 64 :z 1})
    (await (run-job eng (list set-place {:name :bed :pos [8 64 8] :block "white_bed"})))
    (is (= {:x 1 :y 64 :z 1} (place eng :bed)) "the old place stays")
    (is (= [:no-such-block] (mapv :reason (events-of seen :place.refused))))
    (is (empty? (events-of seen :place.set)))))))

(deftest set-place-with-two-candidates-asks-for-an-exact-position
  (async done (run-world done {:blocks {"9,64,9" "chest" "9,64,11" "chest"}} (fn ^:async t [{:keys [eng seen]}]
    (await (run-job eng (list set-place {:name :chest :pos [9 64 10] :block "chest"})))
    (is (nil? (place eng :chest)))
    (is (= [:ambiguous] (mapv :reason (events-of seen :place.refused))))))))

(deftest set-place-with-a-block-in-an-unloaded-chunk-gives-up
  (async done (run-world done {:unloaded ["9,64,9"]} (fn ^:async t [{:keys [eng seen]}]
    (await (run-job eng (list set-place {:name :chest :pos [9 64 9] :block "chest"})))
    (is (nil? (place eng :chest)))
    (is (= [:not-loaded] (mapv :reason (events-of seen :place.refused))))))))

(defn ^:async try-job
  "Run job with args in a fresh world after (prepare! eng): {:eng :seen :reasons [refusal reasons]}."
  [prepare! job args]
  (let [{:keys [eng seen]} (setup {})]
    (prepare! eng)
    (await (run-job eng (list job args)))
    {:eng eng :seen seen :reasons (mapv :reason (events-of seen :place.refused))}))

(deftest set-place-refuses-bad-arguments-as-data-and-writes-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args reason] [[{:pos [1 64 1]} :bad-name]
                               [{:name "Home Base" :pos [1 64 1]} :bad-name]
                               [{:name :hurt :pos [1 64 1]} :reserved-name]
                               [{:name :home :pos [1 999 1]} :bad-pos]
                               [{:name :home :pos [1 64 1] :block 5} :bad-block]
                               [{:name :home :pos [1 64 1] :block ""} :bad-block]]]
          (let [{:keys [eng seen reasons]} (await (try-job identity set-place args))]
            (is (nil? (place eng :home)) (pr-str args))
            (is (= [reason] reasons) (pr-str args))
            (is (every? string? (mapv :text (events-of seen :place.refused))))))))))

(deftest set-place-will-not-overwrite-another-kind-of-memory
  (async done (run-world done {} (fn ^:async t [{:keys [eng seen]}]
    (mem/write! (:store eng) :my-notes {:n 1})
    (await (run-job eng (list set-place {:name :my-notes :pos [1 64 1]})))
    (is (= [{:n 1}] (entries eng :my-notes)))
    (is (= {:cap 50 :ttl (* 60 60 1000)} (mem/policy (mem/view (:store eng)) :my-notes)))
    (is (= [:not-a-place] (mapv :reason (events-of seen :place.refused))))))))

(deftest a-kind-holding-positions-under-another-policy-is-not-a-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[job args] [[set-place {:name :my-spots :pos [2 64 2]}] [forget-place {:name :my-spots}]]]
          (let [{:keys [eng reasons]} (await (try-job #(mem/write! (:store %) :my-spots {:pos {:x 1 :y 64 :z 1}}) job args))]
            (is (= [:not-a-place] reasons) (str job))
            (is (= [{:pos {:x 1 :y 64 :z 1}}] (entries eng :my-spots)))))))))

(deftest places-survive-a-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng dir]} (setup {:blocks {"5,64,5" "white_bed"}})]
          (await (run-job eng (list set-place {:name :home :pos [3 64 5]})))
          (await (run-job eng (list set-place {:name :bed :pos [5 64 5] :block "bed"})))
          (core/shutdown! eng)
          (let [again (:eng (setup {} dir))]
            (is (= {:x 3 :y 64 :z 5} (place again :home)))
            (is (= {:x 5 :y 64 :z 5} (place again :bed)))
            (is (= {:cap 1 :ttl :forever} (mem/policy (mem/view (:store again)) :home)))))))))

;; ------------------------------------------------------------------ forget-place

(deftest forget-place-removes-one-place-and-leaves-the-others
  (async done (run-world done {} (fn ^:async t [{:keys [eng seen]}]
    (know-place! eng :home {:x 0 :y 64 :z 0})
    (know-place! eng :chest {:x 9 :y 64 :z 9})
    (await (run-job eng (list forget-place {:name :home})))
    (is (nil? (place eng :home)))
    (is (= {:x 9 :y 64 :z 9} (place eng :chest)))
    (is (= [{:name :home :was {:x 0 :y 64 :z 0}}]
           (mapv #(select-keys % [:name :was]) (events-of seen :place.forgotten))))))))

(deftest forget-place-also-clears-a-gone-bed
  (async done (run-world done {} (fn ^:async t [{:keys [eng]}]
    (mem/write! (:store eng) :bed {:gone true :was {:x 0 :y 64 :z 0}} mem/place-policy)
    (await (run-job eng (list forget-place {:name :bed})))
    (is (empty? (entries eng :bed)))))))

(deftest forget-place-refuses-what-it-should-not-touch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args prepare! reason] [[{:name :home} identity :no-such-place]
                                        [{:name :hurt} #(mem/write! (:store %) :hurt {:health 3}) :reserved-name]
                                        [{:name :my-notes} #(mem/write! (:store %) :my-notes {:n 1}) :not-a-place]
                                        [{:name "Not Valid"} identity :bad-name]
                                        [{} identity :bad-name]]]
          (let [{:keys [eng seen reasons]} (await (try-job prepare! forget-place args))]
            (is (= [reason] reasons) (pr-str args))
            (is (empty? (events-of seen :place.forgotten)))
            (is (every? #(seq (entries eng %)) (filter #{:hurt :my-notes} [(:name args)]))
                "another kind of memory is still there")))))))

;; ------------------------------------------------------------------ the control route

(deftest a-control-route-submit-sets-a-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {})
              submit (fn [id spec] (api/mutate! eng {:op :submit :request-id id :by "steward"
                                                     :generation-id (:generation-id (core/state eng)) :spec spec}))]
          (is (= true (:ok (submit "set-home" '(jobs.memory.set-place {:name :home :pos [2 64 3]})))))
          (await (run-until-empty eng 8))
          (is (= {:x 2 :y 64 :z 3} (place eng :home)))
          (is (false? (:ok (submit "set-junk" '(jobs.memory.set-place {:name :home :pos [2 3]})))) "a malformed position is refused at submit")
          (is (= true (:ok (submit "set-bad" '(jobs.memory.set-place {:name :home :pos [2 999 3]})))) "an impossible one comes as an event")
          (await (run-until-empty eng 8))
          (is (= [:bad-pos] (mapv :reason (events-of seen :place.refused))))
          (is (= {:x 2 :y 64 :z 3} (place eng :home)))
          (is (= true (:ok (submit "forget-home" '(jobs.memory.forget-place {:name :home})))))
          (await (run-until-empty eng 8))
          (is (nil? (place eng :home)))
          (core/shutdown! eng))))))

;; ------------------------------------------------------------------ the sleep job records the bed it slept in

(def night 14000)
(def bed-a {:x 6 :y 64 :z 0})
(def bed-b {:x 20 :y 64 :z 0})

(defn ^:async sleep-in
  "Run the sleep job with :bed arg in a night world; the setup map plus :recorded (what :bed memory held)."
  [{:keys [blocks recorded time arg] :or {time night arg bed-a}}]
  (let [r (setup {:time time :blocks (merge {"6,64,0" "red_bed"} blocks)})]
    (when recorded (mem/write! (:store (:eng r)) :bed recorded mem/place-policy))
    (await (run-job (:eng r) (list 'jobs.survival.sleep {:bed arg})))
    r))

(deftest sleeping-in-an-unrecorded-bed-records-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (sleep-in {}))]
          (is (.-isDay (.self p)) "it slept")
          (is (= bed-a (place eng :bed)))
          (is (= [{:pos bed-a}] (entries eng :slept)))
          (is (= [] (events-of seen :place.kept))))))))

(deftest sleeping-in-the-recorded-bed-changes-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (sleep-in {:recorded {:pos bed-a}}))]
          (is (= bed-a (place eng :bed)))
          (is (= [] (events-of seen :place.kept))))))))

(deftest sleeping-in-another-bed-keeps-a-live-recorded-bed-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (sleep-in {:recorded {:pos bed-b} :blocks {"20,64,0" "white_bed"}}))]
          (is (= bed-b (place eng :bed)))
          (is (= [{:name :bed :kept bed-b :offered bed-a}]
                 (mapv #(select-keys % [:name :kept :offered]) (events-of seen :place.kept)))))))))

(deftest sleeping-in-another-bed-replaces-a-recorded-bed-that-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [recorded [{:pos bed-b} {:gone true :was bed-b}]]
          (let [{:keys [eng seen]} (await (sleep-in {:recorded recorded}))]
            (is (= bed-a (place eng :bed)) (pr-str recorded))
            (is (= [] (events-of seen :place.kept)))))))))

(deftest a-recorded-bed-in-an-unloaded-chunk-is-not-called-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (setup {:time night :blocks {"6,64,0" "red_bed"} :unloaded ["20,64,0"]})]
          (mem/write! (:store (:eng r)) :bed {:pos bed-b} mem/place-policy)
          (await (run-job (:eng r) (list 'jobs.survival.sleep {:bed bed-a})))
          (is (= bed-b (place (:eng r) :bed))))))))

(deftest a-bed-argument-with-no-bed-there-records-nothing-and-keeps-the-recorded-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await (sleep-in {:recorded {:pos bed-b} :blocks {"20,64,0" "white_bed"} :arg {:x 7 :y 64 :z 0}}))]
          (is (= bed-b (place eng :bed)) "a missing bed that was only offered does not retract the recorded one")
          (is (empty? (entries eng :slept))))))))

(deftest a-missing-recorded-bed-is-still-retracted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (sleep-in {:recorded {:pos {:x 7 :y 64 :z 0}} :arg {:x 7 :y 64 :z 0}}))]
          (is (nil? (place eng :bed)))
          (is (= 1 (count (events-of seen :bed_missing)))))))))

(deftest the-sleep-job-without-an-argument-still-uses-the-recorded-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks {"6,64,0" "red_bed"}})]
          (mem/write! (:store eng) :bed {:pos bed-a} mem/place-policy)
          (await (run-job eng '(jobs.survival.sleep)))
          (is (.-isDay (.self p)))
          (is (= bed-a (place eng :bed))))))))

;; ------------------------------------------------------------------ storage jobs and the :chest place

(def chest-a {:x 10 :y 64 :z 0})
(def chest-b {:x 20 :y 64 :z 0})
(def bread [{:name "bread" :count 5}])

(defn ^:async deposit-to
  "Run deposit of bread to chest arg (default chest-a) in a world with a chest at 10,64,0 and the body carrying
  bread; opts: :recorded (:chest memory), :blocks, :containers, :inventory, :unloaded, :transfer (forced result),
  :report-zero (the real transfer, but it reports 0 moved)."
  [{:keys [recorded blocks containers inventory unloaded transfer report-zero arg]
    :or {arg chest-a inventory bread}}]
  (let [r (setup (cond-> {:blocks (merge {"10,64,0" "chest"} blocks)
                          :containers (merge {"10,64,0" []} containers)
                          :inventory inventory}
                   unloaded (assoc :unloaded unloaded)))]
    (when recorded (mem/write! (:store (:eng r)) :chest recorded mem/place-policy))
    (when transfer (.override (.-world (:p r)) "transfer" (fn ^:async f [_ _ _] (clj->js transfer))))
    (when report-zero (.override (.-world (:p r)) "transfer" (fn ^:async f [token a impl]
                                                                (let [res (await (impl token a))]
                                                                  (set! (.-moved res) 0)
                                                                  res))))
    (await (run-job (:eng r) (list 'jobs.storage.deposit {:chest arg :items ["bread"]})))
    r))

(deftest a-deposit-to-a-given-chest-records-it-when-none-is-recorded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (deposit-to {}))]
          (is (= chest-a (place eng :chest)))
          (is (= [] (events-of seen :place.kept))))))))

(deftest a-deposit-to-a-chest-given-as-a-vector-works
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (deposit-to {:arg [10 64 0]}))]
          (is (= chest-a (place eng :chest)))
          (is (= [] (events-of seen :place.kept))))))))

(deftest a-deposit-to-the-recorded-chest-changes-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (deposit-to {:recorded {:pos chest-a}}))]
          (is (= chest-a (place eng :chest)))
          (is (= [] (events-of seen :place.kept))))))))

(deftest a-deposit-never-overwrites-a-live-recorded-chest-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (deposit-to {:recorded {:pos chest-b} :blocks {"20,64,0" "chest"}}))]
          (is (= chest-b (place eng :chest)))
          (is (= [{:name :chest :kept chest-b :offered chest-a}]
                 (mapv #(select-keys % [:name :kept :offered]) (events-of seen :place.kept)))))))))

(deftest a-deposit-replaces-a-recorded-chest-that-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [recorded [{:pos chest-b} {:gone true :was chest-b}]]
          (let [{:keys [eng]} (await (deposit-to {:recorded recorded}))]
            (is (= chest-a (place eng :chest)) (pr-str recorded))))))))

(deftest a-deposit-that-moved-nothing-records-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [opts [{:inventory []}
                      {:transfer {:status "full" :moved 0}}
                      {:report-zero true}]]
          (let [{:keys [eng seen]} (await (deposit-to opts))]
            (is (nil? (place eng :chest)) (pr-str opts))
            (is (= [] (events-of seen :place.kept)))))))))

(deftest a-recorded-chest-that-is-missing-is-retracted-by-deposit-with-one-event
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (deposit-to {:recorded {:pos chest-b} :arg chest-b :blocks {"20,64,0" "stone"}}))]
          (is (nil? (place eng :chest)))
          (is (= [{:pos chest-b}] (mapv #(select-keys % [:pos]) (events-of seen :chest_missing)))))))))

(deftest a-chest-in-an-unloaded-chunk-is-not-retracted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (deposit-to {:recorded {:pos chest-b} :arg chest-b :unloaded ["20,64,0"]}))]
          (is (= chest-b (place eng :chest)))
          (is (= [] (events-of seen :chest_missing))))))))

(deftest a-given-chest-that-is-missing-leaves-a-different-recorded-chest-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (deposit-to {:recorded {:pos chest-a} :arg chest-b :blocks {"20,64,0" "stone"}}))]
          (is (= chest-a (place eng :chest)))
          (is (= [] (events-of seen :chest_missing))))))))

(deftest withdraw-retracts-a-recorded-chest-that-is-missing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:blocks {"20,64,0" "stone"}})]
          (mem/write! (:store eng) :chest {:pos chest-b} mem/place-policy)
          (await (run-job eng (list 'jobs.storage.withdraw {:items {"bread" 3}})))
          (is (nil? (place eng :chest)))
          (is (= [{:pos chest-b}] (mapv #(select-keys % [:pos]) (events-of seen :chest_missing)))))))))

(deftest withdraw-does-not-retract-when-the-given-chest-is-another
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:blocks {"20,64,0" "stone"} :containers {"10,64,0" bread}})]
          (mem/write! (:store eng) :chest {:pos chest-a} mem/place-policy)
          (await (run-job eng (list 'jobs.storage.withdraw {:chest chest-b :items {"bread" 3}})))
          (is (= chest-a (place eng :chest)))
          (is (= [] (events-of seen :chest_missing))))))))
