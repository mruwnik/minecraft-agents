(ns world-test.lease-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.lease :as l]))

(defn fake-dir
  "files: atom of {index pid}. create! is exclusive like fs 'wx'."
  [files alive-pids]
  {:create! (fn [i pid] (if (contains? @files i) false (do (swap! files assoc i pid) true)))
   :holder (fn [i] (get @files i))
   :reclaim! (fn [i _holder] (swap! files dissoc i))
   :alive? (fn [pid] (contains? alive-pids pid))})

(deftest a-free-plot-is-taken-from-the-first-index
  (let [files (atom {})]
    (is (= 0 (l/acquire (fake-dir files #{}) {:pid 10 :first 0 :total 400})))
    (is (= {0 10} @files))))

(deftest two-runners-never-get-the-same-plot
  (let [files (atom {})
        a (l/acquire (fake-dir files #{10 11}) {:pid 10 :first 0 :total 400})
        b (l/acquire (fake-dir files #{10 11}) {:pid 11 :first 0 :total 400})]
    (is (= [0 1] [a b]))))

(deftest a-lease-of-a-dead-pid-is-reclaimed
  (let [files (atom {0 99 1 10})]
    (is (= 0 (l/acquire (fake-dir files #{10 11}) {:pid 11 :first 0 :total 400})))
    (is (= {0 11 1 10} @files))))

(deftest a-lease-held-by-a-live-pid-is-skipped
  (let [files (atom {0 10})]
    (is (= 1 (l/acquire (fake-dir files #{10 11}) {:pid 11 :first 0 :total 400})))))

(deftest a-lease-that-vanishes-while-checking-is-retried
  (let [files (atom {})
        d (assoc (fake-dir files #{}) :create! (let [n (atom 0)]
                                                  (fn [i pid] (if (zero? (swap! n inc)) false (do (swap! files assoc i pid) true))))
                 :holder (fn [_] nil))]
    (is (= 0 (l/acquire d {:pid 5 :first 0 :total 400})))))

(deftest reclaim-is-told-which-dead-holder-it-may-remove
  (let [files (atom {0 99})
        seen (atom [])
        d (assoc (fake-dir files #{11}) :reclaim! (fn [i h] (swap! seen conj [i h]) (swap! files dissoc i)))]
    (is (= 0 (l/acquire d {:pid 11 :first 0 :total 400})))
    (is (= [[0 99]] @seen))))

(deftest an-empty-lease-file-is-waited-on-a-while-then-reclaimed-not-spun-on-forever
  (let [files (atom {0 :empty})
        settles (atom 0)
        d (assoc (fake-dir files #{11}) :settle! (fn [] (swap! settles inc)))]
    (is (= 0 (l/acquire d {:pid 11 :first 0 :total 400})))
    (is (pos? @settles))
    (is (= {0 11} @files))))

(deftest an-empty-lease-that-gets-its-pid-meanwhile-is-skipped
  (let [files (atom {0 :empty})
        d (assoc (fake-dir files #{10 11}) :settle! (fn [] (swap! files assoc 0 10)))]
    (is (= 1 (l/acquire d {:pid 11 :first 0 :total 400})))))

(deftest no-free-plot-is-an-error
  (let [files (atom {0 10 1 10})]
    (is (thrown? js/Error (l/acquire (fake-dir files #{10}) {:pid 11 :first 0 :total 2})))))

(deftest the-time-log-stamp-is-local-iso-with-offset
  (is (= "2026-10-05T15:04:09.007+01:00" (l/local-iso [2026 10 5 15 4 9 7] 60)))
  (is (= "2026-10-05T03:04:09.120-05:30" (l/local-iso [2026 10 5 3 4 9 120] -330)))
  (is (= "2026-01-02T00:00:00.000+00:00" (l/local-iso [2026 1 2 0 0 0 0] 0))))

(deftest bodies-with-a-live-lease-are-not-other-players
  (is (= #{"BodyA"} (l/leased-bodies {"BodyA" 10 "BodyB" 99} (fn [pid] (= pid 10))))))

(deftest the-online-list-reply-is-parsed-into-names
  (is (= ["A" "B"] (l/parse-online "There are 2 of a max of 20 players online: A, B")))
  (is (= [] (l/parse-online "There are 0 of a max of 20 players online: "))))

(deftest near-players-skip-leased-bodies-and-self
  (is (= ["Someone"] (l/strangers ["Me" "BodyA" "Someone"] "Me" #{"BodyA"})))
  (is (= [] (l/strangers ["Me" "BodyA"] "Me" #{"BodyA"}))))

(deftest the-near-check-excludes-every-leased-body-by-name
  (is (= "execute if entity @a[name=!Me,name=!BodyA,x=1,y=2,z=3,distance=..500]"
         (l/near-command "Me" #{"BodyA"} [1 2 3]))))

(defn fake-share
  "files: atom of {pid entry}. guard just runs the thunk."
  [files alive-pids]
  {:guard (fn [f] (f))
   :entries (fn [] @files)
   :put! (fn [pid e] (swap! files assoc pid e))
   :remove! (fn [pid] (swap! files dissoc pid))
   :alive? (fn [pid] (contains? alive-pids pid))})

(defn hold [phase] {:phase phase :state :hold :seq 1})
(defn want [phase seq] {:phase phase :state :want :seq seq})

(deftest a-free-time-lock-is-taken-as-first-holder
  (let [files (atom {})]
    (is (= {:held true :first? true} (l/try-share (fake-share files #{10}) 10 :day 5)))
    (is (= :hold (:state (get @files 10))))
    (is (= :day (:phase (get @files 10))))))

(deftest holders-of-the-same-phase-share-and-only-the-first-sets-the-time
  (let [files (atom {10 (hold :day)})]
    (is (= {:held true :first? false} (l/try-share (fake-share files #{10 11}) 11 :day 5)))
    (is (= #{10 11} (set (keys @files))))))

(deftest the-other-phase-waits-for-every-holder-and-registers-as-waiting
  (let [files (atom {10 (hold :day) 12 (hold :day)})]
    (is (= {:waiting-on [10 12]} (l/try-share (fake-share files #{10 11 12}) 11 :night 5)))
    (is (= :want (:state (get @files 11))))
    (is (= 5 (:seq (get @files 11))))))

(deftest the-waiter-is-granted-once-the-holders-are-gone-and-keeps-first
  (let [files (atom {11 (want :night 5)})]
    (is (= {:held true :first? true} (l/try-share (fake-share files #{11}) 11 :night 5)))
    (is (= :hold (:state (get @files 11))))))

(deftest a-new-holder-of-the-current-phase-queues-behind-a-waiter-of-the-other-phase
  (let [files (atom {10 (hold :day) 11 (want :night 5)})]
    (is (= {:waiting-on [11]} (l/try-share (fake-share files #{10 11 12}) 12 :day 9)))
    (is (= :want (:state (get @files 12))))))

(deftest an-older-same-phase-waiter-is-not-blocked-by-a-newer-other-phase-waiter
  (let [files (atom {10 (hold :day) 11 (want :night 8) 12 (want :day 5)})]
    (is (= {:held true :first? false} (l/try-share (fake-share files #{10 11 12}) 12 :day 5)))))

(deftest the-oldest-waiter-goes-first-when-nobody-holds
  (let [files (atom {11 (want :night 5) 12 (want :day 7)})
        d (fake-share files #{11 12})]
    (is (= {:waiting-on [11]} (l/try-share d 12 :day 7)))
    (is (= {:held true :first? true} (l/try-share d 11 :night 5)))))

(deftest dead-holders-and-waiters-are-reclaimed
  (let [files (atom {10 (hold :day) 13 (want :night 1)})]
    (is (= {:held true :first? true} (l/try-share (fake-share files #{11}) 11 :night 5)))
    (is (= #{11} (set (keys @files))))))

(deftest releasing-removes-only-the-own-entry
  (let [files (atom {10 (hold :day) 11 (hold :day)})]
    ((:remove! (fake-share files #{10 11})) 11)
    (is (= #{10} (set (keys @files))))))

(deftest manual-time-sets-name-their-phase
  (is (= :day (l/phase-of-ticks 1000)))
  (is (= :day (l/phase-of-ticks 6000)))
  (is (= :night (l/phase-of-ticks 14000)))
  (is (= :night (l/phase-of-ticks 18000)))
  (is (= :day (l/phase-of-ticks 24000)))
  (is (= :night (l/phase-of-ticks 38000))))

(deftest a-case-holds-the-phase-it-needs
  (is (= :day (l/time-phase {:time :day})))
  (is (= :night (l/time-phase {:time :night :act [[:time-set 1000]]})))
  (is (= :night (l/time-phase {:time :any :act [[:wait-s 1] [:time-set 14000]]})))
  (is (nil? (l/time-phase {:time :any :act [[:wait-s 1]]}))))

(deftest an-exclusive-night-holder-shares-with-nobody-not-even-night
  (let [files (atom {10 (hold :night-x)})]
    (is (= {:waiting-on [10]} (l/try-share (fake-share files #{10 11}) 11 :night-x 5)))
    (is (= {:waiting-on [10]} (l/try-share (fake-share files #{10 11}) 11 :night 5)))
    (is (= {:waiting-on [10]} (l/try-share (fake-share files #{10 11}) 11 :day 5)))))

(deftest an-exclusive-night-case-waits-for-plain-night-holders
  (let [files (atom {10 (hold :night) 12 (hold :night)})]
    (is (= {:waiting-on [10 12]} (l/try-share (fake-share files #{10 11 12}) 11 :night-x 5)))))

(deftest an-older-waiter-of-any-phase-blocks-a-new-exclusive-holder
  (let [files (atom {11 (want :night 3)})]
    (is (= {:waiting-on [11]} (l/try-share (fake-share files #{11 12}) 12 :night-x 5)))))

(deftest time-phase-of-an-exclusive-night-case
  (is (= :night-x (l/time-phase {:time :night-exclusive}))))

(defn simulate-runners
  "n runners, each taking `cases` day/night cases through try-share in a pseudo-random interleaving (seeded). Returns
  {:overlaps steps where holders of both phases existed, :wrong-time steps where a holder saw the world in the other phase,
  :sets the logged time sets, :done cases finished}. The first holder of a phase sets the world time and logs it."
  [seed n cases]
  (let [files (atom {}) alive (set (range 1 (inc n)))
        dir (fake-share files alive)
        world (atom :day) sets (atom []) overlaps (atom 0) wrong (atom 0) done (atom 0)
        plan (into {} (map (fn [p] [p (vec (take cases (iterate (fn [x] (mod (+ (* x 7) 3) 11)) (+ p seed))))]) alive))
        st (atom (into {} (map (fn [p] [p {:todo (plan p) :holding nil :seq nil}]) alive)))
        rnd (atom seed)
        next-rnd! (fn [] (swap! rnd #(mod (+ (* % 1103515245) 12345) 2147483648)) (quot @rnd 65536))]
    (loop [steps 0]
      (let [active (filter (fn [p] (let [s (@st p)] (or (seq (:todo s)) (:holding s)))) (sort alive))]
        (when (and (seq active) (< steps 20000))
          (let [p (nth active (mod (next-rnd!) (count active)))
                {:keys [todo holding seq] :as s} (@st p)]
            (if holding
              (do (when (not= holding @world) (swap! wrong inc))
                  (swap! files dissoc p)
                  (swap! done inc)
                  (swap! st assoc p {:todo (rest todo) :holding nil :seq nil}))
              (let [phase (if (even? (first todo)) :day :night)
                    sq (or seq steps)
                    r (l/try-share dir p phase sq)]
                (if (:held r)
                  (do (when (:first? r) (when (not= phase @world) (swap! sets conj phase)) (reset! world phase))
                      (swap! st assoc p (assoc s :holding phase :seq sq)))
                  (swap! st assoc p (assoc s :seq sq)))))
            (when (> (count (distinct (map :phase (filter #(= :hold (:state %)) (vals @files))))) 1) (swap! overlaps inc))
            (recur (inc steps))))))
    {:overlaps @overlaps :wrong-time @wrong :sets @sets :done @done}))

(deftest day-and-night-cases-of-several-runners-never-overlap-and-every-time-set-is-logged
  (doseq [seed [1 2 3 4 5 6 7 8] n [2 3 8]]
    (let [r (simulate-runners seed n 12)]
      (is (= 0 (:overlaps r)) (str "seed " seed " n " n))
      (is (= 0 (:wrong-time r)) (str "seed " seed " n " n))
      (is (= (* n 12) (:done r)) (str "every case ran, seed " seed " n " n))
      (is (every? #{:day :night} (:sets r))))))

(deftest the-time-is-set-only-when-a-phase-group-starts-so-each-set-is-logged-once
  (let [files (atom {10 (hold :day)})]
    (is (false? (:first? (l/try-share (fake-share files #{10 11}) 11 :day 5))) "joining a held phase sets nothing")))
