(ns dashboard.thumbs-test
  (:require [cljs.test :refer [deftest are is async]]
            [dashboard.thumbs :as thumbs]))

(deftest recycle-table
  (are [loaded expected] (= expected (thumbs/recycle? loaded 400))
    nil false
    0 false
    400 false
    401 true))

(deftest fresh-table
  (are [entry mtime now expected] (= expected (thumbs/fresh? entry mtime now 2000))
    nil 5 10000 false
    {:pose-mtime-ms 5 :rendered-at 1} 5 10000 true     ; same pose: always fresh
    {:pose-mtime-ms 4 :rendered-at 9000} 5 10000 true  ; newer pose but rendered 1 s ago
    {:pose-mtime-ms 4 :rendered-at 8000} 5 10000 false ; newer pose, rendered 2 s ago
    {:pose-mtime-ms 4 :rendered-at 1} 5 10000 false))

(defn run
  "Runs the promise-returning thunk f inside a cljs.test async block."
  [done f]
  (-> (js/Promise.resolve)
      (.then f)
      (.catch (fn [e] (is (nil? e) (str "async test threw: " e))))
      (.finally done)))

(defn fake
  "A thumbnailer over a recording render. Poses: an atom name -> mtime."
  [poses clock & [{:keys [loaded]}]]
  (let [calls (atom [])
        active (atom 0)
        peak (atom 0)
        recycled (atom 0)
        render (fn [name]
                 (swap! peak max (swap! active inc))
                 (swap! calls conj name)
                 (-> (js/Promise. (fn [resolve] (js/setTimeout resolve 5)))
                     (.then (fn [_]
                              (swap! active dec)
                              (when-not (= name "NoPose")
                                {:png (str "png-" name "-" (count @calls)) :ms 7 :loaded loaded})))))]
    {:t (thumbs/make {:render render
                      :pose-mtime #(get @poses %)
                      :recycle #(swap! recycled inc)
                      :now #(:t @clock)
                      :min-interval-ms 2000
                      :column-cap 400})
     :calls calls :peak peak :recycled recycled}))

(deftest renders-once-then-serves-cache
  (async done
    (run done
         (fn []
           (let [{:keys [t calls]} (fake (atom {"A" 1000}) (atom {:t 10000}))]
             (-> (js/Promise.all #js [((:get t) "A") ((:get t) "A")])
                 (.then (fn [_] ((:get t) "A")))
                 (.then (fn [entry]
                          (is (= ["A"] @calls))
                          (is (= "png-A-1" (:png entry)))
                          (is (= 1000 (:pose-mtime-ms entry)))))))))))

(deftest unknown-and-unrenderable-bodies-give-nil
  (async done
    (run done
         (fn []
           (let [{:keys [t]} (fake (atom {"NoPose" 5}) (atom {:t 0}))]
             (-> (js/Promise.all #js [((:get t) "Nobody") ((:get t) "NoPose")])
                 (.then (fn [r] (is (= [nil nil] (vec r)))))))))))

(deftest changed-pose-rerenders-after-min-interval-only
  (async done
    (run done
         (fn []
           (let [poses (atom {"A" 1000})
                 clock (atom {:t 10000})
                 {:keys [t calls]} (fake poses clock)]
             (-> ((:get t) "A")
                 (.then (fn [_] (reset! poses {"A" 2000}) (swap! clock update :t + 500) ((:get t) "A")))
                 (.then (fn [e] (is (= 1000 (:pose-mtime-ms e)) "too soon") (swap! clock update :t + 2000) ((:get t) "A")))
                 (.then (fn [e]
                          (is (= 2000 (:pose-mtime-ms e)))
                          (is (= ["A" "A"] @calls))))))))))

(deftest renders-never-overlap
  (async done
    (run done
         (fn []
           (let [{:keys [t peak calls]} (fake (atom {"A" 1 "B" 2 "C" 3}) (atom {:t 0}))]
             (-> (js/Promise.all #js [((:get t) "A") ((:get t) "B") ((:get t) "C")])
                 (.then (fn [_]
                          (is (= 1 @peak))
                          (is (= ["A" "B" "C"] @calls))))))))))

(deftest worker-is-recycled-past-the-column-cap
  (async done
    (run done
         (fn []
           (let [{:keys [t recycled]} (fake (atom {"A" 1}) (atom {:t 0}) {:loaded 401})]
             (-> ((:get t) "A")
                 (.then (fn [_] (is (= 1 @recycled))))))))))

(deftest stats-count-renders
  (async done
    (run done
         (fn []
           (let [{:keys [t]} (fake (atom {"A" 1 "B" 2}) (atom {:t 0}))]
             (-> (js/Promise.all #js [((:get t) "A") ((:get t) "B")])
                 (.then (fn [_] (is (= {:renders 2 :last-ms 7 :mean-ms 7 :queue 0 :bodies 2} ((:stats t))))))))))))
