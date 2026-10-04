(ns view.scene-test
  "A scene over fakes of what scene.mjs hands it (GL world, decoder pool, tables, fetch): following the pose, filling the
   column window within the fetch and decode limits, and the frame params."
  (:require [clojure.test :refer [deftest are is async]]
            [view.scene :as scene]))

(defn tick
  "A promise that resolves after the pending promise callbacks have run."
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve 0))))

(defn burn
  "Busy-waits ms (a slow GPU upload)."
  [ms]
  (let [end (+ (js/performance.now) ms)]
    (while (< (js/performance.now) end))))

(defn fake-world [calls burn-ms]
  #js {:allocate (fn [n height] (vswap! calls conj [:allocate n height]))
       :uploadColumn (fn [sx sz] (burn burn-ms) (vswap! calls conj [:upload sx sz]))
       :clearSlot (fn [sx sz] (vswap! calls conj [:clear sx sz]))
       :setBiomes (fn [_] true)
       :dispose (fn [] (vswap! calls conj [:dispose]))})

(defn decoded []
  #js {:header #js {:worldHeight 32 :minY -16} :mats #js [] :flags #js [] :light #js [] :biomes #js [] :ms 1 :lightMs 1 :mainMs 0})

;; decode: :now resolves at once, :never never resolves
(defn fake-decoder [mode calls]
  #js {:decode (fn [k _]
                 (vswap! calls conj [:decode k])
                 (case mode
                   :now (js/Promise.resolve (decoded))
                   :never (js/Promise. (fn [_ _]))))
       :cancel (fn [k] (vswap! calls conj [:cancel k]))})

(defn response [status]
  #js {:status status :ok (= 200 status) :arrayBuffer (fn [] (js/Promise.resolve (js/ArrayBuffer. 4))) :json (fn [] (js/Promise.resolve nil))})

;; fetch: column urls answer `column-status` (or never, for :never), anything else 404
(defn fake-fetch [column-status urls]
  (fn [url]
    (vswap! urls conj url)
    (cond
      (not (.includes url "/columns/")) (js/Promise.resolve (response 404))
      (= column-status :never) (js/Promise. (fn [_ _]))
      :else (js/Promise.resolve (response column-status)))))

(defn make-scene [{:keys [radius decode column-status calls urls burn-ms] :or {radius 1 decode :now column-status 200 burn-ms 0}}]
  (scene/create-scene #js {:agent "w/Bob" :radius radius :interp false :ownStream false
                           :world (fake-world calls burn-ms) :decoder (fake-decoder decode calls)
                           :tables #js {:ensure (fn [_] (js/Promise.resolve #js {}))}
                           :fetch (fake-fetch column-status urls)
                           :cameraBasis (fn [cam] cam) :sceneTime (fn [_ _] #js {:time 0 :rain 0}) :skyDarken (fn [_ _] 0)}))

(defn pose [x z] #js {:mtime (js/Date.now) :pose #js {:t (js/Date.now) :status "online" :world "w" :mcVersion "1.21" :eye #js {:x x :y 70 :z z} :yaw 0 :pitch 0}})

(defn of-kind [calls kind] (filterv #(= kind (first %)) @calls))

(defn counts [^js s] (js->clj (.counts s) :keywordize-keys true))

(defn after-ticks [n] (reduce (fn [p _] (.then p tick)) (js/Promise.resolve) (range n)))

(deftest a-pose-fills-the-window-and-frame-draws-it
  (async done
    (let [calls (volatile! []) urls (volatile! [])
          ^js s (make-scene {:calls calls :urls urls})]
      (.feed s "pose" (pose 8 8))
      (-> (after-ticks 10)
          (.then (fn []
                   (is (= "/columns/w/0.0.bin" (first (filter #(.includes % "/columns/") @urls))) "the eye's column is fetched first")
                   (let [^js params (loop [n 0] (let [p (.frame s 0 nil)] (if (or (zero? (:uploads (counts s))) (> n 20)) p (recur (inc n)))))]
                     (.drew s)
                     (is (= 9 (count (filter #(= :upload (first %)) @calls))))
                     (is (= [:allocate 3 32] (first (filter #(= :allocate (first %)) @calls))))
                     (is (= {:loaded 9 :wanted 9 :status "online"} (select-keys (js->clj (.stats s) :keywordize-keys true) [:loaded :wanted :status])))
                     (is (true? (.. s -metrics -ready)))
                     ;; the window starts at chunk -1: the eye is 24 blocks in, 86 above minY -16; slot of chunk -1 is 2
                     (is (= {:x 24 :y 86 :z 24} (js->clj (.-eye params) :keywordize-keys true)))
                     (is (= {:x 32 :z 32} (js->clj (.-slotOff params) :keywordize-keys true))))
                   (.close s)
                   (done)))))))

(deftest nothing-to-draw-before-a-pose
  (let [^js s (make-scene {:calls (volatile! []) :urls (volatile! [])})]
    (is (nil? (.frame s 0 nil)))
    (is (= "connecting" (.-status (.stats s))))
    (.close s)))

(deftest fetches-and-decodes-stay-within-their-limits
  (async done
    (let [^js fetching (make-scene {:radius 2 :column-status :never :calls (volatile! []) :urls (volatile! [])})
          ^js decoding (make-scene {:radius 2 :decode :never :calls (volatile! []) :urls (volatile! [])})]
      (.feed fetching "pose" (pose 8 8))
      (.feed decoding "pose" (pose 8 8))
      (-> (after-ticks 10)
          (.then (fn []
                   (is (= {:inFlight 8 :decoding 0 :uploads 0 :needs 25} (counts fetching)))
                   ;; no fetch starts with 12 decoding, but the 8 in flight at 11 still land: at most 11 + 8
                   (is (= {:inFlight 0 :decoding 19 :uploads 0 :needs 6} (counts decoding)))
                   (.close fetching)
                   (.close decoding)
                   (done)))))))

(deftest moving-a-chunk-cancels-the-decodes-of-the-columns-it-left
  (async done
    (let [calls (volatile! [])
          ^js s (make-scene {:radius 1 :decode :never :calls calls :urls (volatile! [])})]
      (.feed s "pose" (pose 8 8))
      (-> (after-ticks 10)
          (.then (fn []
                   (vreset! calls [])
                   (.feed s "pose" (pose 24 8))
                   (is (= (set (map #(vector :cancel (str (.-id s) "|-1." %)) [-1 0 1])) (set (of-kind calls :cancel))))
                   (.close s)
                   (done)))))))

(deftest moving-a-chunk-clears-the-slots-taken-over
  (async done
    (let [calls (volatile! [])
          ^js s (make-scene {:radius 1 :calls calls :urls (volatile! [])})]
      (.feed s "pose" (pose 8 8))
      (-> (after-ticks 10)
          (.then (fn []
                   (.frame s 0 nil) ; uploads, so the world has its dims
                   (vreset! calls [])
                   (.feed s "pose" (pose 24 8))
                   (is (= #{[:clear 2 0] [:clear 2 1] [:clear 2 2]} (set (filter #(= :clear (first %)) @calls))))
                   (.close s)
                   (done)))))))

(deftest a-missing-column-is-marked-missing-and-not-uploaded
  (async done
    (let [calls (volatile! [])
          ^js s (make-scene {:column-status 404 :calls calls :urls (volatile! [])})]
      (.feed s "pose" (pose 8 8))
      (-> (after-ticks 10)
          (.then (fn []
                   (.frame s 0 nil)
                   (is (empty? (filter #(= :upload (first %)) @calls)))
                   (is (= {:loaded 0 :wanted 9} (select-keys (js->clj (.stats s) :keywordize-keys true) [:loaded :wanted])))
                   (is (= 0 (:needs (counts s))))
                   (.close s)
                   (done)))))))

(deftest a-column-event-refetches-a-wanted-column-only
  (async done
    (let [urls (volatile! [])
          ^js s (make-scene {:calls (volatile! []) :urls urls})]
      (.feed s "pose" (pose 8 8))
      (-> (after-ticks 10)
          (.then (fn []
                   (vreset! urls [])
                   (.feed s "column" #js {:cx 1 :cz 1 :mtime 5})
                   (.feed s "column" #js {:cx 9 :cz 9 :mtime 5})
                   (is (= ["/columns/w/1.1.bin"] @urls))
                   (after-ticks 10)))
          (.then (fn []
                   (is (= 0 (:needs (counts s))) "nothing is left owed for the unwanted column")
                   (.close s)
                   (done)))))))

(deftest decode-priority-is-the-dist-of-a-wanted-column
  (async done
    (let [^js s (make-scene {:radius 1 :decode :never :calls (volatile! []) :urls (volatile! [])})]
      (.feed s "pose" (pose 8 8))
      (-> (after-ticks 10)
          (.then (fn []
                   (is (= js/Math.SQRT2 (scene/decode-priority (str (.-id s) "|1.1"))))
                   (is (= js/Infinity (scene/decode-priority (str (.-id s) "|7.7"))))
                   (.close s)
                   (is (= js/Infinity (scene/decode-priority (str (.-id s) "|1.1"))))
                   (done)))))))

(deftest entity-boxes-are-the-nearest-64-relative-to-the-origin
  (let [entities (into-array (map (fn [i] #js {:pos #js {:x i :y 0 :z 0} :type (if (zero? i) "hostile" "animal")}) (range 70 -1 -1)))
        boxes (scene/entity-boxes entities #js {:x 0 :y -64 :z 0} #js {:x 0 :y 0 :z 0})
        ^js first-box (aget boxes 0)]
    (is (= 64 (alength boxes)))
    (is (= [[-0.3 64 -0.3] [0.3 65.8 0.3] [0.88 0.14 0.14]] (mapv vec [(.-min first-box) (.-max first-box) (.-color first-box)])))))

(deftest entity-boxes-carry-the-name-label-and-kind-the-views-write-and-colour-by
  (are [e expected] (= expected (let [^js box (aget (scene/entity-boxes (into-array [(clj->js (assoc e :pos {:x 1 :y 2 :z 3}))]) #js {:x 0 :y 0 :z 0} #js {:x 0 :y 0 :z 0}) 0)]
                                  [(.-name box) (.-label box) (.-kind box)]))
    {:name "zombie" :type "hostile"} ["zombie" nil "hostile"]
    {:name "player" :type "player" :username "Bob_2"} ["player" "Bob_2" "player"]
    {:name "cow" :type "animal"} ["cow" nil "animal"]
    {:name "item" :kind "Drops"} ["item" nil "item"]
    {:name "bat" :kind "Hostile mobs"} ["bat" nil "hostile"]
    {:name "pig" :kind "Passive mobs"} ["pig" nil nil]))

(deftest entity-colors
  (are [e expected] (= expected (vec (scene/entity-color (clj->js e))))
    {:type "player"} [0.2 0.4 0.95]
    {:username "x"} [0.2 0.4 0.95]
    {:kind "Hostile mobs"} [0.88 0.14 0.14]
    {:kind "Animals"} [0.55 0.38 0.22]
    {:name "item"} [1 0.88 0.16]
    {:type "other"} [0.5 0.5 0.5]))

(deftest loaded-count-counts-the-loaded-columns
  (let [columns (js/Map.)]
    (doseq [[k status] [["0.0" "loaded"] ["0.1" "pending"] ["1.0" "loaded"] ["1.1" "missing"]]]
      (.set columns k #js {:status status}))
    (is (= 2 (scene/loaded-count columns)))
    (is (= 0 (scene/loaded-count (js/Map.))))))

(deftest the-upload-step-stops-at-its-budget-on-the-initial-fill
  (async done
    (let [calls (volatile! []) urls (volatile! [])
          ^js s (make-scene {:calls calls :urls urls :burn-ms 5})]
      (.feed s "pose" (pose 8 8))
      (-> (after-ticks 10)
          (.then (fn []
                   (is (= 9 (:uploads (counts s))) "all nine columns are decoded and waiting")
                   (.frame s 0 nil)
                   ;; each upload takes 5 ms, over the 4 ms budget: the first column is uploaded, the rest wait for the next frame
                   (is (= 1 (count (filter #(= :upload (first %)) @calls))))
                   (is (= 8 (:uploads (counts s))))
                   (.close s)
                   (done)))))))

(deftest without-a-fetch-option-the-scene-uses-the-global-fetch
  (async done
    (let [urls (volatile! [])
          original (.-fetch js/globalThis)
          ^js s (do (set! (.-fetch js/globalThis) (fake-fetch 200 urls))
                    (scene/create-scene #js {:agent "w/Bob" :radius 1 :interp false :ownStream false
                                             :world (fake-world (volatile! []) 0) :decoder (fake-decoder :now (volatile! []))
                                             :tables #js {:ensure (fn [_] (js/Promise.resolve #js {}))}
                                             :cameraBasis (fn [cam] cam) :sceneTime (fn [_ _] #js {:time 0 :rain 0}) :skyDarken (fn [_ _] 0)}))]
      (.feed s "pose" (pose 8 8))
      (-> (after-ticks 10)
          (.then (fn []
                   (set! (.-fetch js/globalThis) original)
                   (is (= "/columns/w/0.0.bin" (first (filter #(.includes % "/columns/") @urls))))
                   (is (some #(.includes % "/biomes/w.json") @urls))
                   (.close s)
                   (done)))))))
