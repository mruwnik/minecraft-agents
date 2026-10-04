(ns view.hub-test
  "The hub over a fake surface and fake scenes: which targets a frame renders, the per-scene bookkeeping, the shared stream."
  (:require [clojure.test :refer [deftest is async]]
            [view.hub :as hub]))

(defn fake-scene [agent calls]
  (let [feeds (volatile! [])]
    #js {:id agent :agent agent :radius 2 :world agent
         :metrics #js {:ready true}
         :frame (fn [_] (vswap! calls conj [:frame agent]) (when-not (= agent "w/empty") #js {:eye 1}))
         :drew (fn [] (vswap! calls conj [:drew agent]))
         :stats (fn [] #js {:loaded 9 :wanted 9 :poseAge 1 :status "online"})
         :counts (fn [] #js {:uploads 0})
         :feed (fn [event data] (vswap! calls conj [:feed agent event (.-n ^js data)]))
         :close (fn [] (vswap! calls conj [:close agent]))
         :feeds feeds}))

(defn fake-surface [calls]
  #js {:rendererName "fake" :gpuTimer false
       :createScene (fn [^js options] (fake-scene (.-agent options) calls))
       :fit (fn [w h] (vswap! calls conj [:fit w h]))
       :draw (fn [world _ w h _] (vswap! calls conj [:draw world w h]))
       :poll (fn [])
       :copy (fn [_ w h] (vswap! calls conj [:copy w h]) 0.5)
       :placeholder (fn [_ w h] (vswap! calls conj [:placeholder w h]))
       :bitmap (fn [w h] (js/Promise.resolve [:bitmap w h]))
       :finish (fn [])
       :memory (fn [] #js {:total 0})
       :close (fn [] (vswap! calls conj [:surface-close]))})

(defn fake-source [sources]
  (fn [url]
    (let [listeners (js/Map.)
          ^js source #js {:url url
                          :addEventListener (fn [event f] (.set listeners event f))
                          :emit (fn [event data] ((.get listeners event) #js {:data (js/JSON.stringify data)}))
                          :closed false}]
      (set! (.-close source) (fn [] (set! (.-closed source) true)))
      (vswap! sources conj source)
      source)))

(defn make-hub [calls & [sources]]
  (hub/hub #js {:fps 6 :maxScenes 3 :openStream (fake-source (or sources (volatile! [])))} (fake-surface calls)))

(defn canvas [] #js {:width 300 :height 150})
(defn now [] (js/performance.now))
(defn of-kind [calls kind] (filterv #(= kind (first %)) @calls))

(deftest an-attach-sizes-the-canvas-fits-the-hidden-one-and-shows-the-placeholder
  (let [calls (volatile! [])
        ^js h (make-hub calls)
        ^js s (.addScene h #js {:agent "w/a"})
        c (canvas)]
    (.attach s c #js {:width 320 :height 180})
    (is (= [320 180] [(.-width c) (.-height c)]))
    (is (= [[:fit 320 180] [:placeholder 320 180]] (filterv #(#{:fit :placeholder} (first %)) @calls)))
    (.close h)))

(deftest a-frame-renders-a-due-target-copies-it-and-tells-the-scene-once
  (let [calls (volatile! [])
        ^js h (make-hub calls)
        ^js s (.addScene h #js {:agent "w/a"})]
    (.attach s (canvas) #js {:width 320 :height 180})
    (.attach s (canvas) #js {:width 64 :height 32})
    (vreset! calls [])
    (.tick h (+ (now) 1000))
    (is (= [[:draw "w/a" 320 180] [:draw "w/a" 64 32]] (of-kind calls :draw)))
    (is (= 2 (count (of-kind calls :copy))))
    (is (= 1 (count (of-kind calls :frame))) "the params are sampled once per frame")
    (is (= [[:drew "w/a"]] (of-kind calls :drew)))
    (vreset! calls [])
    (.tick h (+ (now) 1016))
    (is (empty? (of-kind calls :draw)) "at 6 fps the next frame is not due")
    (is (= [{"width" 320 "height" 180 "fps" 6 "copyMs" 0.5 "copyMsP95" 0.5}
            {"width" 64 "height" 32 "fps" 6 "copyMs" 0.5 "copyMsP95" 0.5}]
           (mapv #(dissoc % "measuredFps") (js->clj (.-attaches (.stats s))))))
    (.close h)))

(deftest a-scene-with-nothing-to-draw-shows-the-placeholder
  (let [calls (volatile! [])
        ^js h (make-hub calls)
        ^js s (.addScene h #js {:agent "w/empty"})]
    (.attach s (canvas) #js {:width 32 :height 16})
    (vreset! calls [])
    (.tick h (+ (now) 1000))
    (is (empty? (of-kind calls :draw)))
    (is (= [[:placeholder 32 16]] (of-kind calls :placeholder)))
    (.close h)))

(deftest a-detached-or-closed-scene-is-not-rendered
  (let [calls (volatile! [])
        ^js h (make-hub calls)
        ^js a (.addScene h #js {:agent "w/a"})
        ^js b (.addScene h #js {:agent "w/b"})
        c (canvas)]
    (.attach a c #js {})
    (.attach b (canvas) #js {})
    (.detach a c)
    (.close b)
    (vreset! calls [])
    (.tick h (+ (now) 1000))
    (is (empty? (of-kind calls :draw)))
    (is (= {"w/a" true} (into {} (map (fn [k] [k true]) (js/Object.keys (.-scenes (.stats h)))))))
    (.close h)))

(deftest the-hub-refuses-too-many-scenes-and-a-bad-fps
  (let [^js h (make-hub (volatile! []))]
    (dotimes [i 3] (.addScene h #js {:agent (str "w/" i)}))
    (is (thrown-with-msg? js/Error #"at most 3 scenes" (.addScene h #js {:agent "w/x"})))
    (is (thrown-with-msg? js/Error #"positive number or 'raf'" (.attach (.addScene (make-hub (volatile! [])) #js {:agent "w/y"}) (canvas) #js {:fps 0})))
    (.close h)))

(deftest a-snapshot-draws-now-and-resolves-the-bitmap
  (async done
    (let [calls (volatile! [])
          ^js h (make-hub calls)
          ^js s (.addScene h #js {:agent "w/a"})]
      (-> (.snapshot s #js {:width 8 :height 4})
          (.then (fn [bitmap]
                   (is (= [:bitmap 8 4] bitmap))
                   (is (= [[:draw "w/a" 8 4]] (of-kind calls :draw)))
                   (is (= [[:drew "w/a"]] (of-kind calls :drew)))
                   (.close h)
                   (done)))))))

(deftest one-stream-for-all-scenes-its-events-go-to-the-scenes-of-their-agent-and-replay-to-a-later-one
  (async done
    (let [calls (volatile! [])
          sources (volatile! [])
          ^js h (make-hub calls sources)]
      (.addScene h #js {:agent "w/b"})
      (.addScene h #js {:agent "w/a"})
      (js/setTimeout
       (fn []
         (is (= 1 (count @sources)))
         (let [^js source (first @sources)]
           (is (= "/poses?agents=w%2Fa,w%2Fb&radius=2" (.-url source)))
           (vreset! calls [])
           (.emit source "pose" #js {:agent "w/a" :n 1})
           (is (= [[:feed "w/a" "pose" 1]] (of-kind calls :feed)))
           (vreset! calls [])
           (.addScene h #js {:agent "w/a"})
           (is (= [[:feed "w/a" "pose" 1]] (of-kind calls :feed)) "a later scene of a streamed agent gets the last pose at once"))
         (.close h)
         (is (.-closed ^js (first @sources)))
         (done))
       600))))

(deftest without-a-surface-the-hub-is-unsupported-and-leaves-the-canvas-alone
  (async done
    (let [^js h (hub/view-hub #js {} nil)
          ^js s (.addScene h #js {:agent "w/Bob"})
          c (canvas)]
      (.attach s c #js {:width 320 :height 180})
      (is (false? (.-supported h)))
      (is (= [300 150] [(.-width c) (.-height c)]))
      (is (= "unsupported" (.-status (.stats s))))
      (-> (.snapshot s #js {:width 8 :height 8})
          (.then (fn [_] (is false "the snapshot must reject")) (fn [_] (is true)))
          (.then (fn [] (.detach s c) (.close s) (.close h) (done)))))))
