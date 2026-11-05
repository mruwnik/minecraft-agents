(ns engine.night-worth-test
  "The worths the night passes to its child calls: answer-worth and enclose-worth over shelter-prices, light/worth and light!'s call."
  (:require [cljs.test :refer [deftest is async]]
            [engine.ctx :as ctx]
            [engine.test-util :as tu]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.torch :as torch]
            [jobs.survival.night :as night]
            [jobs.survival.night.hold :as hold]
            [jobs.survival.night.light :as light]))

(defn ^:async with-prices
  "Run (f c) with night/shelter-prices answering each of results in turn."
  [results f]
  (let [orig night/shelter-prices
        left (atom results)]
    (set! night/shelter-prices (fn [_] (let [r (first @left)] (swap! left rest) (js/Promise.resolve r))))
    (try (await (f {}))
         (finally (set! night/shelter-prices orig)))))

(deftest answer-worth-is-what-the-other-answers-cost-more-than-the-answer-priced-afresh
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [priced (fn [niche flee] [{:niche niche :flee flee} nil])
              ws (await (with-prices [(priced 4 10) (priced 4 20) :searching]
                          (fn ^:async f [c] [(await (night/answer-worth c :niche)) (await (night/answer-worth c :niche)) (await (night/answer-worth c :niche))])))]
          (is (= [6 16 ##Inf] ws) "a fresh price each call, ##Inf only while the flee search goes on"))))))

(deftest enclose-worth-is-at-least-the-shelter-price-and-waits-for-the-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ws (await (with-prices [[{:shelter 20 :flee 5} nil] [{:shelter 5 :flee 20} nil] :searching]
                          (fn ^:async f [c] [(await (night/enclose-worth c)) (await (night/enclose-worth c)) (await (night/enclose-worth c))])))]
          (is (= [20 15 nil] ws) "a cheaper other answer still leaves the walked-to wall-in worth its own price; nil until priced"))))))

(defn ^:async with-light-stubs
  "Run (f c) with light-cost answering light-s, way-cost ways-s and a torch-holding body in a dark room; mem the job memory."
  [light-s ways-s mem f]
  (let [saved [ctx/mem torch/torches-held sh-inside/dark-room-at? reach-room/room light/light-cost light/way-cost]]
    (set! ctx/mem (fn [_] mem))
    (set! torch/torches-held (fn [_] 1))
    (set! sh-inside/dark-room-at? (fn [_ _] true))
    (set! reach-room/room (fn [_ _] {:cells [:cell]}))
    (set! light/light-cost (fn [_ _] (js/Promise.resolve light-s)))
    (set! light/way-cost (fn [_] (js/Promise.resolve ways-s)))
    (try (await (f {:primitives (tu/fake {})}))
         (finally
           (set! ctx/mem (nth saved 0))
           (set! torch/torches-held (nth saved 1))
           (set! sh-inside/dark-room-at? (nth saved 2))
           (set! reach-room/room (nth saved 3))
           (set! light/light-cost (nth saved 4))
           (set! light/way-cost (nth saved 5))))))

(deftest light-worth-is-what-the-cheapest-way-costs-more-than-lighting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= 7 (await (with-light-stubs 3 10 {} light/worth))) "ways 10 less light 3")
        (is (nil? (await (with-light-stubs 11 10 {} light/worth))) "lighting costs more than a wall, pit or pocket")
        (is (nil? (await (with-light-stubs 3 10 {:light-tried true} light/worth))) "once a night")))))

(deftest light-calls-light-area-with-the-cells-and-the-worth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [orig-call ctx/call orig-busy hold/busy! orig-update ctx/update-mem!
              seen (atom nil)]
          (set! ctx/call (fn [_ _ _ args] (reset! seen args) (js/Promise.resolve :continue)))
          (set! hold/busy! (fn [_] nil))
          (set! ctx/update-mem! (fn [& _] nil))
          (try (await (with-light-stubs 3 10 {} (fn ^:async f [c] (await (light/light! c 7)))))
               (finally (set! ctx/call orig-call) (set! hold/busy! orig-busy) (set! ctx/update-mem! orig-update)))
          (is (= {:cells [:cell] :worth 7} @seen)))))))
