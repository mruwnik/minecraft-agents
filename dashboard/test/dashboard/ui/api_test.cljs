(ns dashboard.ui.api-test
  (:require [cljs.test :refer [deftest are is async]]
            [dashboard.ui.api :as api]
            [re-frame.core :as rf]))

(deftest edn-outcome-cases
  (are [ok? status text expected] (= expected (api/edn-outcome ok? status text))
    true 200 "{:a 1}" [:ok {:a 1}]
    false 500 "{:error \"boom\"}" [:err "boom"]
    false 502 "<html>" [:err "http 502"]
    false 404 "nil" [:err "http 404"]))

(deftest an-unparseable-ok-body-is-an-error
  (let [[outcome message] (api/edn-outcome true 200 "{:a")]
    (is (= :err outcome))
    (is (re-find #"^bad reply: " message))))

(defn fake-response [status body]
  #js {:ok (<= 200 status 299) :status status
       :json (fn [] (js/Promise.resolve (clj->js body)))
       :text (fn [] (js/Promise.resolve (pr-str body)))})

(defn with-fake-fetch
  "Runs f with js/fetch and rf/dispatch replaced; calls done with the dispatched events once the request settles."
  [fetch-fn request-fn done check]
  (let [original js/fetch
        original-dispatch rf/dispatch
        events (atom [])]
    (reset! api/in-flight #{})
    (set! js/fetch fetch-fn)
    (set! rf/dispatch #(swap! events conj %))
    (-> (request-fn)
        (.then (fn [_] (check @events)))
        (.catch (fn [e] (is false (str e))))
        (.finally (fn [] (set! js/fetch original) (set! rf/dispatch original-dispatch) (done))))))

(deftest a-hung-fetch-is-aborted-and-reported
  (async done
    (with-redefs [api/request-timeout-ms 20]
      (with-fake-fetch
        (fn [_ opts] (js/Promise. (fn [_ reject] (.addEventListener (.-signal opts) "abort" #(reject (doto (js/Error. "aborted") (set! -name "AbortError")))))))
        #(api/fetch-edn! {:key :k :url "/x" :on-ok [:ok] :on-err [:err]})
        done
        (fn [events]
          (is (= [[:err "request timed out"]] events))
          (is (empty? @api/in-flight)))))))

(deftest a-handler-throw-is-not-a-fetch-error
  (async done
    (let [events (atom [])
          original js/fetch
          original-dispatch rf/dispatch]
      (reset! api/in-flight #{})
      (set! js/fetch (fn [_ _] (js/Promise.resolve (fake-response 200 {:a 1}))))
      (set! rf/dispatch (fn [e] (swap! events conj e) (when (= [:ok {:a 1}] e) (throw (js/Error. "handler")))))
      (-> (api/fetch-edn! {:key :k :url "/x" :on-ok [:ok] :on-err [:err]})
          (.catch (fn [_] nil))
          (.finally (fn [] (set! js/fetch original)
                      (set! rf/dispatch original-dispatch)
                      (is (= [[:ok {:a 1}]] @events))
                      (done)))))))

(deftest an-identical-post-is-never-dropped
  (async done
    (let [calls (atom 0)]
      (with-fake-fetch
        (fn [_ _] (swap! calls inc) (js/Promise.resolve (fake-response 200 {:ok true})))
        (fn [] (js/Promise.all #js [(api/post-edn! {:url "/api/restart" :body {} :on-ok [:ok] :on-err [:err]})
                                    (api/post-edn! {:url "/api/restart" :body {} :on-ok [:ok] :on-err [:err]})]))
        done
        (fn [events]
          (is (= 2 @calls))
          (is (= [[:ok {:ok true}] [:ok {:ok true}]] events)))))))

(deftest a-slow-post-is-not-aborted-by-the-timeout
  (async done
    (with-redefs [api/request-timeout-ms 20]
      (with-fake-fetch
        (fn [_ opts] (js/Promise. (fn [resolve reject]
                                    (some-> (.-signal opts) (.addEventListener "abort" #(reject (doto (js/Error. "aborted") (set! -name "AbortError")))))
                                    (js/setTimeout #(resolve (fake-response 200 {:ok true})) 100))))
        #(api/post-edn! {:url "/api/drive" :body {} :on-ok [:ok] :on-err [:err]})
        done
        (fn [events] (is (= [[:ok {:ok true}]] events)))))))

(deftest a-different-post-to-the-same-url-is-not-dropped
  (async done
    (let [calls (atom 0)]
      (with-fake-fetch
        (fn [_ _] (swap! calls inc) (js/Promise.resolve (fake-response 200 {:ok true})))
        (fn [] (js/Promise.all #js [(api/post-edn! {:url "/api/chat" :body {:text "a"} :on-ok [:ok] :on-err [:err]})
                                    (api/post-edn! {:url "/api/chat" :body {:text "b"} :on-ok [:ok] :on-err [:err]})]))
        done
        (fn [events]
          (is (= 2 @calls))
          (is (= 2 (count events))))))))

(deftest a-stalled-body-read-is-aborted-and-reported
  (async done
    (with-redefs [api/request-timeout-ms 20]
      (with-fake-fetch
        (fn [_ opts]
          (js/Promise.resolve
           #js {:ok true :status 200
                :text (fn [] (js/Promise. (fn [_ reject] (.addEventListener (.-signal opts) "abort" #(reject (doto (js/Error. "aborted") (set! -name "AbortError")))))))}))
        #(api/fetch-edn! {:key :k :url "/x" :on-ok [:ok] :on-err [:err]})
        done
        (fn [events]
          (is (= [[:err "request timed out"]] events))
          (is (empty? @api/in-flight)))))))
