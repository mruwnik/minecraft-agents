(ns jobs.animals.bring
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.ready :as ready]))

(def doc
  "Bring one animal of type :mob to :pos (or to the fence post :fence). A one-shot order that starts and ends itself;
  it picks the way and runs that job as its one child, passing its ending up unchanged (kind, cause, data) with the
  data's :method added.

  Ways (:method):
  - :lead: jobs.animals.lead-to. Puts a lead on the animal, walks, then ties it to :fence or lets it go at :pos.
  - :lure: jobs.animals.lure-to. The animal follows the breeding food in hand to :pos (inside :box when given). No lead.
  - :auto (default): :lead with :fence; :lure with :box or :uuid; else :lead when a lead or a slime ball is carried or the
    mob has no breeding food, else :lure.

  A :fence needs :lead and :box or :uuid need :lure; either with the other way is never startable (:bad-way). Without
  :mob or a destination it can never start (:no-mob, :no-destination). Fetching what the way needs (a lead, the food) is
  the child's.

  Result: the child's. Both ways give {:end :animal :animal-at :at :reason :text}; :end is :tie (tied to the post), :free
  (let go at :pos; lead only) or :led (lure only: the animal stands at the spot and still follows); lead adds :still-led
  and :gathered, lure adds :near, :in-box and :distance. Done only with an :end.")

(a/defargs args
  {:mob {:doc "the animal's name, such as \"cow\"" :spec a/name? :default nil}
   :pos {:doc "where to bring it {:x :y :z}; the :fence cell when nil (lead only)" :spec ::a/pos :default nil}
   :fence {:doc "lead only: the fence post {:x :y :z} to tie it to; let go at :pos when nil" :spec ::a/pos :default nil}
   :box {:doc "lure only: the destination pen {:min :max}; animals in it are not lured" :spec pen-read/box-arg :default nil}
   :uuid {:doc "lure only: bring only this animal (its uuid or the start of it)" :spec string? :default nil}
   :method {:doc "the way: :lead, :lure or :auto (see the job doc)" :spec #{:auto :lead :lure} :default :auto}
   :radius {:doc "animals within this many blocks of where the job starts are taken" :spec (a/num-in 0 nil) :default 8}
   :gather-radius {:doc "lead without :fence: the animal is let go once it is within this many blocks of :pos" :spec (a/num-in 0 nil) :default 3}
   :keep-food {:doc "lure only (ignored by :lead): leave the food in hand when it ends (see jobs.animals.lure-to)" :spec boolean? :default false}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 240 :doc "Seconds of the job's own working time before the bring gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def ways need/fetch-ways)

(def target [:mob :pos :fence])

(def safe? ready/safe-if-lit)
(def fit? ready/fit?)

(def defaults {:spend {:job-time ::job-time-s}})

(defn method-of
  "The way the order takes: :method as given; :auto picks :lead for a :fence, :lure for a :box or :uuid, else
  animals/resolve-method."
  [c]
  (let [{:keys [method fence box uuid mob]} (:args c)]
    (cond
      (not= :auto method) method
      fence :lead
      (or box uuid) :lure
      :else (animals/resolve-method c :auto mob))))

(defn can-start?
  "Never without :mob or a destination, or when :fence or :box/:uuid is asked of the way that cannot take it."
  [c]
  (let [{:keys [mob pos fence box uuid]} (:args c)
        method (method-of c)]
    (cond
      (nil? mob) (contract/never :no-mob "bring needs a :mob")
      (not (or pos fence)) (contract/never :no-destination "bring needs a :pos or a :fence")
      (and (= :lure method) fence) (contract/never :bad-way "a :fence is tied only by :lead")
      (and (= :lead method) (or box uuid)) (contract/never :bad-way ":box and :uuid are for :lure")
      (and (= :lure method) (nil? pos)) (contract/never :no-destination "a lure needs a :pos")
      :else (contract/ok :order "the order is complete"))))

(defn quote-of [c]
  {:uses {:job-time [0 60 ##Inf]}})

(defn done?
  "Ok once the child ended done with an :end; else later."
  [c]
  (if-let [end (:end (:brought (ctx/mem c)))]
    (contract/ok end "the animal is brought" :units :animals :done 1 :left 0 :result (:brought (ctx/mem c)))
    (contract/later :bringing "the animal is on its way" :units :animals :done 0 :left 1)))

(def child-job {:lead 'jobs.animals.lead-to :lure 'jobs.animals.lure-to})

(defn child-args
  "The args the way's job takes, from this call's."
  [c method]
  (let [args (:args c)
        common (cond-> (select-keys args [:mob :pos :radius :ignore-zones?])
                 (some? (:ways args)) (assoc :ways (:ways args)))]
    (case method
      :lead (merge common (select-keys args [:fence :gather-radius]))
      :lure (merge common (select-keys args [:box :uuid :keep-food])))))

(defn ^:async step [c]
  (let [method (or (:method (ctx/mem c)) (method-of c))
        _ (ctx/update-mem! c assoc :method method)
        e (await (ctx/call c :bring (child-job method) (child-args c method)))]
    (if (= :continue e)
      :continue
      (let [data (assoc (:data e) :method method)]
        (when (= :done (:kind e)) (ctx/update-mem! c assoc :brought data))
        (contract/with-because (ctx/end c (:kind e) (:cause e) data) e)))))

(defn ^:async round [c]
  (await (pace/steps! c #(step c))))
