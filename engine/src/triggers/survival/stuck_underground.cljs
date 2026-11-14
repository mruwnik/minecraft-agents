(ns triggers.survival.stuck-underground
  "The stuck-underground trigger: an idle body deep underground (its feet :below blocks or more under the remembered
  :home, else :bed, and buried: no sky light at its cell) walks home. mine, tunnel and the digs end where they stop and
  never walk back, so this is what gets an idle body out of a hole. Idle: no job listed, or the body's own body.idle
  record (memory :body-idle) written since the clock below started with no declared hold (:holds, e.g. :cooking), so
  a listed job that only waits keeps it no longer and one that holds still on purpose does. It holds only after the body has been in that state :idle-s seconds (a clock kept per body here, restarted
  while the state is off, when the listed jobs change, while a listed job works (no such record yet) and, with a job
  listed, when the body leaves its cell; a record from before the cell change still counts), so the gap between two agent calls walks nowhere. It needs a remembered home or bed, and holds nothing while the body sleeps or a death is unrecovered."
  (:require [engine.memory :as mem]
            [jobs.lib.body :as body]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def defaults {:below 6 :idle-s 120})

(def since
  "Per primitives object: {:t :from :cell :live}, its body's stuck clock start, the time from which a body.idle record
  counts, the feet cell and the listed ids when the clock started; cleared when the state ends."
  (js/WeakMap.))

(defn stuck-underground
  [p view args _kn live]
  (let [{:keys [below idle-s]} (merge defaults args)
        pos (sh/home-target view)
        feet (u/self-pos {:primitives p})
        cell (sh-sense/feet p)
        now (:now view)
        stuck? (and pos
                    (not (body/recovery-pending? view :stuck-underground))
                    (not (sh-sense/sleeping? p))
                    (>= (- (:y pos) (:y feet)) below)
                    (sh-sense/buried? p))
        prev (.get since p)
        fresh {:t now :from now :cell cell :live live}
        idle-since? (fn [t] (or (empty? live)
                                (let [r (mem/latest view :body-idle)]
                                  (and (some-> r :t (>= t)) (empty? (get-in r [:data :holds]))))))]
    (cond
      (not stuck?) (.delete since p)
      (or (nil? prev) (not= live (:live prev)) (not (idle-since? (:from prev)))) (.set since p fresh)
      (and (seq live) (not= cell (:cell prev))) (.set since p (assoc prev :t now :cell cell)))
    (boolean
     (when stuck?
       (let [{:keys [t from]} (.get since p)]
         (and (>= (- now t) (* 1000 idle-s)) (idle-since? from)))))))
