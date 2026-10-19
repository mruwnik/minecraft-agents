(ns engine.core.quote
  "Quotes (engine.contract) as the engine asks them. ctx/quote runs a job's quote-of under a quote ctx: a token-less
  ctx of the would-be call (its call keys, clamped as ctx/call would, and ctx/left of them), with a wall-clock quote
  deadline handed down; asked past it, time is ##Inf (a finished answer is kept: load stalls must not turn it into ##Inf). A call equal to a caller's or a quoting ancestor's
  is refused as ctx/call refuses it. A job with no quote-of quotes the mean time of its :job-report entries. ctx/next-way
  picks a contract job's next way and books the ways that failed."
  (:require [engine.contract :as contract]
            [engine.core.base :refer [now owner?]]
            [engine.core.ending :as ending]
            [engine.core.limits :as limits]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]))

(defn deadline-of
  "The quote deadline of a quote asked under base with given (wall-clock ms, or nil): the earliest of given and
  base's, else ::contract/quote-s from now."
  [eng base given]
  (let [ds (remove nil? [given (:quote-deadline base)])]
    (if (seq ds)
      (apply min ds)
      (+ (now eng) (* 1000 (settings/get contract/settings ::contract/quote-s))))))

(defn default-quote [eng def]
  (contract/default-quote (keep #(when (and (:sym def) (= (:sym def) (get-in % [:data :job]))) (get-in % [:data :report]))
                                (mem/entries (mem/view (:store eng)) :job-report))
                          (settings/get contract/settings ::contract/default-time-s)))

(defn checked [def q]
  (when-let [problem (contract/quote-problem q)]
    (throw (js/Error. (str (or (:sym def) "a job") "'s quote-of returned " problem ": " (pr-str q)))))
  q)

(defn ^:async quote!
  "What def expects for a call with args under base: a quote, or the duplicate-call ending. self? asks the job of
  base for its own (no duplicate check). (make-ctx qd) is the quote ctx, qd the quote deadline."
  [eng base def args given self? make-ctx]
  (let [def (ending/wrap def)
        qd (deadline-of eng base given)]
    (or (when-not self? (ending/duplicate-ending eng base def args))
        (cond
          (> (now eng) qd) contract/too-expensive
          (nil? (:quote-of def)) (default-quote eng def)
          :else (checked def (await ((:quote-of def) (make-ctx qd))))))))

(defn ^:async next-way
  "{:way k :dropped {way why}}: the first of c's job ways (in its ways order) the call allows, that has not failed on
  thing this call and whose quote, (quote-of-way way) (a quote, a promise of one, an ending or nil), crosses no line
  of ctx/left by its least; :way nil when none is left. The way chosen is booked in c's frame: a later child ending
  other than :done books it as failed on thing."
  [c quote-of-way thing]
  (let [eng (:engine c)
        call (ctx/frozen-call c)
        failed (get-in (mem/job-mem ((:view c)) (:root c) (:slots c)) [:contract :failed])
        left (ctx/left c)
        levels (limits/levels (:primitives c))]
    (loop [[w & more] (:job-ways c) dropped {}]
      (if-not w
        {:way nil :dropped dropped}
        (let [why (cond
                    (not (contains? (:ways call) w)) :not-allowed
                    (contains? failed [w thing]) {:failed (get failed [w thing])}
                    :else (let [q (await (quote-of-way w))]
                            (cond
                              (nil? q) nil
                              (contract/ending? q) (or (:cause q) :ending)
                              :else (contract/breaks q left levels (now eng)))))]
          (if why
            (recur more (assoc dropped w why))
            (do (when (owner? eng (:token c))
                  (mem/update-job! (:store eng) (:root c) (:slots c) assoc-in [:contract :way] {:way w :thing thing}))
                {:way w :dropped dropped})))))))

(defn clear-way!
  "Forget the way the frame of base chose last: it succeeded, so a later failure is no failure of it."
  [eng {:keys [root slots]}]
  (when (get-in (mem/job-mem (mem/view (:store eng)) root slots) [:contract :way])
    (mem/update-job! (:store eng) root slots update :contract dissoc :way)))

(defn book-failure!
  "Book child ending e as a failure of the way the frame of base chose last with next-way, if any."
  [eng {:keys [root slots]} e]
  (when-let [{:keys [way thing]} (get-in (mem/job-mem (mem/view (:store eng)) root slots) [:contract :way])]
    (mem/update-job! (:store eng) root slots assoc-in [:contract :failed [way thing]] (contract/cause-key e))))
