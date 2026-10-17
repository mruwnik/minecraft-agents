(ns jobs.access.tunnel.torches
  "The torches of a tunnel (jobs.access.tunnel): which sites stand, hanging one, booking one left out."
  (:require [engine.ctx :as ctx]
            [jobs.access.stair :as stair]
            [jobs.access.stair.judge :as judge]
            [jobs.access.tunnel.line :as ln]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.placement :as placement]
            [jobs.lib.reach :as reach]
            [jobs.lib.torch :as torch]
            [jobs.lib.util :as u]
            [plan.shape :as shape]))

(defn head-of [cell] (shape/add cell [0 1 0]))

(defn torch-cell
  "The cell of site s (head, then feet) that holds a torch in the world, or nil."
  [block-at cells s]
  (first (filter #(torch/torch-blocks (block-at %)) [(head-of (cells s)) (cells s)])))

(defn standing-torches
  "[{:cell :site :block}] for the sites of plan whose torch stands now."
  [block-at plan]
  (let [cells (ln/line-cells plan)]
    (vec (keep (fn [s] (when-let [cell (torch-cell block-at cells s)] {:cell cell :site s :block (block-at cell)}))
               (:sites plan)))))

(defn book-unlit!
  "Book site s as :unlit with reason, at the cell its torch would take (cell, else the site's head cell); the first of
  the tunnel warns."
  [c plan s reason cell]
  (let [cell (or cell (head-of ((ln/line-cells plan) s)))
        first? (empty? (:unlit (ctx/mem c)))]
    (ctx/update-mem! c update :unlit (fnil conj []) {:cell cell :site s :reason reason})
    (when first?
      (ctx/emit! c :tunnel.unlit :warn {:reason reason :cell cell
                                        :text (str "tunnel torch left out at " (pr-str cell) ": " (name reason))}))
    nil))

(defn torch-choice
  "How to hang the torch of site s from the eye: jobs.lib.torch/torch-at for the head and feet cells of s."
  [plan s eye block-at]
  (torch/torch-at (shape/headings (:heading plan)) ((ln/line-cells plan) s) eye block-at))

(defn ^:async hang!
  "Hang the torch of site s from the body's place, or book the site :unlit with why not. The ledger entry (a dead
  end) is written before the place and confirmed when the cell holds the torch. :again after a place."
  [c in plan s]
  (let [p (:primitives c)
        block-at (:block-at in)
        choice (torch-choice plan s (placement/eye (u/self-pos c)) block-at)
        cell (:cell choice)
        verdict (when cell (rules/may-place? (assoc in :cell cell)))
        reason (cond (zero? (torch/torches-held p)) :no-torches
                     (:refused choice) (:refused choice)
                     (not (:ok verdict)) (:reason verdict))]
    (if reason
      (book-unlit! c plan s reason cell)
      (let [keep? (:keep (:args c))
            intended (ledger/intend (ledger/reconcile (ledger/open-entries (ctx/view c)) block-at)
                                    {:cell cell :item (:block choice) :before "air" :job (:id c) :purpose :tunnel-torch})
            _ (when-not keep? (ledger/remember! c intended))
            held? (await (torch/place! c choice block-at))]
        (when-not keep? (ledger/remember! c (if held? (ledger/confirm intended cell) (ledger/reconcile intended block-at))))
        (when-not held? (book-unlit! c plan s :place-failed cell))
        :again))))

(defn ^:async torch-step!
  "With the body on line index k: book the sites it passed without a torch, and hang the one due (site k-1, not
  :unlit, nothing hanging in its cells). :again after a place, else nil."
  [c k]
  (let [{:keys [plan unlit]} (ctx/mem c)
        in (judge/rules-in c (reach/standing-vec (:primitives c)))
        cells (ln/line-cells plan)
        booked (set (map :site unlit))
        pending (remove #(or (booked %) (torch-cell (:block-at in) cells %)) (filter #(<= (inc %) k) (:sites plan)))]
    (doseq [s (filter #(< (inc %) k) pending)] (book-unlit! c plan s :passed nil))
    (when-let [due (first (filter #(= (inc %) k) pending))]
      (await (hang! c in plan due)))))
