(ns world-test.triage
  "tools/sweep-triage.mjs: reads a sweep's pool results and the board (read-only sqlite) and reports what the sweep agent acts on:
  each non-passing case as known (the previous sweep failed the same check), new, flaky or inconclusive; each live card
  with the cases its live plan names; the cards to tick, to send to pending, to hold and the new failures that need a bug card.
  Pure parts: classify, card-plan-text, triage, summary."
  (:require ["child_process" :as cp]
            ["fs" :as fs]
            [cljs.reader :as reader]
            [clojure.string :as str]))

(def usage
  (str "usage: node tools/sweep-triage.mjs --dir DIR [--previous RESULTS.edn ...] [--rev R] [--repo PATH] [--plans PLANS.edn] [--db FILE] [--board ID | --board-file BOARD.edn] [--out report.edn]\n"
       "       node tools/sweep-triage.mjs --unskip-list --previous RESULTS.edn ... [--repo PATH] [--rev R] [--prev-rev P] [--db FILE] [--board ID] --out FILE: the known failures named by the live plans of testing, live and in_review cards whose commits are in the revision, and all known failures of fixture files changed since P, one id per line, for world-sweep --unskip-ids.\n"
       "DIR holds sweep-<pass>.edn (day, night, night-exclusive, any sweep-*.edn except sweep-summary.edn). --previous: the earlier sweep's result files, as the pool's --durations.\n"
       "--plans: EDN map of card id prefix -> text naming fixtures/cases, for a card whose own plan names none. "
       "--rev (default HEAD) and --repo (default .): a card is covered when its commits are ancestors of the revision. Prints a text summary; --out writes the full EDN report."))

(def default-db (str (.. js/process -env -HOME) "/.local/share/differ/review.db"))
(def default-board "d6883cd9-f687-4bc4-a73c-2e6c4a0cd7f9")

(defn read-results [file] (reader/read-string (fs/readFileSync file "utf8")))

(defn fail-sig
  "Which check a failed case failed on: the :expect of its first failed expectation, else its :why; an error's :evidence."
  [{:keys [status expects why evidence]}]
  (if (= status :error)
    evidence
    (or (some #(when (= :fail (:status %)) (:expect %)) expects) why "")))

(defn last-results
  "Result vectors in order (later wins) -> id -> [status sig] for the ids whose last result failed or errored."
  [previous]
  (let [last-of (reduce (fn [m c] (assoc m (:id c) c)) {} (apply concat previous))]
    (into {} (keep (fn [[id c]]
                     (case (:status c)
                       (:fail :error) [id [(:status c) (fail-sig c)]]
                       :skipped-known [id [(:previous-status c) (:previous-failure c)]] ; a skipped case keeps the failure it was skipped for
                       nil)))
          last-of)))

(defn classify
  "Cases + previous result vectors -> the cases with :class (:pass :flaky :known :new :inconclusive) and :sig.
  Skipped-known = the pool did not run it (its failure is known and its bug card still open). Known = the last previous result failed (or errored) on the same check; with no previous result at all the pool's :known-failure flag decides."
  [cases previous]
  (let [prev (last-results previous)
        seen-before? (set (map :id (apply concat previous)))]
    (mapv (fn [c]
            (let [sig (fail-sig c)
                  cls (case (:status c)
                        :pass :pass
                        :flaky :flaky
                        :inconclusive :inconclusive
                        :skipped-known :skipped-known
                        (cond
                          (= [(:status c) sig] (prev (:id c))) :known
                          (and (:known-failure c) (not (seen-before? (:id c)))) :known
                          :else :new))]
              (assoc (select-keys c [:id :file :status]) :class cls :sig sig)))
          cases)))

(defn slug [id] (subs id (inc (.indexOf id "/"))))

(defn tokens
  "Words of a plan text; a fixture path and its .edn suffix are cut off, so engine/fixtures/world/a.edn/x reads a/x."
  [text]
  (into #{} (comp (map #(str/replace % #"[-_/]+$" "")) (remove str/blank?))
        (-> (str/lower-case (or text ""))
            (str/replace #"(?:[\w./-]*/)?fixtures/world/" "")
            (str/replace #"\.edn\b" "")
            (str/split #"[^a-z0-9_/-]+"))))

(defn case-plan-lines
  "The lines under the newest note's 'Live plan (cases):' heading, up to the next blank line (plan-notes are oldest first; older plans may name cases that are gone); empty when no note has one."
  [plan-notes]
  (let [marker "live plan (cases):"]
    (when-let [n (last (filter #(str/includes? (str/lower-case %) marker) plan-notes))]
      (->> (str/split-lines (subs n (+ (str/index-of (str/lower-case n) marker) (count marker))))
           (map str/trim) (drop-while str/blank?) (take-while (complement str/blank?)) vec))))

(defn description-case-lines
  "A bug card's description lists its failing cases as '- fixture/case (...)' lines; those are cases the card owes, whatever its plan note names."
  [description]
  (vec (keep #(second (re-matches #"- (\S+/\S+) \(.*" %)) (str/split-lines (or description "")))))

(defn card-plan-text
  "The text a card's live plan lives in: with a 'Live plan (cases):' heading only the lines under it plus the cases its description lists (extra-plan too); else its title, then each note's text from 'live plan' on (unit-test lists before it do not count); with no such note the description from its first line that mentions live (all of it when none does). :extra-plan is text the sweep agent adds by hand."
  [{:keys [title description plan-notes extra-plan]}]
  (if-let [lines (seq (case-plan-lines plan-notes))]
    (str/join "\n" (cons extra-plan (concat lines (description-case-lines description))))
    (let [from-live-plan (fn [n] (when-let [i (str/index-of (str/lower-case n) "live plan")] (subs n i)))
        notes (keep from-live-plan plan-notes)
        lines (str/split-lines (or description ""))
        from-live (drop-while #(not (re-find #"(?i)live" %)) lines)]
    (str/join "\n" (concat [title extra-plan]
                           (if (seq notes) notes (if (seq from-live) from-live lines)))))))

(defn missing-cases
  "The refs (file/case, case may be cut short) on a card's 'Live plan (cases):' lines that name a fixture file of the run or one of known-files (fixture files in the repo) but match none of the run's cases; other slashed tokens are no case refs."
  [{:keys [plan-notes description]} cases known-files]
  (let [ids (map (comp str/lower-case :id) cases)
        files (into (set known-files) (map :file) cases)]
    (vec (remove (fn [r] (some #(str/starts-with? % r) ids))
                 (filter #(and (str/includes? % "/") (files (first (str/split % #"/"))))
                         (mapcat tokens (concat (case-plan-lines plan-notes) (description-case-lines description))))))))

(defn named-cases
  "Plan text + all classified cases -> the cases the plan names. A fixture token alone names all its cases; fixture/slug (slug may be cut short) or a bare slug of a named fixture's case narrows it to those."
  [text cases]
  (let [toks (tokens text)
        by-file (group-by :file cases)
        files (into #{} (keep #(let [f (first (str/split % #"/"))] (when (by-file f) f))) toks)
        specific (fn [file]
                   (let [prefix (str file "/")
                         cuts (into [] (keep #(when (and (str/starts-with? % prefix) (> (count %) (+ 4 (count file)))) (subs % (count prefix)))) toks)]
                     (filter (fn [c]
                               (let [s (slug (:id c))]
                                 (or (toks s) (toks (str prefix s)) (some #(str/starts-with? s %) cuts))))
                             (by-file file))))]
    (vec (mapcat (fn [file] (let [sp (specific file)] (if (seq sp) sp (by-file file)))) (sort files)))))

(defn verdict
  "A named case that failed on a check -> :pending; no failure but a flaky, an error, an inconclusive, a skipped known failure or a failure with no reason -> :hold; else :tick."
  [named]
  (let [real-fail? #(and (= :fail (:status %)) (not= "" (:sig %)))]
    (cond
      (empty? named) :no-plan
      (some real-fail? named) :pending
      (some #(or (#{:inconclusive :flaky} (:class %)) (#{:error :fail :flaky :skipped-known} (:status %))) named) :hold
      :else :tick)))

(def unskip-columns #{"testing" "live" "in_review"})

(defn fixture-stems
  "Output of git diff --name-only -> the set of fixture file names (no directory, no .edn) it lists."
  [diff-out]
  (into #{} (keep #(second (re-matches #"engine/fixtures/world/(?:.*/)?([^/]+)\.edn" %))) (str/split-lines diff-out)))

(defn unskip-ids
  "{:previous [vector ...] :cards :ancestor? :changed-files} -> sorted ids of the known failures to run again: those that the live plans of testing, live and in_review cards name, for the cards whose commits are all in the revision, and all of those in a changed fixture file (a set of file names)."
  [{:keys [previous cards ancestor? changed-files]}]
  (let [failing (set (keys (last-results previous)))
        cases (into [] (comp (map :id) (distinct) (filter failing) (map (fn [id] {:id id :file (first (str/split id #"/"))})))
                    (apply concat previous))
        waiting (filter #(and (unskip-columns (:column %))
                              (every? ancestor? (:commits %)))
                        cards)]
    (vec (sort (distinct (concat (mapcat #(map :id (named-cases (card-plan-text %) cases)) waiting)
                                 (map :id (filter #(contains? (set changed-files) (:file %)) cases))))))))

(defn bug-matches [bugs {:keys [id file]}]
  (mapv :id (filter #(let [toks (tokens (:title %))] (or (toks id) (toks file))) bugs)))

(defn triage
  "{:results cases :previous [vector ...] :cards [{:id :column :title :commits :description :plan-notes}] :bugs [{:id :title}] :ancestor? sha -> bool :fixture-files #{names of the repo's fixture files}} -> report."
  [{:keys [results previous cards bugs ancestor? fixture-files]}]
  (let [classified (classify results previous)
        by-id (into {} (map (juxt :id identity)) classified)
        card-reports (mapv (fn [c]
                             (let [named (named-cases (card-plan-text c) classified)
                                   covered? (every? ancestor? (:commits c))
                                   missing (missing-cases c classified fixture-files)
                                   v (cond (not covered?) :not-in-rev
                                           (seq missing) :no-plan
                                           :else (verdict named))]
                               {:id (:id c) :title (:title c) :column (:column c)
                                :cases (mapv #(select-keys % [:id :status :class]) named)
                                :missing missing
                                :failing (mapv :id (filter #(= :fail (:status %)) named))
                                :bug-cards (vec (distinct (mapcat #(bug-matches bugs %) (filter #(= :fail (:status %)) named))))
                                :verdict v}))
                           cards)
        named-by (reduce (fn [m c] (reduce #(update %1 (:id %2) (fnil conj []) (:id c)) m (:cases c))) {} (remove #(= :not-in-rev (:verdict %)) card-reports))
        new-bugs (mapv (fn [c] {:id (:id c) :sig (:sig c) :cards (vec (named-by (:id c))) :existing (bug-matches bugs c)})
                       (filter #(= :new (:class %)) classified))
        ids-of #(mapv :id (filter (fn [c] (= % (:verdict c))) card-reports))]
    {:counts (frequencies (map :class classified))
     :failures (vec (remove #(#{:pass} (:class %)) classified))
     :cards card-reports
     :tick (ids-of :tick)
     :pending (ids-of :pending)
     :hold (ids-of :hold)
     :no-plan (ids-of :no-plan)
     :new-bugs new-bugs}))

(defn summary [{:keys [counts tick pending hold no-plan new-bugs cards]}]
  (let [n #(get counts % 0)
        nplan (count (filter #(= :no-plan (:verdict %)) cards))]
    (str/join "\n"
              [(str "cases: pass " (n :pass) ", flaky " (n :flaky) ", known " (n :known) ", new " (n :new) ", inconclusive " (n :inconclusive) ", skipped-known " (n :skipped-known))
               (str "cards: " (count cards) " checked; tick " (count tick) ", to pending " (count pending) ", hold " (count hold) ", no plan " nplan)
               (str "tick: " (str/join " " (map #(subs % 0 8) tick)))
               "pending (card: failing cases [existing bug cards]):"
               (str/join "\n" (for [c cards :when (= :pending (:verdict c))]
                                (str "  " (subs (:id c) 0 8) ": " (str/join ", " (:failing c)) (when (seq (:bug-cards c)) (str " [" (str/join " " (map #(subs % 0 8) (:bug-cards c))) "]")))))
               (str "hold: " (str/join " " (map #(subs % 0 8) hold)))
               (str "new failures needing a bug card (" (count new-bugs) "):")
               (str/join "\n" (map #(str "  " (:id %) (when (seq (:existing %)) (str " [existing " (str/join " " (map (fn [b] (subs b 0 8)) (:existing %))) "]"))
                                      (when (seq (:cards %)) (str " cards " (str/join " " (map (fn [b] (subs b 0 8)) (:cards %))))))
                                   new-bugs))])))

;; ---- board and git, the impure edge

(defn sql-json [db sql]
  (let [r (cp/spawnSync "sqlite3" #js ["-json" (str "file:" db "?mode=ro") sql] #js {:encoding "utf8" :maxBuffer (* 256 1024 1024)})]
    (when-not (zero? (.-status r)) (throw (js/Error. (str "sqlite3: " (.-stderr r)))))
    (if (str/blank? (.-stdout r)) [] (js->clj (js/JSON.parse (.-stdout r)) :keywordize-keys true))))

(defn read-board [db board & [columns]]
  (let [cols (str "(" (str/join "," (map #(str "'" % "'") (or columns ["live"]))) ")")
        q (fn [s] (sql-json db (-> s (str/replace "BOARD" (str "'" board "'")) (str/replace "COLS" cols))))
        tasks (q "select id, title, description, status as column from tasks where board_id=BOARD and status in COLS")
        notes (group-by :task_id (q "select task_id, content from task_notes where task_id in (select id from tasks where board_id=BOARD and status in COLS) and lower(content) like '%live plan%' order by created_at"))
        commits (group-by :task_id (q "select task_id, sha from task_commits where task_id in (select id from tasks where board_id=BOARD and status in COLS)"))
        bugs (q "select id, title from tasks where board_id=BOARD and status not in ('done','rejected') and title like 'Bug%'")]
    {:cards (mapv (fn [t] (assoc t :plan-notes (mapv :content (notes (:id t))) :commits (mapv :sha (commits (:id t))))) tasks)
     :bugs bugs}))

(defn ancestor-fn
  "One git call: the revision's history, indexed by the first 7 hex digits; a shorter sha is scanned."
  [repo rev]
  (let [r (cp/spawnSync "git" #js ["rev-list" rev] #js {:cwd repo :encoding "utf8" :maxBuffer (* 256 1024 1024)})
        _ (when-not (zero? (.-status r)) (throw (js/Error. (str "git rev-list " rev ": " (.-stderr r)))))
        all (str/split-lines (.-stdout r))
        by-7 (group-by #(subs % 0 7) all)]
    (fn [sha]
      (if (< (count sha) 7)
        (boolean (some #(str/starts-with? % sha) all))
        (boolean (some #(str/starts-with? % sha) (by-7 (subs sha 0 7))))))))

(defn changed-fixtures
  "The fixture file names changed between two revisions of the repo."
  [repo from to]
  (let [r (cp/spawnSync "git" #js ["diff" "--name-only" (str from ".." to) "--" "engine/fixtures/world"] #js {:cwd repo :encoding "utf8"})]
    (when-not (zero? (.-status r)) (throw (js/Error. (str "git diff " from ".." to ": " (.-stderr r)))))
    (fixture-stems (.-stdout r))))

(defn repo-fixture-files
  "The fixture file names (no .edn) under engine/fixtures/world of the repo's working tree."
  [repo]
  (let [dir (str repo "/engine/fixtures/world")]
    (if (fs/existsSync dir)
      (fixture-stems (str/join "\n" (map #(str "engine/fixtures/world/" %) (array-seq (fs/readdirSync dir)))))
      #{})))

(defn with-plans [board plans]
  (update board :cards (fn [cards] (mapv (fn [c] (if-let [t (some (fn [[p t]] (when (str/starts-with? (:id c) p) t)) plans)] (assoc c :extra-plan t) c)) cards))))

(defn parse-args [argv]
  (loop [[a & more :as args] argv, o {:previous [] :repo "." :rev "HEAD" :db default-db :board default-board}]
    (cond
      (empty? args) o
      (= a "--previous") (let [[files rst] (split-with #(not (str/starts-with? % "--")) more)] (recur rst (update o :previous into files)))
      (#{"--dir" "--rev" "--repo" "--db" "--board" "--board-file" "--plans" "--out" "--prev-rev"} a) (recur (rest more) (assoc o (keyword (subs a 2)) (first more)))
      (= a "--unskip-list") (recur more (assoc o :unskip-list true))
      :else (recur more (update o :extra (fnil conj []) a)))))

(defn result-files [dir]
  (->> (array-seq (fs/readdirSync dir))
       (filter #(and (re-matches #"sweep-.*\.edn" %) (not= % "sweep-summary.edn")))
       sort
       (mapv #(str dir "/" %))))

(defn main
  "argv (array) -> exit code."
  [argv]
  (let [args (vec (array-seq argv))
        o (parse-args args)]
    (cond
      (and (:unskip-list o) (not (:extra o)) (:out o))
      (let [ids (unskip-ids (merge {:previous (mapv read-results (:previous o))
                                    :ancestor? (ancestor-fn (:repo o) (:rev o))}
                                   {:changed-files (when-let [pr (:prev-rev o)] (changed-fixtures (:repo o) pr (:rev o)))}
                                   (read-board (:db o) (:board o) ["testing" "live" "in_review"])))]
        (fs/writeFileSync (:out o) (apply str (map #(str % "\n") ids)))
        (js/console.log (str (count ids) " ids -> " (:out o)))
        0)
      (or (some #{"--help" "-h"} args) (not (:dir o)) (:extra o)) (do (js/console.log usage) (if (some #{"--help" "-h"} args) 0 2))
      :else
      (let [report (triage (merge {:results (vec (mapcat read-results (result-files (:dir o))))
                                   :previous (mapv read-results (:previous o))
                                   :ancestor? (ancestor-fn (:repo o) (:rev o))
                                   :fixture-files (repo-fixture-files (:repo o))}
                                  (with-plans (if-let [f (:board-file o)] {:cards (reader/read-string (fs/readFileSync f "utf8")) :bugs []} (read-board (:db o) (:board o)))
                                              (some-> (:plans o) (fs/readFileSync "utf8") reader/read-string))))]
        (when (:out o) (fs/writeFileSync (:out o) (pr-str report)))
        (js/console.log (summary report))
        0))))
