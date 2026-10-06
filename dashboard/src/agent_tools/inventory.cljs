(ns agent-tools.inventory)

(defn- short-name [value]
  (when (string? value) (subs value 0 (min 80 (count value)))))

(defn- stack-view [stack]
  (when (and (map? stack) (short-name (:name stack))
             (integer? (:count stack)) (pos? (:count stack)))
    (cond-> {:name (short-name (:name stack)) :count (:count stack)}
      (and (integer? (:slot stack)) (<= 0 (:slot stack) 45)) (assoc :slot (:slot stack)))))

(defn- equipment-view [items]
  (into {}
        (keep (fn [[slot item]]
                (cond
                  (and (keyword? slot) (= :empty item)) [slot :empty]
                  (and (keyword? slot) (map? item) (short-name (:name item)))
                  [slot (cond-> {:name (short-name (:name item))}
                          (and (integer? (:count item)) (pos? (:count item))) (assoc :count (:count item))
                          (and (number? (:durability item)) (not (neg? (:durability item))))
                          (assoc :durability (:durability item)))])))
        items))

(defn compact
  "Token-light inventory/equipment output. Stack slots are included only when requested."
  ([value mode] (compact value mode false))
  ([value mode include-slots?]
   (if (false? (:ok value))
     value
     (let [stacks (->> (:inventory value) (keep stack-view) (take 46) vec)
           equipment (equipment-view (:equipment value))]
       (if (= mode :equipment)
         (cond-> {:equipment (if (seq equipment) equipment :none)}
           (:last-known value) (assoc :last-known true :offline (:offline value)))
         (let [counts (reduce (fn [result {:keys [name count]}]
                                (update result name (fnil + 0) count)) (sorted-map) stacks)
               total (reduce + 0 (vals counts))]
           (cond-> {:total-items total :kinds (count counts)}
             (seq counts) (assoc :counts counts)
             (seq equipment) (assoc :equipment equipment)
             (and include-slots? (seq stacks)) (assoc :slots stacks)
             (true? (:more? value)) (assoc :more? true)
             (:last-known value) (assoc :last-known true :offline (:offline value)))))))))
