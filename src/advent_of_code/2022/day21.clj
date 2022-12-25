(ns advent-of-code.2022.day21
  "Day 21: Monkey Math https://adventofcode.com/2022/day/21"
  (:require
   [clojure.string :as str]
   [clojure.set]))

(def input-filename "inputs/2022/day21.txt")

(def lines
  (->> (slurp input-filename)
       (str/split-lines)))

(def specific-numbers-monkeys 
  (let [line->kv (fn [s]
                   (let [[_ node-s val-s] (re-matches #"(\w+): (\d+)" s)]
                     (when (and node-s val-s)
                       [(keyword node-s) (Long/parseLong val-s)])))]

    (->> lines
         (map line->kv)
         (remove nil?)
         (into {}))))

(def equation-monkeys
  (let [line->kv (fn [s]
                   (let [[_ node-k left-k op-k right-k]
                         (->> s
                              (re-matches #"(\w+): (\w+) ([+-/*]) (\w+)")
                              (map keyword))]
                     (when (and node-k left-k op-k right-k)
                       [node-k [op-k left-k right-k]])))]

    (->> lines
         (map line->kv)
         (remove nil?)
         (into {}))))

(assert (= (count lines) 
           (+ (count specific-numbers-monkeys) (count equation-monkeys))))

(defn list-to-calculate-in-order-fn [equations k-to-calculate]
  (reverse (tree-seq #(and (keyword? %)
                           (contains? equations %))
                     (fn [node]
                       (->> node
                            (get equations)
                            rest))
                     k-to-calculate)))

(defn calculate-value-fn [calculated-values equation]
  (let [[op-k left-k right-k] equation
        left-v  (get calculated-values left-k)
        right-v (get calculated-values right-k)]
    (case op-k
      :+ (+ left-v right-v)
      :- (- left-v right-v)
      :/ (quot left-v right-v)
      :* (* left-v right-v))))

(assert (= 30
           (let [calculate-values {:dbpl 5, :zczc 2, :dvpt 3, :lfqf 4, :humn 5, :ljgn 2, :sllz 4, :hmdt 32}
                 equation [:- :hmdt :zczc]]
             (calculate-value-fn calculate-values equation))))

(defn calculate-all-vals [equations already-known-vals list-to-calculate-in-order]
  (->> (loop [remaining-list  list-to-calculate-in-order
              calculated-vals already-known-vals]
         (if-let [node-k (first remaining-list)]
           (if (contains? calculated-vals node-k)
             (recur (rest remaining-list) calculated-vals)
             (let [node-v (calculate-value-fn calculated-vals (get equations node-k))]
               (recur (rest remaining-list)
                      (assoc calculated-vals node-k node-v))))
         ;; if remaining-list is empty, return calculated-vals
           calculated-vals))))

(defn binary-equation->other-two-equations [binary-equation]
  (let [[res-k [op-k left-k right-k]] binary-equation
        val
        (case op-k
          :+ [[left-k  [:- res-k right-k]]
              [right-k [:- res-k left-k]]]
          :- [[left-k  [:+ res-k right-k]]
              [right-k [:- left-k res-k]]]
          :* [[left-k  [:/ res-k right-k]]
              [right-k [:/ res-k left-k]]]
          :/ [[left-k  [:* res-k right-k]]
              [right-k [:/ left-k res-k]]])]
    val))

(let [list-in-order-for-1     (list-to-calculate-in-order-fn equation-monkeys :root)
      calculated-values-for-1 (calculate-all-vals equation-monkeys specific-numbers-monkeys list-in-order-for-1)
      answer-1                (:root calculated-values-for-1)


      node->parent (->> equation-monkeys
                        (mapcat (fn [[top [_op left right]]]
                                  [[left top]
                                   [right top]]))
                        (into {}))
      path-from-humn-to-root (loop [curr-node :humn
                                    acc [:humn]]
                               (if-let [node-parent (node->parent curr-node)]
                                 (recur node-parent
                                        (conj acc node-parent))
                                 acc))
      ;; when we change :humn and :root, only these values change
      known-values-for-2 (-> (apply dissoc calculated-values-for-1 path-from-humn-to-root)
                             (assoc :root 0))
      ;; for root, change the operation to minus and value to 0
      equation-monkeys-for-2   (assoc-in equation-monkeys [:root 0] :-)
      ;; we actually need to traverse in opposite direction
      complementary-equations  (->> equation-monkeys-for-2
                                    (mapcat binary-equation->other-two-equations)
                                    (into {}))
      list-in-order-for-2      (reverse path-from-humn-to-root)
      calculated-values-for-2  (calculate-all-vals complementary-equations known-values-for-2 list-in-order-for-2)
      answer-2                 (:humn calculated-values-for-2)]
  [answer-1 answer-2])
;; answers part 1 and 2: [155708040358220 3342154812537]