(ns advent-of-code.2022.day13
  "Day 13: Distress Signal https://adventofcode.com/2022/day/13"
  (:require
   [clojure.string :as str]
   [clojure.set :refer [map-invert]]))

;; so glad that input matches EDN

(def pair-data-list (let [pairs-as-str (-> (slurp "inputs/2022/day13.txt")
                                            (str/split #"\n\n"))]
                      (->> pairs-as-str
                           (map #(->> (str/split % #"\n")
                                      (map read-string))))))

(defn ->list 
  "takes either an integer or a list. In case it's a list, return as it is. otherwise, wrap it in a list"
  [int-or-list]
  (if-not (coll? int-or-list)
    (list int-or-list)
    int-or-list))

;; converting from clojure's compare's return types to the keywords I'm using
(def compare-res-int->k {-1 :correct-order
                         0 :proceed
                         1 :incorrect-order})

(def compare-res-k->int (map-invert compare-res-int->k))

(defn data-compare [left right]
  (let [int-compare  (comp compare-res-int->k compare)
        list-compare (fn [x y]
                       (let [len-x (count x)
                             len-y (count y)
                             outcome-of-comparing-comparable-sections-of-lists
                             ;; if two lists are of different lengths, comparable sections would be entire smaller list and equal size portion of larger list
                             (->> (map data-compare x y)
                                  (remove #(= % :proceed))
                                  first)]
                         (or outcome-of-comparing-comparable-sections-of-lists
                             (if (= len-x len-y)
                               :proceed
                               (if (> len-y len-x) :correct-order :incorrect-order)))))]
    (cond
      (and (integer? left) (integer? right))
      (int-compare left right)
      (and (coll? left) (coll? right))
      (list-compare left right)
      (or (integer? left) (integer? right))
      (list-compare (->list left) (->list right)))))

(->> pair-data-list
     (map (fn [[left right]]
            (data-compare left right)))
     (map-indexed vector)
     (filter (fn [[_ order-k]] (= order-k :correct-order)))
     ;; get indexes and increment because to calculate as required in the question, it needs to be 1-indexed
     (map (comp inc first))
     (apply +))
;; part 1 answer: 6656

;;;; PART 2
;; soltion seems to just be to sort them given the comparator function
;; is there a way to do it faster? by getting a score for a single list which we can then memoize maybe?
;;     given the comparator function, I assume not. So I'm just going to proceed naively

(let [additional-packets-list '([[2]] [[6]])
      is-divider-packet? (set additional-packets-list)
      packets-in-order
      (->> pair-data-list
           (apply concat additional-packets-list)
           (sort (comp compare-res-k->int data-compare)))
      ]
  (->> (map-indexed vector packets-in-order)
       (filter (comp is-divider-packet? second))
       ;; get indices and make them 1-indexed
       (map (comp inc first))
       (apply *)))

;; part 2 answer: 19716