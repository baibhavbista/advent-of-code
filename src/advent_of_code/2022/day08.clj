(ns advent-of-code.2022.day08
  "https://adventofcode.com/2022/day/8"
  (:require
   [clojure.string :as str]))

(def input-filename "inputs/2022/day08.txt")

(def tree-grid (->> (slurp input-filename)
                    (str/split-lines)
                    (mapv (fn [line-s] (->> (str/split line-s #"")
                                            (mapv #(Integer/parseInt %)))))))

(defn transpose 
  "essentially swaps rows and columns"
  [grid] (apply map list grid))

(defn reverse-rows 
  "reverses direction of all the rows"
  [grid] (map reverse grid))

;; following functions take a `transform-fn` which is applied in the direction corresponding to the name of the function
;; from-left-matrix for example applies transform-fn to all rows
;;   from-rght-matrix does the same but reverses the row before passing it to transform-fn
;; from-top-matrix for example applies transform-fn to all columns
;;  from-botton-matrix does the same but reverses the column before passing it to transform-fn

;; the reason it was done in such a way is so that one function (say a function for calculating the visibility of tree or 
;; a function for calculating number of tress visible) could be coded such that it calculates required value roww-se from the left 
;; and it can then be used for all 4 directions 

(defn from-left-matrix [transform-fn]
  (->> tree-grid
       (map transform-fn)))

(defn from-top-matrix [transform-fn]
  (->> tree-grid
       transpose
       (map transform-fn)
       transpose))

(defn from-right-matrix [transform-fn]
  (->> tree-grid
       reverse-rows
       (map transform-fn)
       reverse-rows))

(defn from-bottom-matrix [transform-fn]
  (->> tree-grid
       transpose
       reverse-rows
       (map transform-fn)
       reverse-rows
       transpose))

;;;; PART 1

;; function which traverses a row and returns true for the trees that are visible from the left
(defn tree-row-visible-from-left-bools [tree-row]
  (:acc (reduce (fn [{:keys [acc max]} tree-h]
                  (let [new-max? (< max tree-h)]
                    {:acc (conj acc new-max?)
                     :max (if new-max? tree-h max)}))
                {:max -1 ; since there are trees of height 0
                 :acc []}
                tree-row)))

(assert (= (tree-row-visible-from-left-bools '(2 5 5 1 2))
           [true true false false false]))

;; bool lists for each row
(def bool-matrix-visible-trees
  (->> tree-row-visible-from-left-bools
       ((juxt from-left-matrix from-top-matrix from-right-matrix from-bottom-matrix))
       (apply map vector)
       (map (fn [bool-lists-for-a-row] (reduce (fn [acc row]
                                                 (map #(or %1 %2) acc row))
                                               bool-lists-for-a-row)))))

(->> bool-matrix-visible-trees
     flatten
     (filter true?)
     count)
;; part 1: 1662

;; PART 2
;; fuck it. I'm switching over to using indexes 

;; function which traverses a row and returns numbers of trees visible to the left
(defn num-trees-visible-to-left [tree-row i]
  (let [val-i (nth tree-row i)]
    (loop [m (dec i)]
      (if (< m 0)
        i
        (if (<= val-i (nth tree-row m))
          (- i m)
          (recur (dec m)))))))

(defn num-trees-visible-to-left-fn-for-whole-row [tree-row] (map (partial num-trees-visible-to-left tree-row) (range (count tree-row))))

(def matrix-scenic-scores
  (->> num-trees-visible-to-left-fn-for-whole-row
       ((juxt from-left-matrix from-top-matrix from-right-matrix from-bottom-matrix))
       (apply map vector)
       (map (fn [num-tress-visible-lists-for-a-row]
              (reduce (fn [acc row]
                        (map #(* %1 %2) acc row))
                      ;; contains 4 elems, one for each direction
                      num-tress-visible-lists-for-a-row)))))

(->> matrix-scenic-scores
     flatten
     (apply max))
;; part 2: 537600

