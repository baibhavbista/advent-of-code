(ns advent-of-code.2022.day20
  "Day 20: Grove Positioning System https://adventofcode.com/2022/day/20"
  (:require
   [clojure.string :as str]
   [clojure.set])
  (:import
   (java.util
    ArrayList)))

(def input-filename "inputs/2022/day20.txt")

(def original-list
  (->> (slurp input-filename)
       (str/split-lines)
       (map #(Long/parseLong %))))

;; In order to make the removal from one place and insertion into another place faster, I think we'll need a sort of linked list structure
;;   we can have a separate list where i'th element is a pointer to the originally ith element
;;   clojure immutable data structures mean that I probably can't do pointer based techniques in clojure
;; Falsified due to:: Naive version turned out to work perfectly good enough for the size of data we received

;; the naive version is going to be a pain (read computationally expensive) to do in pure clojure datastructures
;; will probably have to drop to Java data structures

(defn grove-coords-sum 
  ([original-list]
   (grove-coords-sum original-list 1 1))
  ([original-list decryption-key num-of-mixing]
   (let [cnt (count original-list)
         actual-list-after-key (map #(* decryption-key %) original-list)
         ^ArrayList array-idxs (new ArrayList (range cnt))]
     (doseq [_ (range num-of-mixing)]
       (doseq [[i-orig val] (map-indexed vector actual-list-after-key)]
         (let [old-index (.indexOf array-idxs i-orig)
               new-index (mod (+ old-index val) (dec cnt))]
           (doto array-idxs
             (.remove old-index)
             (.add new-index i-orig))
           #_(println "new list: " (map #(nth actual-list-after-key %) array-idxs)))))
     (let [i-zero-in-orig-list  (.indexOf actual-list-after-key 0)
           i-0-in-mixed-list (.indexOf array-idxs (long i-zero-in-orig-list))
           i-1000-in-mixed-list (mod (+ 1000 i-0-in-mixed-list) cnt)
           val-1000 (nth actual-list-after-key (nth array-idxs i-1000-in-mixed-list))
           i-2000-in-mixed-list (mod (+ 2000 i-0-in-mixed-list) cnt)
           val-2000 (nth actual-list-after-key (nth array-idxs i-2000-in-mixed-list))
           i-3000-in-mixed-list (mod (+ 3000 i-0-in-mixed-list) cnt)
           val-3000 (nth actual-list-after-key (nth array-idxs i-3000-in-mixed-list))]
       (+ val-1000 val-2000 val-3000)))))

(grove-coords-sum original-list)
;; answer 1: 8721

(grove-coords-sum original-list 811589153 10)
;; answer 2: 831878881825