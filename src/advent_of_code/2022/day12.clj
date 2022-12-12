(ns advent-of-code.2022.day12
  "Day 12: Hill Climbing Algorithm. https://adventofcode.com/2022/day/12"
  (:require
   [clojure.string :as str]))

;;;; UTILS start

;; got this from @cgrand's stack overflow answer https://stackoverflow.com/a/4831170
(defn find-thing [needle haystack]
  (keep-indexed #(when (= %2 needle) %1) haystack))

(defn one-d-coord->two-d-coord [height-grid-dimensions one-d-coord]
  (let [[row-num col-num] height-grid-dimensions
        row-i (quot one-d-coord col-num)
        col-i (mod one-d-coord col-num)]
    [row-i col-i]))

(assert (= (one-d-coord->two-d-coord [5 5] 5) [1 0]))

(defn find-val-occurences-in-2d-vect [vect val]
  (let [dims [(count vect) (count (first vect))]]
    (->> vect
         flatten
         (find-thing val)
         (map #(one-d-coord->two-d-coord dims %)))))

;; dijkstra's shortest path algo
;; replaced my wacky implementation of the algo by a much cleaner implementation taken almost wholesale from https://gist.github.com/loganlinn/5437067

(def ^:private INFINITE (Long/MAX_VALUE))

(defn update-costs
  "Update costs for `curr`'s neighbours if less than cost currently in `costs`"
  [node->neighbors+step-costs costs curr unvisited]
  (let [curr-cost (costs curr)
        neightbors-of-curr-in-unvisited (select-keys (node->neighbors+step-costs curr) unvisited)]
    (reduce
     (fn [c [nbr nbr-cost]] (update-in c [nbr] (partial min (+ curr-cost nbr-cost))))
     costs
     neightbors-of-curr-in-unvisited)))

(defn dijkstra
  "returns shortest path lengths of all nodes from source node"
  [node->neighbors+step-costs src]
  (loop [costs (-> (zipmap (keys node->neighbors+step-costs) (repeat INFINITE))
                   (assoc src 0))
         curr  src
         unvisited (-> (apply hash-set (keys node->neighbors+step-costs))
                       (disj src))]
    (if (or (empty? unvisited) (= INFINITE (costs curr)))
      costs
      (let [costs' (update-costs node->neighbors+step-costs costs curr unvisited)
            curr'  (first (sort-by costs' unvisited))
            unvisited' (disj unvisited curr')]
        (recur costs' curr' unvisited')))))


;;;; UTILS end

;;;; PROBLEM START

;; problem seems like shortest path search
;; dijkastra's algorithm?
;; maybe simpler problem - with minimum number of edges rather than minimum cost
;; do we use the version which finds shortest path between two nodes, 
;; or do we use the variant which finds shortest path from source to all other nodes (producing a shortest-path tree)
;; I think I will go with the more general solution in both cases - because I'm sure part 2 will want some changes to be done in one of these dimensions

;; let us represent all squares by coords [i j] where i is the row and j is the column (both are 0 indexed)
;; so square at top left is [0 0]

(def input-filename "inputs/2022/day12.txt")

(def height-in-chars-grid
  (->> (slurp input-filename)
       str/split-lines
       (mapv #(str/split % #""))))

(def graph-data
  (let [height-grid-dims [(count height-in-chars-grid) (count (first height-in-chars-grid))]
        pos-of-S (first (find-val-occurences-in-2d-vect height-in-chars-grid "S"))
        pos-of-E (first (find-val-occurences-in-2d-vect height-in-chars-grid "E"))
        correct-height-in-chars-grid (-> height-in-chars-grid
                                         (assoc-in pos-of-S "a")
                                         (assoc-in pos-of-E "z"))
        char-str->int (fn [char-str]
                        (let [c (.charAt char-str 0)]
                          (- (int c) (int \a))))
        height-in-ints-grid (->> correct-height-in-chars-grid
                                 (mapv #(mapv char-str->int %)))]
    {:height-in-ints-grid height-in-ints-grid
     :height-grid-dims    height-grid-dims
     :start-coord         pos-of-S
     :end-coord           pos-of-E}))

(defn neighbors+step-costs-forward-direction-fn [{:keys [height-grid-dims height-in-ints-grid] :as graph-data} square-coord]
  (let [[x y] square-coord
        height-square-coord (get-in height-in-ints-grid square-coord)
        [num-rows num-cols] height-grid-dims
        possible-neighbors [[(dec x) y]
                            [(inc x) y]
                            [x (dec y)]
                            [x (inc y)]]]
    (->> possible-neighbors
         ;; filtering based on the fact that we are in a bounded box
         (filter (fn [[i j]]
                   (and (< -1 i num-rows)
                        (< -1 j num-cols))))
         ;; can only move to squares whose elevation is at most one higher than that of current square 
         (filter (fn [n-coord]
                   (let [height-n-coord (get-in height-in-ints-grid n-coord)]
                     (<= height-n-coord (inc height-square-coord)))))
         (map #(vector % 1))
         (into {}))))

(neighbors+step-costs-forward-direction-fn graph-data [1 1])

(defn square-coord->neighbors+step-costs-fn
  "mapping of nodes (/squares/their coords) to map of neighboring nodes and corresponding step costs"
  [graph-data neighbors+step-costs-fn]
  (let [{:keys [height-grid-dims] :as graph-data} graph-data
        [num-rows num-cols] height-grid-dims]
    (->> (for [row-i (range num-rows)
               col-i (range num-cols)]
           (let [coords-i [row-i col-i]]
             [coords-i (neighbors+step-costs-fn graph-data coords-i)]))
         (into {}))))

(let [{:keys [start-coord end-coord]} graph-data
      square-coord->neighbors+step-costs (square-coord->neighbors+step-costs-fn graph-data neighbors+step-costs-forward-direction-fn)]
  (-> (dijkstra square-coord->neighbors+step-costs start-coord)
      (get end-coord)))
;; answer for part 1: 468

;; part 2
;; oh lol I'd considered that we might want to change E, but I hadn't expected we'd want to change S lol

;; 2 ways now
;; 1. find all items with height `a` and run above separately, then find shortest one
;; 2. (much more efficient). Make the old E the new starting point and then get the shortest path length to each node, then filter for those with elevation a and get shortest
;;        will also need to change the neighbors function
;; going with option 2

(defn neighbors+step-costs-reverse-direction-fn [{:keys [height-grid-dims height-in-ints-grid] :as graph-data} square-coord]
  (let [[x y] square-coord
        height-square-coord (get-in height-in-ints-grid square-coord)
        [num-rows num-cols] height-grid-dims
        possible-neighbors [[(dec x) y]
                            [(inc x) y]
                            [x (dec y)]
                            [x (inc y)]]]
    (->> possible-neighbors
         ;; filtering based on the fact that we are in a bounded box
         (filter (fn [[i j]]
                   (and (< -1 i num-rows)
                        (< -1 j num-cols))))
         ;; reverse of filtering done at same step in neighbors+step-costs-forward-direction-fn
         ;;     by reverse, I mean that we can just swap height-n-coord and height-square-coord
         ;; can only move to squares whose elevation is at most one level lower (can go to any neighboring block which is arbitrarily higher, ) 
         (filter (fn [n-coord] 
                   (let [height-n-coord (get-in height-in-ints-grid n-coord)]
                     (<= height-square-coord (inc height-n-coord)))))
         (map #(vector % 1))
         (into {}))))

(let [{:keys [end-coord height-in-ints-grid]} graph-data
      square-coord->neighbors+step-costs (square-coord->neighbors+step-costs-fn graph-data neighbors+step-costs-reverse-direction-fn)
      costs-from-end (dijkstra square-coord->neighbors+step-costs end-coord)
      square-coords-with-elevation-0 (find-val-occurences-in-2d-vect height-in-ints-grid 0)]
  (->> square-coords-with-elevation-0
       (map costs-from-end)
       (apply min)))
;; answer for part 2: 459