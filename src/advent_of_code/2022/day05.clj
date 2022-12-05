(ns advent-of-code.2022.day05
  "https://adventofcode.com/2022/day/5"
  (:require
   [clojure.string :as str]))

(def input-filename "inputs/2022/day05.txt")


;;;; DATA STRUCTURES

;; STATE
;; Given the following config
;;     [D]
;; [N] [C]
;; [Z] [M] [P]
;; 1   2   3
;; it is represented as the following vector: [(\N \Z) (\D \C \M) (\P)]
;; since the outer structure is a vector, can use assoc and update like in maps

;; MOVES
;; Each move is represented by a vector (move-vec) of the type [n a b]
;; means that we are moving n things from column a to b (NOTE that these are 0-indexed)
;; "move 1 from 4 to 3" -> [1 3 2]


(defn move-str->move-vec [move-str]
  (let [[n a-1-indexed b-1-indexed] (->> move-str
                                          (re-matches #"move (\d+) from (\d+) to (\d+)")
                                          ;; drop the whole string match
                                          (drop 1)
                                          (map #(Integer/parseInt %)))]
       [n (dec a-1-indexed) (dec b-1-indexed)]))

(defn start-state [start-state-strs]
  (let [;; remove the one line with just the stack numbers
        start-state-strs (drop-last 1 start-state-strs)
        levels           (map #(->> %
                                    (partition-all 4)
                                    (map second))
                                  start-state-strs)
        init-state (->> levels
                        ;; if you think of a 2D coll as a matrix, essentially makes rows columns and vice versa
                        (apply map list)
                        ;; remove the \space chars, these are places where there should be empty space.
                        ;; Since they are now at the start of each list (i.e. the top of the stack), they can now be removed
                        (map #(remove (fn [c] (= c \space)) %)))]
    (vec init-state)))

(defn top-of-all-stacks [state]
  (->> state
       (map first)
       (apply str)))

;; for part 1
(defn state-transition-fn-move-1-at-a-time
  [state-map [n a b]]
  (let [items-to-move (take n (get state-map a))]
    (-> state-map
        (update a #(drop n %))
        (update b #(concat (reverse items-to-move) %)))))

;; for part 2
;; the same as above except for the reverse
(defn state-transition-fn-move-n-at-a-time
  [state-map [n a b]]
  (let [items-to-move (take n (get state-map a))]
    (-> state-map
        (update a #(drop n %))
        (update b #(concat items-to-move %)))))

(let [lines (->> (slurp input-filename)
                 (str/split-lines))
      [start-state-strs blank-line+moves] (split-with (comp not str/blank?) lines)
      moves      (remove str/blank? blank-line+moves)
      moves-vecs (map move-str->move-vec moves)
      start-state (start-state start-state-strs)]
  [ ;; answer 1
   (let [final-state (reduce state-transition-fn-move-1-at-a-time
                             start-state
                             moves-vecs)]
     (top-of-all-stacks final-state))
   ;; answer 2
   (let [final-state (reduce state-transition-fn-move-n-at-a-time
                             start-state
                             moves-vecs)]
     (top-of-all-stacks final-state))]
  )

;; answer 1: "VGBBJCRMN"
;; answer 2: "LBBVJBRMH"