(ns advent-of-code.2022.day09
  "https://adventofcode.com/2022/day/9"
  (:require
   [clojure.string :as str]))

(def input-filename "inputs/2022/day09.txt")

(defn input-line->command-seq [input-line]
  (let [[cmd-str num-str] (str/split input-line #" ")
        num-repeat (Integer/parseInt num-str)
        cmd        (keyword cmd-str)]
    (take num-repeat (repeat cmd))))


(def moves (->> (slurp input-filename)
                (str/split-lines)
                (map input-line->command-seq)
                flatten))

moves
;; something like (:R :R :R :R :U :U :U :U :L :L :L :D :R :R :R :R :D :L :L :L :L :L :R :R)


(defn is-adjacent? [[x1 y1] [x2 y2]]
  (and (<= (Math/abs (- x1 x2)) 1)
       (<= (Math/abs (- y1 y2)) 1)))

(assert (is-adjacent? [1 0] [0 1]))
(assert (is-adjacent? [0 0] [0 0]))
(assert (is-adjacent? [0 1] [0 0]))
(assert (is-adjacent? [0 0] [1 0]))

;; move can be :L :R :U :D

(defn new-pos [old-pos offset]
  (mapv + old-pos offset))

(defn move->head-pos-offset [move-kw]
  (case move-kw
    :L [-1 0]
    :R [1 0]
    :U [0 1]
    :D [0 -1]))

;; assumption: standard XY axis and starting at origin
;; i.e. starting point is [0 0]. If you move right, x increases. If you move up, y increases

(defn tail-catchup [tail-old-pos head-new-pos]
  (if (is-adjacent? tail-old-pos head-new-pos)
    tail-old-pos
    ;; otherwise have to catch up 
    (let [[tail-old-x tail-old-y] tail-old-pos
          [head-new-x head-new-y] head-new-pos]
      (if (or (= tail-old-x head-new-x) (= tail-old-y head-new-y))
        ;; can just take the average of both coords since this is only triggered when 2 steps away in one dimension
        [(/ (+ tail-old-x head-new-x) 2)
         (/ (+ tail-old-y head-new-y) 2)]
        ;; if head and tail are not touching and aren't in same row or col, tail moves one step diagonally to keep up
        (let [offset [(if (> head-new-x tail-old-x) 1 -1)
                      (if (> head-new-y tail-old-y) 1 -1)]]
          (new-pos tail-old-pos offset))))))

;; case where do not need to move tail
(assert (= (tail-catchup [0 0] [1 0]) [0 0]))
;; case where tail needs to move because 2 steps away in one dimension
(assert (= (tail-catchup [1 1] [3 1]) [2 1]))
;; case where tail needs to move diagonally
(assert (= (tail-catchup [1 1] [3 2]) [2 2]))


(defn positions-after-move-2-knots [[head-old-pos tail-old-pos] move-kw]
  (let [head-offset  (move->head-pos-offset move-kw)
        head-new-pos (new-pos head-old-pos head-offset)
        tail-new-pos (tail-catchup tail-old-pos head-new-pos)]
    [head-new-pos tail-new-pos]))


(let [init-positions         [[0 0] [0 0]]
      positions-list-reverse (reduce (fn [positions-list-reverse move-kw]
                                       (let [last-positions (first positions-list-reverse)
                                             new-positions  (positions-after-move-2-knots last-positions move-kw)]
                                         (conj positions-list-reverse new-positions)))
                                     (list init-positions)
                                     moves)]
  (->> positions-list-reverse
      (map second) 
       (into #{})
       count))
;; answer 1: 6037

;; part 2
;; now we have 10 knots instead of just head and tail (i.e. 2 knots) before
;; calling them H, 1, 2, ..., 9

(defn positions-after-move-10-knots [[head-old-pos & other-knots-pos-list] move-kw]
  (let [head-offset  (move->head-pos-offset move-kw)
        head-new-pos (new-pos head-old-pos head-offset)]
    (reduce (fn [acc this-knot-old-pos]
              (let [last-knot-new-pos (last acc)]
                (conj acc (tail-catchup this-knot-old-pos last-knot-new-pos))))
            (vector head-new-pos)
            other-knots-pos-list)))

(let [init-positions         (take 10 (repeat [0 0]))
      positions-list-reverse (reduce (fn [positions-list-reverse move-kw]
                                       (let [last-positions (first positions-list-reverse)
                                             new-positions  (positions-after-move-10-knots last-positions move-kw)]
                                         (conj positions-list-reverse new-positions)))
                                     (list init-positions)
                                     moves)]
  (->> positions-list-reverse
       (map last)
       (into #{})
       count))
;; answer 2: 2485
