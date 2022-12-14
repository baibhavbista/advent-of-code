(ns advent-of-code.2022.day14
  "Day 14: Regolith Reservoir https://adventofcode.com/2022/day/14"
  (:require
   [clojure.string :as str]
   [clojure.set]))

;; IMPORTANT NOTE: The axes provided in the question are different from one generally used in CS. 
;;   The first coordinate is the X coordinate (going left to right), and second coordinate is the Y coord (going top to bottom)

(def input-filename "inputs/2022/day14.txt")

(def rock-struct-strs (->> (slurp input-filename)
                           str/split-lines))

(defn numbers-between-inclusive
  "works even when x > y"
  [x y]
  (if (< x y)
    (range x (inc y))
    (range y (inc x))))

(defn line-segment-ends->coords [[start-coord-x start-coord-y] [end-coord-x end-coord-y]]
  (cond
    ;; if x coord is the same - a vertical line
    (= start-coord-x end-coord-x)
    (map vector (repeat start-coord-x) (numbers-between-inclusive start-coord-y end-coord-y))
    ;; if y coord is the same - a horizontal line
    (= start-coord-y end-coord-y)
    (map vector (numbers-between-inclusive start-coord-x end-coord-x) (repeat start-coord-y))
    :else (throw (ex-info "fn line-segment-coords only works for vertical or for horizontal lines" {:data [[start-coord-x start-coord-y] [end-coord-x end-coord-y]]}))))

(defn rock-struct-str->coords-set [rock-struct-str]
  (let [line-end-point-coords
        (->> (str/split rock-struct-str #" -> ")
             (map (fn [coord-str] (->> (str/split coord-str #",")
                                       (mapv #(Integer/parseInt %))))))]
    (loop [start-point  (first line-end-point-coords)
           other-points (rest line-end-point-coords)
           all-coords   #{}]
      (if (empty? other-points)
        all-coords
        (let [next-point (first other-points)
              new-coords (line-segment-ends->coords start-point next-point)]
          (recur next-point
                 (rest other-points)
                 (apply conj all-coords new-coords)))))))

(def all-rocks-set
  (->> rock-struct-strs
       (map rock-struct-str->coords-set)
       (apply clojure.set/union)))

(def sand-source-coord [500 0])


;;;; PART 1 

(def coord-limits
  (let [all-relevant-coords (cons sand-source-coord all-rocks-set)
        [min-x min-y] (apply map min all-relevant-coords)
        [max-x max-y] (apply map max all-relevant-coords)]
    [[min-x max-x]
     [min-y max-y]]))

;; so we can think of our viewport as having these (inclusive) edges/limits
;; if a calculated position of sand at any point does not fit within these limits, it would be out of the region with rocks
;;   and so we can consider it falling into the endless void

(defn inside-of-region-with-rocks? [[x y]]
  (let [[[min-x max-x]
         [min-y max-y]] coord-limits]
    (and (<= min-x x max-x)
               (<= min-y y max-y))))

(defn sand-drop-next-coord-part-1
  "if sand can go down, go down-left or go down-right (priority in that order), returns respected coord
   if sand is stuck, returns the same as current-coord"
  [obstructions-coords-set current-coord]
  (let [[current-coord-x current-coord-y] current-coord
        coord-down       [current-coord-x       (inc current-coord-y)]
        coord-down-left  [(dec current-coord-x) (inc current-coord-y)]
        coord-down-right [(inc current-coord-x) (inc current-coord-y)]]
    (cond
      (not (obstructions-coords-set coord-down))       coord-down
      (not (obstructions-coords-set coord-down-left))  coord-down-left
      (not (obstructions-coords-set coord-down-right)) coord-down-right
      :else                                            current-coord)))

(let [sand+rocks-set-after-stable-state 
      (loop [obstructions-coords-set all-rocks-set]
        (let [{:keys [status coord]
               :as status-after-following-one-sand}
              (loop [sand-drop-coord sand-source-coord]
                (let [new-coord (sand-drop-next-coord-part-1 obstructions-coords-set sand-drop-coord)]
                  (cond
                    (not (inside-of-region-with-rocks? new-coord))
                    {:status :endless-fall}
                    (= new-coord sand-drop-coord)
                    {:status :at-rest
                     :coord  sand-drop-coord}
                    :else
                    (recur new-coord))))]
          (case status
            ;; if one sand endless fall, means that it's over 
            :endless-fall obstructions-coords-set
            ;; if sand drop settles to rest at coord, then add that to obstructions and loop agaibn
            :at-rest      (recur (conj obstructions-coords-set coord)))))
      sand-set (clojure.set/difference sand+rocks-set-after-stable-state all-rocks-set)]
  (count sand-set))

;; answer part 1: 885


;;;; PART 2

(def floor-y-coord
  (let [all-relevant-coords (cons sand-source-coord all-rocks-set)]
    (->> all-relevant-coords
         (map second)
         (apply max)
         (+ 2))))

(defn sand-drop-next-coord-part-2
  "For Part 2 (with infinite floor) 
   if sand can go down, go down-left or go down-right (priority in that order), returns respected coord
   if sand is stuck, returns the same as current-coord"
  [obstructions-coords-set current-coord]
  (let [[current-coord-x current-coord-y] current-coord]
    (if (= (inc current-coord-y) floor-y-coord)
      ;; stuck there due to infinite wall at depth = floor-y-coord
      current-coord
      (let [coord-down       [current-coord-x       (inc current-coord-y)]
            coord-down-left  [(dec current-coord-x) (inc current-coord-y)]
            coord-down-right [(inc current-coord-x) (inc current-coord-y)]]
        (cond
          (not (obstructions-coords-set coord-down))       coord-down
          (not (obstructions-coords-set coord-down-left))  coord-down-left
          (not (obstructions-coords-set coord-down-right)) coord-down-right
          :else                                            current-coord)))))

(let [sand+rocks-set-after-stable-state
      (loop [obstructions-coords-set all-rocks-set]
        (let [final-coord-after-following-one-sand-drop
              (loop [sand-drop-coord sand-source-coord]
                (let [new-coord (sand-drop-next-coord-part-2 obstructions-coords-set sand-drop-coord)]
                  (if (= new-coord sand-drop-coord)
                    sand-drop-coord
                    (recur new-coord))))
              new-obstructions-set (conj obstructions-coords-set final-coord-after-following-one-sand-drop)]
          (if (= final-coord-after-following-one-sand-drop sand-source-coord)
            new-obstructions-set
            (recur new-obstructions-set))))
      sand-set (clojure.set/difference sand+rocks-set-after-stable-state all-rocks-set)]
  (count sand-set))

;; part 2 answer: 28691