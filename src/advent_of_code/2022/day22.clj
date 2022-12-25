(ns advent-of-code.2022.day22
  "Day 22: Monkey Map https://adventofcode.com/2022/day/22"
  (:require
   [clojure.string :as str]
   [clojure.set]))

(def input-filename "inputs/2022/day22.txt")


;; rows and cols are 0-indexed in code and converted to 1-indexed at the last possible time

;; x axis is vertical, y axis is horizontal


;; got this from @cgrand's stack overflow answer https://stackoverflow.com/a/4831170
(defn find-thing [needle haystack]
  (keep-indexed #(when (= %2 needle) %1) haystack))

(defn transpose
  "essentially swaps rows and columns"
  [grid] (apply map list grid))

(defn normalize-str-by-adding-blank-space-to-end [s required-length]
  (let [curr-length (count s)
        blank-spaces-at-end (- required-length curr-length)]
    (vec (concat (seq s) (take blank-spaces-at-end (repeat \space) )))))

(defn print-char-grid [char-grid]
  (println (->> char-grid
                (map #(apply str %))
                (reduce (fn [acc-s s]
                          (str acc-s "\n" s))))))

(def facing-direction->movement-offset
  {:left  [0 -1]
   :right [0  1]
   :up    [-1 0]
   :down  [1  0]})

;; the following 3 defs could be replaced by an ordered list and some modulo operations, but I'm sick of module atm
(def clockwise-rotation-facing-dir-change
  {:left :up
   :up :right
   :right :down
   :down :left})

(def anti-clockwise-rotation-facing-dir-change (clojure.set/map-invert clockwise-rotation-facing-dir-change))

(def facing-direction->value
  {:right 0
   :down 1
   :left 2
   :up 3})


(defn next-facing-direction-fn 
  "turn-str can be R for clockwise or L for anti clockwise"
  [curr-facing-direction turn-str]
  #_(println "next-facing-direction-fn: " curr-facing-direction turn-str)
  (let [curr->next-facing-dir (if (= turn-str "R")
                                clockwise-rotation-facing-dir-change
                                anti-clockwise-rotation-facing-dir-change)]
    (get curr->next-facing-dir curr-facing-direction)))


(let [[board-str steps-and-dirs-str]     (-> (slurp input-filename)
                                             (str/split #"\n\n"))
      steps-and-dirs-seq (->> steps-and-dirs-str
                              (partition-by #{\R \L})
                              (map #(apply str %))
                              (map (fn [s]
                                     (if (or (= s "R") (= s "L"))
                                       s
                                       (Long/parseLong s)))))
      board-row-strs                 (str/split-lines board-str)
      board-char-grid                (let [max-len (apply max (map count board-row-strs))]
                                       (mapv #(normalize-str-by-adding-blank-space-to-end % max-len) board-row-strs))
      ;; bound inclusive of start, exclusive of end
      board-row-bounds-fn (fn [board-row-char-vec]
                            (let [start-index  (count (take-while #(= % \space) board-row-char-vec))
                                  rem-char-vec (drop start-index board-row-char-vec)
                                  actual-data-vec (take-while #(not= % \space) rem-char-vec)
                                  end-index   (+ start-index (count actual-data-vec))]
                              [start-index end-index]))
      board-row-bounds    (mapv board-row-bounds-fn board-char-grid)
      board-col-bounds    (mapv board-row-bounds-fn (transpose board-char-grid))
      wall-coords-set     (->> board-row-strs
                               (map #(vec (find-thing \# %)))
                               (map-indexed (fn [row cols-with-wall]
                                              (map #(vector row %) cols-with-wall)))
                               (apply concat)
                               (into #{}))
      next-coord-fn      (fn next-coord [curr-coord facing-dir]
                           (let [offset (facing-direction->movement-offset facing-dir)
                                 [x y] (mapv + curr-coord offset)
                                 #_#__ (println "[x y]:" [x y])
                                 possible-next-coord-with-wrapping
                                 (if (facing-dir #{:left :right})
                                    ;; moving in y axis, i.e. along row
                                   (let [[y-min y-max] (get board-row-bounds x)
                                         new-y (+ y-min (mod (- y y-min) (- y-max y-min)))]
                                     [x new-y])
                                   ;; moving along x axis, i.e. along column
                                   (let [[x-min x-max] (get board-col-bounds y)
                                         new-x (+ x-min (mod (- x x-min) (- x-max x-min)))]
                                     [new-x y]))]
                             (if (contains? wall-coords-set possible-next-coord-with-wrapping)
                               curr-coord
                               possible-next-coord-with-wrapping)))
      final-state        (reduce (fn [{:as state :keys [coords facing-direction]} next-step-or-direction]
                                   (if (string? next-step-or-direction)
                                     ;; just update facing direction
                                     (let [next-facing-direction (next-facing-direction-fn facing-direction next-step-or-direction)]
                                       (assoc state :facing-direction next-facing-direction))
                                     ;; loop for next-step-or-direction steps
                                     (let [new-coords (loop [coords-i coords
                                                             i      next-step-or-direction]
                                                        #_(println "coords:" coords)
                                                        (if (zero? i)
                                                          coords-i
                                                          (recur (next-coord-fn coords-i facing-direction)
                                                                 (dec i))))]
                                       (assoc state :coords new-coords))))
                                 {:coords           [0 (ffirst board-row-bounds)]
                                  :facing-direction :right}
                                 steps-and-dirs-seq)

      {:keys [coords facing-direction]} final-state
      row-num (inc (first coords))
      col-num (inc (second coords))
      answer-1 (+ (* 1000 row-num)
                  (* 4 col-num)
                  (facing-direction->value facing-direction))]
  answer-1)

;; answer 1: 123046

