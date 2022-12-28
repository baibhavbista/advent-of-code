(ns advent-of-code.2022.day23
  "Day 23: Unstable Diffusion https://adventofcode.com/2022/day/23"
  (:require
   [clojure.string :as str]
   [clojure.set]))

(def sample-input-filename "inputs/2022/day23-sample.txt")
(def input-filename        "inputs/2022/day23.txt")

;; got this from @cgrand's stack overflow answer https://stackoverflow.com/a/4831170
(defn find-thing [needle haystack]
  (keep-indexed #(when (= %2 needle) %1) haystack))

;; x axis is down, y axis is right

(def starting-positions 
  (->> (slurp input-filename)
       str/split-lines
       (map #(find-thing \# %))
       (map-indexed (fn [row-i col-is]
                      (map #(vector row-i %) col-is)))
       (apply concat)
       (into #{})))

(def possible-proposed-steps-in-order
  [{:offsets-to-check  [[-1 0] [-1 -1] [-1 1]]
    :offset-to-propose [-1 0]}
   {:offsets-to-check  [[1 0] [1 -1] [1 1]]
    :offset-to-propose [1 0]}
   {:offsets-to-check  [[0 -1] [1 -1] [-1 -1]]
    :offset-to-propose [0 -1]}
   {:offsets-to-check  [[0 1] [1 1] [-1 1]]
    :offset-to-propose [0 1]}])

(def all-surrounding-offsets
  [[-1 0] [-1 -1] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn pos+offset [[pos-x pos-y] [offset-x offset-y]]
  [(+ pos-x offset-x) (+ pos-y offset-y)])

(defn propose-step-to-try-in-round-i [i]
  (let [i-norm (mod i (count possible-proposed-steps-in-order))]
    (concat (drop i-norm possible-proposed-steps-in-order)
            (take i-norm possible-proposed-steps-in-order))))

(propose-step-to-try-in-round-i 2)

(defn proposed-step-for-elf [elf-positions-set steps-to-try elf-pos]
  (let [surrounding-positions (map (partial pos+offset elf-pos) all-surrounding-offsets)
        surr-positions-free?  (every? #(not (contains? elf-positions-set %)) surrounding-positions)]
    (if surr-positions-free?
      elf-pos
      (loop [steps-to-try steps-to-try]
        (when-let [step-to-try (first steps-to-try)]
          (let [positions-to-check (map (partial pos+offset elf-pos) (:offsets-to-check step-to-try))
                positions-free?    (every? #(not (contains? elf-positions-set %)) positions-to-check)]
            (if positions-free?
              (pos+offset elf-pos (:offset-to-propose step-to-try))
              (recur (rest steps-to-try)))))))))

(defn simulate-round [elf-positions-set round-i]
  #_(println "simulate-round" elf-positions-set round-i)
  (let [steps (propose-step-to-try-in-round-i round-i)
        curr-pos+proposed-new-pos
        (map #(vector % (proposed-step-for-elf elf-positions-set steps %)) elf-positions-set)
        freq-map-of-proposed-new-pos (frequencies (->> curr-pos+proposed-new-pos
                                                       (map second)
                                                       (remove nil?)))]
    (->> (for [[curr-pos proposed-new-pos] curr-pos+proposed-new-pos]
           (if-not proposed-new-pos
             curr-pos
             (if (< 1 (freq-map-of-proposed-new-pos proposed-new-pos))
               curr-pos
               proposed-new-pos)))
         (into #{}))))

(defn empty-tiles-in-smallest-rectangle-that-contains-each-elf [elf-positions]
  (let [[x-max y-max] (apply map max elf-positions)
        [x-min y-min] (apply map min elf-positions)
        #_#__ (println "size:" (inc (- x-max x-min)) (inc (- y-max y-min)))
        total-number-of-squares (* (inc (- x-max x-min)) (inc (- y-max y-min)))]
    (- total-number-of-squares (count elf-positions))))

#_#_(let [elf-positions (reduce simulate-round
                            starting-positions
                            (range 10))]
  )

(reduce simulate-round
        starting-positions
        (range 10))

(->> (loop [iter (range 10)
            curr-positions starting-positions]
       (if-let [i (first iter)]
         (let [next-positions (simulate-round curr-positions i)]
           (if (= next-positions curr-positions)
             curr-positions
             (recur (rest iter) next-positions)))
         curr-positions))
     empty-tiles-in-smallest-rectangle-that-contains-each-elf)
;; answer 1: 4336

(let [[_final-positions rounds-simulated] 
      (loop [iter (range)
             curr-positions starting-positions]
        (let [i (first iter)]
          (let [next-positions (simulate-round curr-positions i)]
            (if (= next-positions curr-positions)
              [curr-positions (inc i)]
              (recur (rest iter) next-positions)))))]
  rounds-simulated)
;; answer 2: 1005

