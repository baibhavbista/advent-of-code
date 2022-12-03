(ns advent-of-code.2022.day03
  "https://adventofcode.com/2022/day/3"
  (:require
   [clojure.string :as str]
   [clojure.set]))

(def input-filename "inputs/2022/day03.txt")

(defn score [one-char-str]
  (let [char      (.charAt one-char-str 0)
        int-char  (int char)
        int-a     (int \a)
        int-A     (int \A)]
    (if (> int-char int-a)
      (+ 1  (- int-char int-a))
      (+ 27 (- int-char int-A)))))

(defn rucksack->failure-type [rucksack-str]
  (let [arr    (str/split rucksack-str #"")
        c      (/ (count arr) 2)
        halves (split-at c arr)]
    (first (apply clojure.set/intersection (map set halves)))))

(def rucksacks (->> (slurp input-filename)
                    (str/split-lines)))

(def part-1-answer
  (->> rucksacks
       (map (comp score rucksack->failure-type))
       (apply +)))

part-1-answer

(defn elf-group->badge-type [elf-group]
  (->> elf-group
       (map (comp set #(str/split % #"")))
       (apply clojure.set/intersection)
       first))

;; part 2
(->> rucksacks
     (partition-all 3)
     (map (comp score elf-group->badge-type))
     (apply +))
