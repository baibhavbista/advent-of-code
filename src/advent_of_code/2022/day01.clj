(ns advent-of-code.2022.day01
  "https://adventofcode.com/2022/day/1"
  (:require
   [clojure.string :as str]))

(def input-filename "inputs/2022/day01.txt")

(def total-calories-carried-by-each-elf
  (->> (slurp input-filename)
       (str/split-lines)
       (partition-by str/blank?)
       (remove #(= % '("")))
       ;; we now have (("4887" "9307") ("8895" "8136" "6292" "10177" "4077" "8228") ...)
       (map (fn [list-cal-strs]
              (->> (map #(Integer/parseInt %) list-cal-strs)
                   (reduce +))))))

(def part-1-answer
  "Maximum number of calories carried by any elf"
  (->> total-calories-carried-by-each-elf
       (reduce max)))

(def part-2-answer
  "Sum of calories carried by the 3 elves carrying the 3 highest number of calories"
  (->> total-calories-carried-by-each-elf
       sort
       (take-last 3)
       (reduce +)))

(defn -main []
  (println "part-1 answer:" part-1-answer)
  (println "part-2 answer:" part-2-answer))

(comment
  (-main)
  )