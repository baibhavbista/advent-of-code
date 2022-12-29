(ns advent-of-code.2022.day25
  "Day 25: Full of Hot Air https://adventofcode.com/2022/day/25"
  (:require
   [clojure.string :as str]
   [clojure.set]))

(def input-filename        "inputs/2022/day25.txt")

(def snafu-base 5)

(def snafu-digit-char->offset
  {\0  0
   \1  1
   \2  2
   \= -2
   \- -1})

(defn snafu->decimal [snafu-str]
  (reduce (fn [acc snafu-digit-char]
            (+ (* acc snafu-base) (snafu-digit-char->offset snafu-digit-char)))
          0
          snafu-str))

(defn decimal->snafu [decimal-int]
  (loop [snafu-digits      '()
         remaining-decimal decimal-int]
    (if (zero? remaining-decimal)
      (apply str snafu-digits)
      (let [m (mod remaining-decimal snafu-base)
            remaining-decimal (quot remaining-decimal snafu-base)]
        (case m
          ;; inc for first 2 cases because 1 needs to essentially be "carried up"
          3 (recur (conj snafu-digits "=") (inc remaining-decimal))
          4 (recur (conj snafu-digits "-") (inc remaining-decimal))
          (recur (conj snafu-digits (str m)) remaining-decimal))))))

(->> input-filename
     slurp
     str/split-lines
     (map snafu->decimal)
     (apply +)
     ;; got sum
     decimal->snafu)
;; answer 1: 2-0-020-1==1021=--01

;; LOL part 2 gets you a star only when you have all other 49 stars for the year

