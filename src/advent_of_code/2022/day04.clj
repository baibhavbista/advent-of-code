(ns advent-of-code.2022.day04
  "https://adventofcode.com/2022/day/4"
  (:require
   [clojure.string :as str]))

(def input-filename "inputs/2022/day04.txt")

(defn section-assignment-pair-str->vec [section-assignment-pair-str]
  (->> section-assignment-pair-str
       (#(str/split % #"[,-]"))
       (map #(Integer/parseInt %))))

(def section-assignment-pair-vecs
  (->> (slurp input-filename)
       (str/split-lines)
       (map section-assignment-pair-str->vec)))

;; NAMING: lets call the section assignment boundary ints a b c d

(defn one-is-fully-contained-in-another? [[a b c d]]
  ;; trivially included if they share start/end positions (if c = a or b = d)
  ;; other cases are a < c <= d < b 
  ;;             and c < a <= b < d
  ;;             note the <= in the middle (an assignment can contain only 1 item)
  (or (= a c)
      (= b d)
      (and (< a c) (< d b))
      (and (< c a) (< b d))))

(def answer-1
  (->> section-assignment-pair-vecs
       (map one-is-fully-contained-in-another?)
       (filter true?)
       count))
;; answer 1: 605

(defn do-they-overlap? [[a b c d]]
  ;; swap if required to make it such that a <= c. 
  ;; then we can just check if second pair starts on or before first pair ends i.e. c <= b
  (let [[a b c d] (if (< c a) [c d a b] [a b c d])]
    (<= c b)))

(def answer-2
  (->> section-assignment-pair-vecs
       (map do-they-overlap?)
       (filter boolean)
       count))
;; answer 2: 914


[answer-1 answer-2]





