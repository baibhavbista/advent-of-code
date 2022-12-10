(ns advent-of-code.2022.day10
  "Cathode Ray Tube: https://adventofcode.com/2022/day/10"
  (:require
   [clojure.string :as str]))

(def input-filename "inputs/2022/day10.txt")

;; Input
;; noop
;; addx 3
;; addx -5

;; internal format: [n x] where n corresponds to cycle number and x corresponds to value of register X starting from that n
;; for every [n1 x1] [n2 x2], it means that for every n1 <= n < n2, value of signal (x) = z1
;; list of changes then would be '([1 1] [2 1] [4 4] [6 -1])


(defn cmd-str->int-format-offset [cmd-str]
  (if (= cmd-str "noop")
    [1 0]
    ;; else addx
    (let [[_addx-cmd num-str] (str/split cmd-str #" ")
          num (Integer/parseInt num-str)]
      [2 num])))

(assert (= (cmd-str->int-format-offset "noop") [1 0]))
(assert (= (cmd-str->int-format-offset "addx -5") [2 -5]))

(defn new-val [old-val offset]
  (mapv + old-val offset))

(def list-changes
  (let [starting-state [1 1]
        cmd-lines      (->> (slurp input-filename)
                            (str/split-lines))]
    (->> cmd-lines
         (reduce (fn [change-list-reverse cmd-str]
                   (let [last-val (first change-list-reverse)
                         new-val  (new-val last-val (cmd-str->int-format-offset cmd-str))]
                     (conj change-list-reverse new-val)))
                 (list starting-state))
         reverse)))

(defn val-at-cycle-i [list-changes i]
  (->> list-changes
       (take-while (fn [[n _]] (<= n i)))
       last
       second))

(def interesting-cycles [20 60 100 140 180 220])

(->> interesting-cycles
     (map (partial val-at-cycle-i list-changes))
     (map * interesting-cycles)
     (apply +))
;; answer 1: 11720


;;;; part 2

;; vals for X for every cycle starting from cycle 1
(def x-vals-list
  (let [list-changes-with-fake-final (concat list-changes [[241 0]])]
    (:x-vals-list (reduce (fn [{:keys [x-vals-list last-val-change]} this-val-change]
                            (let [[n1 x1]     last-val-change
                                  [n2 x2]     this-val-change
                                  vals-to-add (take (- n2 n1) (repeat x1))]
                              {:x-vals-list     (concat x-vals-list vals-to-add)
                               :last-val-change this-val-change}))
                          {:x-vals-list     '()
                           :last-val-change (first list-changes-with-fake-final)}
                          (rest list-changes-with-fake-final)))))

(->> x-vals-list
     (map-indexed vector)
     (reduce (fn [acc [cycle-num-minus-1 x]]
               (let [next-addition (if (<= (dec x) (mod cycle-num-minus-1 40) (inc x)) "#" ".")]
                 (conj acc next-addition)))
             [])
     (partition-all 40)
     (map #(apply str %)))

;; OUTPUT

;; ####.###...##..###..####.###...##....##.
;; #....#..#.#..#.#..#.#....#..#.#..#....#.
;; ###..#..#.#....#..#.###..#..#.#.......#.
;; #....###..#....###..#....###..#.......#.
;; #....#.#..#..#.#.#..#....#....#..#.#..#.
;; ####.#..#..##..#..#.####.#.....##...##..

;; answer part 2: What eight capital letters appear on your CRT?:
;;   ERCREPCJ