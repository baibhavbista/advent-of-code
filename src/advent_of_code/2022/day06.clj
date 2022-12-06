(ns advent-of-code.2022.day06
  "https://adventofcode.com/2022/day/6")

(def input-filename "inputs/2022/day06.txt")

(defn first-n-in-list-unique [n list]
  (->> list
       (take n)
       distinct
       count
       (= n)))

(defn index-after-n-unique-chars [n datastream-buffer]
  ;; assumes that there is an answer
  (loop [char-list (seq datastream-buffer)
         i         n]
    ;; sanity check to see if there are enough chars, but should not come into play for the inputs for this challenge
    (when (<= n (count char-list))
      (if (first-n-in-list-unique n char-list)
        i
        (recur (rest char-list) (inc i))))))

(def start-of-packet-index (partial index-after-n-unique-chars 4))

(assert (= (start-of-packet-index "mjqjpqmgbljsphdztnvjfqwrcgsmlb") 7))
(assert (= (start-of-packet-index "bvwbjplbgvbhsrlpgdmjqwftvncz") 5))
(assert (= (start-of-packet-index "nppdvjthqldpwncqszvftbrmjlhg") 6))
(assert (= (start-of-packet-index "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") 10))
(assert (= (start-of-packet-index "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") 11))

;; part 1
(->> (slurp input-filename)
     start-of-packet-index)
;; 1850


(def start-of-message-index (partial index-after-n-unique-chars 14))

(assert (= (start-of-message-index "mjqjpqmgbljsphdztnvjfqwrcgsmlb") 19))
(assert (= (start-of-message-index "bvwbjplbgvbhsrlpgdmjqwftvncz") 23))
(assert (= (start-of-message-index "nppdvjthqldpwncqszvftbrmjlhg") 23))
(assert (= (start-of-message-index "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") 29))
(assert (= (start-of-message-index "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") 26))

;; part 2
(->> (slurp input-filename)
     start-of-message-index)
;; 2823


