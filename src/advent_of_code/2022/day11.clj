(ns advent-of-code.2022.day11
  "Day 11: Monkey in the Middle. https://adventofcode.com/2022/day/11"
  (:require
   [clojure.string :as str]))

(def input-filename "inputs/2022/day11.txt")

;; some ASSUMPTIONS need to be made about the input otherwise just the parsing and converting into
;; operations will take forever
;; 1. operation uses vars `new `and `old `and the op is either + or *
;; 2. test is always "divisible by"
;; 3. actions after test is always "throw to monkey x"

;; DATA FORMAT for each monkey

;; Assume following input 
;; Monkey 3:
;;   Starting items: 74
;;   Operation: new = old + 3
;;   Test: divisible by 17
;;     If true: throw to monkey 0
;;     If false: throw to monkey 1

;; {
;;  :monkey-num 3
;;  :holded-items-vec [74]
;;  :worry-change-when-inspect-fn (fn [old] (+ old 2))
;;  :worry-change-when-bored-fn   (fn [old] (int (/ old 3)))
;;  :throw-to-fn (fn [worry-level] (if (= (mod worry-level 17) 0) 0 1))
;;  }

(def clean-input-separated-by-monkey
  (->> (str/split (slurp input-filename) #"\n\n")
       (map (fn [str-for-one-monkey] (->> str-for-one-monkey
                                          str/split-lines
                                          (map str/trim))))))

(defn monkey-spec-strs->monkey-map [monkey-spec-strs]
  (let [[monkey-num-str starting-items-str operation-str test-str test-if-true-str test-if-false-str] monkey-spec-strs
        monkey-num (->> (re-matches #"Monkey (\d+):" monkey-num-str)
                        second
                        Integer/parseInt)
        starting-items (->> starting-items-str
                            (re-matches #"Starting items: ([,\d\w\s]+)")
                            second
                            (#(str/split % #"[,]"))
                            (map str/trim)
                            (mapv #(Integer/parseInt %)))
        worry-change-when-inspect-fn (let [[_ operand-a operator operand-b] (re-matches #"Operation: new = ([\d\w]+) (.) ([\d\w]+)" operation-str)]
                                       (fn [old]
                                         (eval (read-string (str "(let [old " old "] (" operator " " operand-a " " operand-b "))")))))
        test-mod-by-num (->> test-str
                             (re-matches #"Test: divisible by ([\d]+)")
                             second
                             Integer/parseInt)
        test-if-true-monkey-num (->> test-if-true-str
                                     (re-matches #"If true: throw to monkey ([\d]+)")
                                     second
                                     Integer/parseInt)
        test-if-false-monkey-num (->> test-if-false-str
                                      (re-matches #"If false: throw to monkey ([\d]+)")
                                      second
                                      Integer/parseInt)
        throw-to-fn (fn [worry-level] (if (= (mod worry-level test-mod-by-num) 0) test-if-true-monkey-num test-if-false-monkey-num))]
    {:monkey-num monkey-num
     :inspected-cnt 0
     :holded-items-vec starting-items
     :test-mod-base    test-mod-by-num
     :worry-change-when-inspect-fn worry-change-when-inspect-fn
     :worry-change-when-bored-fn (fn [old] (int (/ old 3))) ;; same for every monkey
     :throw-to-fn throw-to-fn}))

(def init-state
  (->> clean-input-separated-by-monkey
       (map monkey-spec-strs->monkey-map)
       (map (juxt :monkey-num identity))
       (into {})))

init-state

(defn handle-one-monkey-turn-in-cycle [start-state monkey-num]
  (let [{:keys [worry-change-when-inspect-fn worry-change-when-bored-fn throw-to-fn]} (get start-state monkey-num)]
    (loop [state start-state]
      (if-let [item-to-handle (first (get-in state [monkey-num :holded-items-vec]))]
        (let [#_#__ (println "item-to-handle:" item-to-handle)
              worry-level (->> item-to-handle
                               worry-change-when-inspect-fn
                               worry-change-when-bored-fn)
              throw-to-num (throw-to-fn worry-level)]
          (recur (-> state
                     (update-in [monkey-num :holded-items-vec] (comp vec rest))
                     (update-in [monkey-num :inspected-cnt] inc)
                     (update-in [throw-to-num :holded-items-vec] conj worry-level))))
        state))))

(defn cycle [start-state]
  (reduce handle-one-monkey-turn-in-cycle
          start-state
          (range (count start-state))))

(let [n-cycles 20
      final-state (loop [state init-state
                         i 0]
                    (if (< i n-cycles)
                      (recur (cycle state) (inc i))
                      state))]
  (->> final-state
       vals
       (map :inspected-cnt)
       sort
       reverse
       (take 2)
       (apply *)))

;; answer 1: 57838


;; PART 2

#_(def state-with-no-relief-when-bored
  (->> init-state
       (map (fn [[i monkey-map]]
              [i (assoc monkey-map :worry-change-when-bored-fn identity)]))
       (into {})))

;; tried naive and it didn't work
;; ooohhh so the issue is that we run into integer overflow due to the large numbers 
;; (because we are not dividing by 3, it always increases)
;; that's what they meant by "ridiculous levels" in the question

;; {{[[TODO]]}} need to find another way to keep worry levels manageable
;; fuck I will HAVE to look into modular arithmetic now, won't I?

;; okay I took a brief look into it and found out that modular addition, subtraction and multiplication are 
;; consistent with normal arithmetic
;; however, the problem is that different monkey's have different "base" of modulus

;; not completely sure if the math checks out but one hypothesis is that we can use the 
;; LCM (Least Common Multiple) of all the modulus bases. That makes sense from the context of treating modulus
;; as a circle, since if we do LCM mod any-base, it would always give 0

;; verification
;; I think checking for the code given in the example will be faster than mathematically verifying it
;; LCM(23,19,13,17) = 96577
;; so, I should be able to just do mod 96577 before transferring the worry level to another monkey or even before doing the check
;; oh niceee looks like that worked

(defn gcd [x y]
  (loop [x x
         y y]
    (if (= y 0) 
      x 
      (recur y (mod x y)))))

(defn lcm 
  ([x y]
   (/ (* x y) (gcd x y)))
  ([x y & rest]
   (reduce lcm
           (lcm x y)
           rest)))

(assert (= (lcm 23 19 13 17) 96577))

(def state-with-no-relief-when-bored
  (let [lcm-of-mods (->> init-state
                         vals
                         (map :test-mod-base)
                         (apply lcm))]
    (->> init-state
         (map (fn [[i monkey-map]]
                [i (assoc monkey-map :worry-change-when-bored-fn #(mod % lcm-of-mods))]))
         (into {}))))

(let [n-cycles 10000
      final-state (loop [state state-with-no-relief-when-bored
                         i 0]
                    (if (< i n-cycles)
                      (recur (cycle state) (inc i))
                      state))]
  (->> final-state
       vals
       (map :inspected-cnt)
       sort
       reverse
       (take 2)
       (apply *)))

;; answer 2: 15050382231