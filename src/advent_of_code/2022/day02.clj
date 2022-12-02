(ns advent-of-code.2022.day02
  "https://adventofcode.com/2022/day/2"
  (:require
   [clojure.string :as str]))

;;;; PART 1

;; possible moves
;; other player's move: A (Rock), B (Paper), C (Scissors)
;; my moves:            X (Rock), Y (Paper), Z (Scissors)

;; just 3x3 = 9 possibilities so I could just hard code them, and that might even be the best for readability

;; HOWEVER, I want to see if I can use some ascii math here
;; we can use 0 1 2 (lets call them `move-int`s) to represent rock paper and scissors respectively (subtracting from \A or \X)
;; then shape-score = (inc my-move-int)
;; for outcome-score, 
;;    (score 6): to win, my move-int has to be one ahead of other player's move (considering it as a cycle)
;;    (score 3): draw is easy both move-ints are the same
;;    (score 0): the only other case is loss (my move-int is two steps ahead/one step behind)
;;    lets say outcome-int = 0 1 2 for loss, draw and win (so that outcome-score = outcome-int * 3)

(def input-filename "inputs/2022/day02.txt")

(def rounds (->> (slurp input-filename)
                 (str/split-lines)))

(defn first-char-offset-from [s s-base]
  (let [char      (.charAt s 0)
        char-base (.charAt s-base 0)]
    (- (int char) (int char-base))))

(defn shape-score-fn [my-move-int]
  (inc my-move-int))

(defn outcome-int-fn [their-move-int my-move-int]
  ;; 0=loss 1=draw 2=win
  (-> (- my-move-int their-move-int)
      inc
      (mod 3)))

(defn outcome-score-fn [outcome-int]
  (* outcome-int 3))

(defn round-str->part-1-score [round-str]
  (let [[their-move my-move] (str/split round-str #" ")
        their-move-int (first-char-offset-from their-move "A")
        my-move-int    (first-char-offset-from my-move "X")
        shape-score    (shape-score-fn my-move-int)
        outcome-int    (outcome-int-fn their-move-int my-move-int)
        outcome-score  (outcome-score-fn outcome-int)]
    (+ shape-score outcome-score)))

(def part-1-answer
  "Total score I get if I follow the input plan"
  (->> rounds
       (map round-str->part-1-score)
       (apply +))
  )
;; 17189


;;;; PART 2

;; oooh two is interesting
;; A B C have the same definition

;; however, now X = Lose, Y = Draw, Z = win
;; 0 1 2 (outcome-int-fn) now corresponds to char - \X
;; 0 3 6 (scores)

;; so the problem is that I have to find my move given the other's move and the outcome

(defn my-move-int-fn [their-move-int outcome-int]
  (-> (+ their-move-int outcome-int)
      dec
      (mod 3)))

;; there must be a better way of doing these modulo arithmetic 
;; and by that I mean, given an algebraic equation with modulo, transfer them to other size so as to get eqn for another var
;; pretty sure I learnt this in school at some point, but I don't remember + don't have a piece of paper handy either
;; so I'm going to make sure this function works by doing a test across all 

;; making sure my-move-int fn defined above gives correct answer for all cases
;; check with outcome-int which I'm sure works because it gave correct answer in step 1
(assert
 (every? true? (for [their-move-i (range 3)
                     my-move-i    (range 3)]
                 ;; all potential (9) cases
                 (let [outcome-int (outcome-int-fn their-move-i my-move-i)]
                   ;; checking if doing my-move-int-fn and outcome-int-fn are complementary (is that the right word?)
                   (= my-move-i (my-move-int-fn their-move-i outcome-int)))))
 "my-move-int fn does not match up with outcome-int fn. At least one is broken")

(defn round-str->part-2-score [round-str]
  (let [[their-move outcome] (str/split round-str #" ")
        their-move-int       (first-char-offset-from their-move "A")
        outcome-int          (first-char-offset-from outcome "X")
        my-move-int          (my-move-int-fn their-move-int outcome-int)
        shape-score          (shape-score-fn my-move-int) 
        outcome-score        (outcome-score-fn outcome-int)]
    (+ shape-score outcome-score)))

(def part-2-answer
  "Total score I get assuming second input is the outcome"
  (->> rounds
       (map round-str->part-2-score)
       (apply +)))
;; 13490

(defn -main []
  (println "part-1 answer:" part-1-answer)
  (println "part-2 answer:" part-2-answer))

(comment
  (-main)
  )