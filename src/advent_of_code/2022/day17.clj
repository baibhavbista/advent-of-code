(ns advent-of-code.2022.day17
  "Day 17: Pyroclastic Flow https://adventofcode.com/2022/day/17"
  (:require
   [clojure.string :as str]
   [clojure.set]))


(def input-filename "inputs/2022/day17.txt")

(def jet-pattern 
  (let [char-arr (-> (slurp input-filename)
                     (str/split #""))]
    (map {">" :right
          "<" :left}
         char-arr)))


;; kinds of rocks
;; ####

;; .#.
;; ###
;; .#.

;; ..#
;; ..#
;; ###

;; #
;; #
;; #
;; #

;; ##
;; ##

(def rock-types 
  [
    [[0 0] [1 0] [2 0] [3 0]]
  
    [      [1 2]
     [0 1] [1 1] [2 1]
           [1 0]]
  
    [            [2 2]
                 [2 1]
     [0 0] [1 0] [2 0]]
  
    [[0 3]
     [0 2]
     [0 1]
     [0 0]]
  
    [[0 1] [1 1]
     [0 0] [1 0]]
  ])

;; state
;; :all-rocks-coords
;; :tower-height
;; :falling-rock-coords
;; :num-rocks-stopped 0
;; :next-jet-i


(def init-state {:all-rocks-coord-set  #{}
                 :tower-height         0
                 :num-rocks-stopped    0
                 :next-jet-i           0
                 :height-gains         []})

(defn next-jet-i-fn [jet-i]
  (mod (inc jet-i) (count jet-pattern)))

(defn new-coords [old-coords offset]
  (map (fn [coord] (map + coord offset))
       old-coords))

(defn rock-starting-coords
  [{:as _state :keys [tower-height num-rocks-stopped]}]
  (let [bottom-left-corner-coord [2 (+ tower-height 3)]
        next-rock-type-i         (mod num-rocks-stopped (count rock-types))]
    (new-coords (nth rock-types next-rock-type-i) bottom-left-corner-coord)))


(defn outside-of-bounds [[x y]]
  ;; on x axis bounds are the walls. on y axis, bound is the floor
  (or (< y 0) (< x 0) (< 6 x)))

(defn is-movement-blocked? [all-rocks-coord-set possible-next-coords-for-rock]
  (or (some outside-of-bounds possible-next-coords-for-rock)
      (some all-rocks-coord-set possible-next-coords-for-rock)))

(defn state-after-next-rock-stopped
  [{:as start-state :keys [all-rocks-coord-set height-gains tower-height num-rocks-stopped next-jet-i]}]
  (let [rock-starting-coords (rock-starting-coords start-state)
        [rock-landed-coords next-jet-i]
        (loop [rock-coords rock-starting-coords
               jet-i  next-jet-i]
          #_(println "loop" rock-coords (nth jet-pattern jet-i))
          (let [;; next coord if can go left or right
                next-coords-if-can-horizontal-movement
                (new-coords rock-coords (case (nth jet-pattern jet-i)
                                          :left  [-1 0]
                                          :right [1 0]))
                rock-coords-after-horizontal-movement
                (if (is-movement-blocked? all-rocks-coord-set next-coords-if-can-horizontal-movement)
                  rock-coords
                  next-coords-if-can-horizontal-movement)
                ;; next coord if can move down
                next-coords-if-can-move-down (new-coords rock-coords-after-horizontal-movement [0 -1])
                has-landed? (is-movement-blocked? all-rocks-coord-set next-coords-if-can-move-down)
                next-jet-i  (next-jet-i-fn jet-i)]
            (if has-landed?
              [rock-coords-after-horizontal-movement next-jet-i]
              (recur next-coords-if-can-move-down next-jet-i))))
        new-height (->> rock-landed-coords
                        (map second)
                        (map inc)
                        (apply max tower-height))]
    ;; new state
    {:all-rocks-coord-set (apply conj all-rocks-coord-set rock-landed-coords)
     :tower-height        new-height
     :height-gains        (conj height-gains (- new-height tower-height))
     :num-rocks-stopped   (inc num-rocks-stopped)
     :next-jet-i          next-jet-i}))

(time (:tower-height (reduce (fn [old-state _i]
                               (state-after-next-rock-stopped old-state))
                             init-state
                             ;; need to call it 2022 times
                             (range 2022))))
;; part 1: 3067

;; part 2
;; order of magnitude increases by 8 so no way brute force is going to cut it
;; I assume there eventually is a cycle in something like height offsets

;; do it 10000 times and get height-gains
;; then try to find cycle in it

;; for different inputs, one might have to change the 10k value below (size to calculate to check for cycle in)
;;     and maybe also the 100 in (take 100 ...) below

(def height-gains-after-10k-rocks
  (:height-gains
    (reduce (fn [old-state _i]
              (state-after-next-rock-stopped old-state))
            init-state
            (range 10000))))

(let [n-rocks 1000000000000
      my-seq height-gains-after-10k-rocks
      ;; following loop is the required portion of Floyd's tortoise and hare algo
      ;; my heuristic for when there is a cycle is if next 100 height offsets match
      [t-i h-i] 
      (loop [tortoise my-seq
             t-i      0
             hare     (rest my-seq)
             h-i      1]
        (if (= (take 100 tortoise) (take 100 hare))
          [t-i h-i]
          (recur (rest tortoise) (inc t-i) (rest (rest hare)) (+ h-i 2))))
      start-portion        (take t-i my-seq)
      repeat-portion       (->> my-seq
                                (take h-i)
                                (drop t-i))
      n-rocks-after-start  (- n-rocks (count start-portion))
      times-repeat         (quot n-rocks-after-start (count repeat-portion))
      n-rocks-after-repeat (mod n-rocks-after-start (count repeat-portion))
      remaining-portion    (take n-rocks-after-repeat repeat-portion)]
  (+ (apply + start-portion)
     (* times-repeat (apply + repeat-portion))
     (apply + remaining-portion)))

;; answer 2: 1514369501484

