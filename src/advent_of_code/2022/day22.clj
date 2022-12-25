(ns advent-of-code.2022.day22
  "Day 22: Monkey Map https://adventofcode.com/2022/day/22"
  (:require
   [clojure.string :as str]
   [clojure.set]))

(def sample-input-filename "inputs/2022/day22-sample.txt")
(def input-filename        "inputs/2022/day22.txt")


;; META: in retrospect, part 2 probably should have been done by constructing a graph (that would give a general solution), but I don't have time to do that right now
;; my solution is dependent on the 2D format in which cube is given, so, sample input and test input have different functions (sample input's is commented out)

;; rows and cols are 0-indexed in code and converted to 1-indexed at the last possible time

;; x axis is vertical, y axis is horizontal


;; got this from @cgrand's stack overflow answer https://stackoverflow.com/a/4831170
(defn find-thing [needle haystack]
  (keep-indexed #(when (= %2 needle) %1) haystack))

(defn transpose
  "essentially swaps rows and columns"
  [grid] (apply map list grid))

(defn normalize-str-by-adding-blank-space-to-end [s required-length]
  (let [curr-length (count s)
        blank-spaces-at-end (- required-length curr-length)]
    (vec (concat (seq s) (take blank-spaces-at-end (repeat \space) )))))

(defn print-char-grid [char-grid]
  (println (->> char-grid
                (map #(apply str %))
                (reduce (fn [acc-s s]
                          (str acc-s "\n" s))))))

(def facing-direction->movement-offset
  {:left  [0 -1]
   :right [0  1]
   :up    [-1 0]
   :down  [1  0]})

;; the following 3 defs could be replaced by an ordered list and some modulo operations, but I'm sick of module atm
(def clockwise-rotation-facing-dir-change
  {:left :up
   :up :right
   :right :down
   :down :left})

(def anti-clockwise-rotation-facing-dir-change (clojure.set/map-invert clockwise-rotation-facing-dir-change))

(def facing-direction->value
  {:right 0
   :down 1
   :left 2
   :up 3})


(defn next-facing-direction-fn 
  "turn-str can be R for clockwise or L for anti clockwise"
  [curr-facing-direction turn-str]
  #_(println "next-facing-direction-fn: " curr-facing-direction turn-str)
  (let [curr->next-facing-dir (if (= turn-str "R")
                                clockwise-rotation-facing-dir-change
                                anti-clockwise-rotation-facing-dir-change)]
    (get curr->next-facing-dir curr-facing-direction)))


;;;; TEST INPUT
;; SHIT I DID NOT CHECK TEST INPUT PROPERLY .. FORMAT OF CUBE IS DIFFERENT from sample input

(let [[board-str steps-and-dirs-str]     (-> (slurp input-filename)
                                             (str/split #"\n\n"))
      length-cube        50
      steps-and-dirs-seq (->> steps-and-dirs-str
                              (partition-by #{\R \L})
                              (map #(apply str %))
                              (map (fn [s]
                                     (if (or (= s "R") (= s "L"))
                                       s
                                       (Long/parseLong s)))))
      board-row-strs                 (str/split-lines board-str)
      board-char-grid                (let [max-len (apply max (map count board-row-strs))]
                                       (mapv #(normalize-str-by-adding-blank-space-to-end % max-len) board-row-strs))
      wall-coords-set     (->> board-row-strs
                               (map #(vec (find-thing \# %)))
                               (map-indexed (fn [row cols-with-wall]
                                              (map #(vector row %) cols-with-wall)))
                               (apply concat)
                               (into #{}))

      ;; hardcoded based on cube 2d pattern
      face-top-left-points
      {1 [0
          (* 1 length-cube)]
       2 [0
          (* 2 length-cube)]
       3 [length-cube
          length-cube]
       4 [(* 2 length-cube)
          0]
       5 [(* 2 length-cube)
          length-cube]
       6 [(* 3 length-cube)
          0]}


      ;; PART 1
      next-coord-fn-1      (let [;; bound inclusive of start, exclusive of end
                                 board-row-bounds-fn (fn [board-row-char-vec]
                                                       (let [start-index  (count (take-while #(= % \space) board-row-char-vec))
                                                             rem-char-vec (drop start-index board-row-char-vec)
                                                             actual-data-vec (take-while #(not= % \space) rem-char-vec)
                                                             end-index   (+ start-index (count actual-data-vec))]
                                                         [start-index end-index]))
                                 board-row-bounds    (mapv board-row-bounds-fn board-char-grid)
                                 board-col-bounds    (mapv board-row-bounds-fn (transpose board-char-grid))]
                             (fn next-coord [curr-coord facing-dir]
                               (let [offset (facing-direction->movement-offset facing-dir)
                                     [x y] (mapv + curr-coord offset)
                                     #_#__ (println "[x y]:" [x y])
                                     possible-next-coord-with-wrapping
                                     (if (facing-dir #{:left :right})
                                    ;; moving in y axis, i.e. along row
                                       (let [[y-min y-max] (get board-row-bounds x)
                                             new-y (+ y-min (mod (- y y-min) (- y-max y-min)))]
                                         [x new-y])
                                   ;; moving along x axis, i.e. along column
                                       (let [[x-min x-max] (get board-col-bounds y)
                                             new-x (+ x-min (mod (- x x-min) (- x-max x-min)))]
                                         [new-x y]))]
                                 (if (contains? wall-coords-set possible-next-coord-with-wrapping)
                                   curr-coord
                                   possible-next-coord-with-wrapping))))
      answer-1 (let [final-state        (reduce (fn [{:as state :keys [coords facing-direction]} next-step-or-direction]
                                                  (if (string? next-step-or-direction)
                                     ;; just update facing direction
                                                    (let [next-facing-direction (next-facing-direction-fn facing-direction next-step-or-direction)]
                                                      (assoc state :facing-direction next-facing-direction))
                                     ;; loop for next-step-or-direction steps
                                                    (let [new-coords (loop [coords-i coords
                                                                            i      next-step-or-direction]
                                                                       #_(println "coords:" coords)
                                                                       (if (zero? i)
                                                                         coords-i
                                                                         (recur (next-coord-fn-1 coords-i facing-direction)
                                                                                (dec i))))]
                                                      (assoc state :coords new-coords))))
                                                {:coords           (face-top-left-points 1)
                                                 :facing-direction :right}
                                                steps-and-dirs-seq)
                     {:keys [coords facing-direction]} final-state
                     row-num (inc (first coords))
                     col-num (inc (second coords))]
                 (+ (* 1000 row-num)
                    (* 4 col-num)
                    (facing-direction->value facing-direction)))

      ;; PART 2
      global-coord->face-coords-if-within-face
      (fn [global-coord face-i]
        (let [[x y] global-coord
              [x-min y-min] (face-top-left-points face-i)
              [x-max y-max] [(+ x-min length-cube) (+ y-min length-cube)]]
          (when (and (<= x-min x) (< x x-max) (<= y-min y) (< y y-max))
            [(- x x-min) (- y y-min)])))
      global-coords->face-i+coords
      (fn [global-coords]
        (let [[face-i-0-idxd local-coords]
              (->> (range 1 7) ; cube always has 6 faces
                   (map (partial global-coord->face-coords-if-within-face global-coords))
                   (map-indexed vector)
                   (remove (comp nil? second))
                   first)]
          [(inc face-i-0-idxd)
           local-coords]))
      face-i+coords->global-coords
      (fn [[face-i local-coords]]
        (let [global-coords-top-left-point-in-face (get face-top-left-points face-i)]
          (map + global-coords-top-left-point-in-face local-coords)))
      in-some-surface? (fn [global-coord]
                         (when-let [c (get-in board-char-grid global-coord)]
                           (not= \space c)))

      ;; the crux of the solution

      max-local-coord (dec length-cube)

      condition->local-coord-action
      {:same-direction-vert               {:deduct-x-from max-local-coord}
       :same-direction-hori               {:deduct-y-from max-local-coord}
       :orientation-change                {:deduct-x-from max-local-coord
                                           :deduct-y-from max-local-coord
                                           :swap-x-and-y  true}
       :dirn-of-vert-axis-change          {:deduct-y-from max-local-coord}
       :dirn-of-hori-axis-change          {:deduct-x-from max-local-coord}
       :both-dirn-axis-orientation-change {:swap-x-and-y true}}

      ;; hardcoded based on cube 2d pattern
      exiting-face+dirn->new-face+dirn+local-coord-action
      {[6 :right] [5 :up    (:both-dirn-axis-orientation-change condition->local-coord-action)]
       [5 :down]  [6 :left  (:both-dirn-axis-orientation-change condition->local-coord-action)]
       [5 :right] [2 :left  (:dirn-of-hori-axis-change          condition->local-coord-action)]
       [2 :right] [5 :left  (:dirn-of-hori-axis-change          condition->local-coord-action)]
       [4 :up]    [3 :right (:both-dirn-axis-orientation-change condition->local-coord-action)]
       [3 :left]  [4 :down  (:both-dirn-axis-orientation-change condition->local-coord-action)]
       [4 :left]  [1 :right (:dirn-of-hori-axis-change          condition->local-coord-action)]
       [1 :left]  [4 :right (:dirn-of-hori-axis-change          condition->local-coord-action)]
       [6 :left]  [1 :down  (:both-dirn-axis-orientation-change condition->local-coord-action)]
       [1 :up]    [6 :right (:both-dirn-axis-orientation-change condition->local-coord-action)]
       [6 :down]  [2 :down  (:same-direction-vert               condition->local-coord-action)]
       [2 :up]    [6 :up    (:same-direction-vert               condition->local-coord-action)]
       [3 :right] [2 :up    (:both-dirn-axis-orientation-change condition->local-coord-action)]
       [2 :down]  [3 :left  (:both-dirn-axis-orientation-change condition->local-coord-action)]}

      coord-deduct-x-from-fn
      (fn [[coord-x coord-y] from-value]
        [(- from-value coord-x) coord-y])

      coord-deduct-y-from-fn
      (fn [[coord-x coord-y] from-value]
        [coord-x (- from-value coord-y)])

      coord-swap-x-and-y-fn
      (fn [[coord-x coord-y]]
        [coord-y coord-x])



      next-coord+dirn-fn-2 (let []
                             (fn [curr-coord curr-facing-dir]
                               (let [offset (facing-direction->movement-offset curr-facing-dir)
                                     possible-next-coord (mapv + curr-coord offset)
                                     #_#__ (println "possible-next-coord:" possible-next-coord)
                                     [next-coord next-facing-dir]
                                     (if (in-some-surface? possible-next-coord)
                                       ;; inside a face / in a directly connect face (directly connected in our 2d grid)
                                       [possible-next-coord curr-facing-dir]
                                       ;; else, facing direction will also need to change
                                       (let [[curr-face curr-local-coords] (global-coords->face-i+coords curr-coord)
                                             [new-face new-dirn local-coord-action]
                                             (exiting-face+dirn->new-face+dirn+local-coord-action [curr-face curr-facing-dir])
                                             new-local-coords (cond-> curr-local-coords
                                                                (:deduct-x-from local-coord-action)
                                                                (coord-deduct-x-from-fn (:deduct-x-from local-coord-action))
                                                                (:deduct-y-from local-coord-action)
                                                                (coord-deduct-y-from-fn (:deduct-y-from local-coord-action))
                                                                (:swap-x-and-y local-coord-action)
                                                                coord-swap-x-and-y-fn)
                                             new-global-coords (face-i+coords->global-coords [new-face new-local-coords])]
                                         [new-global-coords new-dirn]))]
                                 (if (contains? wall-coords-set next-coord)
                                   [curr-coord curr-facing-dir]
                                   [next-coord next-facing-dir]))))
      answer-2 (let [final-state        (reduce (fn [{:as state :keys [coords facing-direction]} next-step-or-direction]
                                                  (if (string? next-step-or-direction)
                                                    ;; just update facing direction
                                                    (let [next-facing-direction (next-facing-direction-fn facing-direction next-step-or-direction)]
                                                      (assoc state :facing-direction next-facing-direction))
                                                    ;; loop for next-step-or-direction steps
                                                    (let [[new-coords new-dirn]
                                                          (loop [coords-i coords
                                                                 dirn-i   facing-direction
                                                                 i        next-step-or-direction]
                                                            #_(println "coords:" coords-i dirn-i)
                                                            (if (zero? i)
                                                              [coords-i dirn-i]
                                                              (let [[next-coord next-dirn] (next-coord+dirn-fn-2 coords-i dirn-i)]
                                                                (recur next-coord
                                                                       next-dirn
                                                                       (dec i)))))]
                                                      (assoc state :coords new-coords
                                                             :facing-direction new-dirn))))
                                                {:coords           (face-top-left-points 1)
                                                 :facing-direction :right}
                                                steps-and-dirs-seq)
                     {:keys [coords facing-direction]} final-state
                     row-num (inc (first coords))
                     col-num (inc (second coords))]
                 (+ (* 1000 row-num)
                    (* 4 col-num)
                    (facing-direction->value facing-direction)))]
  [answer-1 answer-2])
;; answers: [123046 195032]



;;;; SAMPLE INPUT
#_(let [[board-str steps-and-dirs-str]     (-> (slurp sample-input-filename)
                                             (str/split #"\n\n"))
      length-cube        4
      steps-and-dirs-seq (->> steps-and-dirs-str
                              (partition-by #{\R \L})
                              (map #(apply str %))
                              (map (fn [s]
                                     (if (or (= s "R") (= s "L"))
                                       s
                                       (Long/parseLong s)))))
      board-row-strs                 (str/split-lines board-str)
      board-char-grid                (let [max-len (apply max (map count board-row-strs))]
                                       (mapv #(normalize-str-by-adding-blank-space-to-end % max-len) board-row-strs))
      wall-coords-set     (->> board-row-strs
                               (map #(vec (find-thing \# %)))
                               (map-indexed (fn [row cols-with-wall]
                                              (map #(vector row %) cols-with-wall)))
                               (apply concat)
                               (into #{}))

      ;; hardcoded based on cube 2d pattern
      face-top-left-points
      {1 [0
          (* 2 length-cube)]
       2 [length-cube
          0]
       3 [length-cube
          length-cube]
       4 [length-cube
          (* 2 length-cube)]
       5 [(* 2 length-cube)
          (* 2 length-cube)]
       6 [(* 2 length-cube)
          (* 3 length-cube)]}


      ;; PART 1
      next-coord-fn-1      (let [;; bound inclusive of start, exclusive of end
                                 board-row-bounds-fn (fn [board-row-char-vec]
                                                       (let [start-index  (count (take-while #(= % \space) board-row-char-vec))
                                                             rem-char-vec (drop start-index board-row-char-vec)
                                                             actual-data-vec (take-while #(not= % \space) rem-char-vec)
                                                             end-index   (+ start-index (count actual-data-vec))]
                                                         [start-index end-index]))
                                 board-row-bounds    (mapv board-row-bounds-fn board-char-grid)
                                 board-col-bounds    (mapv board-row-bounds-fn (transpose board-char-grid))]
                             (fn next-coord [curr-coord facing-dir]
                               (let [offset (facing-direction->movement-offset facing-dir)
                                     [x y] (mapv + curr-coord offset)
                                     #_#__ (println "[x y]:" [x y])
                                     possible-next-coord-with-wrapping
                                     (if (facing-dir #{:left :right})
                                    ;; moving in y axis, i.e. along row
                                       (let [[y-min y-max] (get board-row-bounds x)
                                             new-y (+ y-min (mod (- y y-min) (- y-max y-min)))]
                                         [x new-y])
                                   ;; moving along x axis, i.e. along column
                                       (let [[x-min x-max] (get board-col-bounds y)
                                             new-x (+ x-min (mod (- x x-min) (- x-max x-min)))]
                                         [new-x y]))]
                                 (if (contains? wall-coords-set possible-next-coord-with-wrapping)
                                   curr-coord
                                   possible-next-coord-with-wrapping))))
      answer-1 (let [final-state        (reduce (fn [{:as state :keys [coords facing-direction]} next-step-or-direction]
                                                  (if (string? next-step-or-direction)
                                     ;; just update facing direction
                                                    (let [next-facing-direction (next-facing-direction-fn facing-direction next-step-or-direction)]
                                                      (assoc state :facing-direction next-facing-direction))
                                     ;; loop for next-step-or-direction steps
                                                    (let [new-coords (loop [coords-i coords
                                                                            i      next-step-or-direction]
                                                                       #_(println "coords:" coords)
                                                                       (if (zero? i)
                                                                         coords-i
                                                                         (recur (next-coord-fn-1 coords-i facing-direction)
                                                                                (dec i))))]
                                                      (assoc state :coords new-coords))))
                                                {:coords           (face-top-left-points 1)
                                                 :facing-direction :right}
                                                steps-and-dirs-seq)
                     {:keys [coords facing-direction]} final-state
                     row-num (inc (first coords))
                     col-num (inc (second coords))]
                 (+ (* 1000 row-num)
                    (* 4 col-num)
                    (facing-direction->value facing-direction)))

      ;; PART 2
      ;; following can also be used to check if we're outside of a 
      global-coord->face-coords-if-within-face
      (fn [global-coord face-i]
        (let [[x y] global-coord
              [x-min y-min] (face-top-left-points face-i)
              [x-max y-max] [(+ x-min length-cube) (+ y-min length-cube)]]
          (when (and (<= x-min x) (< x x-max) (<= y-min y) (< y y-max))
            [(- x x-min) (- y y-min)])))
      global-coords->face-i+coords
      (fn [global-coords]
        (let [[face-i-0-idxd local-coords]
              (->> (range 1 7)
                   (map (partial global-coord->face-coords-if-within-face global-coords))
                   (map-indexed vector)
                   (remove (comp nil? second))
                   first)]
          [(inc face-i-0-idxd)
           local-coords]))
      face-i+coords->global-coords
      (fn [[face-i local-coords]]
        (let [global-coords-top-left-point-in-face (get face-top-left-points face-i)]
          (map + global-coords-top-left-point-in-face local-coords)))
      in-some-surface? (fn [global-coord]
                         (when-let [c (get-in board-char-grid global-coord)]
                           (not= \space c)))

      ;; the crux of the solution

      max-local-coord (dec length-cube)

      condition->local-coord-action
      {:same-direction-vert               {:deduct-x-from max-local-coord}
       :same-direction-hori               {:deduct-y-from max-local-coord}
       :orientation-change                {:deduct-x-from max-local-coord
                                           :deduct-y-from max-local-coord
                                           :swap-x-and-y  true}
       :dirn-of-vert-axis-change          {:deduct-y-from max-local-coord}
       :dirn-of-hori-axis-change          {:deduct-x-from max-local-coord}
       :both-dirn-axis-orientation-change {:swap-x-and-y true}}

      ;; hardcoded based on cube 2d pattern
      exiting-face+dirn->new-face+dirn+local-coord-action
      {[1 :left]  [3 :down  (:both-dirn-axis-orientation-change condition->local-coord-action)]
       [3 :up]    [1 :right (:both-dirn-axis-orientation-change condition->local-coord-action)]
       [1 :up]    [2 :down  (:dirn-of-vert-axis-change          condition->local-coord-action)]
       [2 :up]    [1 :down  (:dirn-of-vert-axis-change          condition->local-coord-action)]
       [1 :right] [6 :left  (:dirn-of-hori-axis-change          condition->local-coord-action)]
       [6 :right] [1 :left  (:dirn-of-hori-axis-change          condition->local-coord-action)]
       [2 :down]  [5 :up    (:dirn-of-vert-axis-change          condition->local-coord-action)]
       [5 :down]  [2 :up    (:dirn-of-vert-axis-change          condition->local-coord-action)]
       [3 :down]  [5 :right (:orientation-change                condition->local-coord-action)]
       [5 :left]  [3 :up    (:orientation-change                condition->local-coord-action)]
       [4 :right] [6 :down  (:orientation-change                condition->local-coord-action)]
       [6 :up]    [4 :left  (:orientation-change                condition->local-coord-action)]
       [2 :left]  [6 :up    (:orientation-change                condition->local-coord-action)]
       [6 :down]  [2 :right (:orientation-change                condition->local-coord-action)]}

      coord-deduct-x-from-fn
      (fn [[coord-x coord-y] from-value]
        [(- from-value coord-x) coord-y])

      coord-deduct-y-from-fn
      (fn [[coord-x coord-y] from-value]
        [coord-x (- from-value coord-y)])

      coord-swap-x-and-y-fn
      (fn [[coord-x coord-y]]
        [coord-y coord-x])



      next-coord+dirn-fn-2 (let []
                             (fn [curr-coord curr-facing-dir]
                               (let [offset (facing-direction->movement-offset curr-facing-dir)
                                     possible-next-coord (mapv + curr-coord offset)
                                     #_#__ (println "possible-next-coord:" possible-next-coord)
                                     [next-coord next-facing-dir]
                                     (if (in-some-surface? possible-next-coord)
                                       ;; inside a face / in a directly connect face (directly connected in our 2d grid)
                                       [possible-next-coord curr-facing-dir]
                                       ;; else, facing direction will also need to change
                                       (let [[curr-face curr-local-coords] (global-coords->face-i+coords curr-coord)
                                             [new-face new-dirn local-coord-action]
                                             (exiting-face+dirn->new-face+dirn+local-coord-action [curr-face curr-facing-dir])
                                             new-local-coords (cond-> curr-local-coords
                                                                (:deduct-x-from local-coord-action)
                                                                (coord-deduct-x-from-fn (:deduct-x-from local-coord-action))
                                                                (:deduct-y-from local-coord-action)
                                                                (coord-deduct-y-from-fn (:deduct-y-from local-coord-action))
                                                                (:swap-x-and-y local-coord-action)
                                                                coord-swap-x-and-y-fn)
                                             new-global-coords (face-i+coords->global-coords [new-face new-local-coords])]
                                         [new-global-coords new-dirn]))]
                                 (if (contains? wall-coords-set next-coord)
                                   [curr-coord curr-facing-dir]
                                   [next-coord next-facing-dir]))))
      answer-2 (let [final-state        (reduce (fn [{:as state :keys [coords facing-direction]} next-step-or-direction]
                                                  (if (string? next-step-or-direction)
                                                    ;; just update facing direction
                                                    (let [next-facing-direction (next-facing-direction-fn facing-direction next-step-or-direction)]
                                                      (assoc state :facing-direction next-facing-direction))
                                                    ;; loop for next-step-or-direction steps
                                                    (let [[new-coords new-dirn]
                                                          (loop [coords-i coords
                                                                 dirn-i   facing-direction
                                                                 i        next-step-or-direction]
                                                            #_(println "coords:" coords-i dirn-i)
                                                            (if (zero? i)
                                                              [coords-i dirn-i]
                                                              (let [[next-coord next-dirn] (next-coord+dirn-fn-2 coords-i dirn-i)]
                                                                (recur next-coord
                                                                       next-dirn
                                                                       (dec i)))))]
                                                      (assoc state :coords new-coords
                                                             :facing-direction new-dirn))))
                                                {:coords           (face-top-left-points 1)
                                                 :facing-direction :right}
                                                steps-and-dirs-seq)
                     {:keys [coords facing-direction]} final-state
                     row-num (inc (first coords))
                     col-num (inc (second coords))]
                 (+ (* 1000 row-num)
                    (* 4 col-num)
                    (facing-direction->value facing-direction)))]
  [answer-1 answer-2])
;; sample answers: [6032 5031]
