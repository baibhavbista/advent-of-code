(ns advent-of-code.2022.day18
  "Day 18: Boiling Boulders https://adventofcode.com/2022/day/18"
  (:require
   [clojure.string :as str]
   [clojure.set]))


(def input-filename "inputs/2022/day18.txt")

(def set-all-cubes
  (->> (slurp input-filename)
       str/split-lines
       (map (fn [line] (->> (str/split line #",")
                            (mapv #(Integer/parseInt %)))))
       (into #{})))

(defn add-coords [coords-a coords-b]
  (map + coords-a coords-b))

;; surrounding only means the ones touching each face
;;     NOT diagonally
(defn surrounding-cubes-coords [cube-coord]
  (let [offsets  [[1 0 0] [-1 0 0]
                  [0 1 0] [0 -1 0]
                  [0 0 1] [0 0 -1]]]
    (map (partial add-coords cube-coord) offsets)))

(defn exposed-sides-count [set-all-cubes cube-coord]
  (let [covered-faces-count (->> cube-coord
                                 surrounding-cubes-coords
                                 (filter set-all-cubes)
                                 count)]
    (- 6 covered-faces-count)))

(assert (= 5  (let [cube-coord [1 1 1]
                    set-all-cubes #{[1 1 1] [2 1 1]}]
                (exposed-sides-count set-all-cubes cube-coord))))

(->> set-all-cubes
     (map (partial exposed-sides-count set-all-cubes))
     (apply +))
;; answer 1: 4504

;; PART 2

;; imagine a cuboid which completely surrounds the entire lava droplet (with minimum 1 "padding" in all directions")
;; because of the padding, we can consider one corner point as the starting point and then 
;;   breadth first traversal into all possible points to get the `set-of-all-points-which-can-be-reached-by-water-or-steam`
;; then we can easily get `coords-surrounding-lava-which-cant-be-reached` and use that to figure out external surface area
(let [[x-max y-max z-max]
      (->> set-all-cubes
           (apply map max)
           (map inc))
      [x-min y-min z-min]
      (->> set-all-cubes
           (apply map min)
           (map dec))
      within-bounds-fn (fn [[coord-x coord-y coord-z]]
                         (and (<= x-min coord-x x-max)
                              (<= y-min coord-y y-max)
                              (<= z-min coord-z z-max)))
      starting-point [x-min y-min z-min]
      ;; breadth first traversal 
      set-of-all-points-which-can-be-reached-by-water-or-steam
      (loop [visited-set #{}
             frontier    #{starting-point}]
        #_(println "loop: (count frontier):" (count frontier))
        (if (empty? frontier)
          visited-set
          (let [new-frontier*   (->> frontier
                                     (mapcat surrounding-cubes-coords)
                                     (filter within-bounds-fn)
                                     (into #{}))
                new-frontier    (clojure.set/difference new-frontier* visited-set set-all-cubes)
                new-visited-set (clojure.set/union visited-set frontier)]
            (recur new-visited-set new-frontier))))
      lava-coord->externally-exposed-sides-count
      (fn [lava-coord]
        (let [surrounding-coords-set (into #{} (surrounding-cubes-coords lava-coord))
              surrounding-coords-exposed-externally
              ;; don't need to account for coords in set-all-cubes here because they are not contained in `set-of-all-points-which-can-be-reached-by-water-or-steam` anyways
              (clojure.set/intersection surrounding-coords-set
                                        set-of-all-points-which-can-be-reached-by-water-or-steam)]
          (count surrounding-coords-exposed-externally)))]
  (->> set-all-cubes
       (map lava-coord->externally-exposed-sides-count)
       (apply +)))
;; answer 2: 2556