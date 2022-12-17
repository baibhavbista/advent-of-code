(ns advent-of-code.2022.day15
  "Day 14: Day 15: Beacon Exclusion Zone https://adventofcode.com/2022/day/15"
  (:require
   [clojure.string :as str]
   [clojure.set]))

(def input-filename "inputs/2022/day15.txt")

(defn manhattan-distance [[a-x a-y] [b-x b-y]]
  (+ (Math/abs (- b-x a-x)) (Math/abs (- b-y a-y))))

(manhattan-distance [2 18] [-2 15])

(defn line-str->sensor-data [line-str]
  (let [[_ & coord-strs]
        (re-matches #"Sensor at x=([+-]?\d+), y=([+-]?\d+): closest beacon is at x=([+-]?\d+), y=([+-]?\d+)"
                    line-str)
        [s-x s-y b-x b-y] (map #(Integer/parseInt %) coord-strs)
        sensor-coord [s-x s-y]
        nearest-beacon-coord [b-x b-y]]
    {:sensor-coord               sensor-coord
     :nearest-beacon-coord       nearest-beacon-coord
     :distance-to-nearest-beacon (manhattan-distance sensor-coord nearest-beacon-coord)})) 

(let [s "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"]
  (line-str->sensor-data s))

(def sensors-data
  (->> (slurp input-filename)
       (str/split-lines)
       (map line-str->sensor-data)))


(let [y-value 2000000
      coords-beacon-cant-be-present-in-acc-to-one-sensor
      (fn [sensor-data]
        (let [{:keys [sensor-coord nearest-beacon-coord distance-to-nearest-beacon]} sensor-data
              [sensor-x sensor-y] sensor-coord
              vertical-distance-from-sensor-to-line-y (Math/abs (- sensor-y y-value))
              is-sensor-feasible? (<= vertical-distance-from-sensor-to-line-y distance-to-nearest-beacon)]
          (when is-sensor-feasible?
            (let [[x-left-bound x-right-bound]
                  (let [possible-x-offset (- distance-to-nearest-beacon vertical-distance-from-sensor-to-line-y)]
                    [(- sensor-x possible-x-offset)
                     (+ sensor-x possible-x-offset)])
                  beacon-cant-be-present-in-this-coord?
                  (fn [coord]
                    (let [distance-to-coord (manhattan-distance sensor-coord coord)]
                      (boolean (and (not= coord nearest-beacon-coord)
                                    (<= distance-to-coord distance-to-nearest-beacon)))))]
              (->> (map vector (range x-left-bound (inc x-right-bound)) (repeat y-value))
                   (filter beacon-cant-be-present-in-this-coord?))))))]
  (->> sensors-data
       (map coords-beacon-cant-be-present-in-acc-to-one-sensor)
       (apply concat)
           (into #{})
         count))
;; answer 1: 5688618

;; PART 2
;; naive would be very very compute-heavy, since x and y can be between 0 and 4000000

;; mentions that there is only one possible position, which makes me think there is a hack
;; / a feature of the problem I'm not considering

;; say you have a sensor-coord. think of it's area as the square (with length from center to diagonal = distance-to-nearest-beacon). 
;; I call it a square because all sides are equal, but it's orientation makes it look diamond shaped
;; lets call the coords right outside of this area as the frontier 

;; I drew some figures and think that the only case where there is only a single possible position is the point is just outside the edges of at least 4 of these diamonds

(def coord-limit 4000000)

(defn tuning-freq [[x y]]
  (+ (* 4000000 x) y))

(defn sensor->frontier-coords [sensor-data]
  (let [{:keys [sensor-coord distance-to-nearest-beacon]} sensor-data
        [sensor-x sensor-y] sensor-coord
        outside-top-right-edge (loop [acc []
                                      next [sensor-x
                                            (- sensor-y (inc distance-to-nearest-beacon))]]
                                 (let [[next-x next-y] next]
                                   (if (or (not (<= 0 next-x coord-limit))
                                           (not (<= 0 next-y coord-limit)))
                                     acc
                                     (if (= next-y sensor-y)
                                       (conj acc next)
                                       (recur (conj acc next)
                                              [(inc next-x) (inc next-y)])))))
        outside-top-left-edge (loop [acc []
                                     next [sensor-x
                                           (- sensor-y (inc distance-to-nearest-beacon))]]
                                (let [[next-x next-y] next]
                                  (if (or (not (<= 0 next-x coord-limit))
                                          (not (<= 0 next-y coord-limit)))
                                    acc
                                    (if (= next-y sensor-y)
                                      (conj acc next)
                                      (recur (conj acc next)
                                             [(dec next-x) (inc next-y)])))))
        outside-bottom-right-edge (loop [acc []
                                         next [sensor-x
                                               (+ sensor-y (inc distance-to-nearest-beacon))]]
                                    (let [[next-x next-y] next]
                                      (if (or (not (<= 0 next-x coord-limit))
                                              (not (<= 0 next-y coord-limit)))
                                        acc
                                        (if (= next-y sensor-y)
                                          (conj acc next)
                                          (recur (conj acc next)
                                                 [(inc next-x) (dec next-y)])))))
        outside-bottom-left-edge (loop [acc []
                                        next [sensor-x
                                              (+ sensor-y (inc distance-to-nearest-beacon))]]
                                   (let [[next-x next-y] next]
                                     (if (or (not (<= 0 next-x coord-limit))
                                             (not (<= 0 next-y coord-limit)))
                                       acc
                                       (if (= next-y sensor-y)
                                         (conj acc next)
                                         (recur (conj acc next)
                                                [(dec next-x) (dec next-y)])))))]
    (into #{} (concat outside-top-right-edge outside-top-left-edge outside-bottom-left-edge outside-bottom-right-edge))))

(defn could-coord-have-beacon? [sensors-data coord]
  (loop [sensors-left-to-test sensors-data]
    #_(println "sensor-to-test " (first sensors-left-to-test))
    (if (empty? sensors-left-to-test)
      true
      (let [sensor-to-test (first sensors-left-to-test)
            {:keys [sensor-coord nearest-beacon-coord distance-to-nearest-beacon]} sensor-to-test
            distance-to-coord (manhattan-distance sensor-coord coord)]
        (if (or (= coord nearest-beacon-coord)
                (<= distance-to-coord distance-to-nearest-beacon))
          false
          (recur (rest sensors-left-to-test)))))))

(let [candidate-coords
      (->> sensors-data
           (map sensor->frontier-coords)
           (apply concat)
           frequencies
           (filter (comp #(<= 4 %) second))
           (sort-by second)
           reverse
           (map first))]
  (->> candidate-coords
       (filter (partial could-coord-have-beacon? sensors-data))
       first
       tuning-freq))

;; answer 2: 12625383204261