(ns advent-of-code.2022.day24
  "Day 24: Blizzard Basin https://adventofcode.com/2022/day/24"
  (:require
   [clojure.string :as str]
   [clojure.set]))

;;;; UTILS start

;; got this from @cgrand's stack overflow answer https://stackoverflow.com/a/4831170
;; and adapted to get pred
(defn find-thing [needle haystack]
  (keep-indexed #(when (= %2 needle) %1) haystack))

;; graph search got from https://gist.github.com/xfthhxk/3810ecfaae8f066e2162ab02108ccadb
;; and then slightly modified for early stopping
(defn graph-search
  "returns [item-if-satisfies-early-stopping-condn ctx]"
  [coll graph f ctx early-stop? start]
  (loop [coll (conj coll start)
         visited #{}
         ctx ctx]
    #_(println "graph-search-inside-loop:" (peek coll))
    (cond
      (empty? coll) [nil ctx]
      (visited (peek coll)) (recur (pop coll) visited ctx)
      :else (let [curr (peek coll)
                  node (graph curr)
                  #_#__ (println "node:" node)
                  ctx (f node ctx)]
              (if (early-stop? node)
                [node ctx]
                (recur (into (pop coll) (:children node))
                       (conj visited curr)
                       ctx))))))

(def bfs (partial graph-search clojure.lang.PersistentQueue/EMPTY))

(def dfs (partial graph-search []))

(defn add-2-item-vecs [[a-x a-y] [b-x b-y]]
  [(+ a-x b-x) (+ a-y b-y)])

;;;; UTILS stop

(def sample-input-filename "inputs/2022/day24-sample.txt")
(def input-filename        "inputs/2022/day24.txt")

;; x axis is down, y axis is right

(defn all-coords-in-2D-grid-matching-needle [grid-2d needle]
  (->> grid-2d
       (map (fn [grid-row] (find-thing needle grid-row)))
       (map-indexed (fn [row-i col-is]
                      (map #(vector row-i %) col-is)))
       (apply concat)))

;; (next-step-blizzard-coords->dirn)

;; we can think of a state as [pos-of-expedition minute-wrapped]
;; start is [start-coord 0]

(let [lines-of-grid (->> input-filename
                         slurp
                         str/split-lines)
      start-coord (vector 0
                          (first (find-thing \. (first lines-of-grid))))
      end-coord   (vector (dec (count lines-of-grid))
                          (first (find-thing \. (last lines-of-grid))))
      dims [(count lines-of-grid) (count (first lines-of-grid))]
      non-wall-dims (add-2-item-vecs dims [-2 -2])
      valid-pos? (let [[x-min-wall x-max-wall y-min-wall y-max-wall]
                       [0 (dec (first dims)) 0 (dec (second dims))]]
                   (fn [coord]
                     (or (= coord start-coord)
                         (= coord end-coord)
                         (and (< x-min-wall (first coord)  x-max-wall)
                              (< y-min-wall (second coord) y-max-wall)))))
      dirn->offset {:up    [-1 0]
                    :right [0 1]
                    :down  [1 0]
                    :left  [0 -1]}
      ;; pretty inefficient but input should not be too large
      [#_wall-coords up-coords right-coords down-coords left-coords]
      (map #(all-coords-in-2D-grid-matching-needle lines-of-grid %) [#_\# \^ \> \v \<])
      starting-coords+dirn (->> (concat (map #(vector % :up)    up-coords)
                                        (map #(vector % :right) right-coords)
                                        (map #(vector % :down)  down-coords)
                                        (map #(vector % :left)  left-coords))
                                (into #{}))
      blizzard-history (loop [blizzard-history [starting-coords+dirn]]
                         (let [coords+dirn (last blizzard-history)
                               next-blizzard-state (->> coords+dirn
                                                        (map (fn [[blizzard-coord blizzard-dirn]]
                                                               (let [coord-non-wall-space (add-2-item-vecs blizzard-coord [-1 -1])
                                                                     [x y]            (add-2-item-vecs coord-non-wall-space (dirn->offset blizzard-dirn))
                                                                     new-coord-non-wall-space [(mod x (first  non-wall-dims))
                                                                                               (mod y (second non-wall-dims))]
                                                                     new-coord (add-2-item-vecs new-coord-non-wall-space [1 1])]
                                                                 [new-coord blizzard-dirn])))
                                                        (into #{})
                                                        #_(into {}))]
                           ;; if cycle found, stop
                           (if-let [_last-repeat-i (first (find-thing next-blizzard-state blizzard-history))]
                             blizzard-history
                             (recur (conj blizzard-history next-blizzard-state)))))
      ;; "just" because it may collapse multiple blizzards at same place
      blizzard-pos-history-one-cycle (mapv #(into #{} (map first %)) blizzard-history)
      ;; wrapped on the basis of blizzard cycle
      blizzard-minute-wrapped-fn (fn [minute] (mod minute (count blizzard-pos-history-one-cycle)))
      blizzard-pos-set-at-minute-fn (fn [minute] (nth blizzard-pos-history-one-cycle (blizzard-minute-wrapped-fn minute)))
      possible-next-states
      (fn [curr-state]
        (let [[curr-coord minute] curr-state
              next-minute (inc minute)
              blizzard-pos-set (blizzard-pos-set-at-minute-fn next-minute)
              ;; can either not move, or can only move up, down, left or right
              offsets-can-move-in [[-1 0] [1 0] [0 -1] [0 1] [0 0]]
              possible-next-coords (map #(add-2-item-vecs curr-coord %) offsets-can-move-in)
              next-coords (->> possible-next-coords
                               (remove blizzard-pos-set)
                               (filter valid-pos?))]
          (map #(vector % next-minute) next-coords)))
      graph-fn (fn [curr-state]
                 {:curr-state curr-state
                  :children   (possible-next-states curr-state)})
      early-stop-fn-factory? (fn [end-coord]
                               (fn [{:keys [curr-state]}]
                                 (let [[curr-coord _minute] curr-state]
                                   (= curr-coord end-coord))))

      bfs-for-this-problem (partial bfs graph-fn (fn [_node ctx] ctx) nil)

      ;; time taken to get from start-coord to end-coord 
      start-state [start-coord 0]
      answer-1 (->> start-state
                    (bfs-for-this-problem (early-stop-fn-factory? end-coord))
                    ((comp :curr-state first))
                    ;; state after reaching goal
                    second)

      ;; time required to reach the goal, go back to the start, then reach the goal again
      answer-2 (->> start-state
                    (bfs-for-this-problem (early-stop-fn-factory? end-coord))
                    ((comp :curr-state first))
                    ;; state after reaching goal first time
                    (bfs-for-this-problem (early-stop-fn-factory? start-coord))
                    ((comp :curr-state first))
                    ;; state after reaching start second time
                    (bfs-for-this-problem (early-stop-fn-factory? end-coord))
                    ((comp :curr-state first))
                    ;; state after reaching goal again (second time)
                    second)]
  [answer-1 answer-2])