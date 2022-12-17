(ns advent-of-code.2022.day16
  "Day 16: Proboscidea Volcanium https://adventofcode.com/2022/day/16"
  (:require
   [clojure.string :as str]
   [clojure.set]))

(def input-filename "inputs/2022/day16.txt")

(defn valve-spec-str->valve-spec [valve-spec-str]
  (let [[_ valve-name flow-rate-str connected-valves-str]
        (re-matches #"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? ([,\w\s]+)"
                    valve-spec-str)
        connected-valves (str/split connected-valves-str #", ")
        flow-rate (Integer/parseInt flow-rate-str)]
    [valve-name {:flow-rate flow-rate
                 :connected-valves connected-valves}]))

(def valve-specs (->> (slurp input-filename)
                      str/split-lines
                      (map valve-spec-str->valve-spec)
                      (into {})))

(def TOTAL-MINUTES 30)

(defn move-to-valve-step [state new-valve]
  (assoc state :at-valve new-valve))

(defn open-valve-step
  [{:as state :keys [at-valve pressure-if-stop-exploration opened-valves-set]} minutes-left]
  (let [flow-rate (get-in valve-specs [at-valve :flow-rate])]
    (assoc state
           :pressure-if-stop-exploration (+ pressure-if-stop-exploration (* flow-rate (dec minutes-left)))
           :opened-valves-set            (conj opened-valves-set at-valve))))

(defn get-next-states [state minutes-left]
  (let [{:keys [at-valve opened-valves-set]} state
        connected-valves (get-in valve-specs [at-valve :connected-valves])]
    (if (= opened-valves-set (keys valve-specs))
      ;; no further movement necessary
      (list state)
      (cond-> (map (partial move-to-valve-step state) connected-valves)
        (not (or (opened-valves-set at-valve)
                 ;; I did not take a look at the inputs until super late and missed on what seems to be the most important optimization
                 (= 0 (get-in valve-specs [at-valve :flow-rate]))))
        (conj (open-valve-step state minutes-left))))))

(defn compress-frontier-by-merging-states-different-only-in-pressure [frontier]
  (->> frontier
       (group-by (juxt :at-valve :opened-valves-set))
       (map (fn [[_common-vals maybe-multiple-states-with-maybe-diff-pressure]]
              (let [max-pressure (->> maybe-multiple-states-with-maybe-diff-pressure
                                      (map :pressure-if-stop-exploration)
                                      (apply max))]
                (assoc (first maybe-multiple-states-with-maybe-diff-pressure) :pressure-if-stop-exploration max-pressure))))))

(time (let [all-states-after-30-mins
            (loop [minutes-left TOTAL-MINUTES
                   frontier     '({:at-valve                     "AA"
                                   :pressure-if-stop-exploration 0
                                   :opened-valves-set            #{}})]
              #_(println "loop: minutes-left (count frontier): " minutes-left (count frontier))
              (if (zero? minutes-left)
                frontier
                (recur (dec minutes-left)
                       (->> frontier
                            (mapcat #(get-next-states % minutes-left))
                            compress-frontier-by-merging-states-different-only-in-pressure))))]
        (->> all-states-after-30-mins
             (apply max-key :pressure-if-stop-exploration)
             :pressure-if-stop-exploration)))

;; answer 1: 1641

;; PART 2 is kicking my ass

;; the only idea I have for it right now:
;;  divide the number of (non-zero flow) valves (I have 15 in my dataset) into 2 groups
;;  I think that is 2^15 combinations. Above code is taking 1500 msec for 26 minutes 
;;  so that would be (/ (* (Math/pow 2 15) 1500) 1000 60) = 819 minutes lollll

;; giving up on this for today
;; FIXME: Come back later and solve part 2 for this problem. Might need to look into graph algorithms (doesn't look like BFS/DFS/A* is going to cut it for this one)