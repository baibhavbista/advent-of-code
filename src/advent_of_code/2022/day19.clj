(ns advent-of-code.2022.day19
  "Day 19: Not Enough Minerals https://adventofcode.com/2022/day/19"
  (:require
   [clojure.string :as str]
   [clojure.set]))

;; Hello graph search my old friend
;; I still have some PTSD from day 16

(def input-filename "inputs/2022/day19.txt")

;; ordering 
;; [ore clay obsidian geode]

(defn blueprint-spec-line->bot-costs [blueprint-spec-line]
  (let [[_ & num-strs]
        (re-matches #"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
                    blueprint-spec-line)
        [_blueprint-id oro cro bro brc gro grb] (map #(Integer/parseInt %) num-strs)]
    [[oro 0   0   0]
     [cro 0   0   0]
     [bro brc 0   0]
     [gro 0   grb 0]]))

(def blueprints 
  (->> (slurp input-filename)
       (str/split-lines)
       (map blueprint-spec-line->bot-costs)))

(defn materials-at-end-of-minute [robots materials]
  (map + materials robots))

(defn can-create-bot-right-now? [materials bot-costs]
  (every? #(>= % 0) (map - materials bot-costs)))

(defn can-create-bot-in-future-given-robots? [robots bot-cost]
  (every? true? (map (fn [bot-num material-req]
                       (if (pos-int? material-req)
                         (pos-int? bot-num)
                         true))
                     robots
                     bot-cost)))

(defn state-after-creating-next-bot 
  [bots-costs bot-type-i {:as state :keys [robots materials mins-left]}]
  (let [bot-cost (nth bots-costs bot-type-i)]
    (when (and state (can-create-bot-in-future-given-robots? robots bot-cost))
      (let [[final-materials mins-left bot-created?]
            (loop [materials materials
                   mins-left mins-left]
              (if-not (< 0 mins-left)
                [materials mins-left false]
                (if (can-create-bot-right-now? materials bot-cost)
                  [(->> (map - materials bot-cost)
                        (materials-at-end-of-minute robots))
                   (dec mins-left)
                   true]
                  (recur (materials-at-end-of-minute robots materials)
                         (dec mins-left)))))
            final-robots (if bot-created?
                           (update robots bot-type-i inc)
                           robots)]
        {:robots    final-robots
         :materials final-materials
         :mins-left mins-left}))))

(defn max-possible-score [{:as _state :keys [robots materials mins-left]}]
  (let [num-geodes       (nth materials 3)
        num-geode-robots (nth robots 3)
        num-geodes-if-build-one-per-minute (quot (- (* mins-left mins-left) mins-left) 2)]
    (+ num-geodes (* num-geode-robots mins-left) num-geodes-if-build-one-per-minute)))

(defn possible-next-states [bots-costs {:as state :keys [mins-left]}]
  (let [next-states-for-each-kind-of-bot (map #(state-after-creating-next-bot bots-costs % state)
                                              (range (count bots-costs)))
        next-state-for-geode-bot (nth next-states-for-each-kind-of-bot 3)]
    ;; if can create geode bot in next step, choose only that step 
    (if (and next-state-for-geode-bot (= (dec mins-left) (:mins-left next-state-for-geode-bot)))
      (list next-state-for-geode-bot)
      (->> next-states-for-each-kind-of-bot
           (remove nil?)
           (sort-by :mins-left)))))


(defn largest-num-geodes-for-given-blueprint [mins-left bots-costs]
  (let [init-state {:robots    [1 0 0 0]
                    :materials [0 0 0 0]
                    :mins-left mins-left}
        ;; if you cannot use more than x of a resource in a minute, never build more than x of that resource's robot
        max-material-which-can-be-used-in-a-minute (apply map max bots-costs)
        robots-num-does-not-exceed-max-material-which-can-be-used-in-a-minute? 
        (fn [{:as _state :keys [robots]}]
          (every? identity (map >= (take 3 max-material-which-can-be-used-in-a-minute) (take 3 robots))))]
    (loop [next-state-stack  (list init-state)
           max-score-upto-now 0]
      (if (empty? next-state-stack)
        max-score-upto-now
        (let [{:as state :keys [mins-left materials]}
              (first next-state-stack)
              #_#__ (println "state: " state (rest next-state-stack))
              next-state-stack (rest next-state-stack)]
          (if (= mins-left 0)
            (let [score (nth materials 3)]
              #_(println "score: " score max-score-upto-now)
              (recur next-state-stack
                     (max score max-score-upto-now)))
            (let [valid-next-states (->> state
                                         (possible-next-states bots-costs)
                                         (filter robots-num-does-not-exceed-max-material-which-can-be-used-in-a-minute?)
                                         (filter #(>= (max-possible-score %) max-score-upto-now)))]
              (recur (apply conj next-state-stack valid-next-states)
                     max-score-upto-now))))))))

(time
 (->> blueprints
      (pmap (partial largest-num-geodes-for-given-blueprint 24))
      (map * (range 1 (inc (count blueprints))))
      (apply +)))

;; part 1: 1127


(time
 (println 
  (->> blueprints
       (take 3)
       (pmap (partial largest-num-geodes-for-given-blueprint 32))
       (apply *))))

;; part 2: 21546