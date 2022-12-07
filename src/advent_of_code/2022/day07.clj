(ns advent-of-code.2022.day07
  "https://adventofcode.com/2022/day/7"
  (:require
   [clojure.string :as str]))

;; assumption is that cd and ls are the only commands
;; this makes things simpler via:
;; 1. any line which does not start with a $ is a file/directory under current-dir
;; 2. you can just ignore the `$ ls` lines

(def input-filename "inputs/2022/day07.txt")

(def terminal-lines 
  (->> (slurp input-filename)
       (str/split-lines)))
 
(defn cmd-lexer [terminal-line]
  (re-matches #"\$ (cd|ls)\s*([^ ]+)?" terminal-line))

;; don't really like that I hacked it doing a memoize, but tired and not enough mental energy to refactor it into a better solution
(def calculate-size
  (memoize 
   (fn [dir-to-files {:keys [type path size] :as file-or-dir-map}] 
     (if (= type :file)
       size
       (->> (get dir-to-files path)
            (map (partial calculate-size dir-to-files))
            (apply +))))))

(let [dir-to-files (:dir-to-files
                    (reduce (fn [{:keys [current-pathv] :as acc-map} terminal-line]
                              (if (str/starts-with? terminal-line "$")
                                (let [[_full-match cmd arg] (cmd-lexer terminal-line)]
                                  (case cmd
                                    "ls" acc-map
                                    "cd" (assoc acc-map :current-pathv (case arg
                                                                         "/"  []
                                                                         ".." (pop current-pathv)
                                                                         (conj current-pathv arg)))
                                    {:error (str "Unsupported command: " cmd)}))
                                ;; if not command then it means it is a file in :current-pathv
                                (let [[dir-or-size name] (str/split terminal-line #" ")
                                      file-map (if (= dir-or-size "dir")
                                                 {:type :dir
                                                  :path (conj current-pathv name)}
                                                 {:type :file
                                                  :name name
                                                  :size (Integer/parseInt dir-or-size)})]
                                  (update-in acc-map [:dir-to-files current-pathv] conj file-map))))
                            {:dir-to-files  {}
                             :current-pathv []}
                            terminal-lines))
      dir-pathv->size (->> (vals dir-to-files)
                           flatten
                           (filter #(= :dir (:type %)))
                           (cons {:type :dir
                                  :path []})
                           (map (juxt :path #(calculate-size dir-to-files %)))
                           (into {}))
      ;; part 1
      max-size-threshold 100000
      part-1-answer   (->> (vals dir-pathv->size)
                           (filter #(<= % max-size-threshold))
                           (apply +))
      
      ;; part 2
      total-disk-space 70000000
      req-min-unused-space 30000000
      current-unused-space (- total-disk-space (get dir-pathv->size []))
      space-needed         (- req-min-unused-space current-unused-space)
      part-2-answer        (->> (vals dir-pathv->size)
                                (filter #(> % space-needed))
                                (apply min))]
  [part-1-answer part-2-answer])

;; solution: [1908462 3979145]