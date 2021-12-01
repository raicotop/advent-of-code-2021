(ns advent-of-code-2021.day-01)

(def task
  (->> (slurp "resources/day-01.txt")
       (clojure.string/split-lines)
       (map #(Integer/parseInt %))))

(defn solution-1
  [vals]
  (->> (partition 2 1 vals)
       (filter (fn [[a b]] (> b a)))
       (count)))

(defn solution-2
  [vals]
  (->> (partition 3 1 vals)
       (map #(reduce + %))
       (solution-1)))

(comment
  (solution-1 task) ;=> 1466
  (solution-2 task) ;=> 1491
,)