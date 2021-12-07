(ns advent-of-code-2021.day-07)

(def task
  (->> (slurp "resources/day-07.txt")
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))

(defn sol-1-fn
  [a b]
  (Math/abs (- a b)))

(defn sol-2-fn
  [a b]
  (let [n (Math/abs (- a b))
        avg (/ (inc n) 2)]
    (* n avg)))

(defn sol
  [task distance-fn]
  (->> (vector min max)
       (map #(reduce % task))
       (apply range)
       (map #(vector % (->> (map (fn [x] (distance-fn x %)) task)
                            (reduce +))))
       (reduce (fn [x y] (if (< (second x) (second y)) x y)))
       (second)))

(defn sol-1
  [task]
  (sol task sol-1-fn))

(defn sol-2
  [task]
  (sol task sol-2-fn))

(comment
  (sol-1 task) ;=> 355521
  (sol-2 task) ;=> 100148777
  )