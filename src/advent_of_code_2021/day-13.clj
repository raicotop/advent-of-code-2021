(ns advent-of-code-2021.day-13)

(def task
  (->> (slurp "resources/day-13.txt")
       (clojure.string/split-lines)
       (partition-by #{""})
       ((fn [[a _ b]]
         (vector 
          (->> (map #(clojure.string/split % #",") a)
               (map (fn [coords] (map #(Integer/parseInt %) coords))))
          (->> (map #(clojure.string/split % #" ") b)
               (map last)
               (map #(clojure.string/split % #"="))
               (map (fn [[s n]] [s (Integer/parseInt n)]))))))))

(defn sol
  [task end?]
  (loop [points (first task)
         folds (second task)]
    (if (end? folds)
        (set points)
        (let [[s n] (first folds)
              points' (if (= s "x")
                        (map (fn [[x y]] [(if (> x n) (- n (- x n)) x) y]) points)
                        (map (fn [[x y]] [x (if (> y n) (- n (- y n)) y)]) points))]
          (recur points' (rest folds))))))

(defn sol-1
  [task]
  (count (sol task #(not= (second task) %))))

(defn sol-2
  [task]
  (sol task empty?))

(defn visualize
  [points]
  (let [x-max (->> (map first points)
                   (reduce max))
        y-max (->> (map second points)
                   (reduce max))]
    (->> (for [y (range (inc y-max))
               x (range (inc x-max))]
           [x y])
         (map #(if (contains? points %) "#" "."))
         (partition (inc x-max))
         (map #(reduce str %)))))

(comment
  (sol-1 task) ;=> 669
  (visualize (sol-2 task)) ;=> ...
  )