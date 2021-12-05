(ns advent-of-code-2021.day-05)

(def task
  (->> (slurp "resources/day-05.txt")
       (clojure.string/split-lines)
       (map #(re-seq #"\d+" %))
       (map #(map (fn [str] (Integer/parseInt str)) %))))

(defn my-range 
  [a b]
  (if (< a b)
    (range a (inc b))
    (range a (dec b) -1)))

(defn sol
  [task remove-diagonal?]
  (->> task
       (filter (if remove-diagonal?
                 (fn [[x0 y0 x1 y1]]
                   (or (= x0 x1)
                       (= y0 y1)))
                 (fn [_] true)))
       (map (fn [[x0 y0 x1 y1]]
              (if (= x0 x1)
                (->> (my-range y0 y1)
                     (map #(vector x0 %)))
                (if (= y0 y1)
                  (->> (my-range x0 x1)
                       (map #(vector % y0)))
                  (->> (vector (my-range x0 x1) (my-range y0 y1))
                       (reduce interleave)
                       (partition 2))))))
       (reduce into)
       (frequencies)
       (filter (fn [[k v]] (> v 1)))
       (count)))

(defn sol-1
  [task]
  (sol task true))

(defn sol-2
  [task]
  (sol task false))

(comment 
  (sol-1 task) ;=> 6572
  (sol-2 task) ;=> 21466
  )