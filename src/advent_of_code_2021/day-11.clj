(ns advent-of-code-2021.day-11)

(def task
  (->> (slurp "resources/day-11.txt")
       (clojure.string/split-lines)
       (map (fn [row] (map #(Byte/parseByte (str %)) row)))
       (map vec)
       (vec)))

(defn sol
  [task part]
  (loop [field task
         flashes-total 0
         turn 0
         to-increase []
         flashed-in-turn #{}]
    (if (empty? to-increase)
      (if (= 100 (if (= 1 part) turn (count flashed-in-turn)))
        (if (= 1 part) flashes-total turn)
        (let [to-increase' (vec (for [x (range (count (first task)))
                                      y (range (count task))]
                                  [x y]))]
          (recur field flashes-total (inc turn) to-increase' #{})))
      (let [[x y] (peek to-increase)
            element-value (get-in field [x y] nil)]
        (if (nil? element-value)
          (recur field flashes-total turn (pop to-increase) flashed-in-turn)
          (if (= 9 element-value)
            (let [field' (assoc-in field [x y] 0)
                  flashes-total' (inc flashes-total)
                  to-increase' (->> (for [x [-1 0 1]
                                          y [-1 0 1]
                                          :when (not (= x y 0))]
                                      [x y])
                                    (map (fn [[x' y']]
                                           [(+ x x') (+ y y')]))
                                    (concat (pop to-increase))
                                    (vec))]
              (recur field' flashes-total' turn to-increase' (conj flashed-in-turn [x y])))
            (let [field' (update-in field [x y] #(if (contains? flashed-in-turn [x y]) 0 (inc %)))
                  to-increase' (pop to-increase)]
              (recur field' flashes-total turn to-increase' flashed-in-turn))))))))

(defn sol-1
  [task]
  (sol task 1))

(defn sol-2
  [task]
  (sol task 2))

(comment
  (sol-1 task) ;=> 1785
  (sol-2 task) ;=> 354
  )