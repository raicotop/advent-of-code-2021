(ns advent-of-code-2021.day-02)

(def task
  (->> (slurp "resources/day-02.txt")
       (clojure.string/split-lines)
       (map #(clojure.string/split % #" "))
       (map (fn [[command value]] 
              [(keyword command) (Integer/parseInt value)]))))

(defn sol-1
  [all-instructions]
  (loop [x 0
         y 0
         instructions all-instructions]
    (if (empty? instructions)
      (* x y)
      (let [[k v] (first instructions)
            dx (if (= k :forward) v 0)
            dy (if (= k :down) v (if (= k :up) (- v) 0))]
        (recur (+ x dx) (+ y dy) (rest instructions))))))

(defn sol-2
  [all-instructions]
  (loop [x 0
         y 0
         aim 0
         instructions all-instructions]
    (if (empty? instructions)
      (* x y)
      (let [[k v] (first instructions)
            dx (if (= k :forward) v 0)
            dy (if (= k :forward) (* v aim) 0)
            daim (if (= k :down) v (if (= k :up) (- v) 0))]
        (recur (+ x dx) (+ y dy) (+ aim daim) (rest instructions))))))

(comment
  (sol-1 task) ;=> 1990000
  (sol-2 task) ;=> 1975421260
  )